# ============================================================================
# 05_robustness.R — 27개 구성 SVAR 그리드 + 일관성 평가 + 대표 모형 선정
#                   (보고서 6장, 표 6·7, 그림 3)
# ----------------------------------------------------------------------------
# 교수님 피드백 반영:
#   1) 표본 분할 폐기 → 전체 표본(193개월) 단일 분석
#   2) 3단계 구조: [해외 1개] → [bok_rate + NEER] → [물가 1개]
#   3) 해외변수 3종(VIX·OIL·미국금리), bok 위치 3종(앞/뒤/없음), 물가 3종
#      → 3 × 3 × 3 = 27개 사양
#   4) 물가변수별 9개 사양의 ERPT 방향성(부호·크기) 일관성 평가
#      - 일관 → 가장 잘 설명하는 1개 채택
#      - 비일관 → "환율-물가 채널 식별 불가" 결론
#
# 선행: R/01 ~ R/02 실행 → df_diff 존재 (d_bok_rate, d_fed_rate 포함)
# 출력: results/grid_27.csv, results/consistency.csv, results/selected.csv,
#       figures/erpt_consistency_box.png
# ============================================================================

# ---- 0. 의존성 + 입력 확인 -------------------------------------------------
required_pkgs <- c("vars", "dplyr", "tidyr", "purrr", "tibble", "ggplot2")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

if (!exists("df_diff")) {
  stop("df_diff 가 없습니다. 먼저 R/02_preprocess.R 를 실행하세요.")
}
needed <- c("d_bok_rate", "d_fed_rate")
miss <- setdiff(needed, names(df_diff))
if (length(miss) > 0) {
  stop("df_diff 에 컬럼 없음: ", paste(miss, collapse = ", "))
}

set.seed(20260515)

OUT_DIR <- "results"   # 표(CSV)
FIG_DIR <- "figures"   # 그림(PNG)
for (d in c(OUT_DIR, FIG_DIR)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# ---- 1. 설정 ---------------------------------------------------------------
EXTERNALS  <- c("vix", "oil", "fed_rate")            # 해외 변수 3종
BOK_POS    <- c("before_neer", "after_neer", "absent") # 한국금리 위치 3종
PRICES     <- c("ipi", "ppi", "cpi")                 # 물가 3종

LAG_MAX        <- 12L
IRF_HORIZON    <- 24L
ERPT_HORIZONS  <- c(1, 3, 6, 12, 24)
SHOCK_VAR      <- "neer"

# 일관성 평가 임계값
SIGN_THRESHOLD <- 7/9     # 9개 중 7개 이상 동일 부호 → 부호 일관
CV_THRESHOLD   <- 0.5     # 변동계수 < 0.5 → 크기 일관

# ---- 2. 데이터 준비: df_diff → data_f (단축 컬럼명) ------------------------
# 외생 펄스 더미(d_201501, d_202204) 컬럼명 확보 — VAR exogen 으로 투입
DUMMY_COLS <- grep("^d_20", names(df_diff), value = TRUE)
cat(sprintf("\n외생 펄스 더미: %s\n",
            if (length(DUMMY_COLS) > 0) paste(DUMMY_COLS, collapse = ", ") else "없음"))

data_f <- df_diff %>%
  dplyr::select(
    date,
    neer     = dl_neer,
    ipi      = dl_import_price_index,
    ppi      = dl_ppi,
    cpi      = dl_cpi,
    oil      = dl_oil,
    vix      = dl_vix,
    bok_rate = d_bok_rate,
    fed_rate = d_fed_rate,
    dplyr::all_of(DUMMY_COLS)
  )

cat(sprintf("\n분석 표본: %s ~ %s (%d개월)\n",
            format(min(data_f$date), "%Y-%m"),
            format(max(data_f$date), "%Y-%m"),
            nrow(data_f)))

# ---- 3. 단일 사양 추정 함수 ------------------------------------------------
# 입력: external(vix/oil/fed_rate), price(ipi/ppi/cpi),
#       bok_position(before_neer/after_neer/absent)
# 출력: list(메타, 진단, ERPT, varx, svar)
fit_one <- function(external, price, bok_position, df,
                    lag_max = LAG_MAX, irf_horizon = IRF_HORIZON) {
  
  # 3-1. Cholesky 순서 구성 (해외 → bok+neer 조합 → 물가)
  mid_chain <- switch(bok_position,
                      before_neer = c("bok_rate", "neer"),  # 정책 → 환율
                      after_neer  = c("neer", "bok_rate"),  # 환율 → 정책
                      absent      = c("neer"),              # bok 미포함 (3변수)
                      stop("unknown bok_position")
  )
  endog_order <- c(external, mid_chain, price)
  n_var <- length(endog_order)

  endog_mat <- as.matrix(df[, endog_order, drop = FALSE])
  colnames(endog_mat) <- endog_order

  # 외생 펄스 더미 행렬 (있으면 exogen 으로 투입)
  exog_mat <- NULL
  if (exists("DUMMY_COLS") && length(DUMMY_COLS) > 0 &&
      all(DUMMY_COLS %in% colnames(df))) {
    exog_mat <- as.matrix(df[, DUMMY_COLS, drop = FALSE])
    colnames(exog_mat) <- DUMMY_COLS
  }

  # 3-2. 시차 선택 — SC(BIC) 우선, 단일 표본이라 자유도 충분
  lag_sel <- VARselect(endog_mat, lag.max = lag_max, type = "const", exogen = exog_mat)
  p_sc  <- as.integer(lag_sel$selection["SC(n)"])
  p_aic <- as.integer(lag_sel$selection["AIC(n)"])
  p_use <- p_sc

  # 3-3. Reduced-form VAR (외생 더미 포함)
  varx <- VAR(endog_mat, p = p_use, type = "const", exogen = exog_mat)

  # PT 실패 시 AIC 시차로 상향 (1회)
  pt_p0 <- tryCatch(
    serial.test(varx, lags.pt = 16, type = "PT.asymptotic")$serial$p.value,
    error = function(e) NA)
  if (!is.na(pt_p0) && pt_p0 < 0.05 && p_aic > p_sc) {
    p_use <- p_aic
    varx  <- VAR(endog_mat, p = p_use, type = "const", exogen = exog_mat)
  }
  
  # 3-4. SVAR (하삼각 A행렬)
  Amat <- diag(n_var)
  for (i in 2:n_var) for (j in 1:(i-1)) Amat[i, j] <- NA
  svar_mdl <- tryCatch(
    suppressWarnings(SVAR(varx, Amat = Amat, Bmat = NULL,
                          hessian = TRUE, max.iter = 5000)),
    error = function(e) NULL)
  svar_ok <- !is.null(svar_mdl)
  
  # 3-5. 진단검정
  pt_p   <- tryCatch(serial.test(varx, lags.pt = 16,
                                 type = "PT.asymptotic")$serial$p.value, error = function(e) NA)
  arch_p <- tryCatch(arch.test(varx, lags.multi = 5,
                               multivariate.only = TRUE)$arch.mul$p.value, error = function(e) NA)
  jb_p   <- tryCatch(normality.test(varx,
                                    multivariate.only = TRUE)$jb.mul$JB$p.value, error = function(e) NA)
  # VAR 안정성 (companion 고유근)
  roots_ok <- tryCatch(all(roots(varx) < 1), error = function(e) NA)
  # CUSUM 구조 안정성 (방정식별 OLS-CUSUM, strucchange::sctest)
  cusum_ok <- tryCatch({
    stab <- stability(varx, type = "OLS-CUSUM")
    ps   <- sapply(stab$stability, function(e) strucchange::sctest(e)$p.value)
    all(ps > 0.05, na.rm = TRUE)
  }, error = function(e) NA)
  
  # 3-6. ERPT (NEER 충격 → 물가 누적 / NEER 누적)
  # ERPT는 reduced-form VAR의 직교화(ortho=TRUE) IRF로 계산한다.
  # 이는 하삼각 A행렬 SVAR과 수학적으로 동일하며, SVAR() 수치최적화는
  # 수렴오차로 점추정치가 달라질 수 있어 IRF 산출에는 쓰지 않는다.
  # (Cpi_final_model.R의 부트스트랩 ERPT와 동일한 방식 → 두 결과가 일치한다.)
  irf_input <- varx
  irf_obj <- tryCatch(
    irf(irf_input, impulse = SHOCK_VAR, response = c(SHOCK_VAR, price),
        n.ahead = irf_horizon, ortho = TRUE, cumulative = TRUE, boot = FALSE),
    error = function(e) NULL)
  
  erpt_vec <- setNames(rep(NA_real_, length(ERPT_HORIZONS)),
                       paste0("h", ERPT_HORIZONS))
  if (!is.null(irf_obj)) {
    im <- irf_obj$irf[[SHOCK_VAR]]
    for (k in seq_along(ERPT_HORIZONS)) {
      h <- ERPT_HORIZONS[k]; idx <- h + 1L
      if (idx <= nrow(im) && im[idx, SHOCK_VAR] != 0)
        erpt_vec[k] <- im[idx, price] / im[idx, SHOCK_VAR]
    }
  }
  
  # 3-7. NEER FEVD 기여도 (24M, reduced-form 기준)
  neer_fevd24 <- tryCatch({
    fv <- fevd(varx, n.ahead = 24)[[price]]
    fv[24, "neer"]
  }, error = function(e) NA_real_)
  
  list(
    external = external, price = price, bok_position = bok_position,
    var_order = endog_order, n_var = n_var,
    lag_use = p_use, n_obs = nrow(endog_mat) - p_use,
    AIC = AIC(varx), BIC = BIC(varx),
    pt_p = pt_p, arch_p = arch_p, jb_p = jb_p,
    roots_ok = roots_ok, cusum_ok = cusum_ok, svar_ok = svar_ok,
    erpt = erpt_vec, neer_fevd24 = neer_fevd24,
    varx = varx, svar = svar_mdl, irf_obj = irf_obj
  )
}

# ---- 4. 27개 사양 그리드 추정 ----------------------------------------------
cat("\n", strrep("=", 60), "\n", sep = "")
cat("  27개 사양 추정 (해외3 × bok위치3 × 물가3)\n")
cat(strrep("=", 60), "\n", sep = "")

grid_res <- list()
for (ext in EXTERNALS) {
  for (pr in PRICES) {
    for (bp in BOK_POS) {
      key <- paste(ext, pr, bp, sep = "_")
      message(sprintf("  추정: %s", key))
      res <- tryCatch(fit_one(ext, pr, bp, data_f),
                      error = function(e) { message("    SKIP: ", e$message); NULL })
      if (!is.null(res)) grid_res[[key]] <- res
    }
  }
}

# ---- 5. 결과 표 (long → wide) ----------------------------------------------
grid_tbl <- purrr::imap_dfr(grid_res, function(r, k) {
  tibble(
    key          = k,
    external     = r$external,
    price        = r$price,
    bok_position = r$bok_position,
    n_var        = r$n_var,
    lag_use      = r$lag_use,
    n_obs        = r$n_obs,
    AIC          = round(r$AIC, 1),
    BIC          = round(r$BIC, 1),
    pt_p         = round(r$pt_p, 4),
    arch_p       = round(r$arch_p, 4),
    jb_p         = round(r$jb_p, 4),
    roots_ok     = r$roots_ok,
    cusum_ok     = r$cusum_ok,
    svar_ok      = r$svar_ok,
    erpt_h1      = round(r$erpt["h1"], 4),
    erpt_h6      = round(r$erpt["h6"], 4),
    erpt_h12     = round(r$erpt["h12"], 4),
    erpt_h24     = round(r$erpt["h24"], 4),
    neer_fevd24  = round(r$neer_fevd24 * 100, 2)
  )
})

cat("\n[27개 사양 그리드 결과]\n")
print(as.data.frame(grid_tbl %>% dplyr::select(
  external, price, bok_position, lag_use, BIC,
  pt_p, arch_p, erpt_h12, erpt_h24, neer_fevd24)),
  row.names = FALSE)

write.csv(grid_tbl, file.path(OUT_DIR, "grid_27.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- 6. 물가변수별 일관성 평가 ---------------------------------------------
cat("\n", strrep("=", 60), "\n", sep = "")
cat("  물가변수별 ERPT 방향성 일관성 평가 (24M 기준)\n")
cat(strrep("=", 60), "\n", sep = "")

consistency <- lapply(PRICES, function(pr) {
  sub <- grid_tbl %>% filter(price == pr)
  e24 <- sub$erpt_h24
  e24 <- e24[is.finite(e24)]
  n_total <- length(e24)
  
  n_neg   <- sum(e24 < 0)
  n_pos   <- sum(e24 > 0)
  dom_sign <- if (n_neg >= n_pos) "음(−)" else "양(+)"
  dom_cnt  <- max(n_neg, n_pos)
  sign_ratio <- dom_cnt / n_total
  sign_ok    <- sign_ratio >= SIGN_THRESHOLD
  
  mean_e <- mean(e24); sd_e <- sd(e24)
  cv <- if (abs(mean_e) > 1e-8) sd_e / abs(mean_e) else NA_real_
  size_ok <- !is.na(cv) && cv < CV_THRESHOLD
  
  verdict <- if (sign_ok && size_ok) "일관 (강건)"
  else if (sign_ok && !size_ok) "부호만 일관 (크기 산발)"
  else "비일관 (식별 곤란)"
  
  data.frame(
    물가      = toupper(pr),
    n         = n_total,
    지배부호  = dom_sign,
    동일부호  = sprintf("%d/%d", dom_cnt, n_total),
    부호일관  = ifelse(sign_ok, "✓", "✗"),
    평균ERPT  = round(mean_e, 4),
    표준편차  = round(sd_e, 4),
    변동계수  = round(cv, 3),
    크기일관  = ifelse(size_ok, "✓", "✗"),
    판정      = verdict,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

cat(sprintf("\n부호 일관 기준: 동일 부호 ≥ %.0f%% (%d/9)\n",
            SIGN_THRESHOLD * 100, ceiling(SIGN_THRESHOLD * 9)))
cat(sprintf("크기 일관 기준: 변동계수(SD/|평균|) < %.1f\n\n", CV_THRESHOLD))
print(consistency, row.names = FALSE)

write.csv(consistency, file.path(OUT_DIR, "consistency.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- 7. 채택 모형 선정 (일관된 물가변수에 대해, 기준 C) --------------------
cat("\n", strrep("=", 60), "\n", sep = "")
cat("  채택 모형 선정 (결과독립 기준: 진단 통과 + BIC 최저)\n")
cat(strrep("=", 60), "\n", sep = "")

select_best <- function(pr) {
  sub <- grid_tbl %>%
    filter(price == pr) %>%
    mutate(
      pass_pt    = !is.na(pt_p)   & pt_p   >= 0.10,
      pass_arch  = !is.na(arch_p) & arch_p >= 0.05,
      pass_root  = roots_ok %in% TRUE,
      pass_cusum = cusum_ok %in% TRUE,
      pass_all   = pass_pt & pass_arch & pass_root & pass_cusum
    )
  cand <- sub %>% filter(pass_all)
  if (nrow(cand) == 0) {
    cat(sprintf("  [%s] 진단을 모두 통과한 사양 없음 → 채택 보류\n", toupper(pr)))
    return(NULL)
  }
  # 선택은 결과(ERPT·FEVD)와 독립적인 기준으로만: 진단을 통과한 구성 중
  # BIC가 가장 낮은(적합도-간결성 최적) 구성을 채택한다.
  # 환율 FEVD는 선택 기준에서 제외 — 환율 기여도가 큰 모형을 고른 뒤
  # 환율 기여도를 해석하면 결과로 결과를 고르는 순환이 발생하기 때문이다.
  # FEVD는 모형을 정한 뒤 보고되는 결과로만 사용한다.
  cand <- cand %>% arrange(BIC)
  best <- cand[1, ]
  cat(sprintf("  [%s] 채택: %s_%s_%s | BIC=%.1f, ERPT24=%.4f (FEVD는 선택 후 보고)\n",
              toupper(pr), best$external, best$price, best$bok_position,
              best$BIC, best$erpt_h24))
  best
}

selected_list <- lapply(PRICES, function(pr) {
  v <- consistency$판정[consistency$물가 == toupper(pr)]
  if (grepl("비일관", v)) {
    cat(sprintf("  [%s] 일관성 평가: %s → 채택하지 않음\n", toupper(pr), v))
    return(NULL)
  }
  select_best(pr)
})
selected <- bind_rows(selected_list)

if (nrow(selected) > 0) {
  cat("\n[최종 채택 모형]\n")
  print(as.data.frame(selected %>% dplyr::select(
    external, price, bok_position, lag_use, BIC,
    pt_p, arch_p, erpt_h12, erpt_h24, neer_fevd24)),
    row.names = FALSE)
  write.csv(selected, file.path(OUT_DIR, "selected.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
} else {
  cat("\n⚠ 모든 물가변수에서 채택 모형 없음 — 식별 곤란 결론\n")
}

# ---- 8. 일관성 시각화 (물가별 9개 사양 ERPT 분포 상자그림) -----------------
price_kor <- c(ipi = "수입물가", ppi = "생산자물가", cpi = "소비자물가")
bok_kor   <- c(before_neer = "환율 앞", after_neer = "환율 뒤", absent = "미포함")

plot_df <- grid_tbl %>%
  filter(is.finite(erpt_h24)) %>%
  mutate(물가 = factor(price_kor[price],
                     levels = c("수입물가","생산자물가","소비자물가")),
         `금리 위치` = factor(bok_kor[bok_position],
                          levels = c("환율 앞","환율 뒤","미포함")))

p_box <- ggplot(plot_df, aes(x = 물가, y = erpt_h24)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_boxplot(fill = "gray88", color = "gray30",
               alpha = 0.9, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.85,
              aes(shape = `금리 위치`), color = "gray25") +
  scale_shape_manual(values = c("환율 앞" = 16, "환율 뒤" = 17, "미포함" = 15)) +
  labs(x = NULL, y = "24개월 누적 전가율", shape = "한국 기준금리 위치") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        axis.title.y  = element_text(size = 16),
        axis.text     = element_text(size = 13),
        legend.text   = element_text(size = 12))

ggsave(file.path(FIG_DIR, "erpt_consistency_box.png"),
       p_box, width = 9, height = 6, dpi = 150)
print(p_box)

# ---- 9. 환경 저장 ----------------------------------------------------------
assign("grid_res_f",     grid_res,     envir = .GlobalEnv)
assign("grid_tbl_f",     grid_tbl,     envir = .GlobalEnv)
assign("consistency_f",  consistency,  envir = .GlobalEnv)
assign("selected_f",     selected,     envir = .GlobalEnv)

cat("\n", strrep("=", 60), "\n", sep = "")
cat("  완료 ✓\n")
cat(sprintf("  추정 사양: %d개 | 출력: %s/, %s/\n", nrow(grid_tbl), OUT_DIR, FIG_DIR))
cat("  환경 객체: grid_res_f, grid_tbl_f, consistency_f, selected_f\n")
cat(strrep("=", 60), "\n", sep = "")

