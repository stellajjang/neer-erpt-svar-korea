# ============================================================================
# 04_svar_main.R — 소비자물가 대표 모형 상세 분석 (보고서 5장, 그림 1·2)
# ----------------------------------------------------------------------------
# 채택 모형: fed_rate -> bok_rate -> NEER -> CPI  (4변수, before_neer)
#   - 05_robustness.R 의 결과독립 기준(진단통과 + BIC 최저)으로
#     fed_rate_cpi_before_neer 채택 → 여기서는 그 구성을 고정해 상세 분석.
#     환율 FEVD는 선택 후 보고용으로만 사용.
#   - 시차 3 고정 (BIC=1은 자기상관 잔존, AIC=3에서 진단 통과)
#
# 식별: 하삼각 재귀구조(Cholesky). 하삼각 A행렬 SVAR과 수학적으로 동일하며
#       irf(varx, ortho=TRUE)로 직접 계산 → 부트스트랩이 훨씬 빠르고
#       SVAR 수치최적화 수렴 경고도 발생하지 않음.
#
# 선행: R/01 ~ R/02 실행 -> df_diff 존재
# 출력: figures/cpi_erpt_ci.png, figures/cpi_fevd.png,
#       results/cpi_erpt_boot.csv, results/cpi_fevd_neer.csv
# ============================================================================

suppressPackageStartupMessages({
  library(vars); library(dplyr); library(ggplot2); library(tidyr)
})

if (!exists("df_diff")) stop("먼저 R/02_preprocess.R 를 실행하세요.")

set.seed(20260515)
OUT_DIR <- "results"   # 표(CSV)
FIG_DIR <- "figures"   # 그림(PNG)
for (d in c(OUT_DIR, FIG_DIR)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# ---- 1. 채택 모형 구성 -----------------------------------------------------
ORDER       <- c("fed_rate", "bok_rate", "neer", "cpi")  # Cholesky 순서
IRF_HORIZON <- 24L
N_BOOT      <- 1000L
SHOCK       <- "neer"
RESP        <- "cpi"
LAG_FIX     <- 3L            # 27사양에서 진단 통과한 시차 (BIC=1은 PT 실패)

data_m <- df_diff %>%
  dplyr::transmute(
    fed_rate = d_fed_rate,
    bok_rate = d_bok_rate,
    neer     = dl_neer,
    cpi      = dl_cpi
  )
endog <- as.matrix(data_m[, ORDER])

# 외생 펄스 더미(d_201501, d_202204) — 27사양 그리드와 동일하게 exogen 투입
DUMMY_COLS <- grep("^d_20", names(df_diff), value = TRUE)
exog <- if (length(DUMMY_COLS) > 0) as.matrix(df_diff[, DUMMY_COLS, drop = FALSE]) else NULL

cat(sprintf("\n채택 모형: %s\n", paste(ORDER, collapse = " -> ")))
cat(sprintf("표본: %d개월 | 외생 더미: %s\n", nrow(endog),
            if (length(DUMMY_COLS) > 0) paste(DUMMY_COLS, collapse = ", ") else "없음"))

# ---- 2. 시차 진단: BIC vs 채택 시차 비교 -----------------------------------
lag_sel <- VARselect(endog, lag.max = 12, type = "const", exogen = exog)
cat(sprintf("\n[시차 진단] BIC 추천=%d, AIC 추천=%d, 채택=%d\n",
            lag_sel$selection["SC(n)"], lag_sel$selection["AIC(n)"], LAG_FIX))

varx_bic <- VAR(endog, p = as.integer(lag_sel$selection["SC(n)"]), type = "const", exogen = exog)
pt_bic <- serial.test(varx_bic, lags.pt = 16, type = "PT.asymptotic")$serial$p.value
cat(sprintf("  - BIC 시차 진단: PT=%.4f  (0.05 미만 -> 자기상관 남음, 부적합)\n", pt_bic))

# ---- 3. 채택 시차(3)로 VAR 추정 --------------------------------------------
p_use <- LAG_FIX
varx  <- VAR(endog, p = p_use, type = "const", exogen = exog)

# 진단
pt_p   <- serial.test(varx, lags.pt = 16, type = "PT.asymptotic")$serial$p.value
arch_p <- arch.test(varx, lags.multi = 5, multivariate.only = TRUE)$arch.mul$p.value
roots_ok <- all(roots(varx) < 1)
cusum_ok <- tryCatch({
  stab <- stability(varx, type = "OLS-CUSUM")
  ps   <- sapply(stab$stability, function(e) strucchange::sctest(e)$p.value)
  all(ps > 0.05, na.rm = TRUE)
}, error = function(e) NA)
cat(sprintf("  - 채택 시차(%d) 진단: PT=%.4f, ARCH=%.4f, 안정성=%s, CUSUM=%s\n",
            p_use, pt_p, arch_p, roots_ok,
            ifelse(is.na(cusum_ok), "NA", ifelse(cusum_ok, "통과", "구조변화 의심"))))

if (pt_p < 0.05) {
  cat("  [경고] 채택 시차에서도 PT 미통과. 시차 재검토 필요.\n")
} else {
  cat("  [OK] 진단 통과 — 부트스트랩 진행\n")
}

# ---- 4. 부트스트랩 IRF (누적, Cholesky 재귀식별) ---------------------------
# irf(varx, ortho=TRUE) = 하삼각 SVAR과 동일. SVAR() 수치최적화 불필요 → 빠름.
cat(sprintf("\n부트스트랩 IRF 계산 중 (%d회)... 약 10~30초\n", N_BOOT))
irf_cum <- irf(varx, impulse = SHOCK, response = c(SHOCK, RESP),
               n.ahead = IRF_HORIZON, ortho = TRUE, cumulative = TRUE,
               boot = TRUE, runs = N_BOOT, ci = 0.90)

# ---- 5. 누적 ERPT 궤적 = 누적 CPI 반응 / 누적 NEER 반응 --------------------
# 신뢰구간: CPI 누적반응의 부트스트랩 90% 하한·상한을 NEER 누적반응 점추정치로 나눈 값
neer_c <- irf_cum$irf[[SHOCK]][, SHOCK]
cpi_c  <- irf_cum$irf[[SHOCK]][, RESP]
lo_c   <- irf_cum$Lower[[SHOCK]][, RESP]
up_c   <- irf_cum$Upper[[SHOCK]][, RESP]

erpt_df <- tibble(
  horizon  = 0:IRF_HORIZON,
  erpt     = ifelse(neer_c != 0, cpi_c / neer_c, NA_real_),
  erpt_lo  = ifelse(neer_c != 0, lo_c  / neer_c, NA_real_),
  erpt_hi  = ifelse(neer_c != 0, up_c  / neer_c, NA_real_)
)

# ---- 6. FEVD (CPI 변동의 충격별 기여) --------------------------------------
fv <- fevd(varx, n.ahead = IRF_HORIZON)[[RESP]]
fevd_df <- as.data.frame(fv) %>%
  mutate(horizon = 1:n()) %>%
  pivot_longer(-horizon, names_to = "shock", values_to = "share") %>%
  mutate(shock = factor(shock, levels = ORDER,
                        labels = c("미국 기준금리","한국 기준금리","환율","소비자물가")))

# ---- 7. 그림 A: 누적 전가율 궤적 + 90% 신뢰구간 ----------------------------
pA <- ggplot(erpt_df, aes(horizon)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = erpt_lo, ymax = erpt_hi), fill = "gray70", alpha = 0.35) +
  geom_line(aes(y = erpt), color = "gray20", linewidth = 1.2) +
  labs(x = "기간(개월)", y = "누적 환율전가율") +
  theme_minimal(base_size = 14) +
  theme(axis.title = element_text(size = 16),
        axis.text  = element_text(size = 13))
ggsave(file.path(FIG_DIR, "cpi_erpt_ci.png"), pA, width = 8, height = 5, dpi = 150)
print(pA)

# ---- 8. 그림 B: 소비자물가 분산분해 면적도 --------------------------------
gray4 <- c("미국 기준금리" = "#2b2b2b", "한국 기준금리" = "#666666",
           "환율"         = "#aaaaaa", "소비자물가"   = "#dddddd")
pB <- ggplot(fevd_df, aes(horizon, share, fill = shock)) +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = gray4) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "기간(개월)", y = "기여 비율", fill = "구조 충격") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        axis.title  = element_text(size = 16),
        axis.text   = element_text(size = 13),
        legend.text = element_text(size = 12))
ggsave(file.path(FIG_DIR, "cpi_fevd.png"), pB, width = 8, height = 5, dpi = 150)
print(pB)

# ---- 9. 콘솔 요약표 --------------------------------------------------------
cat("\n[시점별 누적 ERPT + 90% 신뢰구간]\n")
erpt_tbl <- erpt_df %>%
  filter(horizon %in% c(1, 3, 6, 12, 24)) %>%
  mutate(across(c(erpt, erpt_lo, erpt_hi), ~round(., 4)),
         유의 = ifelse(erpt_lo < 0 & erpt_hi < 0, "유의(음)", "비유의"))
print(as.data.frame(erpt_tbl), row.names = FALSE)

cat("\n[CPI FEVD — 환율(NEER) 기여 비율]\n")
neer_share <- fevd_df %>% filter(shock == "환율", horizon %in% c(1,3,6,12,24)) %>%
  mutate(share = round(share * 100, 2))
print(as.data.frame(neer_share %>% dplyr::select(horizon, share)), row.names = FALSE)

# ---- 10. 보고서용 CSV 저장 + 환경 저장 -------------------------------------
# 부트스트랩 ERPT 표 (보고서 표/본문 수치 자동 주입용)
erpt_tbl_out <- erpt_df %>%
  filter(horizon %in% c(1, 3, 6, 12, 24)) %>%
  mutate(유의 = ifelse(erpt_lo < 0 & erpt_hi < 0, "유의(음)", "비유의"))
write.csv(erpt_tbl_out, file.path(OUT_DIR, "cpi_erpt_boot.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(neer_share %>% dplyr::select(horizon, share),
          file.path(OUT_DIR, "cpi_fevd_neer.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

assign("cpi_varx",  varx,     envir = .GlobalEnv)
assign("cpi_erpt",  erpt_df,  envir = .GlobalEnv)
assign("cpi_fevd",  fevd_df,  envir = .GlobalEnv)

cat("\n", strrep("=", 60), "\n", sep = "")
cat("  CPI 채택 모형 분석 완료\n")
cat(sprintf("  사용 시차: %d | 24M 누적 ERPT = %.4f\n",
            p_use, erpt_df$erpt[erpt_df$horizon == 24]))
cat("  그림: figures/cpi_erpt_ci.png, figures/cpi_fevd.png\n")
cat(strrep("=", 60), "\n", sep = "")
