############################################################
# 06_dummy_check.R — 보조 점검: 이상치 펄스 더미 포함 여부에 따른 24개월 전가율 비교
# ----------------------------------------------------------
# 05_robustness.R 과 같은 27개 구성을 더미 포함/미포함으로 각각 추정하고
# 24개월 누적 전가율의 차이와 부호 변화를 출력한다.
# 선행: R/02_preprocess.R (df_diff)
# 출력: results/dummy_effect_check.csv
############################################################
suppressPackageStartupMessages({
  library(vars); library(dplyr); library(purrr); library(tibble)
})

if (!exists("df_diff")) stop("먼저 R/02_preprocess.R 를 실행하세요.")

EXTERNALS     <- c("vix", "oil", "fed_rate")
BOK_POS       <- c("before_neer", "after_neer", "absent")
PRICES        <- c("ipi", "ppi", "cpi")
IRF_HORIZON   <- 24L
ERPT_HORIZONS <- c(1, 3, 6, 12, 24)
SHOCK_VAR     <- "neer"
dummy_cols    <- grep("^d_20", names(df_diff), value = TRUE)

data_v <- df_diff %>% dplyr::select(
  date,
  neer = dl_neer, ipi = dl_import_price_index,
  ppi = dl_ppi,  cpi = dl_cpi,
  oil = dl_oil,  vix = dl_vix,
  bok_rate = d_bok_rate, fed_rate = d_fed_rate,
  all_of(dummy_cols))

# ortho IRF 기반 fit_one (더미 유무만 파라미터)
fit_one <- function(external, price, bok_position, df, use_dummy = TRUE) {
  mid <- switch(bok_position,
    before_neer = c("bok_rate","neer"), after_neer = c("neer","bok_rate"),
    absent = "neer")
  ord  <- c(external, mid, price)
  endog <- as.matrix(df[, ord])
  exog  <- if (use_dummy && length(dummy_cols) > 0) as.matrix(df[, dummy_cols]) else NULL

  sel   <- VARselect(endog, lag.max=12, type="const", exogen=exog)
  p_sc  <- as.integer(sel$selection["SC(n)"])
  p_aic <- as.integer(sel$selection["AIC(n)"])
  varx  <- VAR(endog, p=p_sc, type="const", exogen=exog)
  pt0   <- tryCatch(serial.test(varx,lags.pt=16,type="PT.asymptotic")$serial$p.value, error=function(e) NA)
  if (!is.na(pt0) && pt0 < 0.05 && p_aic > p_sc)
    varx <- VAR(endog, p=p_aic, type="const", exogen=exog)

  # ortho IRF (항상 varx 직접 사용 — SVAR 안 씀)
  irf_obj <- tryCatch(
    irf(varx, impulse=SHOCK_VAR, response=c(SHOCK_VAR,price),
        n.ahead=IRF_HORIZON, ortho=TRUE, cumulative=TRUE, boot=FALSE),
    error=function(e) NULL)

  erpt <- setNames(rep(NA_real_, length(ERPT_HORIZONS)), paste0("h", ERPT_HORIZONS))
  if (!is.null(irf_obj)) {
    im <- irf_obj$irf[[SHOCK_VAR]]
    for (k in seq_along(ERPT_HORIZONS)) {
      h <- ERPT_HORIZONS[k]; idx <- h+1L
      if (idx <= nrow(im) && im[idx, SHOCK_VAR] != 0)
        erpt[k] <- im[idx, price] / im[idx, SHOCK_VAR]
    }
  }
  tibble(external=external, price=price, bok=bok_position,
         lag=as.integer(varx$p), erpt_h24=erpt["h24"])
}

cat("더미 포함 27개 사양 추정 중...\n")
res_with <- pmap_dfr(expand.grid(e=EXTERNALS,p=PRICES,b=BOK_POS,stringsAsFactors=FALSE),
  function(e,p,b) tryCatch(fit_one(e,p,b,data_v,use_dummy=TRUE),  error=function(e) NULL))

cat("더미 미포함 27개 사양 추정 중...\n")
res_without <- pmap_dfr(expand.grid(e=EXTERNALS,p=PRICES,b=BOK_POS,stringsAsFactors=FALSE),
  function(e,p,b) tryCatch(fit_one(e,p,b,data_v,use_dummy=FALSE), error=function(e) NULL))

comp <- inner_join(
  res_with    %>% rename(with=erpt_h24),
  res_without %>% rename(without=erpt_h24),
  by=c("external","price","bok")) %>%
  mutate(delta = with - without,
         rel_pct = ifelse(abs(without)>1e-6, 100*delta/abs(without), NA))

cat("\n", strrep("=",70), "\n")
cat("  ERPT(24M) 더미 포함(with) vs 미포함(without) — ortho IRF 기준\n")
cat(strrep("=",70), "\n")
print(as.data.frame(comp %>% arrange(price,external,bok) %>%
  mutate(across(c(with,without,delta), ~sprintf("%+.4f",.)),
         rel_pct=sprintf("%+.1f%%",rel_pct))), row.names=FALSE)

cat("\n[요약]\n")
cat(sprintf("  평균 절대 변화:   %.4f\n", mean(abs(comp$delta), na.rm=TRUE)))
cat(sprintf("  평균 상대 변화:   %.1f%%\n", mean(abs(comp$rel_pct), na.rm=TRUE)))
cat(sprintf("  부호 뒤집힌 수:   %d / 27\n",
  sum(sign(as.numeric(comp$with)) != sign(as.numeric(comp$without)), na.rm=TRUE)))

cat("\n[물가별 일관성 변화]\n")
for (pr in PRICES) {
  s <- comp %>% filter(price==pr)
  w_neg  <- sum(as.numeric(s$with)    < 0, na.rm=TRUE)
  wo_neg <- sum(as.numeric(s$without) < 0, na.rm=TRUE)
  cat(sprintf("  %-3s  더미포함 음: %d/9  미포함 음: %d/9\n", toupper(pr), w_neg, wo_neg))
}

if (!dir.exists("results")) dir.create("results", recursive = TRUE)
write.csv(comp, "results/dummy_effect_check.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n06_dummy_check.R 완료 ✓ (results/dummy_effect_check.csv)\n")
