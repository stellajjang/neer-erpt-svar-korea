############################################################
# 02_preprocess.R — 계절조정 · 로그차분 · 이상치 더미
# ----------------------------------------------------------
# 선행: 01_data_collect.R (df_merged)
# 출력: df_log (로그 수준), df_diff (로그차분·차분 + 펄스 더미)
############################################################

if (!requireNamespace("seasonal", quietly = TRUE)) install.packages("seasonal")
library(seasonal); library(dplyr)

if (!exists("df_merged")) stop("먼저 R/01_data_collect.R 를 실행하세요.")

# ─────────────────────────────────────────────
# 1. 물가지수 계절조정 (X-13ARIMA-SEATS)
# ─────────────────────────────────────────────
# 환율·유가·VIX·금리는 조정하지 않음
cat("\n▶ 물가 지수(CPI, PPI, 수입물가) 계절조정 수행 중...\n")

cpi_sa <- seas(ts(df_merged$cpi, start = c(2010, 1), frequency = 12))
df_merged$cpi <- final(cpi_sa)

ppi_sa <- seas(ts(df_merged$ppi, start = c(2010, 1), frequency = 12))
df_merged$ppi <- final(ppi_sa)

imp_sa <- seas(ts(df_merged$import_price_index, start = c(2010, 1), frequency = 12))
df_merged$import_price_index <- final(imp_sa)

cat("▶ 계절조정 완료 ✓\n")

# ─────────────────────────────────────────────
# 2. 로그 변환 및 로그차분
# ─────────────────────────────────────────────
# 지수·가격 변수: 로그 후 전월 대비 차분
# 금리(bok_rate, fed_rate): 이미 %단위이므로 로그 없이 차분(%p)
df_log <- df_merged %>%
  mutate(across(c(neer, import_price_index, ppi, cpi, vix, oil),
                log, .names = "l_{.col}"))

df_diff <- df_log %>%
  mutate(
    across(starts_with("l_"), ~ c(NA, diff(.)), .names = "d{.col}"),
    d_bok_rate = c(NA, diff(bok_rate)),
    d_fed_rate = c(NA, diff(fed_rate))
  ) %>%
  na.omit()

cat(sprintf("[차분 데이터] %s ~ %s (%d개월)\n",
            format(min(df_diff$date)),
            format(max(df_diff$date)),
            nrow(df_diff)))

# ─────────────────────────────────────────────
# 3. 더미변수 생성 (이상치 자동 탐지, 3σ 기준)
# ─────────────────────────────────────────────
# 탐지 대상: 환율과 물가 4변수
# 기준: 각 변수별 평균 ± 3σ 를 벗어나는 월을 펄스 더미로 처리
# 방식: union — 어느 한 변수에서라도 이상치면 더미 생성
outlier_vars <- c("dl_neer", "dl_import_price_index", "dl_ppi", "dl_cpi")

outlier_detail <- lapply(outlier_vars, function(v) {
  x   <- df_diff[[v]]
  mu  <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)
  list(var = v, dates = df_diff$date[abs(x - mu) > 3 * sig])
})

cat("\n[더미변수] 3σ 이상치 탐지 결과:\n")
for (od in outlier_detail) {
  dates_fmt <- if (length(od$dates) > 0)
    paste(format(sort(od$dates), "%Y-%m"), collapse = ", ") else "없음"
  cat(sprintf("  %-28s  %s\n", od$var, dates_fmt))
}

outlier_dates_clean <- sort(unique(as.Date(
  unlist(lapply(outlier_detail, function(od) as.character(od$dates)))
)))
cat(sprintf("  최종 더미 대상 월 (%d개): %s\n",
            length(outlier_dates_clean),
            paste(format(outlier_dates_clean, "%Y-%m"), collapse = ", ")))

# 펄스 더미: 해당 월만 1, 나머지 0 (컬럼명 d_YYYYMM)
for (dt in as.character(outlier_dates_clean)) {
  dt_date  <- as.Date(dt)
  col_name <- paste0("d_", format(dt_date, "%Y%m"))
  df_diff[[col_name]] <- as.integer(df_diff$date == dt_date)
}
dummy_cols <- grep("^d_20", names(df_diff), value = TRUE)
cat(sprintf("  생성된 더미 컬럼: %s\n", paste(dummy_cols, collapse = ", ")))

cat("\n02_preprocess.R 완료 ✓\n")
