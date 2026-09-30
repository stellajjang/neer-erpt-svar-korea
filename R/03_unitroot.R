############################################################
# 03_unitroot.R — 단위근 검정 (ADF + KPSS, 수준 vs 1차차분)
# ----------------------------------------------------------
# - ADF : 귀무가설 = 단위근 존재(비정상). 통계량이 임계값보다 작으면 기각 → 정상
# - KPSS: 귀무가설 = 수준정상.           통계량이 임계값보다 크면 기각 → 비정상
# - 분석에 사용하는 8개 변수를 수준과 1차차분에서 각각 검정
#
# 선행: 02_preprocess.R (df_log, df_diff)
# 출력: results/unit_root.csv
############################################################

library(urca)
if (!exists("df_diff") || !exists("df_log")) stop("먼저 R/02_preprocess.R 를 실행하세요.")

# 별표: ADF는 (stat < 임계값), KPSS는 (stat > 임계값) 기준
.star_adf  <- function(s, cv) if (s < cv["1pct"]) "***" else if (s < cv["5pct"]) "**" else if (s < cv["10pct"]) "*" else ""
.star_kpss <- function(s, cv) if (s > cv["1pct"]) "***" else if (s > cv["5pct"]) "**" else if (s > cv["10pct"]) "*" else ""

.adf_one <- function(x) {
  x <- x[is.finite(x)]
  m <- urca::ur.df(x, type = "drift", selectlags = "AIC")
  stat <- as.numeric(m@teststat[1]); cv <- m@cval[1, ]
  list(stat = stat, txt = sprintf("%.3f%s", stat, .star_adf(stat, cv)),
       reject5 = stat < cv["5pct"])   # TRUE면 정상(단위근 기각)
}
.kpss_one <- function(x) {
  x <- x[is.finite(x)]
  m <- urca::ur.kpss(x, type = "mu", lags = "short")
  stat <- as.numeric(m@teststat); cv <- m@cval[1, ]
  list(stat = stat, txt = sprintf("%.3f%s", stat, .star_kpss(stat, cv)),
       reject5 = stat > cv["5pct"])   # TRUE면 비정상(수준정상 기각)
}

# (라벨, 수준 시계열, 1차차분 시계열)
.ur_spec <- list(
  list("환율",          df_log$l_neer,                 df_diff$dl_neer),
  list("수입물가",      df_log$l_import_price_index,   df_diff$dl_import_price_index),
  list("생산자물가",    df_log$l_ppi,                  df_diff$dl_ppi),
  list("소비자물가",    df_log$l_cpi,                  df_diff$dl_cpi),
  list("국제유가",      df_log$l_oil,                  df_diff$dl_oil),
  list("변동성지수",    df_log$l_vix,                  df_diff$dl_vix),
  list("한국 기준금리", df_log$bok_rate,               df_diff$d_bok_rate),
  list("미국 기준금리", df_log$fed_rate,               df_diff$d_fed_rate)
)

unitroot_tbl <- do.call(rbind, lapply(.ur_spec, function(s) {
  lab <- s[[1]]
  al <- tryCatch(.adf_one(s[[2]]),  error = function(e) list(txt = NA, reject5 = NA))
  kl <- tryCatch(.kpss_one(s[[2]]), error = function(e) list(txt = NA, reject5 = NA))
  ad <- tryCatch(.adf_one(s[[3]]),  error = function(e) list(txt = NA, reject5 = NA))
  kd <- tryCatch(.kpss_one(s[[3]]), error = function(e) list(txt = NA, reject5 = NA))
  # 판정: 수준 정상(ADF 기각 & KPSS 미기각) → I(0), 아니면 차분 ADF 기각 시 I(1)
  level_stationary <- isTRUE(al$reject5) && !isTRUE(kl$reject5)
  diff_stationary  <- isTRUE(ad$reject5)
  verdict <- if (level_stationary) "I(0)" else if (diff_stationary) "I(1)" else "확인필요"
  data.frame(변수 = lab,
             `ADF_수준` = al$txt,   `KPSS_수준` = kl$txt,
             `ADF_차분` = ad$txt,   `KPSS_차분` = kd$txt,
             판정 = verdict, stringsAsFactors = FALSE, check.names = FALSE)
}))

cat("\n[단위근 검정] ADF(절편) / KPSS(수준정상), 별표 ***1% **5% *10%\n")
print(unitroot_tbl, row.names = FALSE)

if (!dir.exists("results")) dir.create("results", recursive = TRUE)
write.csv(unitroot_tbl, "results/unit_root.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n03_unitroot.R 완료 ✓\n")
