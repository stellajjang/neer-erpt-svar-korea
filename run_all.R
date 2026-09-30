############################################################
# run_all.R — 전체 분석 일괄 실행 (프로젝트 루트에서 실행)
#   source("run_all.R")
############################################################

options(repos = c(CRAN = "https://cloud.r-project.org"))

# ---- 패키지 확인 -------------------------------------------------------------
pkgs <- c("httr", "jsonlite", "dplyr", "tidyr", "purrr", "tibble", "readxl",
          "fredr", "seasonal", "urca", "vars", "strucchange",
          "ggplot2", "scales", "showtext")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss) > 0) install.packages(miss)

# ---- 그림용 한글 폰트 (OS 자동 분기) -----------------------------------------
library(showtext)
font_path <- switch(Sys.info()[["sysname"]],
  "Windows" = "C:/Windows/Fonts/malgun.ttf",
  "Darwin"  = "/System/Library/Fonts/Supplemental/AppleGothic.ttf",
  NULL)
if (!is.null(font_path) && file.exists(font_path)) {
  font_add("korean", font_path)
  showtext_auto()
}

# ---- 실행 순서 ---------------------------------------------------------------
source("R/01_data_collect.R")   # 원자료 수집·병합        → df_merged
source("R/02_preprocess.R")     # 계절조정·로그차분·더미   → df_log, df_diff
source("R/03_unitroot.R")       # ADF/KPSS                 → results/unit_root.csv
source("R/04_svar_main.R")      # CPI 대표 모형·부트스트랩 → figures/cpi_*.png
source("R/05_robustness.R")     # 27개 구성·모형 선정      → figures/erpt_consistency_box.png
source("R/06_dummy_check.R")    # (보조) 더미 포함 여부 비교

cat("\n[run_all] 전체 완료 — results/, figures/ 확인\n")
