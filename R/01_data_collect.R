############################################################
# 01_data_collect.R — 원자료 수집 및 병합
# ----------------------------------------------------------
# - BIS 명목실효환율(NEER, Broad, 한국): 직접 내려받은 엑셀 파일을 읽음
# - 한국은행 ECOS: 수입물가·PPI·CPI·한국 기준금리 (Open API)
# - FRED: 미국 기준금리·VIX·두바이유 (fredr)
#
# API 키는 코드에 넣지 않고 프로젝트 루트의 .Renviron 에서 읽는다.
#   ECOS_API_KEY=...
#   FRED_API_KEY=...
# (.Renviron.example 참고, .Renviron 은 .gitignore 로 업로드 제외)
#
# 출력: df_merged (월별 수준 자료, 2010-01 ~ 2026-02)
############################################################

# ─────────────────────────────────────────────
# 0. 패키지
# ─────────────────────────────────────────────
required_packages <- c("httr", "jsonlite", "dplyr", "fredr", "readxl")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(httr); library(jsonlite); library(dplyr); library(fredr); library(readxl)

# ─────────────────────────────────────────────
# 1. API KEY (.Renviron 에서 읽기)
# ─────────────────────────────────────────────
ecos_key <- Sys.getenv("ECOS_API_KEY")
fred_key <- Sys.getenv("FRED_API_KEY")
if (!nzchar(ecos_key) || !nzchar(fred_key)) {
  stop("ECOS_API_KEY / FRED_API_KEY 가 설정되지 않았습니다.\n",
       "  .Renviron.example 을 .Renviron 으로 복사해 키를 입력한 뒤 R 세션을 재시작하세요.")
}
fredr_set_key(fred_key)

# ─────────────────────────────────────────────
# 2. 조회 기간
# ─────────────────────────────────────────────
start_ecos <- "201001"; end_ecos <- "202602"
start_fred <- as.Date("2010-01-01")

# ─────────────────────────────────────────────
# 3. ECOS 수집 함수
# ─────────────────────────────────────────────
get_ecos_monthly <- function(api_key, stat_code, item_code1,
                             start_date, end_date, cycle = "M") {
  url <- paste0(
    "https://ecos.bok.or.kr/api/StatisticSearch/",
    api_key, "/json/kr/1/10000/",
    stat_code, "/", cycle, "/",
    start_date, "/", end_date, "/",
    URLencode(item_code1, reserved = TRUE)
  )
  res       <- GET(url); stop_for_status(res)
  json_data <- fromJSON(content(res, as = "text", encoding = "UTF-8"),
                        flatten = TRUE)
  if (!"StatisticSearch" %in% names(json_data)) stop("API 오류")
  df <- json_data$StatisticSearch$row
  data.frame(
    date  = as.Date(paste0(df$TIME, "01"), format = "%Y%m%d"),
    value = as.numeric(df$DATA_VALUE)
  )
}

# ─────────────────────────────────────────────
# 4. 데이터 수집
# ─────────────────────────────────────────────

# ── (1) BIS NEER (명목실효환율, Broad, 한국) ─────────────
# BIS Data Portal(https://data.bis.org/topics/EER)에서 한국 · Nominal · Broad ·
# Monthly 계열을 Excel로 내려받아 data/raw/bis_neer.xlsx 로 저장한다.
# (자세한 방법은 data/README.md 참고. 파일명을 바꾸려면 BIS_NEER_FILE 환경변수 사용)
bis_file <- Sys.getenv("BIS_NEER_FILE", "data/raw/bis_neer.xlsx")
if (!file.exists(bis_file)) {
  stop("BIS NEER 파일이 없습니다: ", bis_file, "\n  data/README.md 를 참고해 내려받으세요.")
}
neer_raw <- read_excel(bis_file, sheet = "timeseries observations")
neer <- neer_raw %>%
  dplyr::select(
    date_raw = `TIME_PERIOD:Period`,
    neer     = `OBS_VALUE:Value`
  ) %>%
  mutate(
    date = as.Date(format(as.Date(date_raw), "%Y-%m-01")),
    neer = as.numeric(neer)
  ) %>%
  dplyr::select(date, neer) %>%
  filter(!is.na(neer)) %>%
  arrange(date)

cat(sprintf("NEER: %s ~ %s (%d개월)\n",
            min(neer$date), max(neer$date), nrow(neer)))

# ── (2) 수입물가지수 (ECOS 401Y015, 총지수) ─────────────
# 같은 날짜에 여러 행이 반환되며, 원 코드와 동일하게 날짜별 3번째 행을 사용한다.
import_price <- get_ecos_monthly(
  ecos_key, "401Y015", "*AA", start_ecos, end_ecos) %>%
  mutate(row_num = ave(seq_len(n()), date, FUN = seq_along)) %>%
  filter(row_num == 3) %>%
  dplyr::select(date, import_price_index = value) %>%
  arrange(date)

# ── (3) PPI (ECOS 404Y014, 총지수) ──────────────────────
ppi <- get_ecos_monthly(ecos_key, "404Y014", "*AA",
                        start_ecos, end_ecos) %>%
  dplyr::rename(ppi = value) %>% arrange(date)

# ── (4) CPI (ECOS 901Y009, 총지수) ──────────────────────
cpi <- get_ecos_monthly(ecos_key, "901Y009", "0",
                        start_ecos, end_ecos) %>%
  dplyr::rename(cpi = value) %>% arrange(date)

# ── (5) 미국 기준금리 (FRED FEDFUNDS) ───────────────────
fed_rate <- fredr(series_id = "FEDFUNDS",
                  observation_start = start_fred) %>%
  dplyr::select(date, fed_rate = value) %>% arrange(date)

# ── (6) 한국 기준금리 (ECOS 722Y001) ────────────────────
bok_rate <- get_ecos_monthly(
  ecos_key, "722Y001", "0101000", start_ecos, end_ecos) %>%
  dplyr::rename(bok_rate = value) %>% arrange(date)

# ── (7) VIX (FRED VIXCLS, 일별 → 월평균) ───────────────
vix <- fredr(series_id = "VIXCLS",
             observation_start = start_fred) %>%
  mutate(date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(date) %>%
  summarise(vix = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

# ── (8) 두바이유 (FRED POILDUBUSDM) ─────────────────────
oil <- fredr(series_id = "POILDUBUSDM",
             observation_start = start_fred) %>%
  dplyr::select(date, oil = value) %>% arrange(date)

# ─────────────────────────────────────────────
# 5. 병합
# ─────────────────────────────────────────────
df_merged <- neer %>%
  full_join(import_price, by = "date") %>%
  full_join(ppi,          by = "date") %>%
  full_join(cpi,          by = "date") %>%
  full_join(fed_rate,     by = "date") %>%
  full_join(bok_rate,     by = "date") %>%
  full_join(vix,          by = "date") %>%
  full_join(oil,          by = "date") %>%
  arrange(date) %>%
  na.omit()

cat(sprintf("\n전체 기간: %s ~ %s | 관측치: %d\n",
            format(min(df_merged$date)),
            format(max(df_merged$date)),
            nrow(df_merged)))

cat("\n01_data_collect.R 완료 ✓\n")
