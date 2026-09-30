# data/

이 레포에는 원자료를 올리지 않습니다. 출처마다 이용·재배포 조건이 다르고(FRED의 VIX 같은 일부 계열은 제3자 저작권 자료), 코드로 다시 받아올 수 있기 때문입니다.

| 자료 | 받는 방법 |
|---|---|
| 한국은행 ECOS (수입물가·PPI·CPI·한국 기준금리) | `R/01_data_collect.R`가 Open API로 자동 수집 (`ECOS_API_KEY` 필요) |
| FRED (FEDFUNDS, VIXCLS, POILDUBUSDM) | `R/01_data_collect.R`가 `fredr`로 자동 수집 (`FRED_API_KEY` 필요) |
| BIS 명목실효환율 (한국, Broad) | **직접 내려받기** (아래 참고) |

## BIS NEER 파일 준비

1. BIS Data Portal의 실효환율 페이지(https://data.bis.org/topics/EER)에 들어갑니다.
2. 한국(Korea) · Nominal · Broad · Monthly 계열을 골라 Excel로 내보냅니다.
3. 받은 파일을 `data/raw/bis_neer.xlsx`로 저장합니다.

코드는 이 파일의 `timeseries observations` 시트에서 `TIME_PERIOD:Period`, `OBS_VALUE:Value` 열을 읽습니다. 다른 파일명을 쓰려면 `.Renviron`에 `BIS_NEER_FILE=경로`를 추가하세요.

`data/raw/` 안의 파일은 `.gitignore`에 등록되어 있어 커밋되지 않습니다.
