library(jsonlite)
library(tidyverse)
library(R0)

raw <-
  fromJSON(
    "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true"
  )

df_wide <-
  data.frame(
    tested = as.integer(cumsum(raw$testedCases$value)),
    positive = as.integer(raw$totalPositiveTests$value)[-1],
    date = raw$testedCases$date %>% lubridate::as_date() +1,
    day = seq(1, length(raw$testedCases$date))
  ) %>% as_tibble()

gt <- generation.time("empirical", df_wide$positive[-seq_len(33)])
estR <- estimate.R(df_wide$positive[-seq_len(33)], gt, methods = c("EG", "ML", "SB", "TD"))
estR$estimates$SB %>% plot
estR$estimates$SB$R %>% last

