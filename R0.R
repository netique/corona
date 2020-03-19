library(jsonlite)
library(tidyverse)
library(R0)

raw <-
  fromJSON(
    "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true"
  )

df_wide <-
  data.frame(
    positive = as.integer(raw$totalPositiveTests$value),
    date = raw$totalPositiveTests$date %>% lubridate::as_date(),
    day = seq(1, length(raw$totalPositiveTests$value))
  ) %>% as_tibble()

gt <- generation.time("empirical", df_wide$positive)
estR <- estimate.R(df_wide$positive, gt, methods = c("EG", "ML", "SB", "TD"))
estR$estimates$SB %>% plot
estR$estimates$SB$R %>% last

