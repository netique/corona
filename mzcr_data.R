set.seed(1425)
select <- dplyr::select


modified <-
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/testy.json")$modified %>%
  as_datetime %>% with_tz("Europe/Prague")

tested <-
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/testy.json")$data %>%
  transmute(
    date = as_date(datum),
    tested_per_day = prirustkovy_pocet_testu,
    tested_cumul = kumulativni_pocet_testu
    )

infected <-
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")$data %>%
  transmute(
    date = as_date(datum),
    infected_per_day = prirustkovy_pocet_nakazenych,
    infected_cumul = kumulativni_pocet_nakazenych
  )

recovered_dead <-
  fromJSON(
    "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.json"
  )$data %>%
  transmute(
    date = as_date(datum),
    recovered_cumul = kumulativni_pocet_vylecenych,
    dead_cumul = kumulativni_pocet_umrti
  )

# identical(recovered_dead$date, tested$date, infected$date)

# filter out "nonpandemic days"
df <-
  bind_cols(tested, infected, recovered_dead) %>%
  as_tibble %>% dplyr::select(-c(date1, date2)) %>% 
  filter(date >= "2020-03-01")

# quick overview
a <- df %>%
  select(contains("cumul")) %>%
  rename(
    "tested" = 1,
    "infected" = 2,
    "recovered" = 3,
    "dead" = 4
  ) %>%
  tail(1) %>% t()

inc <- df %>% transmute(dates = date, I = infected_per_day)
# moving avrg
# inc$I <- forecast::sma(inc$I, 5) %>% as.numeric
# Multiple seasonal decomposition, only trend retained
inc$I <- mstl(inc$I)[, "Trend"]

# Lognormal (Shape, Scale) 2.02 [1.76,2.31] 2.78 [2.39,3.25] 
si_distr_lognorm <-
  distcrete("lnorm", interval = 1, 2.02, 2.78)$r(2000) %>%
  extract(. < 20)
freq_lognorm <- table(si_distr_lognorm) %>% as.vector
si_distr_lognorm <- c(0, freq_lognorm / sum(freq_lognorm))
# write_rds(si_distr_lognorm, "si_distr_lognorm.rds")

# load generated distribution, for the sake of server load
# si_distr_lognorm <- read_rds("si_distr_lognorm.rds")


dailyR_lognorm <-
  estimate_R(inc,
             method = "non_parametric_si",
             config = make_config(list(si_distr = si_distr_lognorm)))
dailyR_lognorm %>% plot

dailyR_lognorm$dates %>% length
as_date(69, origin = "2020-02-29")


tab <- dailyR_lognorm$R %>%
  rename(
    "Mean" = "Mean(R)",
    "SD" = "Std(R)",
    "2.5th percentile" = "Quantile.0.025(R)",
    "50th percentile" = "Median(R)",
    "97.5th percentile" = "Quantile.0.975(R)"
  ) %>% transmute(
    `Window end` = as_date(t_end, origin = "2020-02-29"),
    `Mean`,
    `SD`,
    `2.5th percentile`,
    `50th percentile`,
    `97.5th percentile`
  )

tab %>% datatable
