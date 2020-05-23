library(jsonlite)
library(tidyverse)
library(magrittr)
library(lubridate)
library(EpiEstim)

infected <-
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")$data %>%
  transmute(
    date = as_date(datum),
    infected_per_day = prirustkovy_pocet_nakazenych,
    infected_cumul = kumulativni_pocet_nakazenych
  )

infected %<>% filter(date >= "2020-03-01")

inc <- infected %>% transmute(dates = date, I = infected_per_day)

# prepare index-secondary raw data by Zhanwei Du's  GitHub repo
si <- read_csv2("serial_int_data_du_2020.csv") %>%
  rename(
    "event" = "Event index",
    "index_id" = "Index ID",
    "secondary_id" = "Secondary ID",
    "index_date" = "Index - symptom onset date",
    "secondary_date" = "Seconday - symptom onset date"
  )

si %<>% mutate(si = secondary_date - index_date)

# same as in the article
si %>% pull(si) %>% hist

# EpiEstim does not support negative SI (it uses gamma, lognormal, weilbull distributions)
si %<>% filter(si > 0)

# si_data format: "EL"   "ER"   "SL"   "SR"   "type"
# type = 2 refers to "exact observations" (interval-censored data not available)
# lower and upper bounds are hence the same
si_data <- si %>% transmute(
  EL = index_date,
  ER = index_date,
  SL = secondary_date,
  SR = secondary_date,
  type = 2
) %>% as.data.frame()

# get "clever" initial params
init_pars <- init_mcmc_params(si_data, "L")

# estimate the SI distribution using MCMC
SI_fit <- coarseDataTools::dic.fit.mcmc(dat = si_data,
                                        dist = "L",
                                        init.pars = init_pars,
                                        burnin = 500,
                                        n.samples = 2500,
                                        seed = 2)

# save the estimated SI sample for further use w/o running MCMC
si_sample <- coarse2estim(SI_fit, thin = 10)$si_sample

write_rds(si_sample, "si_sample.rds")

# R estimation with saved SI data
R_mcmc_estimated_si <- estimate_R(
  inc,
  method = "si_from_sample",
  si_sample = si_sample
  # config = make_config(
  #   list(
  #     n1 = 500,
  #     n2 = 50,
  #     seed = 2
  #   )
  # )
)

# show sampled SI distributions
R_si_from_data %>% plot("SI")

# inspect MCMC chain by iteration
plot(R_mcmc_estimated_si$SI.Moments[, 'Mean'],
     type = 'l',
     xlab = 'Iterations',
     ylab = 'Mean SI')

plot(R_mcmc_estimated_si$SI.Moments[, 'Std'],
     type = 'l',
     xlab = 'Iterations',
     ylab = 'Std SI')


