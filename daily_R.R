library(jsonlite)
library(tidyverse)
library(magrittr)
library(pander)
library(EpiDynamics)
library(earlyR)
remotes::install_github("reconhub/incidence")

library(incidence)
library(distcrete)

remotes::install_github("annecori/EpiEstim")
library(EpiEstim)

select <- dplyr::select

raw <-
  fromJSON(
    "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true"
  )

tibble(
  infected = raw$infected,
  recovered = raw$recovered,
  tested = raw$numberOfTestedGraph$value %>% last) %>% pander

df_cum <- tibble(date = as.Date(raw$totalPositiveTests$date), positive = raw$totalPositiveTests$value)
tested = tibble(tested = raw$numberOfTestedGraph$value, date = as.Date(raw$numberOfTestedGraph$date))

df_cum <- left_join(df_cum, tested, by = "date") %>% mutate_if(is.character, parse_number)

# df_cum %>% view

df_cum %<>% filter(positive > 0)

inc <- tibble(dates = df_cum$date, I = diff(c(0, df_cum$positive)))



# estimates from https://doi.org/10.1016/j.ijid.2020.02.060
# -----------------------------------------------------------
# the mean and standard deviation (SD) of the serial interval were estimated at 4.7
# days (95% CrI: 3.7, 6.0) and 2.9 days (95% CrI: 1.9, 4.9), respectively

# estimates from¨https://doi.org/10.1101/2020.02.19.20025452
# --------------------------------------------------
# The mean and standard deviation are 3.96
# 
# The mean and standard deviation are 3.96 (95% CI 3.53-4.39)
# and 4.75 (95% CI 4.46-5.07) days, respectively

# Wuhan, China, with a mean of 7.5 days and a 
# standard deviation of 3.4 days (Li et al., 2020).


# https://doi.org/10.1101/2020.02.19.20025452   <<<<<-----------
#  mean and standard deviation are 3.96
# (95% CI 3.53-4.39) and 4.75 (95% CI 4.46-5.07) days,
# respectively, with 12.6% of reports
# indicating pre-symptomatic transmission. 

dailyR <- estimate_r(inc, method = )

# instR_uncertain <- estimate_R(inc, method = "uncertain_si",
#                               config = make_config(
#                                 list(
#                                   mean_si = 4.7,
#                                   min_mean_si = 3.53,
#                                   max_mean_si = 6,
#                                   
#                                   std_si = 2.9,
#                                   min_std_si = 1.9,
#                                   max_std_si = 5.07,
#                                   
#                                   std_mean_si = 4,
#                                   std_std_si = 2,
#                                   n1 = 100,
#                                   n2 = 100
#                                 )
#                               ))
# 
# plot(instR_uncertain, "all")
#   instR_uncertain$R %>% pander
