library(tidyverse)
library(magrittr)
library(EpiModel)

raw <-
  fromJSON(
    "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true"
  )

df_cum <-
  tibble(
    date = as.Date(raw$totalPositiveTests$date),
    positive = as.integer(raw$totalPositiveTests$value)
  )

df_cum %<>% filter(positive > 0 & date + days(1) + hours(10) < now())

inc <- tibble(dates = df_cum$date, I = diff(c(0, df_cum$positive)))

#####
citizens <- 1319000 # Praha

i <- raw$infectedByRegion[1,2] # Praha
r <- 2
s <- citizens - r

c <- 1 # constant for N scaling

R_0 <- 2.127

days_to_recovery <- 18

beta <- R_0 * (1 / days_to_recovery)

param <- param.icm(inf.prob = beta, act.rate = 5, rec.rate = 1/days_to_recovery)
init <- init.icm(s.num = (s-i)/c, i.num = i/c, r.num = r/c)
control <- control.icm(type = "SIR", nsims = 5, nsteps = 30, verbose = T)
mod <- icm(param, init, control)

plot(mod, y = "i.num", sim.lines = T, mean.smooth = F, qnts.smooth = F, popfrac = T, grid = T, legend = T)
plot(mod)
