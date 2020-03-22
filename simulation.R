library(tidyverse)
library(magrittr)
library(EpiModel)

# assumptions:
# consider 12% probability of infection per transmissible act
# between a susceptible and an infected person
# 
# 5 transmissible acts per day (close co-worker contacts)
# 18 days to recovery (presume that worker is productive after 18 days)
# 
# intervention begins at day 10 (worker starts to show symptoms and quarantine himself)
# intervention is maximally effective, lowering transmission prob. 10 times

# starting with 49 workers and 1 infected (zero recovered)

sim_days <- 60 # days to simulate for


param <- param.icm(inf.prob = 0.12, act.rate = 5, rec.rate = 1/(18),
                  # inter.start = 10, inter.eff = .95
                   )
init <- init.icm(s.num = 49, i.num = 1, r.num = 0)
control <- control.icm(type = "SIR", nsims = 100,
                       nsteps = sim_days, verbose = T)
mod <- icm(param, init, control)

plot(mod, y = "r.num", sim.lines = T, mean.smooth = F, qnts.smooth = F, popfrac = T, grid = T, legend = T)
plot(mod, propfrac = T)

available_workers <- apply(mod$epi$s.num, 1, median)+ apply(mod$epi$r.num, 1, median)



param_inter <- param.icm(inf.prob = 0.12, act.rate = 5, rec.rate = 1/(18),
                   inter.start = 10, inter.eff = .9
)
init_inter <- init.icm(s.num = 49, i.num = 1, r.num = 0)
control_inter <- control.icm(type = "SIR", nsims = 100,
                       nsteps = sim_days, verbose = T)
mod_inter <- icm(param_inter, init_inter, control_inter)

plot(mod_inter, y = "i.num", sim.lines = T, mean.smooth = F, qnts.smooth = F, popfrac = T, grid = T, legend = T)
plot(mod_inter, propfrac = T)

available_workers_inter <- apply(mod_inter$epi$s.num, 1, median)+ apply(mod_inter$epi$r.num, 1, median)


daily_prodictivity <- tibble(day = seq(1,sim_days),
                             productivity = available_workers * (34125/30),
                             productivity_inter = available_workers_inter * (34125/30))

daily_prodictivity %<>% mutate(saved = productivity_inter - productivity)
daily_prodictivity$saved[1:30] %>% sum

daily_prodictivity %>% pivot_longer(-c(day,saved)) %>%  ggplot(aes(day, value, col= name)) + geom_line()
