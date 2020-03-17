library(jsonlite)
library(tidyverse)
library(magrittr)
library(pander)
library(lubridate)
library(EpiDynamics)
library(earlyR)
library(incidence)
library(projections)
library(shinySIR)
library(distcrete)
library(EpiEstim)
library(deSolve)

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

df_cum %>% view

df_cum %<>% filter(positive > 0)

inc_dates <- rep(df_cum$date, diff(c(0, df_cum$positive)))

inc <- incidence(inc_dates) # last_date = as.Date("2020-03-16")

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

get_R(inc, si_mean = 4.7, si_sd = 2.9) %>% plot(sub = "si_mean = 4.7, si_sd = 2.9 (doi.org/10.1016/j.ijid.2020.02.060)")
get_R(inc, si_mean = 3.96, si_sd = 4.75) %>% plot(sub = "si_mean = 3.96, si_sd = 4.75 (doi.org/10.1101/2020.02.19.20025452)")
get_R(inc, si_mean = 7.5, si_sd = 3.4) %>% plot(sub = "si_mean = 7.5, si_sd = 3.4 ( čísla z Wuhanu, Li et al., 2020)") # wuhan

# presume that
# R_0 = 2

si <- distcrete("norm", interval = 1L,
                mean = 4.7,
                sd = 2.9) # https://github.com/aakhmetz/nCoVSerialInterval2020/blob/master/results/summary-certain_and_probable-no_truncation.xlsx

project(inc, growth_R0, hubei_res_parametric_si$si_distr, 100000, 14, model = "poisson") %>%
  plot + labs(title = "Incidence", caption = "Vychází z plausibilních R0 odhadovaných z growth rate (n = 2500) \n a serial intervalu s průměrem 4.62 a SD 2.78; \n hodnoty R0 jsou samplovány pro každý krok každého odhadu jednotlivých křivek, \n které jsou rozlišeny kvantilovými pásmy.")



#####

Infected <- df_cum %>% pull(positive)
Day <- 1:(length(Infected))
N <- 10500000
init <- c(S = N - Infected[1], I = Infected[1], R = 0)

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.0005, 0.00000000005), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))

# check for convergence
Opt$message

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par[2] <- Opt_par[1] * 2

sir_start_date <- "2020-03-01"
# time in days for predictions
t <- 1:as.integer(ymd("2020-06-30") - ymd(sir_start_date))
# get the fitted values from our SIR model
fitted_cumulative_incidence <- data.frame(ode(y = init, times = t, 
                                              func = SIR, parms = Opt_par))

fitted_cumulative_incidence$date <- df_cum$date[1:nrow(fitted_cumulative_incidence)]
fitted_cumulative_incidence %>% pivot_longer(-c(time, date), names_to = "compartment", values_to = "cases") %>%
  ggplot(aes(time, cases, col = compartment)) + geom_line() + geom_point(data = df_pd,
                                                                         mapping = aes(time, cases),
                                                                         col = "red")

########
mu <- 4.7  # days
sigma <- 2.9  # days
param <- gamma_mucv2shapescale(mu, sigma/mu)

w <- distcrete("gamma", interval = 1, shape = param$shape, scale = param$scale, 
               w = 0)

df_pd <- tibble(time = seq(0, length(df_cum$date)- 1, 1), cases = diff(c(0, df_cum$positive)))

peak <- find_peak(inc)
inc_fit <- incidence::fit(inc, split = peak)

R0 <- lm2R0_sample(inc_fit$before$model, w, n = 2500)
hist(R0, col = "grey", border = "white", main = "Distribution of R0", sub = " SI mean 4.7, SD 2.9, function r2R0 from epitrix, gamma distr.")
summary(growth_R0)



# plot(inc) %>% add_incidence_fit(inc_fit)

# nothing ot show after peak - no peak to date

plot_Ri <- function(estimate_R_obj) {
  p_I <- plot(estimate_R_obj, "incid", add_imported_cases = F, )  # plots the incidence
  p_SI <- plot(estimate_R_obj, "SI")  # plots the serial interval distribution
  p_Ri <- plot(estimate_R_obj, "R")
  return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
}

hubei_res_parametric_si <-
  estimate_R(inc,
             method = "parametric_si",
             config = make_config(list(mean_si = 4.7,
                                       std_si = 2.9)))

plot(hubei_res_parametric_si, "R") + labs(title = "Estimated Instantaneous Reproduction Number", caption = "7-day sliding window, ")
plot(hubei_res_parametric_si, "SI") + labs(caption = "serial interval distribution")


hubei_res_uncertain_si <- estimate_R(inc, method = "uncertain_si", 
                                     config = make_config(list(mean_si = 4.7,
                                                               std_mean_si = 2, #
                                                               min_mean_si = 3.53,
                                                               max_mean_si = 6,
                                                               std_si = 2.9,
                                                               std_std_si = 1, # 
                                                               min_std_si = 1.9,
                                                               max_std_si = 5.07,
                                                               n1 = 1000, n2 = 1000)))

plot(hubei_res_uncertain_si, what = "SI", options_SI = list(transp = .05)) + labs(caption = "Serial intervaly simulované dle několika dostupných článků.")
plot(hubei_res_uncertain_si, what = "R", options_R = list(xlim = c(as.Date("2020-03-08"), today()))) +  labs(title = "Daily estimates of the reproduction numbers R", caption = "Nejistota zohledněna simululací Serial intervalů podle dostupných článků. Sliding window 7 dnů.")


#######
sir_1 <- function(beta, gamma, S0, I0, R0, times) {
  require(deSolve) # for the "ode" function
  
  # the differential equations:
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <-  beta * I * S - gamma * I
      dR <-  gamma * I
      return(list(c(dS, dI, dR)))
    })
  }
  
  # the parameters values:
  parameters_values <- c(beta  = beta, gamma = gamma)
  
  # the initial values of variables:
  initial_values <- c(S = S0, I = I0, R = R0)
  
  # solving
  out <- ode(initial_values, times, sir_equations, parameters_values)
  
  # returning the output:
  as.data.frame(out)
}

(df_sir <- sir_1(beta = 5e-6, gamma = 1/9, S0 = 1000000, I0 = 436, R0 = 3, times = seq(15, 200, 1/10)))

# df_sir %>% pivot_longer(-time) %>% ggplot(aes(time, value, col = name)) + geom_line()

df_pd <- tibble(time =1:31, cases = a)

df_sir %>% pivot_longer(-time, names_to = "compartment", values_to = "cases") %>%
  ggplot(aes(time, cases, col = compartment)) + geom_line() + geom_point(data = df_pd, mapping = aes(time, cases), col = "red")


# estim params
ss <- function(beta, gamma, data = df_pd, N = sum(df_pd$cases)) {
  I0 <- data$cases[1]
  times <- data$time
  predictions <- sir_1(beta = beta, gamma = gamma,   # parameters
                       S0 = N - I0, I0 = I0, R0 = 0, # variables' intial values
                       times = times)                # time points
  sum((predictions$I[-1] - data$cases[-1])^2)
}

ss(beta = 0.002126263, gamma = 0.4407071)


beta_val <- seq(from = 0.00000000000005, to = 0.004, le = 100)
ss_val <- sapply(beta_val, ss, gamma = 0.5)

min_ss_val <- min(ss_val)
min_ss_val

beta_hat <- beta_val[ss_val == min_ss_val]
beta_hat

plot(beta_val, ss_val, type = "l", lwd = 2,
     xlab = expression(paste("infectious contact rate ", beta)),
     ylab = "sum of squares")
# adding the minimal value of the sum of squares:
abline(h = min_ss_val, lty = 2, col = "grey")
# adding the estimate of beta:
abline(v = beta_hat, lty = 2, col = "grey")


gamma_val <- seq(from = 0.4, to = 3, le = 500)
ss_val <- sapply(gamma_val, function(x) ss(beta_hat, x))
(min_ss_val <- min(ss_val))

(gamma_hat <- gamma_val[ss_val == min_ss_val])

plot(gamma_val, ss_val, type = "l", lwd = 2,
     xlab = expression(paste("recovery rate ", gamma)),
     ylab = "sum of squares")
abline(h = min_ss_val, lty = 2, col = "grey")
abline(v = gamma_hat, lty = 2, col = "grey")

n <- 10 # number of parameter values to try
beta_val <- seq(from = 0.002, to = 0.0035, le = n)
gamma_val <- seq(from = 0.3, to = 0.65, le = n)
param_val <- expand.grid(beta_val, gamma_val)
ss_val <- with(param_val, Map(ss, Var1, Var2))
ss_val <- matrix(unlist(ss_val), n)
persp(beta_val, gamma_val, -ss_val, theta = 40, phi = 30,
      xlab = "beta", ylab = "gamma", zlab = "-sum of squares")

n <- 30 # number of parameters values
beta_val <- seq(from = 0.002, to = 0.0035, le = n)
gamma_val <- seq(from = 0.3, to = 0.65, le = n)
# calculating the sum of squares:
param_val <- expand.grid(beta_val, gamma_val)
ss_val <- with(param_val, Map(ss, Var1, Var2))
ss_val <- unlist(ss_val)

# minimum sum of squares and parameters values:
(ss_min <- min(ss_val))
## [1] 4843.681
ind <- ss_val == ss_min
(beta_hat <- param_val$Var1[ind])
## [1] 0.002568966
(gamma_hat <- param_val$Var2[ind])
## [1] 0.4810345
# visualizing the sum of squares profile:
ss_val <- matrix(ss_val, n)
image(beta_val, gamma_val, ss_val,
      xlab = expression(paste("infectious contact rate ", beta, " (/person/day)")),
      ylab = expression(paste("recovery rate ", gamma, " (/day)")))
contour(beta_val, gamma_val,ss_val, add = TRUE)
points(beta_hat, gamma_hat, pch = 3)
box(bty = "o")



ss2 <- function(x) {
  ss(beta = x[1], gamma = x[2])
}
ss2(c(0.0001, 0.5))

starting_param_val <- c(0.0001, 0.4)
ss_optim <- optim(starting_param_val, ss2)
ss_optim




####

library(deSolve)
library(RColorBrewer)

#https://en.wikipedia.org/wiki/Timeline_of_the_2019%E2%80%9320_Wuhan_coronavirus_outbreak#Cases_Chronology_in_Mainland_China
Infected <- c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515)
day <- 0:(length(Infected)-1)
N <- 1400000000 #pop of china

###edit 1: use different boundary condiotion
###init <- c(S = N-1, I = 1, R = 0)
init <- c(S = N-Infected[1], I = Infected[1], R = 0)
plot(day, Infected)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  ####edit 2; use equally scaled variables 
  with(par, { dS <- -beta * (S/N) * I
  dI <- beta * (S/N) * I - gamma * I
  dR <- gamma * I
  list(c(dS, dI, dR))
  })
}

SIR2 <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  ####
  #### use as change of variables variable
  #### const = (beta-gamma)
  #### delta = gamma/beta
  #### R0 = beta/gamma > 1 
  #### 
  #### beta-gamma = beta*(1-delta)
  #### beta-gamma = beta*(1-1/R0)
  #### gamma = beta/R0
  with(par, { 
    beta  <- const/(1-1/R0)  
    gamma <- const/(R0-1)  
    dS <- -(beta * (S/N)      ) * I 
    dI <-  (beta * (S/N)-gamma) * I 
    dR <-  (             gamma) * I
    list(c(dS, dI, dR))
  })
}

RSS.SIR2 <- function(parameters) {
  names(parameters) <- c("const", "R0")
  out <- ode(y = init, times = day, func = SIR2, parms = parameters)
  fit <- out[ , 3]
  RSS <- sum((Infected - fit)^2)
  return(RSS)
}

### plotting different values R0

# use the ordinary exponential model to determine const = beta - gamma
const <- coef(mod)[2]




RSS.SIR <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  RSS <- sum((Infected - fit)^2)
  return(RSS)
}

lower = c(0, 0)
upper = c(1, 1)  ###adjust limit because different scale 1/N

### edit: get a good starting condition
mod <- nls(Infected ~ a*exp(b*day), 
           start = list(a = Infected[1],
                        b = log(Infected[2]/Infected[1])))
optimsstart <- c(2,1)*coef(mod)[2]

set.seed(12)
Opt <- optim(optimsstart, RSS.SIR, method = "L-BFGS-B", lower = lower, upper = upper,
             hessian = TRUE)
Opt

### estimated covariance matrix of coefficients
### note the large error, but also strong correlation (nearly 1)
## note scaling with estimate of sigma because we need to use Hessian of loglikelihood
sigest <- sqrt(Opt$value/(length(Infected)-1))
solve(1/(2*sigest^2)*Opt$hessian) 

####
####  using alternative parameters
####  for this we use the function SIR2
####

optimsstart <- c(coef(mod)[2],5)
lower = c(0, 1)
upper = c(1, 10^3)  ### adjust limit because we use R0 now which should be >1

set.seed(12)
Opt2 <- optim(optimsstart, RSS.SIR2, method = "L-BFGS-B",lower=lower, upper=upper,
              hessian = TRUE, control = list(maxit = 1000, 
                                             parscale = c(10^-3,1)))
Opt2

# now the estimated variance of the 1st parameter is small
# the 2nd parameter is still with large variance
#
# thus we can predict beta - gamma very well
# this beta - gamma is the initial growth coefficient
# but the individual values of beta and gamma are not very well known
#
# also note that hessian is not at the MLE since we hit the lower boundary
#
sigest <- sqrt(Opt2$value/(length(Infected)-1))
solve(1/(2*sigest^2)*Opt2$hessian)

#### We can also estimated variance by
#### Monte Carlo estimation
##
## assuming data to be distributed as mean +/- q mean
## with q such that mean RSS = 52030
##
## 
##


### Two functions RSS to do the optimization in a nested way
RSS.SIRMC2 <- function(const,R0) {
  parameters <- c(const=const, R0=R0)
  out <- ode(y = init, times = day, func = SIR2, parms = parameters)
  fit <- out[ , 3]
  RSS <- sum((Infected_MC - fit)^2)
  return(RSS)
}
RSS.SIRMC <- function(const) {
  optimize(RSS.SIRMC2, lower=1,upper=10^5,const=const)$objective
}

getOptim <- function() {
  opt1 <- optimize(RSS.SIRMC,lower=0,upper=1)
  opt2 <- optimize(RSS.SIRMC2, lower=1,upper=10^5,const=opt1$minimum)
  return(list(RSS=opt2$objective,const=opt1$minimum,R0=opt2$minimum))
}

# modeled data that we use to repeatedly generate data with noise
Opt_par <- Opt2$par
names(Opt_par) <- c("const", "R0")
modInfected <- data.frame(ode(y = init, times = day, func = SIR2, parms = Opt_par))$I

# doing the nested model to get RSS
set.seed(1)
Infected_MC <- Infected
modnested <- getOptim()

errrate <- modnested$RSS/sum(Infected) 


par <- c(0,0)
for (i in 1:100) {
  Infected_MC <- rnorm(length(modInfected),modInfected,(modInfected*errrate)^0.5)
  OptMC <- getOptim()
  par <- rbind(par,c(OptMC$const,OptMC$R0))
}
par <- par[-1,]

plot(par, xlab = "const",ylab="R0",ylim=c(1,1))
title("Monte Carlo simulation")
cov(par)


###conclusion: the parameter R0 can not be reliably estimated

##### End of Monte Carlo estimation


### plotting different values R0

# use the ordinary exponential model to determine const = beta - gamma
const <- coef(mod)[2]
R0 <- 1.1

# graph
plot(-100,-100, xlim=c(0,80), ylim = c(1,N), log="y", 
     ylab = "infected", xlab = "days", yaxt = "n")
axis(2, las=2, at=10^c(0:9),
     labels=c(expression(1),
              expression(10^1),
              expression(10^2),
              expression(10^3),
              expression(10^4),
              expression(10^5),
              expression(10^6),
              expression(10^7),
              expression(10^8),
              expression(10^9)))
axis(2, at=rep(c(2:9),9)*rep(10^c(0:8),each=8), labels=rep("",8*9),tck=-0.02)
title(bquote(paste("scenario's for different ", R[0])), cex.main = 1)

# time
t <- seq(0,60,0.1)

# plot model with different R0
for (R0 in c(1.1,1.2,1.5,2,3,5,10)) {
  fit <- data.frame(ode(y = init, times = t, func = SIR2, parms = c(const,R0)))$I
  lines(t,fit)
  text(t[601],fit[601],
       bquote(paste(R[0], " = ",.(R0))),
       cex=0.7,pos=4)
}

# plot observations
points(day,Infected)

