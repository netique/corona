library(jsonlite)
library(plotly)
library(tidyverse)
library(magrittr)
library(lubridate)
library(EpiEstim)
library(sf)
library(CzechData)
library(RCzechia)

khs <-
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.json")$data

counts <- khs %>% count(okres_lau_kod) %>% transmute(KOD_LAU1 = okres_lau_kod, count = n)



RCzechia::okresy("low") %>%
  inner_join(counts, by = "KOD_LAU1") %>% 
  ggplot() +
  geom_sf(aes(fill = count))
  
  
  
  counts <- khs %>% count(kraj_nuts_kod) %>% transmute(KOD_CZNUTS3 = kraj_nuts_kod, count = n)
  ggplot(RCzechia::kraje() %>% left_join(counts)) + geom_sf(aes(fill = count))

  
khs %<>% as_tibble() %>% mutate(datum = as_date(datum), pohlavi = factor(pohlavi))

khs %>% ggplot(aes(datum, col = nakaza_v_zahranici)) +
  geom_density()

khs %>% ggplot(aes(datum, vek)) +
  geom_jitter(alpha = .1) +
  geom_density2d()

kraje_s_nazvy <- khs %>%
  left_join(CzechData::kraje %>% rename("kraj_nuts_kod" = "nuts3_kod"), by = "kraj_nuts_kod")

kraje_s_nazvy %>% 
  count(nazev, datum) %>%
  ggplot(aes(datum, n)) +
  geom_col() +
  facet_wrap(~ nazev)



kraje_s_nazvy %>% filter(nazev == "Moravskoslezský kraj") %>% 
  count(nazev, datum) %>%
  ggplot(aes(datum, n)) +
  geom_col()

i_msk <- kraje_s_nazvy %>% filter(nazev == "Moravskoslezský kraj") %>% 
  count(nazev, datum) %>% pull(n) %>% forecast::mstl() %>% .[, "Trend"]

i_msk_df <- kraje_s_nazvy %>% filter(nazev == "Moravskoslezský kraj") %>% 
  count(nazev, datum) %>% bind_cols(trend = i_msk) %>% transmute(date = datum, I = trend)

i_msk_df %>%
  ggplot(aes(date, I)) +
  geom_col()

est_R <- estimate_R(i_msk_df,
                    method = "uncertain_si",
                    config = make_config(
                      list(
                        mean_si = si_mean,
                        std_mean_si = 5,
                        # high SD
                        min_mean_si = si_mean - si_mean_trunc,
                        max_mean_si = si_mean + si_mean_trunc,
                        std_si = si_sd,
                        std_std_si = 4,
                        # hight SD
                        min_std_si = si_sd - si_sd_trunc,
                        max_std_si = si_sd + si_sd_trunc,
                        n1 = 100,
                        n2 = 100
                      )
                    ))

est_R %>% plot("R") + ylim(0,2)

si_distr_lognorm <- read_rds("data/si_distr_lognorm.rds")


inc_import <- kraje_s_nazvy %>%
  filter(nazev == "Liberecký kraj" & nakaza_v_zahranici == TRUE) %>%
  count(datum) %>% 
  rename(dates = datum, imported = n)
  
inc_local <- kraje_s_nazvy %>%
  filter(nazev == "Liberecký kraj" & nakaza_v_zahranici == FALSE) %>%
  count(datum) %>% 
  rename(dates = datum, local = n)
  
a <- right_join(inc_local, inc_import, by = "dates") %>% replace_na(list(local = 0, imported = 0)) %>% 
estimate_R(method = "non_parametric_si",
           config = make_config(list(si_distr = si_distr_lognorm))) %>% plot(add_imported_cases = T)


inc <- kraje_s_nazvy %>%
  filter(nazev == nazev) %>%
  count(datum) %>% 
  rename(dates = datum, I = n)





min_date <- min(inc$dates)
max_date <- max(inc$dates)

R_dle_kraju <- map(CzechData::kraje$nazev,
                   ~ {
                     inc <- kraje_s_nazvy %>%
                       filter(nazev == .x) %>%
                       count(datum) %>%
                       rename(dates = datum, I = n) %>%
                       complete(dates = seq.Date(min(min_date), max(max_date), by="day")) %>%
                       replace_na(list(I = 0))
                     
                     
                     si_mean <- 3.96
                     si_sd <- 4.75
                     
                     si_mean_trunc <- 2.75
                     si_sd_trunc <- 4
                     
                     est_R <- estimate_R(inc,
                                         method = "uncertain_si",
                                         config = make_config(
                                           list(
                                             mean_si = si_mean,
                                             std_mean_si = 5,
                                             # high SD
                                             min_mean_si = si_mean - si_mean_trunc,
                                             max_mean_si = si_mean + si_mean_trunc,
                                             std_si = si_sd,
                                             std_std_si = 4,
                                             # hight SD
                                             min_std_si = si_sd - si_sd_trunc,
                                             max_std_si = si_sd + si_sd_trunc,
                                             n1 = 100,
                                             n2 = 100
                                           )
                                         ))
                     
                     tibble(date_end = est_R$dates[est_R$R$t_end], R = est_R$R$`Mean(R)`, low = est_R$R$`Quantile.0.025(R)`, upp = est_R$R$`Quantile.0.975(R)`)
                   })


map2_dfr(.x = R_dle_kraju,
         .y = 1:14,
         ~ bind_cols(.x,
                     kraj = rep(CzechData::kraje$nazev[.y], nrow(.x)))) %>%
  ggplot(aes(date_end, R)) +
  geom_line() +
  facet_wrap(~ kraj) +
  coord_cartesian(ylim = c(0,3)) + geom_hline(yintercept = 1) + geom_ribbon(aes(ymin = low, ymax = upp), alpha = .2)


R_dle_kraju[[13]] %>% 
ggplot(aes(date_end, R)) +
  geom_line() +
  coord_cartesian(ylim = c(0,3), xlim = c(as_date("2020-06-01"), as_date("2020-07-01"))) + geom_hline(yintercept = 1) + geom_ribbon(aes(ymin = low, ymax = upp), alpha = .2)

    