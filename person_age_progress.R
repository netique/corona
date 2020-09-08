library(tidyverse)
library(magrittr)
library(jsonlite)
library(lubridate)
if (!require(mtaux)) {
  remotes::install_github("netique/mtaux")
} else {
  library(mtaux)
} # just theme

persons_all <- fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.json")

persons <- persons_all$data

persons %<>%
  transmute(
    date = ymd(datum),
    age = vek,
    sex = as.factor(pohlavi),
    nuts = kraj_nuts_kod,
    lau = okres_lau_kod,
    inf_abroad = nakaza_v_zahranici,
    country = nakaza_zeme_csu_kod
  ) %>% as_tibble()

persons %<>%
  arrange(date) %>%
  mutate(
    week = isoweek(date),
    age_group = case_when(
      age %in% 0:9 ~ "0-9",
      age %in% 10:19 ~ "10-19",
      age %in% 20:29 ~ "20-29",
      age %in% 30:39 ~ "30-39",
      age %in% 40:49 ~ "40-49",
      age %in% 50:59 ~ "50-59",
      age %in% 60:69 ~ "60-69",
      age %in% 70:79 ~ "70-79",
      age %in% 80:89 ~ "80-89",
      TRUE ~ "90+"
      )
  ) %>%
  relocate(date, week, age, age_group)

persons %<>% group_by(week) %>% mutate(n_days = n_distinct(date)) %>% ungroup



persons %>%
  filter(n_days == 7) %>%
  ggplot(aes(week, age_group)) +
  geom_bin2d(drop = FALSE, binwidth = 1) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Weekly incidence by age categories in Czechia",
    subtitle = "only complete weeks (i.e. with sunday observations) are shown",
    y = "age group",
    caption = "by Jan Netík, source at github.com/netique/corona"
  ) +
  mtaux::theme_mt(
    axis.text.x = element_text(angle = 90, vjust = .5),
    axis.ticks = element_line(color = "gray60"),
    panel.background = element_rect(fill = "gray80"),
    plot.caption = element_text(face = "italic")
  ) +
  theme(
    plot.title = element_text(hjust = .1),
    plot.subtitle = element_text(hjust = .1, margin = margin(b=10), )
  )


persons %>%
  ggplot(aes(date, age)) +
  geom_bin2d(drop = FALSE, binwidth = 1) +
  scale_x_date(date_breaks = "1 week") +
  scale_y_continuous(n.breaks = 15) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  mtaux::theme_mt(
    axis.text.x = element_text(angle = 90, vjust = .5),
    axis.ticks = element_line(color = "gray60"),
    panel.background = element_rect(fill = "gray80")
  )
