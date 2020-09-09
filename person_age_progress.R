library(tidyverse)
library(magrittr)
library(jsonlite)
library(lubridate)
library(patchwork)
library(here)
if (!require(mtaux)) {
  remotes::install_github("netique/mtaux")
} else {
  library(mtaux)
} # just theme

# only "validated" data from HKS!!!
khs_raw <- fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.json")
khs <- khs_raw$data
data_updated <- khs_labs_raw$modified %>% as_datetime(tz = "Europe/Prague")

khs %<>%
  transmute(
    date = ymd(datum),
    age = vek,
    sex = as.factor(pohlavi),
    nuts = kraj_nuts_kod,
    lau = okres_lau_kod,
    inf_abroad = nakaza_v_zahranici,
    country = nakaza_zeme_csu_kod
  ) %>% as_tibble()

# all avaiable data
khs_labs_raw <- fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")
khs_labs <- khs_labs_raw$data %>%
  as_tibble() %>%
  transmute(date = ymd(datum), inc_khs_labs = prirustkovy_pocet_nakazenych)

# aggregated khs data and khs_labs comparison
comparison <- khs %>%
  count(date, name = "inc_khs") %>%
  left_join(khs_labs)

# khs_labs excess over khs only incidence
comparison %<>%
  mutate(diff = inc_khs_labs - inc_khs, diff_plus_one = diff + 1)


khs %<>%
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

complete_weeks <- khs %>% group_by(week) %>%
  transmute(n_days = n_distinct(date)) %>%
  filter(n_days == 7) %>% pull(week) %>% unique()


age_time_weeks <- khs %>%
  filter(week %in% complete_weeks) %>%
  ggplot(aes(week, age_group)) +
  geom_bin2d(drop = FALSE, binwidth = 1) +
  scale_fill_viridis_c() +
  scale_x_continuous(n.breaks = 15) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Weekly incidence by age categories in Czechia",
    subtitle = "based only on highly incomplete, so-called validated KHS data; only completely observed weeks are shown",
    y = "age group",
    caption = paste("data updated", data_updated %>% strftime(usetz = TRUE), " |  © 2020 Jan Netík, source at github.com/netique/corona")
  ) +
  mtaux::theme_mt(
    axis.ticks = element_line(color = "gray80"),
    plot.caption = element_text(face = "italic"),
    plot.title.position = "panel",
    plot.subtitle = element_text(margin = margin(b = 12, t = -3))
  )

ggsave(here("plots", "age_time_week.png"), width = 9.81, height = 6.76)


# Jakub Steiner's idea, divide counts by total incidence,
# so the proportion of given age group in the given week is shown
khs %>%
  count(week, age_group, name = "week_age_inc") %>%
  group_by(week) %>%
  mutate(week_inc = sum(week_age_inc), pct_age = week_age_inc / week_inc) %>%
  group_by(week) %>%
  # filter(week %in% complete_weeks) %>%
  ggplot(aes(week, age_group, fill = pct_age)) +
  geom_tile() +
  scale_x_continuous(n.breaks = 15) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Distribution of weekly incidence among age groups in Czechia",
    subtitle = "based only on highly incomplete, so-called validated KHS data",
    x = "week (ISO 8601)",
    y = "age group",
    fill = "% of week",
    caption = paste("data updated", data_updated %>% strftime(usetz = TRUE), " |  © 2020 Jan Netík, source at github.com/netique/corona")
  ) +
  mtaux::theme_mt(
    axis.ticks = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.caption = element_text(face = "italic"),
    plot.title.position = "panel",
    plot.subtitle = element_text(margin = margin(b = 12, t = -3))
  )

ggsave(here("plots", "age_prop_time_week.png"), width = 9.81, height = 6.76)


age_time_days <- khs %>%
  ggplot(aes(date, age)) +
  geom_bin2d(drop = FALSE, binwidth = 1) +
  scale_x_date(date_breaks = "2 weeks", labels = function(x) {paste0(day(x), "/", month(x))}) +
  scale_y_continuous(n.breaks = 15) +
  xlab("day/month") +
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  mtaux::theme_mt(
    axis.ticks = element_line(color = "gray80"),
    plot.caption = element_text(face = "italic"),
    plot.title.position = "panel"
  )

age_time_days_sep <- age_time_days + labs(
  title = "Daily incidence by age in Czechia",
  subtitle = "based only on highly incomplete, so-called validated KHS data",
  caption = "by Jan Netík, source at github.com/netique/corona"
) + theme(plot.subtitle = element_text(margin = margin(b = 12, t = -3)))

ggsave(here("plots", "age_time_days.png"), width = 9.81, height = 6.76)


labs_excess <- comparison %>%
  ggplot(aes(date, diff)) +
  geom_line() +
  scale_y_continuous(n.breaks = 3) +
  ylab("labs exc. inc.") +
  coord_cartesian(expand = FALSE) +
  mtaux::theme_mt(axis.text.x = element_blank(), axis.title.x = element_blank())

labs_excess_log <- comparison %>%
  ggplot(aes(date, diff_plus_one)) + # for log
  geom_line() +
  scale_y_log10(n.breaks = 5) +
  scale_x_date(date_breaks = "1 week") +
  coord_cartesian(expand = FALSE) +
  mtaux::theme_mt(axis.text.x = element_blank(), axis.title = element_blank())


patchwork <- labs_excess + age_time_days + plot_layout(heights = c(1, 5))

patchwork + plot_annotation(
  title = "Daily incidence by age in Czechia",
  subtitle = "only KHS data stratified by age available, excess incidence reported by labs shown in the upper plot",
  caption = paste("data updated", data_updated %>% strftime(usetz = TRUE), " |  © 2020 Jan Netík, source at github.com/netique/corona")
) &
  theme(
    plot.title = element_text(family = "Roboto", face = "bold", size = 16, colour = "grey30"),
    plot.subtitle = element_text(family = "Roboto Condensed", size = 11, colour = "grey30"),
    plot.caption = element_text(family = "Roboto Condensed", face = "italic", colour = "grey30")
  )

ggsave(here("plots", "age_time_days_with_labs_excess_incidence.png"), width = 9.81, height = 6.76)
