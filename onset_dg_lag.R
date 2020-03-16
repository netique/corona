library(tidyverse)
library(readxl)
library(naniar)
library(lubridate)
library(magrittr)

df_outside <- read_excel("nCoV2019_2020_line_list_open.xlsx", 
                         sheet = "outside_Hubei")
df_hubei <- read_excel("nCoV2019_2020_line_list_open.xlsx", 
                         sheet = "Hubei")

df_ons_con <- df_outside %>%
  transmute(age, sex, country, city, chronic_disease_binary,
    onset = dmy(date_onset_symptoms),
    confirmation = dmy(date_confirmation),
    days_to_confirmation = dmy(date_confirmation) - dmy(date_onset_symptoms)
  ) %>% filter(days_to_confirmation >= 0 & !is.na(onset) & !is.na(confirmation))

# %>% rowid_to_column() %>% 
#   pivot_longer(-c(rowid, dif)) %>% 
#   ggplot(aes(value)) 
  # geom_col(aes(fill = name), position = position_dodge(1))

df_ons_con %>%
  # sample_n(100000, replace = T) %>% 
  ggplot(aes(days_to_confirmation)) + geom_density()

df_ons_con %>% filter(!is.na(chronic_disease_binary) & chronic_disease_binary != "N/A") %>% 
  # sample_n(100000, replace = T) %>% 
  ggplot(aes(days_to_confirmation, col = chronic_disease_binary)) + geom_density()

df_ons_con %>%
  # sample_n(100000, replace = T) %>% 
  ggplot(aes(as.numeric(age), days_to_confirmation)) +
  geom_point(position = "jitter")
