library(jsonlite)
library(tidyverse)
select <- dplyr::select

raw <-
  fromJSON(
    "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true"
  )

raw$totalTested
(now <- max(c(raw$infected, raw$fromBabisNewspapers$totalInfected)))

df_wide <-
  data.frame(
    tested = as.integer(cumsum(raw$testedCases$value)),
    positive = as.integer(raw$totalPositiveTests$value)[-1],
    date = raw$testedCases$date %>% lubridate::as_date() +1,
    day = seq(1, length(raw$testedCases$date))
  ) %>% as_tibble()

df_wide_pd <- tibble(
  date = raw$testedCases$date[-1] %>% lubridate::as_date() + 1,
  day = 1:(length(raw$testedCases$value) - 1),
  tested_pd = diff(df_wide$tested),
  positive_pd = diff(df_wide$positive)
)

df_long <- df_wide %>% pivot_longer(-date, names_to = "type")
df_long_pd <-df_wide_pd %>%  pivot_longer(-day, names_to = "type")

df_long  %>% #filter(date > "2020-02-25") %>%
  ggplot(aes(date, value, col = type)) +
  geom_line()

df_long_pd %>% filter(day > 20) %>% ggplot(aes(day, value)) +
  geom_col(aes(fill = type), position = position_dodge(1)) #+ facet_wrap(~type) #+ geom_smooth()
  # geom_smooth(method = "glm", method.args = list(family = "poisson"))

df_forfit <- df_wide[34:(nrow(df_wide)), ] # from first day (34) to last complete day
days <- seq(34, 34 + 30)

fit_positive <- lm(log(positive + 1) ~ day, data = df_forfit)
pred_positive <- exp(predict(fit_positive, list(day=days))) - 1

fit_tested <- lm(log(tested + 1) ~ day, data = df_forfit)
pred_tested <- exp(predict(fit_tested, list(day = days))) - 1

summary(fit_positive)[["adj.r.squared"]]
summary(fit_tested)[["adj.r.squared"]]

df_pred <- tibble(pred_positive, pred_tested, day = days)

df_comp <- left_join(df_pred, df_forfit)
df_comp$true_cases <- 1.36 * pred_positive #  according to foreing spread share 66%
df_comp$outbreak_day <- df_comp$day - 34 # 0 day is the outbreak day

df_comp_long <-
  df_comp %>%
  select(pred_positive, positive, pred_tested, tested, true_cases, outbreak_day, day, date) %>%
  pivot_longer(-c(date, day, outbreak_day)) %>% 
  mutate(type = as.factor(name))

df_comp_long %>% ggplot(aes(outbreak_day, value)) +
  geom_point(data = df_comp_long %>% filter(!is.na(value) &
                                              type %in% c("positive", "tested")), aes()) +
  geom_line(data = df_comp_long %>% filter(is.na(date) &
                                             type %in% c("pred_positive", "pred_tested")), aes(col = type)) +
  geom_line(data = df_comp_long %>% filter(type == "true_cases")) +
  ylim(0,10000) +
  labs(title = "Vývoj počtu testovaných, nakažených s pozitivním testem a skutečně nakažených",
       x = "den od prvních potvrzených případů",
       y = "počet",
       caption = "Černá křivka ukazuje odhad skutečně nakažených a jeho vývoj na základě modelu Tomas Pueyo \n(https://medium.com/@tomaspueyo/coronavirus-act-today-or-people-will-die-f4d3d9cd99ca).",
       col = "typ") + 
  scale_color_manual(labels = c("Predikovaní nakažení", "Predikovaní testovaní", "a"), values = c("red", "blue")) +
  theme(legend.position = c(.2, .82))

# --------------------
library(R0)
gt <- generation.time("empirical", df_wide$positive[-seq_len(33)])
estR <- estimate.R(df_wide$positive[-seq_len(33)], gt, methods = c("EG", "ML", "SB", "TD"))
estR$estimates$SB %>% plot
estR$estimates$SB$R %>% last


# --------------------
# case by case  ---- onset estimation

ons_con <- read_csv("onset_confirm_data.csv")

prediction <- df_comp %>% select(pred_positive, date)
prediction$date_imputed <- seq.Date(prediction$date[1], length.out = nrow(prediction), by = 1)
prediction$cases_round <- round(prediction$pred_positive)

prediction$cases_pd <- c(prediction$cases_round[1], diff(prediction$cases_round))
  
ons_con_sample <-
  sample_n(
    ons_con %>% select(days_to_confirmation),
    size = prediction$cases_round %>% last,
    replace = T
  ) %>% pull


dates_counts <- NULL
  for (i in 1:length(prediction$date_imputed)) {
    dates_counts[[i]] <-
      rep(prediction$date_imputed[i], prediction$cases_pd[i])
  }
dates_counts %<>% unlist %>% as_date

casebycase <-
  tibble(
    confirmation = dates_counts,
    days_to_confirmation = ons_con_sample
  ) %>%
  mutate(est_onset = confirmation - days_to_confirmation)

# get range of dates
date_range <- seq(min(casebycase$est_onset), max(casebycase$est_onset), by="days")
# fill missings dates with 0 and CUMSUM
cbc_cum <- casebycase %>%
  count(est_onset) %>%
  complete(data.frame(est_onset = date_range), fill = list(n = 0)) %>% 
  mutate(est_onset_cum = cumsum(n)) %>% add_column(.before = )

df_shift <-
  rbind(
    df_wide %>% transmute(date, positive, type = "confirmed"),
    cbc_cum %>% transmute(
      date = est_onset,
      positive = est_onset_cum,
      type = "skuteční"
    ),
    prediction %>% transmute(date = date_imputed, positive = cases_round, type = "potvrzení")
  )

df_shift %>% rename() %>% ggplot(aes(date, positive)) +
  geom_line(data = df_shift %>% filter(type != "confirmed"), aes(col = type), size = 1.25) +
  geom_point(data = df_shift %>% filter(type == "confirmed")) +
  coord_cartesian(ylim = c(0, 35000)) +
  xlim(as.Date("2020-02-15"), as.Date("2020-03-25")) +
  # geom_hline(yintercept = 3711) +
  geom_hline(yintercept = 26210, col = "red", linetype = "dashed") +
  # geom_vline(xintercept = as.Date("2020-03-16")) +
  geom_vline(xintercept = as.Date("2020-03-23"), col = "red", linetype = "dashed") +
  geom_point(aes(x=as.Date("2020-03-23"), y=26210), colour="red", size = 2) +
  scale_color_manual(values = c("blue", "red", "black")) +
  labs(col= "Nakažení", title = "Odhad a projekce skutečných a potvrzených případů")+
  theme(legend.position = c(.2, .33)) 

a <- df_wide %>% transmute(date, positive, type = "confirmed")
b <-  cbc_cum %>% transmute(
    date = est_onset,
    positive = est_onset_cum,
    type = "est_onset")

left_join(a,b, by = "date") %>%
  na.omit %>% transmute(date, cases_confirmed = positive.x, cases_onset = positive.y) %>%
  write_csv("confirmed_estimated_onset.csv") %>% view


b<- df_wide %>% transmute(date, positive, type = "confirmed")
c<-cbc_cum %>% transmute(
  date = est_onset,
  positive = est_onset_cum,
  type = "est_onset"
)
d<-prediction %>% transmute(date = date_imputed, positive = cases_round, type = "est_confirmed")

e <- left_join(d,c, by ="date")
e[,c(1,2,4)] %>% pivot_longer(-date) %>% ggplot(aes(date, value, col = name)) + geom_line()


