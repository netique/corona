
library(jsonlite)
library(plotly)
library(tidyverse)
library(magrittr)
library(lubridate)
library(EpiEstim)

  tested <-
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/testy.json")$data %>%
    transmute(
      date = as_date(datum),
      tested_per_day = prirustkovy_pocet_testu,
      tested_cumul = kumulativni_pocet_testu
    )
  
  infected <-
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")$data %>%
    transmute(
      date = as_date(datum),
      infected_per_day = prirustkovy_pocet_nakazenych,
      infected_cumul = kumulativni_pocet_nakazenych
    )
  
  recovered_dead <-
    fromJSON(
      "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.json"
    )$data %>%
    transmute(
      date = as_date(datum),
      recovered_cumul = kumulativni_pocet_vylecenych,
      dead_cumul = kumulativni_pocet_umrti
    )
  
  # when data change in the morning, JSONs are modifed separately,
  # which causes an error probably due to uneven nrows of tables
  # being bound together -- so ensure to work with equal nrows
  minimal_nrows <- min(nrow(tested), nrow(infected), nrow(recovered_dead))
  
  # bind together & filter out "nonpandemic days"
  df <- bind_cols(tested[seq(1, minimal_nrows), ],
            infected[seq(1, minimal_nrows), ],
            recovered_dead[seq(1, minimal_nrows), ]) %>%
    as_tibble %>% dplyr::select(-c(date1, date2)) %>%
    filter(date >= "2020-03-01")



  inc <- df %>% transmute(dates = date, I = infected_per_day)
  
  # Lognormal (Shape, Scale) 2.02 [1.76,2.31] 2.78 [2.39,3.25]
  # si_distr_lognorm <-
  #   distcrete("lnorm", interval = 1, 2.02, 2.78)$r(2000) %>%
  #   extract(. < 20)
  # freq_lognorm <- table(si_distr_lognorm) %>% as.vector
  # si_distr_lognorm <- c(0, freq_lognorm / sum(freq_lognorm))
  # write_rds(si_distr_lognorm, "si_distr_lognorm.rds")

  # 3.96 [3.53, 4.39] 4.75 
  
  
  # symetric truncation
  si_mean <- 3.96
  si_sd <- 4.75
  
  si_mean_trunc <- 2.75
  
  si_sd_trunc <- 4
  
  dailyR_lognorm <-
    estimate_R(inc,
               method = "uncertain_si",
               config = make_config(
                 list(
                   mean_si = si_mean,
                   std_mean_si = 5,
                   min_mean_si = si_mean - si_mean_trunc,
                   max_mean_si = si_mean + si_mean_trunc,
                   std_si = si_sd,
                   std_std_si = 4,
                   min_std_si = si_sd - si_sd_trunc,
                   max_std_si = si_sd + si_sd_trunc,
                   n1 = 100,
                   n2 = 100
                 )
               ))
  
  #####
  si_distr_lognorm <- read_rds("R_number_daily/si_distr_lognorm.rds")
  
  dailyR_lognorm <-
    estimate_R(inc,
               method = "non_parametric_si",
               config = make_config(list(si_distr = si_distr_lognorm)))
  
  dailyR_lognorm
  })
  
  
  #####
  
  dailyR_lognorm %>% plot("SI")

  plot_decomposed <- dailyR_lognorm %>% plot("R") %>% ggplot_build()
  
  p <- plot_decomposed$plot$data
  
  line_text <-
    paste0(
           "R: ",
           p$meanR %>% round(2),
           "\nWindow end: ",
           p$end,
           "\nLower CrI: ",
           p$lower %>% round(2),
           "\nUpper CrI: ",
           p$upper %>% round(2)
          )
  
  plt <- p %>% 
    ggplot(aes(x = end, y = as.numeric(meanR))) +
    geom_hline(yintercept = 1, linetype = "dashed") + 
    geom_ribbon(aes(ymin = lower, ymax = upper, text = "", fill = "95% CrI"), alpha = .7) + scale_fill_manual("", values = "grey") +
    geom_line(aes(text = line_text, col = "R(t) mean", group = 1)) +
    scale_colour_manual("", values = "black") +
    labs(title = "R(t) estimate",
         x = "Window end date",
         y = "R(t)") +
    scale_x_date(date_breaks = "4 days") +
    coord_cartesian(ylim = c(0, 4)) +
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 40))
  
  plt %<>% ggplotly(tooltip = "text") 
  
  plt$x$data[[2]]$hoverinfo <- "none"
  plt$x$data[[2]]$name <- "95% CrI"
  plt$x$data[[3]]$name <- "R(t) mean"
  
  plt %>% layout(hoverlabel=list(bgcolor="white")) %>% plotly::config(displayModeBar = F)

  
  
  ######
  
  plot_decomposed <-
    dailyR_lognorm %>% plot("SI") %>% ggplot_build()
  
  p <- plot_decomposed$plot$data
  
  if ("SIDistr" %in% names(p)) {
  p %<>% transmute(value = SIDistr, Var1 = Times, Var2 = 1)
  }
  
  
  
  line_text <-
    paste0("Frequency: ",
           p$value,
           "\nSerial interval: ",
           p$Var1,
           "\nDistribution ID: ",
           p$Var2)
  
  plt <-
    p %>% ggplot(aes(Var1, value, group = Var2)) +
    geom_line(aes(text = line_text), alpha = .15) +
    labs(title = "Explored serial interval distribution(s)",
         x = "Serial interval [days]",
         y = "Frequency") +
    theme_minimal()
  
  plt %>% ggplotly(tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white")) %>% plotly::config(displayModeBar = F)
  
  