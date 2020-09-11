library(shiny)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(shinycssloaders)
library(lubridate)
# remotes::install_github("annecori/EpiEstim")
library(EpiEstim)
library(DT)
library(dqshiny)
library(forecast)
library(shinythemes)
library(plotly)
library(patchwork)
if (!require(mtaux)) {
  remotes::install_github("netique/mtaux")
} else {
  library(mtaux)
} # just theme


server <- function(input, output) {

  # R ESTIMATE ##############################################################
  ## ___disclaimer ----
  shinyalert("", 'The number presented here is <b>only an estimate</b> and can be inaccurate and even erroneous.<br><br>Please, read the "About" tab carefully first!', type = "info", confirmButtonText = "Got it!", closeOnClickOutside = TRUE, showCancelButton = FALSE, confirmButtonCol = "#32CD32", html = TRUE)

  # ___title w/ math format ----
  output$title <-
    renderUI({
      strong(withMathJax("$R_t$", " in Czechia"), style = "font-size:22px")
    })

  # ___subtitle (dynamically updated) ----
  output$subtitle <- renderUI({
    paste(
      "based on",
      switch(input$inc_method,
        raw = "raw",
        decomp = "seasonality adjusted"
      ),
      "incidence &",
      switch(
        input$si_method,
        par = "parametric",
        uncert = "uncertain",
        mcmc = "MCMC-MH estimated"
      ),
      "serial interval"
    )
  })

  # ___R heading style ----
  output$r_title <- renderUI({
    h3(strong(withMathJax("Estimated $R_t$")))
  })

  # ___SI heading style + dynamic update ----
  output$si_title <- renderUI({
    h3(strong(ifelse(input$si_method == "par", "Explored serial distribution", "Explored serial distributions")))
  })

  # ___data updated at source ----
  output$data_sourced <- renderText({
    HTML(
      fromJSON(
        "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json"
      )$modified %>%
        as_datetime() %>% with_tz("Europe/Prague") %>% as.character()
    )
  })

  # ___conditional main R number rendering ----
  output$r <- renderUI({
    r_num <- round(last(fit()$R[, 3]), 2)
    if (r_num > 1) {
      withMathJax(HTML(paste0('<span style=\"color:red;font-size: 56px;\">',
        "$R_t$ = ",
        r_num,
        "</span>",
        sep = ""
      )))
    } else {
      withMathJax(HTML(paste0('<span style=\"font-size: 56px;\">',
        "$R_t$ = ",
        r_num,
        sep = ""
      )))
    }
  })

  # ___week window end ----
  output$last_day <- renderUI({
    HTML(paste0(
      "End of the last window: ",
      table() %>% tail(1) %>% pull(`Window end`),
      "<br>Source updated: ", fromJSON(
        "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json"
      )$modified %>%
        as_datetime() %>% with_tz("Europe/Prague") %>% as.character() %>% str_remove(".{3}$")
    ))
  })

  # ___daily incidence, w/ optional seasonality decomposition ----
  inc <- reactive({
    inc <- fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")$data %>%
      transmute(
        dates = as_date(datum),
        I = prirustkovy_pocet_nakazenych
      ) %>%
      filter(dates >= "2020-03-01")

    if (input$inc_method == "raw") {
      inc
    } else {
      inc$I <- mstl(inc$I)[, "Trend"] %>% abs()

      inc
    }
  })

  # ___the R estimate itself ----
  fit <- reactive({
    if (input$si_method == "par") {

      # load generated distribution, for the sake of server load
      si_distr_lognorm <- read_rds("data/si_distr_lognorm.rds")

      estimate_R(inc(),
        method = "non_parametric_si",
        config = make_config(list(si_distr = si_distr_lognorm))
      )
    } else if (input$si_method == "uncert") {

      # shorcut for defining symmetrical truncation
      si_mean <- 3.96
      si_sd <- 4.75

      si_mean_trunc <- 2.75
      si_sd_trunc <- 4

      estimate_R(inc(),
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
        )
      )
    } else if (input$si_method == "mcmc") {

      # load PRE-generated distribution, for the sake of server load
      si_sample <- read_rds("data/si_sample.rds")

      estimate_R(inc(),
        method = "si_from_sample",
        si_sample = si_sample
      )
    }
  })


  # Interactive plots w/ plotly ---------------------------------------------
  # ___R ----
  output$plot_r <- plotly::renderPlotly({
    plot_decomposed <- plot(fit(), "R") %>% ggplot_build()

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
      geom_ribbon(aes(
        ymin = lower,
        ymax = upper,
        text = "",
        fill = "95% CrI"
      ), alpha = .7) +
      scale_fill_manual("", values = "grey") +
      geom_line(aes(
        text = line_text,
        col = "R(t) mean",
        group = 1
      )) +
      scale_colour_manual("", values = "black") +
      labs(
        x = "Window end date",
        y = "R(t)"
      ) +
      scale_x_date(date_breaks = "7 days") +
      coord_cartesian(ylim = c(0, 4)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 40))

    plt %<>% ggplotly(tooltip = "text")

    plt$x$data[[2]]$hoverinfo <- "none"
    plt$x$data[[2]]$name <- "95% CrI"
    plt$x$data[[3]]$name <- "R(t) mean"

    plt %>%
      layout(hoverlabel = list(bgcolor = "white")) %>%
      plotly::config(displayModeBar = F)
  })

  # ___SI ----
  output$plot_si <- renderPlotly({
    plot_decomposed <-
      plot(fit(), "SI") %>%
      ggplot_build()

    p <- plot_decomposed$plot$data

    if ("SIDistr" %in% names(p)) {
      p %<>% transmute(
        value = SIDistr,
        Var1 = Times,
        Var2 = 1
      )
    }

    line_text <-
      paste0(
        "Frequency: ",
        p$value %>% round(3),
        "\nSerial interval: ",
        p$Var1,
        "\nDistribution: ",
        p$Var2,
        "/",
        p$Var2 %>% max()
      )

    plt <-
      p %>% ggplot(aes(Var1, value, group = Var2)) +
      geom_line(aes(text = line_text), alpha = .15) +
      labs(
        x = "Serial interval [days]",
        y = "Frequency"
      ) +
      theme_minimal()

    plt %>%
      ggplotly(tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white")) %>%
      plotly::config(displayModeBar = F)
  })

  # ___epidemic curve (daily incidence lineplot) ----
  output$plot_inc <- renderPlotly({
    plot_decomposed <-
      plot(fit(), "incid") %>% ggplot_build()

    p <- plot_decomposed$plot$data

    col_text <-
      paste0(
        "Incidence: ",
        p$counts,
        "\nDate: ",
        p$dates
      )

    plt <-
      p %>%
      transmute(Date = dates, "Daily incidence" = counts) %>%
      ggplot(aes(x = Date, y = `Daily incidence`)) +
      geom_col(aes(text = col_text)) +
      labs(
        x = "Date",
        y = "Daily incidence [count]"
      ) +
      scale_x_date(date_breaks = "7 days") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 40))

    plt %<>% ggplotly(tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white")) %>%
      plotly::config(displayModeBar = F)
  })

  # ___downloadable table constructor ----
  table <- reactive({
    tab <- fit()$R %>%
      rename(
        "Mean" = "Mean(R)",
        "SD" = "Std(R)",
        "2.5th perc." = "Quantile.0.025(R)",
        "50th perc." = "Median(R)",
        "97.5th perc." = "Quantile.0.975(R)"
      ) %>%
      transmute(
        `Window end` = as.character(as_date(t_end, origin = "2020-02-29")),
        `Mean`,
        `SD`,
        `2.5th perc.`,
        `50th perc.`,
        `97.5th perc.`
      )

    tab
  })

  output$table <-
    renderDT(
      {
        table() %>%
          datatable(
            rownames = FALSE,
            selection = "none",
            callback = JS('table.page("last").draw(false);'),
            options = list(
              dom = "tip",
              searching = FALSE,
              pageLength = 10
            )
          ) %>%
          formatRound(
            columns = c("Mean", "SD", "2.5th perc.", "50th perc.", "97.5th perc."),
            digits = 2
          )
      },
      server = FALSE
    )

  # ___table download handler  ----
  output$download_table <- downloadHandler(
    filename = function() {
      paste0(
        "Czech_R_t_",
        switch(
          input$si_method,
          par = "parametric",
          uncert = "uncertain",
          mcmc = "MCMC-MH"
        ),
        "_SI_",
        switch(input$inc_method,
          raw = "raw",
          decomp = "trend"
        ),
        "_incidence_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      write_csv(table(), file)
    }
  )



  # INCIDENCE & AGE #########################################################
  # only "validated" data from HKS!!!
  khs_raw <- reactive({
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.json")
  })

  khs_updated <- reactive({
    khs_raw()$modified %>%
      as_datetime(tz = "Europe/Prague") %>%
      strftime(usetz = TRUE)
  })

  khs <- reactive({
    khs_raw()$data %>%
      transmute(
        date = ymd(datum),
        age = vek
        # sex = as.factor(pohlavi),
        # nuts = kraj_nuts_kod,
        # lau = okres_lau_kod,
        # inf_abroad = nakaza_v_zahranici,
        # country = nakaza_zeme_csu_kod
      ) %>%
      as_tibble() %>%
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
  })

  # all avaiable data
  khs_labs_raw <- reactive({
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")
  })

  khs_labs_updated <- reactive({
    khs_labs_raw()$modified %>%
      as_datetime(tz = "Europe/Prague") %>%
      strftime(usetz = TRUE)
  })

  khs_labs <- reactive({
    khs_labs_raw()$data %>%
      as_tibble() %>%
      transmute(date = ymd(datum), inc_khs_labs = prirustkovy_pocet_nakazenych)
  })

  # last data timestamp
  data_updated <- reactive({
    if (identical(khs_updated(), khs_labs_updated())) {
      paste("KHS & labs data both updated", kh_updated())
    } else {
      paste("data updated:", khs_updated(), "(KHS),", khs_labs_updated(), "(labs)")
    }
  })

  # aggregated khs data and khs_labs comparison
  comparison <- reactive({
    comparison <- khs() %>%
      count(date, name = "inc_khs") %>%
      left_join(khs_labs())

    # khs_labs excess over khs only incidence
    comparison %>%
      mutate(diff = inc_khs_labs - inc_khs, diff_plus_one = diff + 1)
  })


  # weeks with complete (i.e. 7) observations
  complete_weeks <- reactive({
    khs() %>%
      group_by(week) %>%
      transmute(n_days = n_distinct(date)) %>%
      filter(n_days == 7) %>%
      pull(week) %>%
      unique()
  })


  age_time_weeks <- reactive({
    khs() %>%
      filter(week %in% complete_weeks()) %>%
      ggplot(aes(week, age_group)) +
      geom_bin2d(drop = FALSE, binwidth = 1) +
      scale_fill_viridis_c() +
      scale_x_continuous(n.breaks = 15) +
      coord_cartesian(expand = FALSE) +
      labs(
        title = "Weekly incidence by age categories in Czechia",
        subtitle = "based only on highly incomplete, so-called validated KHS data; only completely observed weeks are shown",
        y = "age group",
        caption = paste("data updated", khs_updated() %>% strftime(usetz = TRUE), " |  © 2020 Jan Netík, source at github.com/netique/corona")
      ) +
      mtaux::theme_mt(
        axis.ticks = element_line(color = "gray80"),
        plot.caption = element_text(face = "italic"),
        plot.title.position = "panel",
        plot.subtitle = element_text(margin = margin(b = 12, t = -3))
      )
  })

  output$age_time_weeks <- renderPlotly({
    (age_time_weeks() + ggtitle("")) %>%
      ggplotly() %>%
      plotly::config(displayModeBar = F)
  })

  output$age_time_weeks_down <- downloadHandler(
    filename = function() {
      "age_time_weeks.pdf"
    },
    content = function(file) {
      ggsave(file, plot = age_time_weeks(), device = cairo_pdf, width = 9.81, height = 6.76)
    }
  )
  # ggsave(here("plots", "age_time_weeks.png"), width = 9.81, height = 6.76)


  # Jakub Steiner's idea, divide counts by total incidence,
  # so the proportion of given age group in the given week is shown
  age_prop_time_week <- reactive({
    khs() %>%
      count(week, age_group, name = "week_age_inc") %>%
      group_by(week) %>%
      mutate(week_inc = sum(week_age_inc), pct_age = week_age_inc / week_inc) %>%
      group_by(week) %>%
      # filter(week %in% complete_weeks() ) %>%
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
        caption = paste("data updated", khs_updated(), " |  © 2020 Jan Netík, source at github.com/netique/corona")
      ) +
      mtaux::theme_mt(
        axis.ticks = element_line(color = "gray80"),
        panel.grid.major.y = element_line(color = "gray80"),
        plot.caption = element_text(face = "italic"),
        plot.title.position = "panel",
        plot.subtitle = element_text(margin = margin(b = 12, t = -3))
      )
  })

  # ggsave(here("plots", "age_prop_time_week.png"), width = 9.81, height = 6.76)

  # maximal granularity plot (year of age / days)
  age_time_days <- reactive({
    khs() %>%
      ggplot(aes(date, age)) +
      geom_bin2d(drop = FALSE, binwidth = 1) +
      scale_x_date(date_breaks = "2 weeks", labels = function(x) {
        paste0(day(x), "/", month(x))
      }) +
      scale_y_continuous(n.breaks = 15) +
      xlab("day/month") +
      scale_fill_viridis_c() +
      coord_cartesian(expand = FALSE) +
      mtaux::theme_mt(
        axis.ticks = element_line(color = "gray80"),
        plot.caption = element_text(face = "italic"),
        plot.title.position = "panel"
      )
  })

  age_time_days_sep <- reactive({
    age_time_days() + labs(
      title = "Daily incidence by age in Czechia",
      subtitle = "based only on highly incomplete, so-called validated KHS data",
      caption = "by Jan Netík, source at github.com/netique/corona"
    ) + theme(plot.subtitle = element_text(margin = margin(b = 12, t = -3)))
  })

  # ggsave(here("plots", "age_time_days.png"), width = 9.81, height = 6.76)

  labs_excess_patch <- reactive({
    comparison() %>%
      ggplot(aes(date, diff)) +
      geom_line() +
      scale_y_continuous(n.breaks = 3) +
      ylab("labs exc. inc.") +
      coord_cartesian(expand = FALSE) +
      mtaux::theme_mt(axis.text.x = element_blank(), axis.title.x = element_blank())
  })


labs_excess <- reactive({
  comparison() %>%
    ggplot(aes(date, switch(input$lab_excess_scale,
      "log10" = diff_plus_one,
      "linear" = diff
    ))) + # for log
    geom_line() +
    {
      switch(input$lab_excess_scale,
        "log10" = scale_y_log10(n.breaks = 6),
        "linear" = scale_y_continuous()
      )
    } +
    scale_x_date(date_breaks = "2 weeks", labels = function(x) {
      paste0(day(x), "/", month(x))
    }) +
    xlab("day/month") +
    ylab(paste0("labs excess incidence", if (input$lab_excess_scale == "log10") {" [log10]"})) +
    coord_cartesian(expand = FALSE) +
    mtaux::theme_mt()
})

output$labs_excess <- renderPlotly({
  labs_excess() %>% ggplotly() %>%
    plotly::config(displayModeBar = F)
})



  patchwork <- reactive({
    labs_excess_patch() + age_time_days() + plot_layout(heights = c(1, 5))
  })

  res_patchwork <- reactive({
    patchwork() + plot_annotation(
      title = "Daily incidence by age in Czechia",
      subtitle = "only KHS data stratified by age available, excess incidence reported by labs shown in the upper plot",
      caption = paste(data_updated(), " |  © 2020 Jan Netík, source at github.com/netique/corona")
    ) &
      theme(
        plot.title = element_text(family = "Roboto", face = "bold", size = 16, colour = "grey30"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 11, colour = "grey30"),
        plot.caption = element_text(family = "Roboto Condensed", face = "italic", colour = "grey30")
      )
  })


  output$age_time_days <- renderPlotly({
    age_time_days() %>%
      ggplotly() %>%
      plotly::config(displayModeBar = F)
  })
  output$age_time_days_down <- downloadHandler(
    filename = function() {
      "age_time_days.pdf"
    },
    content = function(file) {
      ggsave(file, plot = res_patchwork(), device = cairo_pdf, width = 9.81, height = 6.76)
    }
  )

  # ggsave(here("plots", "age_time_days_with_labs_excess_incidence.png"), width = 9.81, height = 6.76)
}