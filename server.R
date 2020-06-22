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

server <- function(input, output) {
  
  shinyalert("", 'The number presented here is <b>only an estimate</b> and can be inaccurate and even erroneous.<br><br>Please, read the "About" tab carefully first!', type = "info", confirmButtonText = "Get it", closeOnClickOutside = TRUE, showCancelButton = FALSE, confirmButtonCol = "#32CD32", html = TRUE)
  
  output$title <-
    renderUI({
      strong(withMathJax("$R_t$", " in Czechia"), style = "font-size:22px")
    })
  
  output$subtitle <- renderUI({
    paste(
      "based on",
      switch(input$inc_method,
             raw = "raw",
             decomp = "seasonality adjusted"),
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
  
  output$r_title <- renderUI({
    h3(strong(withMathJax("Estimated $R_t$")))
  })
  
  output$si_title <- renderUI({
    h3(strong(ifelse(input$si_method == "par", "Explored serial distribution", "Explored serial distributions")))
  })
  
  
  output$data_sourced <- renderText({
    HTML(
      fromJSON(
        "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json"
      )$modified %>%
        as_datetime %>% with_tz("Europe/Prague") %>% as.character()
    )
  })
  
  output$r <- renderUI({
    r_num <- round(last(fit()$R[, 3]), 2)
    if (r_num > 1) {
      withMathJax(HTML(paste0('<span style=\"color:red;font-size: 56px;\">',
                  "$R_t$ = ",
                  r_num,
                  '</span>',
                  sep = "")))
    } else {
      withMathJax(HTML(paste0('<span style=\"font-size: 56px;\">',
                              "$R_t$ = ",
                  r_num,
                  sep = "")))
    }
  })
  
  output$last_day <- renderUI({
    HTML(paste0(
      "End of the last window: ",
      table() %>% tail(1) %>% pull(`Window end`),
      "<br>Source updated: ", fromJSON(
        "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json"
      )$modified %>%
        as_datetime %>% with_tz("Europe/Prague") %>% as.character() %>% str_remove(".{3}$")
    ))
  })
  
  inc <- reactive({
      inc <- fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")$data %>%
      transmute(
        dates = as_date(datum),
        I = prirustkovy_pocet_nakazenych
      ) %>% filter(dates >= "2020-03-01")
      
      if (input$inc_method == "raw") {
        inc
      } else {
        inc$I <- mstl(inc$I)[, "Trend"] %>% abs
        
        inc
      }
  })
  
  fit <- reactive({
    
  if (input$si_method == "par") {
    # Lognormal (Shape, Scale) 2.02 [1.76,2.31] 2.78 [2.39,3.25]
    
    # load generated distribution, for the sake of server load
    si_distr_lognorm <- read_rds("data/si_distr_lognorm.rds")
    
    estimate_R(inc(),
               method = "non_parametric_si",
               config = make_config(list(si_distr = si_distr_lognorm)))
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
               ))
  } else if (input$si_method == "mcmc") {
    # load generated distribution, for the sake of server load
    si_sample <- read_rds("data/si_sample.rds")
    
    estimate_R(inc(),
               method = "si_from_sample",
               si_sample = si_sample)
  }
  })

  # plots
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
      labs(x = "Window end date",
           y = "R(t)") +
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
  
  output$plot_si <- renderPlotly({
    plot_decomposed <-
      plot(fit(), "SI") %>%
      ggplot_build()
    
    p <- plot_decomposed$plot$data
    
    if ("SIDistr" %in% names(p)) {
      p %<>% transmute(value = SIDistr,
                       Var1 = Times,
                       Var2 = 1)
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
      labs(x = "Serial interval [days]",
           y = "Frequency") +
      theme_minimal()
    
    plt %>%
      ggplotly(tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white")) %>%
      plotly::config(displayModeBar = F)
  })
  
  output$plot_inc <- renderPlotly({
    plot_decomposed <-
      plot(fit(), "incid") %>% ggplot_build()
    
    p <- plot_decomposed$plot$data
    
    col_text <-
      paste0("Incidence: ",
             p$counts,
             "\nDate: ",
             p$dates)
    
    plt <-
      p %>% transmute(Date = dates, "Daily incidence" = counts) %>%
      ggplot(aes(x = Date, y = `Daily incidence`)) +
      geom_col(aes(text = col_text)) +
      labs(x = "Date",
           y = "Daily incidence [count]") +
      scale_x_date(date_breaks = "7 days") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 40))
    
    plt %<>% ggplotly(tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white")) %>%
      plotly::config(displayModeBar = F)
  })
  
  # table
  table <- reactive({
    tab <- fit()$R %>%
      rename(
        "Mean" = "Mean(R)",
        "SD" = "Std(R)",
        "2.5th perc." = "Quantile.0.025(R)",
        "50th perc." = "Median(R)",
        "97.5th perc." = "Quantile.0.975(R)"
      ) %>% transmute(
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
    renderDT({
      table() %>% datatable(
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
    }, server = FALSE)
  
  # download
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("Czech_R_t_",
             switch(
               input$si_method,
               par = "parametric",
               uncert = "uncertain",
               mcmc = "MCMC-MH"
             ),
             "_SI_",
             switch(input$inc_method,
                    raw = "raw",
                    decomp = "trend"),
             "_incidence_",
             Sys.Date(),
             ".csv")
    },
    content = function(file) {
      write_csv(table(), file)
    }
  )
  
}
