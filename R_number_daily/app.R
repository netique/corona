library(shiny)
library(dqshiny)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(shinycssloaders)
library(lubridate)
# remotes::install_github("annecori/EpiEstim")
library(EpiEstim)
library(DT)

ui <- fluidPage(
  titlePanel(
    strong("Reproduction number in Czechia"),
    "Reproduction number in Czechia"
  ),
  fluidRow(
    column(
      8,
      h4(div(
        style = "text-align:justify",
        HTML(paste0(
          "Based on 7-day sliding window and serial interval distribution approximated by truncated lognormal distribution with parameters from
         <a href='https://doi.org/10.3201/eid2606.200357'>Zhanwei et al. (2020)</a>. Data sourced from
         <a href='https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19'>official JSONs by MZČR & ÚZIS</a> (last change at source: ", textOutput("data_sourced", inline = TRUE),
          " CEST). Analysis based on ", strong("trend component only"), " (as achieved with ", em("multiple seasonal decomposition"), ") is available <a href='https://netique.shinyapps.io/R_CZ_trend/'>here</a>."
        ))
      )),
      em(
        HTML(
          '&copy; 2020 Jan Netík, source freely available at <a href="https://github.com/netique/corona">github.com/netique/corona</a>'
        )
      )
    ),
    column(2,
           h1(strong(uiOutput(
             "r"
           ))),
           strong(uiOutput("last_day")),
           align = "center"),
    column(
      2, h5(strong("Quick overview")),
      tableOutput("quick_table"),
      align = "center"
    )
  ),
  
  fluidRow(column(12, withSpinner(
    plotOutput("plot_r")
  ))),
  fluidRow(column(12, withSpinner(
    plotOutput("plot_inc")
  ))),
  fluidRow(column(12, withSpinner(
    plotOutput("plot_si")
  ))),
  
  fluidRow(column(12, DTOutput("table")), align = "center"), br(),
  fluidRow(column(12, downloadButton("download_table", "Download table")), align = "right"), br()
)

server <- function(input, output) {

  output$data_sourced <- renderText({
    HTML(modified() %>% as.character())
  })
  
  output$last_day <- renderUI({
    HTML(paste0(
      "Last window end:<br>",
      table() %>% tail(1) %>% pull(`Window end`)
    ))
  })
  
  modified <- reactive({
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/testy.json")$modified %>%
      as_datetime %>% with_tz("Europe/Prague")
  })

df <- reactive({
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
  
  # identical(recovered_dead$date, tested$date, infected$date)
  
  # filter out "nonpandemic days"

    bind_cols(tested, infected, recovered_dead) %>%
      as_tibble %>% dplyr::select(-c(date1, date2)) %>%
      filter(date >= "2020-03-01")
  })
  
  fit <- reactive({
    inc <- df() %>% transmute(dates = date, I = infected_per_day)
    
    # Lognormal (Shape, Scale) 2.02 [1.76,2.31] 2.78 [2.39,3.25]
    # si_distr_lognorm <-
    #   distcrete("lnorm", interval = 1, 2.02, 2.78)$r(2000) %>%
    #   extract(. < 20)
    # freq_lognorm <- table(si_distr_lognorm) %>% as.vector
    # si_distr_lognorm <- c(0, freq_lognorm / sum(freq_lognorm))
    # write_rds(si_distr_lognorm, "si_distr_lognorm.rds")
    
    # load generated distribution, for the sake of server load
    si_distr_lognorm <- read_rds("si_distr_lognorm.rds")
    
    dailyR_lognorm <-
      estimate_R(inc,
                 method = "non_parametric_si",
                 config = make_config(list(si_distr = si_distr_lognorm)))
    
    dailyR_lognorm
  })
  
  output$quick_table <- renderTable({
    # quick overview
    df() %>%
      select(contains("cumul")) %>%
      rename(
        "Tested" = 1,
        "Infected" = 2,
        "Recovered" = 3,
        "Dead" = 4
      ) %>%
      tail(1) %>% t
  }, rownames = TRUE, colnames = FALSE)
  
  output$r <- renderUI({
    r_num <- round(last(fit()$R[, 3]), 2)
    if (r_num > 1) {
      HTML(paste0(
        '<span style=\"color:red\">',
        "R = ",
        r_num,
        '</span>',
        sep = ""
      ))
    } else {
      HTML(paste0("R = ",
                  r_num,
                  sep = ""))
    }
  })
  
  output$plot_r <- renderPlot({
    plot(fit(), "R") + scale_fill_manual("", label = "95% CI", values = alpha("black", .15)) + theme_minimal() + theme(legend.position = c(.9, .75))
  }, res = 120)
  
  output$plot_si <- renderPlot({
    plot(fit(), "SI") + theme_minimal()
  }, res = 120)
  
  output$plot_inc <- renderPlot({
    plot(fit(), "incid") + theme_minimal()
  }, res = 120)
  
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
      callback = JS(
        'table.page("last").draw(false);'
      ),
      options = list(dom = "tip",
                     searching = FALSE,
                     pageLength = 10)
    ) %>%
      formatRound(
        columns = c("Mean", "SD", "2.5th perc.", "50th perc.", "97.5th perc."),
        digits = 2
      ) 
  }, server = FALSE)

output$download_table <- downloadHandler(
  filename = function() {
    paste("Czech-R-number_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write_csv(table(), file)
  }
)

}

# Run the application
shinyApp(ui = ui, server = server)
