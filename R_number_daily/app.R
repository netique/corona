library(shiny)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(shinycssloaders)
library(lubridate)
# remotes::install_github("annecori/EpiEstim")
library(EpiEstim)

ui <- fluidPage(
  fluidRow(
    column(
      5,
      titlePanel(
        strong("Reproduction number in Czechia"),
        "Reproduction number in Czechia"
      ),
      h4(div(
        style = "text-align:justify",
        HTML(
          "Based on 7-day sliding window and serial interval distribution approximated by truncated lognormal distribution with parameters from
         <a href='https://doi.org/10.1101/2020.02.19.20025452'>Zhanwei et al. (2020)</a>. Data sourced from
         <a href='https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19'>official JSONs by MZČR & ÚZIS</a>."
        )
      )),
      em(
        HTML(
          '&copy; 2020 Jan Netík, source freely available at <a href="https://github.com/netique/corona">github.com/netique/corona</a>'
        )
      )
    ),
    column(2, br(),
           h1(strong(uiOutput(
             "r"
           ))),
           align = "center"),
    column(
      3,
      br(),
      br(),
      tableOutput("quick_table"),
      uiOutput("timestamp"),
      align = "center"
    )
  ),
  br(),
  
  fluidRow(column(10, withSpinner(
    plotOutput("plots")
  ))),
  fluidRow(column(10, tableOutput("table"), class = "display nowrap"), align = "center")
)

server <- function(input, output) {
  set.seed(1425)
  
  output$timestamp <- renderUI({
    HTML(paste0("Source data updated:<br>", modified(), " CEST"))
  })
  
  modified <- reactive({
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/testy.json")$modified %>%
      as_datetime %>% with_tz("Europe/Prague")
  })
  tested <-
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/testy.json")$data %>%
    transmute(
      date = dmy(datum),
      tested_per_day = `testy-den`,
      tested_cumul = `testy-celkem`
    )
  
  infected <-
    fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/nakaza.json") %>%
    transmute(
      date = as_date(datum),
      infected_per_day = pocetDen,
      infected_cumul = pocetCelkem
    )
  
  recovered_dead <-
    fromJSON(
      "https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/nakazeni-vyleceni-umrti-testy.json"
    )$data %>%
    transmute(
      date = as_date(datum),
      recovered_cumul = kumulovany_pocet_vylecenych,
      dead_cumul = kumulovany_pocet_umrti
    )
  
  # identical(recovered_dead$date, tested$date, infected$date)
  
  # filter out "nonpandemic days"
  df <- reactive({
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
      tail(1)
  })
  
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
  
  output$plots <- renderPlot({
    plot(fit(), "all")
  })
  
  output$table <- renderTable({
    tab <- fit()$R %>% rename(
      "window beginning" = t_start,
      "window ending" = t_end,
      "R mean" = "Mean(R)",
      "R standard deviation" =  "Std(R)",
      "2.5th percentile" = "Quantile.0.025(R)",
      "50th percentile" = "Median(R)",
      "97.5th percentile" = "Quantile.0.975(R)"
    ) %>% transmute(
      `window beginning`,
      `window ending`,
      `R mean`,
      `R standard deviation`,
      `2.5th percentile`,
      `50th percentile`,
      `97.5th percentile`
    )
    
    tab %<>% transmute(
      window = paste0("Days ", `window beginning`, "–", `window ending`),
      `R mean`,
      `R standard deviation`,
      `2.5th percentile`,
      `50th percentile`,
      `97.5th percentile`
    ) %>% column_to_rownames("window")
    tab
    
  }, rownames = T, colnames = T)
}

# Run the application
shinyApp(ui = ui, server = server)
