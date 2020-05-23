estimate <-
  tabPanel(
    "Estimate",
    
    fluidRow(column(9,
                    h1(strong("The time-varying instantaneous", br(),
                              "reproduction number in Czechia"),
                       style = "margin-top: 0px;"),
                    h2(em(strong(uiOutput("subtitle"))),
                       style = "margin-top: 6px; margin-bottom: 24px")),
             column(3,
                    h1(uiOutput(
                      "r"
                    )),
                    strong(uiOutput("last_day")),
                    align = "center")),
    
    uiOutput("r_title"),
    fluidRow(column(12, withSpinner(
      plotlyOutput("plot_r")
    ))),
    h3(strong("Epidemic curve")),
    fluidRow(column(12, withSpinner(
      plotlyOutput("plot_inc")
    ))),
    uiOutput("si_title"),
    fluidRow(column(12, withSpinner(
      plotlyOutput("plot_si")
    ))),
    
    h3(strong("Table")),
    fluidRow(column(12, DTOutput("table")), align = "center"),
    br(),
    fluidRow(column(
      12, downloadButton("download_table", "Download table")
    ), align = "right")
  )