incidence_age <-
  tabPanel(
    "Incidence & age",

    h1(strong("Distribution of incidence among age groups"),
      style = "margin-top: 0px;"
    ),
    p(
      "Following plots are based on many wonderful ideas and inspirational feedback from the members of the", a(href = "https://covid19cz.cz/", "COVID19CZ"),
      "initiative (namely Jakub Steiner). The simpliest one depicts absolute daily incidence (columns) distributed across positive cases' age (rows).",
      "A bit more complicated plots are also available, showing proportional distribution among age categories in particular week (Mon-Su) etc.",
      "All plots are downloadable in a publication-ready PDF format, accompanied with information such as data source timestamp etc. (see the About, or Estimate tabs for further details).",
      
      strong(em("THE SECTION IS UNDER CONSTRUCTION!"))
    ),

    h3(strong("Daily incidence by age")),
    h4(em('based solely on KHS data, as the "complete" ones does not provide such a granularity; the excess reported by the laboratories is provided below')),
    fluidRow(column(12, withSpinner(
      plotlyOutput("age_time_days")
    ))),
    fluidRow(column(11, downloadButton("age_time_days_down"), align = "right")),
    br(),
    
    h3(strong("Cases reported only by laboratories (not KHS)")),
    fluidRow(column(12, withSpinner(
      plotlyOutput("labs_excess")
    ))),
    fluidRow(column(3, offset = 8, selectInput("lab_excess_scale", "Choose the scale of the y-axis.", choices = c("linear", "log10")))),
    br(),

    h3(strong("Weekly incidence by age categories")),
    fluidRow(column(12, withSpinner(
      plotlyOutput("age_time_weeks")
    ))),
    fluidRow(column(11, downloadButton("age_time_weeks_down"), align = "right")),
    br()
  )