#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(pander)
library(rmarkdown)
library(growthcurver)
library(distcrete)
library(shinycssloaders)
library(lubridate)
# remotes::install_github("reconhub/incidence")
library(incidence)
# remotes::install_github("annecori/EpiEstim")
library(EpiEstim)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(column(5, style='padding-left:52px;', titlePanel(strong("Reproduction number in Czechia"), "Reproduction number in Czechia"), h4(div(style="text-align:justify",
    HTML("Based on 7-day sliding windows and serial interval distribution approximated by truncated lognormal distribution with parameters from
         <a href='https://doi.org/10.1101/2020.02.19.20025452'>Zhanwei et al. (2020)</a>."))),
    em(HTML('&copy; 2020 Jan Netík, source freely available at <a href="https://github.com/netique/corona">github.com/netique/corona</a>')
    )),
             column(2, br(),
                    h1(strong(uiOutput("r"))),
                    strong(uiOutput("last_day")),
                    align = "center"),
    column(3, br(),br(), tableOutput("quick_table"), uiOutput("timestamp"), align = "center")
    ), br(),
    
    fluidRow(
        column(10, withSpinner(plotOutput("plots")))
    ),
    fluidRow(column(10, tableOutput("table"), class = "display nowrap"), align = "center"),
   
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    
    set.seed(1425)

    output$timestamp <- renderUI({
        HTML(paste0("Data sourced from MZČR<br>(", now("CET"), ")"))
    })
        raw <- reactive({fromJSON("https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")})
    
    
    output$quick_table <- renderTable({tibble(
        infected = raw()$infected,
        tested = raw()$totalTested,
        recovered = raw()$recovered
    )})
    
    df_cum <- reactive({
        tibble(
            date = as.Date(raw()$totalPositiveTests$date),
            positive = as.integer(raw()$totalPositiveTests$value)
        ) %>% filter(positive > 0) %>% filter(date + days(1) + hours(10) < now())
    })
    
    inc <- reactive({tibble(dates = df_cum()$date, I = diff(c(0, df_cum()$positive)))})
    
    fit <- reactive({
        # Lognormal (Shape, Scale) 2.02 [1.76,2.31] 2.78 [2.39,3.25] 
        si_distr_lognorm <- distcrete("lnorm", interval = 1, 2.02, 2.78)$r(2000) %>% extract(.<20)
        
        freq_lognorm <- table(si_distr_lognorm) %>% as.vector
        si_distr_lognorm <- c(0, freq_lognorm/sum(freq_lognorm))
        
        dailyR_lognorm <- estimate_R(inc(), 
                                     method = "non_parametric_si",
                                     config = make_config(list(
                                         si_distr = si_distr_lognorm)))
        
        dailyR_lognorm
    })
    
    output$r <- renderUI({
        HTML(paste0("R = ", round(last(fit()$R[, 3]), 2)))
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
        
        tab_transposed <- tab %>% select(-c(`window beginning`, `window ending`)) %>% t
        colnames(tab_transposed) <- tab %>% transmute(window = paste0("Days ", `window beginning`, "–", `window ending`)) %>% pull
            
        tab_transposed
        
    }, rownames = T, colnames = T)
    
    output$last_day <- renderUI({
        HTML(paste0("Last day of window:<br>", pull(tail(df_cum(), 1)[1])))
})
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
