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
library(shinyalert)

source("ui/ui_estimate.R", local = T, encoding = "UTF-8")
source("ui/ui_settings.R", local = T, encoding = "UTF-8")
source("ui/ui_about.R", local = T, encoding = "UTF-8")

ui <-
  navbarPage(
    windowTitle = "R estimate in Czechia",
    uiOutput("title"),
    # theme = "bootstrap.css",
    selected = "Estimate",
    
    estimate,
    settings,
    about,
    
    footer = list(
      HTML(
        '<div style = "clear: both; height: 112px;"></div>
         <div class = "panel-footer", style = "opacity: 1.00; z-index: 1000; position: fixed; right: 0;bottom: 0;left: 0;">
         <div class = "footer-title" style="padding-bottom: 6px;">
            $R_t$ in Czechia
         </div>
         <div class = "footer-copyright">
            &copy; 2020 Jan Netík | source freely available at <a href="https://github.com/netique/corona"          >github.com/netique/corona</a>
         </div>
        <script type="text/x-mathjax-config">
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [["$","$"]]}
            });
            </script>'
      )
    )
  )