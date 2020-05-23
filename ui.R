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

source("ui/ui_estimate.R", local = T, encoding = "UTF-8")
source("ui/ui_settings.R", local = T, encoding = "UTF-8")
source("ui/ui_about.R", local = T, encoding = "UTF-8")

ui <-
  navbarPage(
    windowTitle = "R estimate in Czechia",
    uiOutput("title"),
    # theme = "bootstrap.css",
    selected = "About",
    
    estimate,
    settings,
    about,
    
    # inline math fix
    tags$div(
      HTML(
        "<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            "
      )
    ),
    footer = list(
      HTML('<div class = "panel-footer", style = "opacity: 1.00; z-index: 1000;position: fixed;right: 0;bottom: 0;left: 0;"">
                    
                    <div class = "footer-title" style="
    padding-bottom: 6px;
">
                    $R_t$ in Czechia
                    </div>
                    <div class = "footer-copyright">
                    &copy; 2020 Jan Netík | source freely available at <a href="https://github.com/netique/corona">github.com/netique/corona</a>
                    </div>'),
      HTML('</div></div>'))
  )