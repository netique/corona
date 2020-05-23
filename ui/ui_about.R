about <- tabPanel("About",
                  HTML('<blockquote>
    <p>The reproduction number, R, is the average number of secondary cases of disease caused by a single infected individual over his or her infectious period.</p>
    <footer>
        <cite><a href="https://doi.org/10.1093/aje/kwt133">Cori et al. (2013)</a></cite>
    </footer>
</blockquote>'),
                  p(style = "text-align:justify", "To best of our knowledge, the web application provided here presents the only non-governmental estimate of the effective reproduction number $R_t$.",
                  "Our estimate strongly relies upon the results of papers published in impacted, peer-reviewed epidemiological journals (cited below, open access) and is based on serial interval (SI) parameters from Du et al. (2020). Daily incidence comes via an", a(href = "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19", "official API"), "by the Ministry of Health of the Czech Republic and The Institute of Health Information and Statistics of the Czech Republic.",
                  HTML('The coputation is done by R package "EpiEstim" (Cori et. al, 2013) develeped under the auspices of <a href="https://www.repidemicsconsortium.org/">R Epidemics Consortium (RECON)</a>.')),
                  p(style = "text-align:justify", strong('In order to obtain the $R_t$ estimate, please continue to the "Estimate" tab at the top left. You can make several settings (incidence adjustment for "seasonality" and SI distribution(s) aproximation method) in the "Settings" tab next to it. Note that all plots in the "Estimate" section are interactive, so feel free to inspect them by pointing with the cursor. Also note the downloadable table below the plots.')),

                  h3("Quick overview"),
                  tags$ul(
                    tags$li("based on probabilistic reconstruction of transmission trees, via the incidence data and distribution(s) of the serial interval (the time difference between the symptom onset in a primary and secondary case)"),
                  tags$li(HTML("Bayesian approach, intervals are thus 95% credible intervals (<a href='https://en.wikipedia.org/wiki/Credible_interval'>more on this topis there</a>).")),
                  tags$li("$R_t$ is calculated over 7-day windows and the number hence refers to the average transmissibility over that time window"),
                  tags$li("the uncertainty in the serial interval distribution parameters can be taken in the account by integrating over a range of means and standard deviations of the serial interval (see the Settings tab)")),
                  
                  h3("Important assumptions"),
                  tags$ul(
                  tags$li("the instantaneous reproduction number is constant within the time window (precision of these estimates depends directly on the number of incident cases in the time window)"),
                  tags$li("infectiousness starts at the time or after the disease onset (otherwise it leads to at least time-lagged estimates of $R_t$)"),
                  tags$li("all cases were detected - it was, however, shown in a simulation study by Cori et al. (2013) that this should not dramatically affect estimates as long as the proportion of asymptomatic cases and the reporting rate are constant through time"),
                  tags$li("the serial interval distribution is constant throughout the epidemic")
                  ),
                  
                  
                  h3("References"),
                  tags$ul(class = "biblio", style = "padding-bottom: 82px;",
                    tags$li(HTML('Du, Z., Xu, X., Wu, Y., Wang, L., Cowling, B. J., & Meyers, L. (2020). Serial interval of COVID-19 among publicly reported confirmed cases. <i>Emerging Infectious Diseases, 26</i>(6), 1341&#x2013;1343.'), a(href = "https://dx.doi.org/10.3201/eid2606.200357", "https://dx.doi.org/10.3201/eid2606.200357")),
                    
                    tags$li(HTML("Cori, A., Ferguson, N. M., Fraser, C., &amp; Cauchemez, S. (2013). A new framework and software to estimate time-varying reproduction numbers during epidemics. <i>American Journal of Epidemiology</i>, <i>178</i>(9), 1505&#x2013;1512."), a(href = "https://doi.org/10.1093/aje/kwt133", "https://doi.org/10.1093/aje/kwt133")),
                    tags$li("Komenda M., Karolyi M., Bulhart V., Žofka J., Brauner T., Hak J., Jarkovský J., Mužík J., Blaha M., Kubát J., Klimeš D., Langhammer P., Daňková Š., Májek O., Bartůňková M., Dušek L. COVID‑19: Přehled aktuální situace v ČR. Onemocnění aktuálně [online]. Praha: Ministerstvo zdravotnictví ČR, 2020 [cit. 23.05.2020]. Dostupné z:", a(href = "https://onemocneni-aktualne.mzcr.cz/covid-19", "https://onemocneni-aktualne.mzcr.cz/covid-19")),
                    tags$li(HTML('Thompson, R. N., Stockwin, J. E., van Gaalen, R. D., Polonsky, J. A., Kamvar, Z. N., Demarsh, P. A., &#x2026; Cori, A. (2019). Improved inference of time-varying reproduction numbers during infectious disease outbreaks. <i>Epidemics</i>, <i>29</i>, 100356.'), a(href = "https://doi.org/10.1016/j.epidem.2019.100356", "https://doi.org/10.1016/j.epidem.2019.100356")),
                    tags$li(HTML('Wallinga, J., Teunis, P. (2004). Different epidemic curves for Severe Acute Respiratory Syndrome reveal similar impacts of control measures. <i>American Journal of Epidemiology</i>, <i>160</i>(6), 509&#x2013;516.'), a(href = "https://doi.org/10.1093/aje/kwh255", "https://doi.org/10.1093/aje/kwh255"))
                  )
)