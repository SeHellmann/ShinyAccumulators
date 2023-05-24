ui <- fixedPage(
  #header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 8pt; }")),
  includeCSS("www/style.css"),
  titlePanel("Simulation des weighted evidence and visibility Modells",
             windowTitle = "Simulation des weighted evidence and visibility Modells"),
  fluidRow(
    column(3,
           tags$h4("WEV-Parameter:"),
           sliderInput("c", "Entscheidungskriterium:", min = -3, max = 3, value = 0, step = 0.02, width=NULL),
           sliderInput("d", "Mittlere Evidenz (discriminability) (d):", min = 0, max = 3.0, value = 1.0, step = 0.05, width=NULL),
           sliderInput("v", "Mittlere Sichtbarkeit (v):", min = 0, max = 5, value = 2, step=0.5, width=NULL)
    ),
    column(3,
           tags$h4("."),
           sliderInput("w", "Gewicht auf Sichtbarkeit für Konfidenz (w):", min = 0, max = 3, value = 0.5, step =0.05, width=NULL),
           sliderInput("sd", "Varianz der Evidenz (s_e):", min = 0.1, max = 3, value = 1, step = 0.1, width=NULL),
           sliderInput("sv", "Varianz der Sichtbarkeit (s_v):", min = 0.1, max = 5.0, value = 1.0, step = 0.1, width=NULL)
    ),
    column(3,
           tags$h4("Achsen-Grenzen:"),
           sliderInput("plot_maxX", "X-Achsen-Grenze für Evidenz:", min = 3, max = 15, value = 5, step = 1, width=NULL),
           sliderInput("plot_minV", "untere X-Achsen-Grenze für Sichtbarkeit:", min = -3, max = 5, value = -1, step = 1, width=NULL),
           sliderInput("plot_maxV", "obere X-Achsen-Grenze für Sichtbarkeit:", min = 0, max = 15, value = 5, step = 1, width=NULL)
    ),
    column(3,
           p(actionButton("recalc", "Re-run simulation", icon("random"))),
           sliderInput("plot_minConf", "untere X-Achsen-Grenze für Konfidenz:", min = -3, max = 5, value = -1, step = 1, width=NULL),
           sliderInput("plot_maxConf", "obere X-Achsen-Grenze für Konfidenz:",  min = 0, max = 15, value = 4, step = 1, width=NULL),
           sliderInput("n_sim", "Anzahl der simulierten Beobachtungen:", min = 10, max = 2000, value = 500, step = 10, width=NULL)
    )
  ),
  
   
  # fluidRow(
  #   column(6,
  #          plotOutput("a_distPlot", height = "600px")
  #   ),
  #   column(6,
  #          plotOutput("b_distPlot", height = "600px")
  #   )
  # )
  
  fluidRow(
    plotOutput(outputId = "pathsplot", height="400px")
  )
)
