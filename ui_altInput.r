ui <- fixedPage(
  #header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 8pt; }")),
  includeCSS("www/style.css"),
  titlePanel("Visual Simulation of the weighted evidence and visibility model",
             windowTitle = "Visual Simulation of the weighted evidence and visibility model"),
  fluidRow(
    column(2,
           textInput("man1", "Manipulierte Parameter 1:", value="d, v"),
           textInput("funcs1", "Funktionen für Manipulation 1:", value="x, abs(x)"),
           textInput("steps1", "Werte f. Manipulation 1:", value="0,1,2,3")
    ),
    column(2,
           textInput("man2", "Manipulierte Parameter 2:", value="d, v"),
           textInput("funcs2", "Funktionen für Manipulation 2:", value="x, abs(x)"),
           textInput("steps2", "Werte f. Manipulation 2:", value="0,1,2,3")
    ),
    column(2,
           sliderInput("c", "Entscheidungskriterium:", min = -3, max = 3, value = 0, step = 0.02, width=NULL),
           sliderInput("d", "Mittlere Evidenz (discriminability) (d):", min = 0, max = 3.0, value = 1.0, step = 0.05, width=NULL),
           sliderInput("v", "Mittlere Sichtbarkeit (v):", min = 0, max = 5, value = 2, step=0.5, width=NULL)
    ),
    column(2,
           sliderInput("w", "Gewicht auf Sichtbarkeit für Konfidenz (w):", min = 0, max = 1, value = 0.5, step =0.05, width=NULL),
           sliderInput("sd", "Varianz der Evidenz (s_e):", min = 0.1, max = 3, value = 1, step = 0.1, width=NULL),
           sliderInput("sv", "Varianz der Sichtbarkeit (s_v):", min = 0.1, max = 5.0, value = 1.0, step = 0.1, width=NULL)
    ),
    column(2,
           sliderInput("plot_maxX", "X-Achsen-Grenze für Evidenz:", min = 3, max = 15, value = 5, step = 1, width=NULL),
           sliderInput("plot_minV", "untere X-Achsen-Grenze für Sichtbarkeit:", min = 0, max = 15, value = 5, step = 1, width=NULL),
           sliderInput("plot_maxV", "obere X-Achsen-Grenze für Sichtbarkeit:", min = -3, max = 5, value = -3, step = 1, width=NULL),
           p(actionButton("recalc", "Re-run simulation", icon("random")))
    ),
    column(2,
           sliderInput("plot_minConf", "untere X-Achsen-Grenze für Konfidenz:", min = 0, max = 15, value = 5, step = 1, width=NULL),
           sliderInput("plot_maxConf", "obere X-Achsen-Grenze für Konfidenz:", min = -3, max = 5, value = -3, step = 1, width=NULL),
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
