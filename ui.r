ui <- fixedPage(
  #header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 8pt; }")),
  includeCSS("www/style.css"),
  titlePanel("Visual Simulation of the weighted evidence and visibility model",
             windowTitle = "Visual Simulation of the weighted evidence and visibility model"),
  # input <- list(c = 0.05, sd=1, sv=1, w=0.5, n_sim= 100, 
  #               manipulation1 = c(0, 1, 2, 3),
  #               Einfluss_man1_d = 2, Einfluss_man1_v=0.5, 
  #               name_man1 = "Orientierung"
  #               )
  fluidRow(
    column(4,
           textInput("name_man1", "Was wird manipuliert?", value="Orientierung"),
           textInput("manipulation1", "Stufen der Manipulation", value="0, 1, 2, 3"),
           sliderInput("n_sim", "Anzahl der simulierten Beobachtungen:", min = 10, max = 2000, value = 500, step = 10, width=NULL),
           p(actionButton("recalc", "Re-run simulation", icon("random")))
    ),
    column(4,
           numericInput("Einfluss_man1_d", "Einfluss auf Evidenz (d)", value=1),
           numericInput("Einfluss_man1_v", "Einfluss auf Sichtbarkeit (v)", value=0.5),
           sliderInput("c", "Entscheidungskriterium:", min = -3, max = 3, value = 0, step = 0.02, width=NULL)
    ),
    column(4,
           sliderInput("w", "Gewicht auf Sichtbarkeit für Konfidenz (w):", min = 0, max = 1, value = 0.5, step =0.05, width=NULL),
           sliderInput("sd", "Varianz der Evidenz (s_e):", min = 0.1, max = 3, value = 1, step = 0.1, width=NULL),
           sliderInput("sv", "Varianz der Sichtbarkeit (s_v):", min = 0.1, max = 5.0, value = 1.0, step = 0.1, width=NULL)
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
