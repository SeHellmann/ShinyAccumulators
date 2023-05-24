ui <- fixedPage(
  #header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 8pt; }")),
  includeCSS("www/style.css"),
  titlePanel("Simulation zweier Manipulationen im WEV Model",
             windowTitle = "Simulation zweier Manipulationen im WEV Model"),
  # input <- list(sd=1, sv=1, w=0.5, n_sim= 100, 
  #               manipulation1 = "0, 1, 2, 3",manipulation2 = "1, 3",
  #               Einfluss_man1_d = 2, Einfluss_man1_v=0.5, 
  #               Einfluss_man2_d=1, Einfluss_man2_v=1,
  #               name_man1 = "Orientierung", name_man2="Kontrast"
  # )
  fluidRow(
    column(4,
           tags$h5("Manipulation 1"),
           textInput("name_man1", "Was wird manipuliert?", value="Orientierung"),
           textInput("manipulation1", "Stufen der Manipulation 1", value="0, 1, 2, 3"),
           numericInput("Einfluss_man1_d", "Einfluss von Manipulation 1 auf Evidenz (d)", value=1),
           numericInput("Einfluss_man1_v", "Einfluss von Manipulation 1 auf Sichtbarkeit (v)", value=0.5)
    ),
    column(4,
           tags$h5("Manipulation 2"),
           textInput("name_man2", "Was wird manipuliert?", value="Kontur"),
           textInput("manipulation2", "Stufen der Manipulation 2", value="0.5, 3"),
           numericInput("Einfluss_man2_d", "Einfluss von Manipulation 2 auf Evidenz (d)", value=0.1),
           numericInput("Einfluss_man2_v", "Einfluss von Manipulation 2 auf Sichtbarkeit (v)", value=1.5)
           
    ),
    column(2,
           tags$h5("Weitere Parameter:"),
           sliderInput("w", "Gewicht auf Sichtbarkeit für Konfidenz (w):", min = 0, max = 1, value = 0.5, step =0.05, width=NULL),
           sliderInput("sd", "Varianz der Evidenz (s_e):", min = 0.1, max = 3, value = 1, step = 0.1, width=NULL)
    ),
    column(2,
           tags$h5(" . "),
           sliderInput("n_sim", "Anzahl der simulierten Beobachtungen:", min = 10, max = 2000, value = 500, step = 10, width=NULL),
           sliderInput("sv", "Varianz der Sichtbarkeit (s_v):", min = 0.1, max = 5.0, value = 1.0, step = 0.1, width=NULL)
    )
  ),

  fluidRow(
    plotOutput(outputId = "pathsplot", height="600px")
  )
)
