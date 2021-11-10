ui <- fixedPage(
  #header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 8pt; }")),
  includeCSS("www/style.css"),
  titlePanel("Visual Simulation of Sequential Sampling Models",
             windowTitle = "Visual Simulation of Sequential Sampling Models"),
  fluidRow(
    column(3,
           sliderInput("z", "Mean starting poins (z):", min = 0, max = 1, value = 0.5, step = 0.05, width=NULL),
           sliderInput("v", "Mean drift rate (v):", min = -2.0, max = 2.0, value = 1.0, step = 0.1, width=NULL),
           sliderInput("a", "Boundary separation (a):", min = 0.5, max = 5, value = 2, step=0.5, width=NULL)
    ),
    column(3,
           sliderInput("sv", "Standard deviation of drift rates (sv):", min = 0, max = 3, value = 0.5, step =0.05, width=NULL),
           sliderInput("s", "Diffusion variation (s):", min = 0.1, max = 3, value = 1, step = 0.1, width=NULL),
           sliderInput("tau", "Inter rating interval (tau):", min = 0.1, max = 5.0, value = 1.0, step = 0.1, width=NULL)
    ),
    column(3,
           sliderInput("plot_maxt", "X-Achsen-Grenze (im linken Plot):", min = 3, max = 15, value = 5, step = 1, width=NULL),
           p(actionButton("recalc", "Re-run simulation", icon("random")))
    ),
    column(3,
           sliderInput("delta_t", "Time step size for simulation:", min = 0.01, max = 0.2, value = 0.1, step = 0.01, width=NULL),
           sliderInput("max_rt", "Maximal decision time for simulation:", min = 2, max = 15, value = 5, width=NULL),
           sliderInput("n_sim", "Number of simulated paths:", min = 10, max = 2000, value = 500, step = 10, width=NULL)
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
