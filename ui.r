fluidPage(
  titlePanel("Visual Simulation of Sequential Sampling Models"),
  fluidRow(column(6,
    fluidRow(
    column(4,
           sliderInput("a", "Boundary separation (a):", min = 0.5, max = 15, value = 2, step=0.5, width=NULL),
           sliderInput("sv", "Standard deviation of drift rates:", min = 0, max = 20, value = 0.5, step =0.05, width=NULL),
           sliderInput("sz", "Range of starting points (sz):", min = 0.0, max = 0.2, value = 0, step = 0.05, width=NULL),
           sliderInput("s", "Diffusion variation (s):", min = 0.1, max = 20, value = 1, step = 0.1, width=NULL)
    ),
    column(4,
           sliderInput("z", "Mean starting poins (z):", min = 0, max = 1, value = 0.5, step = 0.05, width=NULL),
           sliderInput("v", "Mean drift rate (v):", min = -5.0, max = 5.0, value = 1.0, step = 0.1, width=NULL),
           sliderInput("tau", "Inter rating interval (tau):", min = 0.1, max = 5.0, value = 1.0, step = 0.1, width=NULL)
    ),
    column(4,
           sliderInput("delta_t", "Time step size for simulation:", min = 0.01, max = 0.2, value = 0.1, step = 0.01, width=NULL),
           sliderInput("max_rt", "Maximal decision time for simulation:", min = 2, max = 15, value = 5, width=NULL),
           sliderInput("n_sim", "Number of simulated paths:", min = 10, max = 2000, value = 500, step = 10, width=NULL),
           p(actionButton("recalc", "Re-run simulation", icon("random")))
    )
  )),
  column(6,
    plotOutput(outputId = "pathsplot", height="500px")
  ))
)