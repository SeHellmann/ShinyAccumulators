require(shinyBS)
library(shinyjs)
library(shiny)
ui <- fluidPage(
  #header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 8pt; }")),
  includeCSS("www/style.css"),
  withMathJax(), useShinyjs(),
  sidebarLayout(position="right",
                mainPanel = mainPanel(plotOutput(outputId = "pathsplot", height="700px")),
                sidebarPanel= sidebarPanel(
                  bsCollapse(id = "settings", open = NULL,
                             bsCollapsePanel("Set model parameters", 
                                             fluidRow(
                                               column(6, "Decision process parameters",
                                                      tags$h4("Basic DDM parameters"),
                                                      sliderInput("v", "Mean decision drift rate (\\(\\nu\\)):", 
                                                                  min = -2.0, max = 2.0, value = 1.0, step = 0.1, width=NULL),
                                                      sliderInput("a", "Boundary separation (\\(a\\)):", 
                                                                  min = 0.2, max = 3, value = 1, step=0.2, width=NULL),
                                                      sliderInput("z", "Mean starting poins (\\(z\\)):", 
                                                                  min = 0, max = 1, value = 0.5, step = 0.05, width=NULL),
                                                      tags$h4("Variabilities in decision process"),
                                                      sliderInput("s", "Decision diffusion variation (\\(s\\)):", 
                                                                  min = 0.1, max = 3, value = 1, step = 0.1, width=NULL),
                                                      sliderInput("sv", "SD of decision drift rates (\\(s_\\nu\\)):", 
                                                                  min = 0, max = 3, value = 0.5, step =0.05, width=NULL),
                                                      sliderInput("sz", "Range of starting points (\\(sz\\)):", 
                                                                  min = 0, max = 1, value = 0.1, step = 0.1, width=NULL)
                                               ),
                                               column(6, "Confidence parameters",
                                                      tags$h4("Visibility process"),
                                                      sliderInput("svis", "Visibility diffusion variation (\\(s_{Vis}\\)):", 
                                                                  min = 0.1, max = 3, value = 1, step = 0.1, width=NULL),
                                                      sliderInput("sigvis", "SD of visibility drift rates (\\(\\sigma_{Vis}\\)):", 
                                                                  min = 0, max = 3, value = 0.5, step =0.05, width=NULL),
                                                      checkboxInput("checkvismu", "Automatically set visibility drift to absolute decision drift", value = TRUE),
                                                      withMathJax(uiOutput("slidrmuvis")),
                                                      tags$h4("Confidence computation"),
                                                      sliderInput("tau", "Inter rating interval (\\(\\tau\\)):", 
                                                                  min = 0.1, max = 5.0, value = 1.0, step = 0.1, width=NULL),
                                                      sliderInput("w", "Weight on decision state (\\(w\\)):", 
                                                                  min = 0, max = 1, value = 0.5, step = 0.1, width=NULL),
                                                      sliderInput("lambda", "Exponent for accumulation time (\\(\\lambda\\)):", 
                                                                  min = 0, max = 2, value = 1, step =0.1, width=NULL)
                                               )
                                             ), style = "info"),
                             bsCollapsePanel("Set simulation and visualization parameters", 
                                             fluidRow(
                                               column(6, "Simulation parameters",
                                                      sliderInput("delta_t", "Time step size for simulation:", 
                                                                  min = 0.005, max = 0.15, value = 0.01, step = 0.005, width=NULL),
                                                      sliderInput("max_rt", "Maximal decision time for simulation:", 
                                                                  min = 2, max = 8, value = 3, width=NULL),
                                                      sliderInput("n_sim", "Number of simulated paths:", 
                                                                  min = 10, max = 2000, value = 500, step = 10, width=NULL)
                                               ),
                                               column(6, "Visualization parameters",
                                                      sliderInput("alpha", "Path transparency (scaled):", 
                                                                  min = 0, max = 1, value = 0.5, step = 0.1, width=NULL)
                                               )
                                             ), style = "info")
                             ),
                  p(actionButton("recalc","Re-run simulation", icon("random")))# HTML(paste("Re-run", "simulation", sep="<br/>"))
                  )
  )
)
# fluidRow(
#   column(6,
#          plotOutput("a_distPlot", height = "600px")
#   ),
#   column(6,
#          plotOutput("b_distPlot", height = "600px")
#   )
# )