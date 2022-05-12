README: Shiny Application for Visualizing Sequential Sampling Confidence Models
====

This repository contains a shiny app that simulates observations of the 2DSD model (Pleskac & Busemeyer, 2010) for various parameter sets. In addition, the branch "OnlyDDM" contains a version, in which only the basic DDM is simulated without any post-decisional accumulation. 

### Usage:
In R or RStudio, run
```
install.packages("shiny")
install.packages(c("dplyr", "tidyr")) # this is used for simulations
library(shiny)
runGitHub(repo="SeHellmann/ShinyAccumulators")
```
For the version that simulates the DDM, only, use the additional argument `ref` to refer to the respective branch of the repository.

```
install.packages("shiny")
install.packages(c("dplyr", "tidyr")) # this is used for simulations
library(shiny)
runGitHub(repo="SeHellmann/ShinyAccumulators", ref="OnlyDDM")
```


### Literature

Pleskac, T. J., & Busemeyer. (2010). Two-stage dynamic signal
detection: A theory of choice, decision time, and
confidence. Psychological review, 117(3). https://doi.org/10.1037/a0019737

Ratcliff, R. (1978). A theory of memory retrieval. Psychological Review, 85(2), 59–108. https://doi.org/10.1037/0033-295X.85.2.59