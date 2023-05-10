README: Shiny Application for simulating the weighted evidence and visibility model (WEV)
====

This branch contains a shiny app that simulates observations of the WEV model (Rausch et al., 2019) for various parameter sets.  

### Usage:
In R or RStudio, run
```
install.packages("shiny")
install.packages(c("dplyr", "tidyr")) # this is used for simulations
library(shiny)
runGitHub(repo="SeHellmann/ShinyAccumulators", ref="WEVmodel")
```


### Literature

Rausch, M., Hellmann, S., & Zehetleitner, M. (2018). Confidence in masked orientation discrimination decisions is informed by both evidence and visibility. Attention, Perception, & Psychophysics, 80, 134-155. https://doi.org/10.3758/s13414-017-1431-5
