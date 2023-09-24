#install.packages(setdiff(c("shiny", "dplyr", "tidyr", "shinyBS"), installed.packages()))
library(shiny)

# simulate_paths <- {
#   #input$recalc
#   if ("sz" %in% names(input)) {
#     print("sz is in input")
#     set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*input$tau*
#                        input$max_rt*input$sv*input$s)*ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+if_else(is.null(input$recalc), 1, 2)))
#     sz <- input$sz
#   } else {
#     set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*input$tau*
#                        input$max_rt*input$sv*input$s)*ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+if_else(is.null(input$recalc), 1, 2)))
#     sz <- 0
#   }
#   paths = simulatepaths(unlist(input[paramNames]), input$delta_t, input$max_rt, input$n_sim)
#   poststeps = ceiling(input$tau/input$delta_t)
#   steps = ceiling(input$max_rt/input$delta_t)
#   simus <- list(XDec   = paths[1:(steps+1),],
#                 Vis    = paths[(poststeps+steps+3):(1+ 2*(1+steps+poststeps)),],
#                 DT     = paths[2+ 2*(1+steps+poststeps),],
#                 Resp   = paths[3+ 2*(1+steps+poststeps),],
#                 VisEnd = paths[5+ 2*(1+steps+poststeps),]
#   )
#   simus$XPost1 <- paths[(steps+2):(steps+poststeps+2),simus$Resp==1]
#   simus$XPost2 <- paths[(steps+2):(steps+poststeps+2),simus$Resp==-1]
#   PostTimes <- matrix(rep(0:poststeps, input$n_sim), ncol=input$n_sim) + 
#     matrix(rep(simus$DT, each=(poststeps+1)), ncol=input$n_sim)
#   simus$PostTimes1 = PostTimes[,simus$Resp==1]
#   simus$PostTimes2 = PostTimes[,simus$Resp==-1]
#   
#   simus$DT1 <- (simus$DT[simus$Resp==1]+poststeps)*input$delta_t
#   simus$DT2 <- (simus$DT[simus$Resp==-1]+poststeps)*input$delta_t
#   simus$MRT1 <- mean(simus$DT1)
#   simus$MRT2 <- mean(simus$DT2)
#   resp_probs <- table(factor(simus$Resp, levels=c(1, -1,0)))/input$n_sim
#   simus$resp_prob1 <- resp_probs[1]
#   simus$resp_prob2 <- resp_probs[2]
#   simus$Conf1 <- paths[6+ 2*(1+steps+poststeps),simus$Resp==1]
#   simus$Conf2 <- paths[6+ 2*(1+steps+poststeps),simus$Resp==-1]
#   simus$XEnd1 <- paths[4+ 2*(1+steps+poststeps),simus$Resp==1]
#   simus$XEnd2 <- paths[4+ 2*(1+steps+poststeps),simus$Resp==-1]
#   simus
# }
source("ui.r")
source("server.r")
InteractivedynaViTE <- shinyApp(ui, server) 
runApp(InteractivedynaViTE)
