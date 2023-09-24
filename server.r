library(shinyjs)
library(Rcpp)
sourceCpp("simulations.cpp")
# sim_names <-  c("delta_t", "max_rt", "n_sim")
# input <- list(z=0.5, sz=0, sv=0.1, v=0.5, a=1.5, delta_t=input$alpha*0.2, max_rt=10, n_sim=100,
#               tau=1, s=1)
input <- list(a=2,  v=0.1, z=0.5, s=0.9, sv=0.5, sz=0.1,
              svis = 0.8, sigvis=0.2, muvis=0.8,
              tau=1,w=0.5, lambda=1.2,
               delta_t=0.1, max_rt=5, n_sim=300, alpha=0.5,
              checkvismu=FALSE)
# remotes::install_github("datasketch/shinypanels")
# install.packages("shinyjs")
# 
# library(shinypanels)

server <- function(input, output, session) {
  observe(input$recalc)
  
  output$slidrmuvis <- renderUI({withMathJax(
    sliderInput("muvis", "Mean visibility drift rate (\\(\\mu_{Vis}\\))", 
                min = 0, max = 3.0, value = 1.0, step = 0.1, width=NULL)
  )})
  observeEvent(input$checkvismu,{
    if (!input$checkvismu) shinyjs::disable(id="muvis")  
    else shinyjs::enable(id="muvis") 
  })
  
  
  simulate_paths <- debounce(millis=400, r=reactive({
    input$recalc
    if ("sz" %in% names(input)) {
      print("sz is in input")
      set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*input$tau*
                         input$max_rt*input$sv*input$s)*ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+ifelse(is.null(input$recalc), 1, 2)))
      sz <- input$sz
    } else {
      set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*input$tau*
                         input$max_rt*input$sv*input$s)*ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+ifelse(is.null(input$recalc), 1, 2)))
      sz <- 0
    }
    if (input$checkvismu) {
      vismu <- abs(input$v)
    } else {
      vismu <- input$muvis
    }
    #paths = simulatepaths(unlist(input[paramNames]), input$delta_t, input$max_rt, input$n_sim)
    paths = simulatepaths(c(input$a, input$v, input$z, input$s, input$sv, input$sz, 
                                 input$svis, input$sigvis, vismu, input$tau, input$w, input$lambda), 
                          input$delta_t, input$max_rt, input$n_sim)
    poststeps = ceiling(input$tau/input$delta_t)
    steps = ceiling(input$max_rt/input$delta_t)
    # remove unfinished decision processes 
    paths <- paths[,!is.na(paths[3+ 2*(1+steps+poststeps),])]
    simus <- list(XDec   = paths[1:(steps+1),],
                  Vis    = paths[(poststeps+steps+3):(1+ 2*(1+steps+poststeps)),],
                  DT     = paths[2+ 2*(1+steps+poststeps),],
                  Resp   = paths[3+ 2*(1+steps+poststeps),],
                  VisEnd = paths[5+ 2*(1+steps+poststeps),]
    )
    simus$XPost1 <- paths[(steps+2):(steps+poststeps+2),simus$Resp==1]
    simus$XPost2 <- paths[(steps+2):(steps+poststeps+2),simus$Resp==-1]
    PostTimes <- matrix(rep(0:poststeps, ncol(paths)), ncol=ncol(paths)) + 
      matrix(rep(simus$DT, each=(poststeps+1)), ncol=ncol(paths))
    simus$PostTimes1 = PostTimes[,simus$Resp==1]
    simus$PostTimes2 = PostTimes[,simus$Resp==-1]
    
    simus$DT1 <- (simus$DT[simus$Resp==1]+poststeps)*input$delta_t
    simus$DT2 <- (simus$DT[simus$Resp==-1]+poststeps)*input$delta_t
    simus$MRT1 <- mean(simus$DT1)
    simus$MRT2 <- mean(simus$DT2)
    resp_probs <- table(factor(simus$Resp, levels=c(1, -1)))/ncol(paths)
    simus$resp_prob1 <- resp_probs[1]
    simus$resp_prob2 <- resp_probs[2]
    simus$Conf1 <- paths[6+ 2*(1+steps+poststeps),simus$Resp==1]
    simus$Conf2 <- paths[6+ 2*(1+steps+poststeps),simus$Resp==-1]
    simus$XEnd1 <- paths[4+ 2*(1+steps+poststeps),simus$Resp==1]
    simus$XEnd2 <- paths[4+ 2*(1+steps+poststeps),simus$Resp==-1]
    simus
  }))
  
  
  
  output$pathsplot <- renderPlot({
    simus <- simulate_paths()
    poststeps <- ceiling(input$tau/input$delta_t)
    steps <- ceiling(input$max_rt/input$delta_t)
    sz <- input$sz
    layout(matrix(c(2,1,1,3, 5, 4, 4, 6),ncol=2))
    # plot paths
    par(mar = c(0.1, 4, 0.1, 1))
    ylims <- range(c(simus$XDec, simus$XPost1, simus$XPost2), na.rm=TRUE)
    ylims[1] <- min(0, ylims[1])-0.2
    ylims[2] <- max(input$a, ylims[2])+0.2
    maxrt <- input$max_rt*1.02
    if ( "maxplotrt"%in%names(input)) maxrt <- min(input$maxplotrt, maxrt)
    p_paths <- matplot(y=simus$XDec,x=(0:steps)*input$delta_t,
                     type = 'l', lwd = 1, lty = 1, col =  rgb(red = 0, green = 0, blue = 0, alpha = min(1, 500/input$n_sim*input$alpha*0.15)),
                     ylab = '', ylim=ylims, yaxt="n", xlim=c(0, maxrt),xlab = '', 
                     main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
    p_paths <- matplot(y=simus$XPost1, x=simus$PostTimes1*input$delta_t,
                       type = 'l', lwd = 1, lty = 1, col =  rgb(red = 0, green = 0.4, blue = 0.9, alpha = min(1, 500/input$n_sim*input$alpha*0.15)),
                       ylab = '', ylim=ylims, yaxt="n", xlim=c(0, maxrt),xlab = '', 
                       main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5, add=TRUE)
    p_paths <- matplot(y=simus$XPost2, x=simus$PostTimes2*input$delta_t,
                       type = 'l', lwd = 1, lty = 1, col =  rgb(red = 0.9, green = 0.1, blue = 0.1, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
                       ylab = '', ylim=ylims, yaxt="n", xlim=c(0, maxrt),xlab = '', 
                       main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5, add=TRUE)
    p_paths <- points(y=simus$XEnd1, simus$DT1, 
                      bg =  rgb(red = 0, green = 0.02, blue = 0.4, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
                      col =  rgb(red = 0, green = 0.02, blue = 0.4, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
                      pch=24)
    p_paths <- points(y=simus$XEnd2, simus$DT2,
                      bg =  rgb(red = 0.6, green = 0.0, blue = 0.0, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
                      col =  rgb(red = 0.6, green = 0.0, blue = 0.0, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
                      pch=25)
    title(ylab = "Decision process", line=2, cex.lab=1.5)
    abline(h = c(0, input$a), lty=1, col="black", lwd=1.5)
    abline(h=input$z  * input$a, lty=2)
    # text(maxrt-0.02, as.numeric(input$z)*as.numeric(input$a),
    #      paste("Not finished:", round(1-descr[1, "prob"]-descr[2, "prob"],2)),
    #      adj=1, cex=1.5) 
    
    if (sz > 0) {
      axis(side=2, at = (c(0,(as.numeric(input$z)-as.numeric(sz)/2), 
                           as.numeric(input$z), as.numeric(input$z)+as.numeric(sz)/2,1)*as.numeric(input$a)), 
           labels = c(0, "", "z", "","a"), cex=1.5, cex.axis=1.5, cex.lab=1.5, line=0) # c(0, "z-sz/2", "z", "z+sz/2","a")
    } else {
      axis(side=2, at = (c(0, input$z,1)*as.numeric(input$a)), labels = c(0, "z", "a"), 
           cex=1.5, cex.axis=1.5, cex.lab=1.5, line=0)
    }
    

    # Plot density of upper DTs 
    par(mar = c(0.1, 4, 3, 1))
    if (length(simus$DT1)>1) {
      d1 <- density(simus$DT1)
      d1$y <- d1$y/max(d1$y)*simus$resp_prob1
      #d1$y <- d1$y/max(d1$y)
      plot(d1, ylim=c(0,1), xlim=c(0, maxrt), #, main="Decision (drift diffusion process)"
           main="",ylab="",
           xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)# 
    } else {
      plot(1, ylim=c(0,1), xlim=c(0, maxrt), #, main="Decision (drift diffusion process)"
           main="",ylab="",
           xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)# 
    }
    title(ylab="Upper time density", line=2, cex.lab=1.5)
    abline(v=simus$MRT1)
    text(maxrt-0.02, 0.9,
         paste("Response probability:", round(simus$resp_prob1,2)),
         adj=1, cex=1.5) 
    text(mean(simus$DT1)+0.02, 0.8,
         paste("Mean accumulation time:", round(simus$MRT1, 2)),
         adj=0, cex=1.5) 

    par(mar = c(3, 4, 0.1, 1))
    #par(mar = c(4, 5, 0.1, 1))
    if (length(simus$DT2)>1) {
      d2 <- density(simus$DT2)
      #d2$y <- d2$y/max(d2$y)
      d2$y <- d2$y/max(d2$y)*simus$resp_prob2
      d2$y <- 1-d2$y
      plot(d2, main="", ylim=c(0,1), xlim=c(0, maxrt),xlab="",ylab="",
           xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)# 
    } else {
      plot(1, type="n", ylim=c(0, 1), xlim=c(0, maxrt),xlab="",ylab="",
           #ylab="Lower DT density",xlab="", 
           xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5) 
    }
    title(ylab="Lower time density", line=2, cex.lab=1.5)
    abline(v=simus$MRT2)
    title(xlab="Time", line=2, cex.lab=1.5)
    axis(side=1, at = seq(0, maxrt, by=round(maxrt/5, 1)), 
         cex=1, cex.axis=1, cex.lab=1, line=0) # c(0, "z-sz/2", "z", "z+sz/2","a")
    text(maxrt-0.02, 0.9,
         paste("Response probability:", round(simus$resp_prob2,2)),
         adj=1, cex=1.5) 
    text(mean(simus$DT2)+0.02, 0.8,
         paste("Mean accumulation time:", round(simus$MRT2, 2)),
         adj=0, cex=1.5)

    
    ylimsabs <- max(abs(simus$Vis), na.rm=TRUE)
    # if ("maxplotvis" %in% names(input)) {
    #   
    # }
    par(mar = c(0.1, 4, 0.1, 1))
    p_paths <- matplot(y=simus$Vis,x=(0:(steps+poststeps))*input$delta_t,
                       type = 'l', lwd = 1, lty = 1, col =  rgb(red = 0, green = 0.4, blue = 0.2, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
                       ylab = '', ylim=c(-ylimsabs, ylimsabs), yaxt="n", xlim=c(0, maxrt),xlab = '', 
                       main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
    p_paths <- points(y=simus$VisEnd, (simus$DT+poststeps)*input$delta_t, 
                      col =  rgb(red = 0, green = 0.1, blue = 0.03, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
                      pch=19)
    title(ylab = "Visibility process", line=2, cex.lab=1.5)
    abline(h = c(0), lty=2)
    
    # text(maxrt-0.02, as.numeric(input$z)*as.numeric(input$a),
    #      paste("Not finished:", round(1-descr[1, "prob"]-descr[2, "prob"],2)),
    #      adj=1, cex=1.5) 
    ylims <- range(c(simus$Conf1, simus$Conf2))
    f1 <- kde2d(simus$DT1, simus$Conf1, lims = c(input$tau*0.85, maxrt, ylims))#, h=c(1.2, input$a))
    f2 <- kde2d(simus$DT2, simus$Conf2, lims = c(input$tau*0.85, maxrt, ylims))
    
    par(mar = c(0.1, 4, 3, 1))
    ylims = ylims+ c(-0.2, 0.2)
    plot(x =simus$DT1, y=simus$Conf1,
         type="p", pch=15, cex=2.5,xlab="",ylab="",
         col =  rgb(red = 0, green = 0.02, blue = 0.4, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
         main="", ylim=ylims, xlim=c(0, maxrt),
         xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
    title(ylab="Cofidence upper decision", line=2, cex.lab=1.5)
    contour(f1, levels  =   floor(max(f1$z, f2$z)*100)/100/c(15, 10, 5, 2, 1), lwd=2, add=TRUE)
    
    
    par(mar = c(3, 4, 0.1, 1))
    plot(x =simus$DT2, y=simus$Conf2,
         type="p", pch=15, cex=2.5, xlab="",ylab="",
         col =  rgb(red = 0.6, green = 0.0, blue = 0.0, alpha = min(1, 500/input$n_sim*input$alpha*0.2)),
         main="", ylim=ylims, xlim=c(0, maxrt),
         xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
    title(ylab="Cofidence lower decision", line=2, cex.lab=1.5)
    title(xlab="Time", line=2, cex.lab=1.5)
    contour(f2, levels  =   floor(max(f1$z, f2$z)*100)/100/c(15, 10, 5, 2, 1), lwd=2, add=TRUE)
    axis(side=1, at = seq(0, maxrt, by=round(maxrt/5, 1)), 
         cex=1, cex.axis=1, cex.lab=1, line=0) # c(0, "z-sz/2", "z", "z+sz/2","a")
    
  })
  
  
  output$resultstable <- renderTable({
    results <- round(matrix(c(simus$resp_prob1, simus$resp_prob2,
                        simus$MRT1, simus$MRT2, 
                        mean(simus$Conf1), mean(simus$Conf2), 
                        cor(simus$DT1, simus$Conf1), cor(simus$DT2, simus$Conf2)), byrow=TRUE, ncol=2), 2)
    rownames(results) <- c("Response probability", "Mean accumulation time", "Mean confidence", "Correlation of accumulation\ntime with confidence")
    colnames(results) <- c("Upper", "Lower")
    results
  }, rownames = TRUE)#, align=c("l", "c", "c"))
}

