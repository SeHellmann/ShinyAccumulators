paramNames <- c("a", "v","z", "s", "sv", "sz", 
                "svis", "sigvis", "muvis", 
                "tau","w", "lambda")

sim_names <-  c("delta_t", "max_rt", "n_sim")
# input <- list(z=0.5, sz=0, sv=0.1, v=0.5, a=1.5, delta_t=0.05, max_rt=10, n_sim=100, 
#               tau=1, s=1)
input <- list(a=2,  v=1, z=0.5, s=0.9, sv=0.5, sz=0.1,
              svis = 0.8, sigvis=0.2, muvis=0.8, 
              tau=1,w=0.5, lambda=1.2,
               delta_t=0.1, max_rt=5, n_sim=50)
# remotes::install_github("datasketch/shinypanels")
# install.packages("shinyjs")
# 
# library(shinypanels)

server <- function(input, output, session) {
  simulate_paths <- reactive({
    list2env(input, envir = environment())
    # require(dplyr)
    # require(tidyr)
    if ("sz" %in% names(input)) {
      print("sz is in input")
      set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*input$tau*
                         input$max_rt*input$sv*input$s)*ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+if_else(is.null(input$recalc), 1, 2)))
      sz <- input$sz
    } else {
      set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*input$tau*
                         input$max_rt*input$sv*input$s)*ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+if_else(is.null(input$recalc), 1, 2)))
      sz <- 0
    }
    paths = simulatepaths(unlist(input[paramNames]), input$delta_t, input$max_rt, input$n_sim)
    #save(paths,file= "paths.RData")
    poststeps = ceiling(input$tau/input$delta_t)
    steps = ceiling(input$max_rt/input$delta_t)
    XDecIndex   <- 1:(steps+1)
    XPostIndex  <- (steps+2):(steps+poststeps+2)
    VisIndex    <- (poststeps+steps+3):(1+ 2*(1+steps+poststeps))
    DTIndex     <- 2+ 2*(1+steps+poststeps)
    RespIndex   <- 3+ 2*(1+steps+poststeps)
    XEndIndex   <- 4+ 2*(1+steps+poststeps)
    VisEndIndex <- 5+ 2*(1+steps+poststeps)
    ConfIndex   <- 6+ 2*(1+steps+poststeps)
    rownames(paths) <- c(paste0("XDec", XDecIndex),paste0("XPost", XPostIndex),
                         paste0("V", VisIndex), "DT", "Resp", "XEnd", "VisEnd", "Conf")
    # sim <- list(X=paths[,1:(steps+poststeps)], 
    #             Vis = paths[,(steps+poststeps+1):(2*(steps+poststeps))],
    #             DT = paths[,(2*(steps+poststeps)+1)],
    #             resp = paths[,(2*(steps+poststeps)+2)],
    #             conf = paths[,(2*(steps+poststeps)+3)])
    #rm(paths)
  })
  
  # gen_plot <- reactive({
  #   
  # })

  output$pathsplot <- renderPlot({
    input$recalc
    sim <- simulate_paths()
    {  
      XDec <- paths[XDecIndex, ]
      XPost <- paths[XPostIndex, ]
      PostTimes <- matrix(rep(0:poststeps, input$n_sim), ncol=input$n_sim) + 
        matrix(rep(paths[DTIndex, ], each=(poststeps+1)), ncol=input$n_sim)
      DTPost <- expand.grid(paths[DTIndex, ])
      resp_probs <- table(factor(paths[RespIndex,], levels=c(1, -1,0)))/input$n_sim
      DT1 <- paths[DTIndex, paths[RespIndex,]==1]*input$delta_t
      DT2 <- paths[DTIndex, paths[RespIndex,]==-1]*input$delta_t
      MRTs <- c(mean(DT1), mean(DT2))

      layout(matrix(c(2,1,1,3, 4, 4, 5, 6),ncol=1))
      # plot paths
      par(mar = c(0.1, 3, 0.1, 1))
      ylims <- range(paths[c(XDecIndex,XPostIndex), ], na.rm=TRUE)
      ylims[1] <- min(0, ylims[1])
      ylims[2] <- max(input$a, ylims[2])
      maxrt <- input$max_rt
      if ( "maxplotrt"%in%names(input)) maxrt <- min(input$maxplotrt, maxrt)
      p_paths <- matplot(y=paths[XDecIndex, ],x=(0:steps)*input$delta_t,
                       type = 'l', lwd = 1, lty = 1, col =  rgb(red = 0, green = 0, blue = 0, alpha = min(1, 500/input$n_sim*0.05)),
                       ylab = '', ylim=ylims, yaxt="n", xlim=c(0, maxrt),xlab = '', 
                       main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
      p_paths <- matplot(y=paths[XPostIndex, ],x=PostTimes*input$delta_t,
                         type = 'l', lwd = 1, lty = 1, col =  rgb(red = 1, green = 0, blue = 0, alpha = min(1, 500/input$n_sim*0.05)),
                         ylab = '', ylim=ylims, yaxt="n", xlim=c(0, maxrt),xlab = '', 
                         main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5, add=TRUE)
      title(ylab = "Decision process", line=2, cex.lab=1.5)
      abline(h = c(0, input$a))
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
      par(mar = c(0.1, 3, 0.1, 1))
      if (length(DT1)>1) {
        d1 <- density(DT1)
        d1$y <- d1$y/max(d1$y)*resp_probs[1]
        #d1$y <- d1$y/max(d1$y)
        plot(d1, ylim=c(0,1), xlim=c(0, maxrt), #, main="Decision (drift diffusion process)"
             main="",#ylab="",
             xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)# 
      } else {
        plot(1, ylim=c(0,1), xlim=c(0, maxrt), #, main="Decision (drift diffusion process)"
             main="",
             xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)# 
      }
      title(ylab="Upper DT density", line=2, cex.lab=1.5)
      abline(v=MRTs[1])
      text(maxrt-0.02, 0.9,
           paste("Response probability:", round(resp_probs[1],2)),
           adj=1, cex=1.5) 
      text(mean(DT1)+0.02, 0.8,
           paste("Mean decision time:", round(MRTs[1], 2)),
           adj=0, cex=1.5) 
 
      par(mar = c(0.1, 3, 0.1, 1))
      #par(mar = c(4, 5, 0.1, 1))
      if (length(DT2)>1) {
        d2 <- density(DT2)
        #d2$y <- d2$y/max(d2$y)
        d2$y <- d2$y/max(d2$y)*resp_probs[2]
        d2$y <- 1-d2$y
        plot(d2, main="", ylim=c(0,1), xlim=c(0, maxrt),
             xaxs="n", yaxs="i", xlab="",yaxt = "n",cex.main=1,  cex.axis=1, cex.lab=1.5)
      } else {
        plot(1, type="n", ylim=c(0, 1), xlim=c(0, maxrt),
             #ylab="Lower DT density",xlab="", 
             xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5) 
      }
      title(ylab="Lower DT density", line=2, cex.lab=1.5)
      abline(v=MRTs[2])
      text(maxrt-0.02, 0.9,
           paste("Response probability:", round(resp_probs[2],2)),
           adj=1, cex=1.5) 
      text(mean(DT2)+0.02, 0.8,
           paste("Mean decision time:", round(MRTs[2], 2)),
           adj=0, cex=1.5)

      
      ylims <- range(paths[VisIndex, ], na.rm=TRUE)
      # if ("maxplotvis" %in% names(input)) {
      #   
      # }
      par(mar = c(0.1, 3, 0.1, 1))
      p_paths <- matplot(y=paths[VisIndex, ],x=(0:(steps+poststeps))*input$delta_t,
                         type = 'l', lwd = 1, lty = 1, col =  rgb(red = 0, green = 0.4, blue = 0.2, alpha = min(1, 500/input$n_sim*0.05)),
                         ylab = '', ylim=ylims, yaxt="n", xlim=c(0, maxrt),xlab = '', 
                         main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
      p_paths <- points(y=paths[VisEndIndex,], (paths[DTIndex,]+poststeps)*input$delta_t, 
                        col =  rgb(red = 0, green = 0.1, blue = 0.03, alpha = min(1, 500/input$n_sim*0.05)),
                        pch=19)
      title(ylab = "Visibility process", line=2, cex.lab=1.5)
      abline(h = c(0), lty=2)
      # text(maxrt-0.02, as.numeric(input$z)*as.numeric(input$a),
      #      paste("Not finished:", round(1-descr[1, "prob"]-descr[2, "prob"],2)),
      #      adj=1, cex=1.5) 

      Xconf1 <- as.matrix(pivot_wider(filter(sim$Xconf, Resp==1),values_from=Sconf, names_from=t, id_cols = N))[,-1]
      Xconf2 <- as.matrix(pivot_wider(filter(sim$Xconf, Resp==-1),values_from=Sconf, names_from=t, id_cols = N))[,-1]
      
      # plot visibility paths
      par(mar = c(2, 0.1, 2, 0.1))
      paths <- matplot(x=input$delta_t*(0:(ncol(Xconf1)-1)),t(Xconf1),
                       type = 'l', lwd = 0.5, lty = 1, col =  rgb(red = 0.5, green = 0.1, blue = 0, alpha = 500/input$n_sim*0.1),
                       ylab = '', ylim=c(-input$a,2*input$a), yaxt="n", xlim=c(0, input$tau),xlab = '', 
                       main = 'Postdecisional Accumulation', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
      matlines(x=input$delta_t*(0:(ncol(Xconf2)-1)),t(Xconf2),
               type = 'l', lwd = 0.5, lty = 1, col =  rgb(red = 0.1, green = 0.1, blue = 0.5, alpha = 500/input$n_sim*0.1))
      
      axis(side=2, at = (c(0, 1)*as.numeric(input$a)), labels = c("", ""), cex=1.5)
      
      
      conf1 <- Xconf1[,ncol(Xconf1)]
      conf2 <- Xconf2[,ncol(Xconf2)]
      confdescr <- c(mean(conf1), mean(conf2))
      dconf1 <- density(conf1)
      dconf2 <- density(conf2)
    }   
    par(mar = c(2, 0.1, 2, 0.1))
    #d1$y <- d1$y/max(d1$y)
    plot(x=dconf1$y, y=dconf1$x, type="l", lwd=1.5,
         main="Confidence distributions", ylim=c(-input$a,2*input$a), xlim=c(0, max(dconf1$y, dconf2$y)*1.02),
         xaxt="n",xaxs="i", yaxs="i", axes=FALSE,yaxt = "n", 
         col= rgb(red = 0.5, green = 0.1, blue = 0), cex=1.5,cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)#
    lines(x=dconf2$y, y=dconf2$x, type="l", lwd=1.5,
          col= rgb(red = 0.1, green = 0.1, blue = 0.5))#  
    
    abline(h=confdescr, col=c(rgb(red = 0.5, green = 0.1, blue = 0),rgb(red = 0.1, green = 0.1, blue = 0.5)),
           lwd=2)
    lines(y=c(confdescr[1]+sd(conf1),confdescr[1]-sd(conf1)),x=c(0,0), type="l", lwd=8, col= rgb(red = 0.5, green = 0.1, blue = 0, alpha=0.5))
    lines(y=c(confdescr[2]+sd(conf2),confdescr[2]-sd(conf2)),x=c(0,0), type="l", lwd=8, col= rgb(red = 0.1, green = 0.1, blue = 0.5, alpha=0.5))
    text(max(dconf1$y, dconf2$y)*0.01, confdescr+input$a*0.1,
         paste("Mean", c("upper","lower"), "confidence:", round(c(1, -1)*confdescr-c(input$a, 0),2)),
         adj=c(0,0), cex=1.5)
    text(max(dconf1$y, dconf2$y)*0.01, confdescr-input$a*0.1,
         paste("SD", c("upper","lower"), "confidence:", round(c(sd(conf1), sd(conf2)),2)),
         adj=c(0,1), cex=1.5)
  })
}

