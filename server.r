# paramNames <- c("z", "sz", "a", "v", "sv", "tau",
#                 "delta_t", "input$n_sim", "max_rt")
# input <- list(z=0.5, sz=0, sv=0.1, v=0.5, a=1.5, delta_t=0.05, max_rt=10, n_sim=100, 
#               tau=1, s=1)
# input <- list(z=0.5, sv=0.5, v=1, a=2, delta_t=0.1, max_rt=5, n_sim=500,
#               tau=1, s=1)
# 
server <- function(input, output, session) {
  
  simulate_paths <- reactive({
    list2env(input, envir = environment())
    require(dplyr)
    require(tidyr)
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
    trial_parameters = data.frame(N= 1:input$n_sim,
                                  mu= rnorm(input$n_sim, input$v, input$sv),
                                  Zs = runif(input$n_sim, input$z-sz/2, input$z+sz/2)*as.numeric(input$a))
    
    
    Xout <- expand.grid(t = input$delta_t*(0:ceiling(input$max_rt/input$delta_t)),
                        N = 1:input$n_sim,
                        X=NA)
    Xout <- left_join(Xout, trial_parameters)
    Xconfout <- expand.grid(t=input$delta_t*(0:ceiling(input$tau/input$delta_t)),
                            N = 1:input$n_sim,
                            Xconf = NA)
    Xconfout <- left_join(Xconfout, trial_parameters)
    respout= expand.grid(N=1:input$n_sim,
                         resp = NA,
                         rt = NA)
    
    Xout <- Xout %>% group_by(N) %>%
      mutate(X = c(0, rnorm(n()-1, mu*input$delta_t, sqrt(input$delta_t)*input$s)),
             S = cumsum(X)+Zs)
    respout <- Xout %>% filter((S >= as.numeric(input$a)) | (S <= 0)) %>%
      summarise(rt = min(t),
                resp = if_else(S[1]>=as.numeric(input$a), 1, -1))
    Xout <- Xout %>% filter(N[1] %in% respout$N)
    
    Xout <- Xout %>% filter(t <= as.numeric(respout[respout$N==cur_group_id(), c("rt")])) %>%
      rowwise() %>% mutate(S=min(max(S,0),as.numeric(input$a)))
    
    Xconfout <- Xconfout %>%
      left_join(respout) %>%
      group_by(N) %>%
      mutate(Xconf = c(0,rnorm(n()-1, mu*input$delta_t, sqrt(input$delta_t)*input$s)),
             Sconf = cumsum(Xconf)+if_else(resp==1, as.numeric(input$a), 0))
    
    sim <- list(X=Xout, Xconf = Xconfout, resp=respout)
  })
  
  # gen_plot <- reactive({
  #   
  # })
  
  output$pathsplot <- renderPlot({
    input$recalc
    sim <- simulate_paths()
    {  
      layout(matrix(c(2,1,3, 4, 4, 4, 5, 5, 5), 3,3))
      descr <- sim$resp %>% group_by(resp) %>%
        summarise(MRT = mean(rt), 
                  prob= n()/input$n_sim) %>%
        full_join(expand.grid(resp=c(-1, 1))) %>%
        mutate(prob =if_else(is.na(prob), 0, prob)) %>% 
        ungroup() %>% arrange(desc(resp))
      if ("plot_maxt" %in% names(input)) {
        maxrt <- min(input$plot_maxt, input$max_rt)
      } else {
        maxrt <- max(sim$X$t)
      }
      
      X <- as.matrix(pivot_wider(sim$X,values_from=S, names_from=t, id_cols = N))[,-1]
      if ("sz" %in% names(input)) {
        sz <- input$sz
      } else {
        sz <- 0 
      }
      # plot paths
      par(mar = c(0.1, 5, 0.1, 1))
      paths <- matplot(x=input$delta_t*(1:ncol(X)),t(X),
                       type = 'l', lwd = 0.5, lty = 1, col =  rgb(red = 0, green = 0, blue = 0, alpha = min(1, 500/input$n_sim*0.1)),
                       ylab = 'Evidence', ylim=c(0, input$a), yaxt="n", xlim=c(0, maxrt),xlab = '', 
                       main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
      text(maxrt-0.02, as.numeric(input$z)*as.numeric(input$a),
           paste("Not finished:", round(1-descr[1, "prob"]-descr[2, "prob"],2)),
           adj=1, cex=1.5) 
      
      if (sz > 0) {
        axis(side=2, at = (c(0,(as.numeric(input$z)-as.numeric(sz)/2), 
                             as.numeric(input$z), as.numeric(input$z)+as.numeric(sz)/2,1)*as.numeric(input$a)), 
             labels = c(0, "z-sz/2", "z", "z+sz/2","a"), cex=1.5, cex.axis=1.5, cex.lab=1.5)
      } else {
        axis(side=2, at = (c(0, input$z,1)*as.numeric(input$a)), labels = c(0, "z", "a"), 
             cex=1.5, cex.axis=1.5, cex.lab=1.5)
      }
      
      
      resp_1 <- filter(sim$resp, resp==1)
      if (nrow(resp_1>0)) {
        par(mar = c(0.1, 5, 4, 1))
        d1 <- density(resp_1$rt)
        d1$y <- d1$y/max(d1$y)*as.numeric(descr[1, "prob"])
        #d1$y <- d1$y/max(d1$y)
        plot(d1, main="Decision (drift diffusion process)", ylim=c(0,1), xlim=c(0, maxrt),
             xaxt="n",xaxs="i", yaxs="i",yaxt = "n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)#  
        abline(v=mean(resp_1$rt))
        text(maxrt-0.02, 0.9,
             paste("Response probability:", round(descr[1, "prob"],2)),
             adj=1, cex=1.5) 
        text(mean(resp_1$rt)+0.02, 0.8,
             paste("Mean decision time:", round(descr[1, "MRT"], 2)),
             adj=0, cex=1.5) 
      }
      
      resp_2 <- filter(sim$resp, resp==-1)
      if (nrow(resp_2>0)) {
        par(mar = c(4, 5, 0.1, 1))
        d2 <- density(resp_2$rt)
        #d2$y <- d2$y/max(d2$y)
        d2$y <- d2$y/max(d2$y)*as.numeric(descr[2, "prob"])
        d2$y <- 1-d2$y
        plot(d2, main="", ylim=c(0,1), xlim=c(0, maxrt),
             xaxs="i", yaxs="i", xlab="Time",yaxt = "n",cex.main=1,  cex.axis=1, cex.lab=1.5)
        abline(v=mean(resp_2$rt))
        text(maxrt-0.02, 0.9,
             paste("Response probability:", round(descr[2, "prob"],2)),
             adj=1, cex=1.5) 
        text(mean(resp_2$rt)+0.02, 0.8,
             paste("Mean decision time:", round(descr[2, "MRT"], 2)),
             adj=0, cex=1.5)
      }
      
      
      Xconf1 <- as.matrix(pivot_wider(filter(sim$Xconf, resp==1),values_from=Sconf, names_from=t, id_cols = N))[,-1]
      Xconf2 <- as.matrix(pivot_wider(filter(sim$Xconf, resp==-1),values_from=Sconf, names_from=t, id_cols = N))[,-1]
      
      # plot paths
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

