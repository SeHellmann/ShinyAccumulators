# paramNames <- c("z", "sz", "a", "v", "sv", "tau",
#                 "delta_t", "input$n_sim", "max_rt")
# input <- list(z=0.5, sz=0, sv=0.1, v=0, a=1.5, delta_t=0.05, max_rt=5, n_sim=30,
#               tau=1, s=0.3)
server <- function(input, output, session) {
  
  simulate_paths <- reactive({
    list2env(input, envir = environment())
    require(dplyr)
    require(tidyr)
    if ("sz" %in% names(input)) {
      print("sz is in input")
      set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*#input$tau*
                         input$max_rt*input$sv*input$s)*ceiling((as.integer(Sys.time()) %% 60)+if_else(is.null(input$recalc), 1, 2)))
      sz <- input$sz
    } else {
      set.seed(ceiling(input$z*input$n_sim*input$delta_t*as.numeric(input$a)*input$v*#input$tau*
                         input$max_rt*input$sv*input$s)*ceiling((as.integer(Sys.time()) %% 60)+if_else(is.null(input$recalc), 1, 2)))
      sz <- 0
    }
    trial_parameters = data.frame(N= 1:input$n_sim,
                                  mu= rnorm(input$n_sim, input$v, input$sv),
                                  Zs = runif(input$n_sim, input$z-sz/2, input$z+sz/2)*as.numeric(input$a))
    
    
    Xout <- expand.grid(t = input$delta_t*(0:ceiling(input$max_rt/input$delta_t)),
                        N = 1:input$n_sim,
                        X=NA)
    Xout <- left_join(Xout, trial_parameters)
    
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
    
    Xout <- Xout %>% filter(t <= as.numeric(respout[respout$N==N[1], c("rt")])) %>%
      rowwise() %>% mutate(S=min(max(S,0),as.numeric(input$a)))
     
    sim <- list(X=Xout, resp=respout)
    sim
  })
  
  # gen_plot <- reactive({
  #   
  # })
  
  output$pathsplot <- renderPlot({
    input$recalc
    sim <- simulate_paths()
    {  
      layout(matrix(c(2,1,3), 3,1))
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
      paths <- matplot(x=input$delta_t*(0:(ncol(X)-1)),(t(X)-input$a/2),
                       type = 'l', lwd = 0.5, lty = 1, col =  rgb(red = 0, green = 0, blue = 0, alpha = min(1,500/input$n_sim*0.1)),
                       ylab = 'Evidence', ylim=c(-2.5, 2.5), yaxt="n", xlim=c(0, maxrt),xlab = '', 
                       main = '', xaxs="i", yaxs="i", xaxt="n",cex.main=1.5,  cex.axis=1.5, cex.lab=1.5)
      arrows(0, (t(X)[1,1]-input$a/2), 0.4, (t(X)[1,1]-input$a/2)+input$v*0.4, 
             col="red", lwd=par("lwd")*3, length=0.1)
      text(0.45, (t(X)[1,1]-input$a/2)+input$v*0.4,expression(nu), 
           col="red", cex=1.5, font=2)
      text(maxrt-0.02, 0.9,
           paste("Prop. not finished:", 1-round(descr[2, "prob"],2)-round(descr[1, "prob"],2)),
           adj=1, cex=1.5) 
      if (sz > 0) {
        axis(side=2, at = (c(0,(as.numeric(input$z)-as.numeric(sz)/2), 
                             as.numeric(input$z), as.numeric(input$z)+as.numeric(sz)/2,1)*as.numeric(input$a)), 
             labels = c(0, "z-sz/2", "z", "z+sz/2","a"), cex=1.5, cex.axis=1.5, cex.lab=1.5)
      } else {
        axis(side=2, at = (c(-0.5, -0.5+input$z,0.5)*as.numeric(input$a)), labels = c(0, "z", "a"), 
             cex=1.5, cex.axis=1.5, cex.lab=1.5)
      }
      

      resp_1 <- filter(sim$resp, resp==1)
      if (nrow(resp_1>0)) {
        par(mar = c(0.1, 5, 4, 1))
        d1 <- density(resp_1$rt, bw="SJ")
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
        d2 <- density(resp_2$rt, bw="SJ")
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
    }
  })
}

