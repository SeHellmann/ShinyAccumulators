#input <- list(c=0.2, sv=1, v=1.5, d=1, sd=1, w=0.5, n_sim=500)
# 
simulate_obersavtions <-  function(d=1, v=1, sd=1, sv=1, w=0.5, c=0, n_sim=500) {
  require(dplyr)
  require(tidyr)
  set.seed(ceiling(d*n_sim*v*as.numeric(sd)*sv*w)*
             ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+if_else(is.null(recalc), 1, 2)))   
  stim <- rep(c(-1, 1), each=n_sim)
  X <- rnorm(2*n_sim, d*stim, sd)
  V <- rnorm(2*n_sim, v, sv)
  resp <- (-1)^(X < c)
  correct <- resp*stim
  conf <- (1-w)*abs(X-c)+w*V
  sim <- data.frame(X, V, resp, correct, conf, stim=stim)
  # D = eval(parse(text=discriminability))
  sim
}

server <- function(input, output, session) {
  library(ggplot2)
  library(ggpubr)
  
  simulate <- reactive(do.call(simulate_observations, input))

  # simulationData <- reactive({
  #   input$recalc
  #   sim <- simulate_observations()
  #   
  # })
  # traced_Confidence <- reactive({
  #   if (!exists(traced_Confidence) || length(trace))
  # })
  
  output$pathsplot <- renderPlot({
    input$recalc
    sim <- simulate()
    {  
      DescX <- sim %>% group_by(stim) %>%
        summarise(Mean=mean(X), 
                  SD = sd(X)) %>% 
        rbind(c(stim=0, Mean=mean(sim$X), SD=sd(sim$X))) %>%
        mutate(left=Mean-SD, right=Mean+SD) %>% pivot_longer(cols=c("left", "right"))
      DescV <- sim %>% group_by(stim) %>%
        summarise(Mean=mean(V), 
                  SD = sd(V)) %>% 
        rbind(c(stim=0, Mean=mean(sim$V), SD=sd(sim$V)))%>%
        mutate(left=Mean-SD, right=Mean+SD) %>% pivot_longer(cols=c("left", "right"))
      DescConf <- sim %>% group_by(correct) %>%
        summarise(Mean=mean(conf), 
                  SD = sd(conf))%>% 
        rbind(c(correct=0, Mean=mean(sim$conf), SD=sd(sim$conf)))%>%
        mutate(left=Mean-SD, right=Mean+SD) %>% pivot_longer(cols=c("left", "right"))
      p_X <- ggplot(sim, aes(x=X, group=stim, color=as.factor(stim)))+
        geom_density()+
        geom_density(data=sim, aes(x=X, group=0, color=factor("0")))+
        scale_color_manual(name = "Stimulus", labels=c("links", "gesamt", "rechts"),
                           breaks=c(-1, 0, 1), values=c("#FFC20A", "black", "#0C7BDC"))+
        geom_vline(xintercept = DescX$Mean[c(1,3,5)], color=c("#FFC20A","#0C7BDC", "black"))+
        geom_line(data=DescX, aes(x=value, y=rep(c(-0.01, 0, 0.01), each=2),color=as.factor(stim)))+
        ggtitle("Evidenzvariable")+ylab("Dichte")+xlim(c(-5, 5))
      p_X
      p_V <- ggplot(sim, aes(x=V, group=stim, color=as.factor(stim)))+
        geom_density()+
        geom_density(data=sim, aes(x=V, group=0, color=factor("0")))+
        scale_color_manual(name = "Stimulus", labels=c("links", "gesamt", "rechts"),
                           breaks=c(-1, 0, 1), values=c("#FFC20A", "black", "#0C7BDC"))+
        geom_vline(xintercept = DescV$Mean[c(1,3,5)], color=c("#FFC20A","#0C7BDC", "black"))+
        geom_line(data=DescV, aes(x=value, y=rep(c(-0.01, 0, 0.01), each=2),color=as.factor(stim)))+
        ggtitle("Sichtbarkeitsvariable")+ylab("Dichte")+xlim(c(-3, 5))
      p_V
      p_Conf <- ggplot(sim, aes(x=conf, color=as.factor(correct), group=correct))+
        geom_density()+        
        geom_density(data=sim, aes(x=conf, group=0, color=factor("0")))+
        scale_color_manual(name = "Richtigkeit", labels=c("falsch", "gesamt", "richtig"),
                           breaks=c(-1, 0, 1), values=c("darkred", "black", "green3"))+
        geom_vline(xintercept = DescConf$Mean[c(1,3,5)], color=c("darkred", "green3", "black"))+
        geom_line(data=DescConf, aes(x=value, y=rep(c(-0.01, 0, 0.01), each=2),color=as.factor(correct)))+
        ggtitle("Konfidenzvariable")+ylab("Dichte")+xlim(-1.5, 4)
      p_Conf

      # p_MConf <- ggplot(plot_trace, aes(x=index, y=Mconf, group=1))+
      #   geom_point(size=2)+
      #   geom_line()
      # p_MConf
      #grid.arrange(p_X, p_V, p_Conf,nrow=1, )
                   #layout_matrix=matrix(c(1,2,3,4,4,4), nrow=2, byrow = TRUE))
      
    }   
    # ggarrange(p_X, p_V, p_Conf,nrow=2, common.legend = FALSE, legend = "bottom")
     ggarrange(p_X, p_V, p_Conf,nrow=1, common.legend = FALSE, legend = "bottom")
    #ggarrange(p_X, p_V, p_Conf,p_MConf,  common.legend = FALSE, legend = "bottom")
  })
}

