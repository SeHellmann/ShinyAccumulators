
input <- list(c=0.02, d=1, v=2, sd=1,  sv=1,w=0.5, n_sim=500,
               plot_maxX= 5,
              plot_minV=-1, plot_maxV= 5,
              plot_minConf=-1, plot_maxConf= 4)

server <- function(input, output, session) {
  
  simulate_observations <- reactive({
    list2env(input, envir = environment())
    require(dplyr)
    require(tidyr)
    set.seed(ceiling(input$d*input$n_sim*input$v*as.numeric(input$sd)*input$sv*input$w)*
               ceiling(abs(as.numeric(Sys.time())-round(as.numeric(Sys.time()), -2))+if_else(is.null(input$recalc), 1, 2)))   
    stim <- rep(c(-1, 1), each=input$n_sim)
    X <- rnorm(2*input$n_sim, input$d*stim, input$sd)
    V <- rnorm(2*input$n_sim, input$v, input$sv)
    resp <- (-1)^(X < input$c)
    correct <- resp*stim
    conf <- (1-input$w)*abs(X-input$c)+input$w*V
    sim <- data.frame(X, V, resp, correct, conf, stim=stim)
    # D = eval(parse(text=input$discriminability))
    sim
  })
  
  # gen_plot <- reactive({
  #   
  # })
  
  output$pathsplot <- renderPlot({
    input$recalc
    sim <- simulate_observations()
    {  
      library(ggplot2)
      library(ggpubr)
      library(gridExtra)
      layout(matrix(c(1,2,3,4,4,4), nrow=2, byrow=TRUE))
      
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
        theme_minimal()+xlim(c(-input$plot_maxX, input$plot_maxX))+
        ggtitle("Evidenzvariable")+ylab("Dichte")
      p_X
      p_V <- ggplot(sim, aes(x=V, group=stim, color=as.factor(stim)))+
        geom_density()+
        geom_density(data=sim, aes(x=V, group=0, color=factor("0")))+
        scale_color_manual(name = "Stimulus", labels=c("links", "gesamt", "rechts"),
                           breaks=c(-1, 0, 1), values=c("#FFC20A", "black", "#0C7BDC"))+
        geom_vline(xintercept = DescV$Mean[c(1,3,5)], color=c("#FFC20A","#0C7BDC", "black"))+
        geom_line(data=DescV, aes(x=value, y=rep(c(-0.01, 0, 0.01), each=2),color=as.factor(stim)))+
        theme_minimal()+xlim(c(input$plot_minV, input$plot_maxV))+
        ggtitle("Sichtbarkeitsvariable")+ylab("Dichte")
      p_V
      p_Conf <- ggplot(sim, aes(x=conf, color=as.factor(correct), group=correct))+
        geom_density()+        
        geom_density(data=sim, aes(x=conf, group=0, color=factor("0")))+
        scale_color_manual(name = "Richtigkeit", labels=c("falsch", "gesamt", "richtig"),
                           breaks=c(-1, 0, 1), values=c("darkred", "black", "green3"))+
        geom_vline(xintercept = DescConf$Mean[c(1,3,5)], color=c("darkred", "green3", "black"))+
        geom_line(data=DescConf, aes(x=value, y=rep(c(-0.01, 0, 0.01), each=2),color=as.factor(correct)))+
        ggtitle("Konfidenzvariable")+ylab("Dichte")+
        theme_minimal()+xlim(c(input$plot_minConf, input$plot_maxConf))+
        theme(legend.position = "bottom")
      p_Conf
      #grid.arrange(p_X, p_V, p_Conf,nrow=1, )
                   #layout_matrix=matrix(c(1,2,3,4,4,4), nrow=2, byrow = TRUE))
      
    }   
    temp_p <- ggarrange(p_X, p_V,nrow=1, common.legend = TRUE, legend = "bottom")
    gridExtra::grid.arrange(temp_p, p_Conf, nrow=1)
  })
}

