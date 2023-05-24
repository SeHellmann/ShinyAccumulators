# paramNames <- c("z", "sz", "a", "v", "sv", "tau",
#                 "delta_t", "input$n_sim", "max_rt")
input <- list(z=0.5, sz=0, sv=0.1, v=0.5, a=1.5, delta_t=0.05, max_rt=10, n_sim=100,
              tau=1, s=1,
              man1 = "d, v", funcs1 = "x, abs(x)", steps1 = "1,2,3,4",
              man1 = "w", funcs1 = "x", steps1 = "0.2, 0.8")
              
#input <- list(c=0.2, sv=1, v=1.5, d=1, sd=1, w=0.5, n_sim=500)
# 
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
    pars1 <- as.character(trimws(unlist(strsplit(input$man1,","))))
    func1 <- as.character(trimws(unlist(strsplit(input$funcs1,","))))
    funcs1 <- list()
    for (i in 1:length(func1)) {
      eval(parse(text = paste('funcs1[[',i,']]', #pars1[i], 
                              ' <- function(x) { return(' , func1[i] , ')}', sep='')))
    }
    for (i in 1:length(func1)) {
      eval(parse(text = paste('funcs1[[',i,']]', #pars1[i], 
                              ' <- function(x) { return(' , func1[i] , ')}', sep='')))
    }
    funcs1
    
    
    pars1 <- as.character(trimws(unlist(strsplit(input$man1,","))))
    pars1 <- as.character(trimws(unlist(strsplit(input$man1,","))))
    pars1 <- as.character(trimws(unlist(strsplit(input$man1,","))))
    
    sim <- simulate_observations()
    {  
      library(ggplot2)
      library(ggpubr)
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
      #grid.arrange(p_X, p_V, p_Conf,nrow=1, )
                   #layout_matrix=matrix(c(1,2,3,4,4,4), nrow=2, byrow = TRUE))
      
    }   
    ggarrange(p_X, p_V, p_Conf,nrow=1, common.legend = TRUE, legend = "bottom")
  })
}

