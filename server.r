# paramNames <- c("z", "sz", "a", "v", "sv", "tau",
#                 "delta_t", "input$n_sim", "max_rt")
require(dplyr)
require(tidyr)
require(ggplot2)
library(ggpubr)

input <- list(c = 0.05, sd=1, sv=1, w=0.5, n_sim= 100, 
              manipulation1 = c(0, 1, 2, 3),manipulation2 = c(1, 3),
              Einfluss_man1_d = 2, Einfluss_man1_v=0.5, 
              Einfluss_man2_d=1, Einfluss_man2_v=1,
              name_man1 = "Orientierung", name_man2="Kontrast"
              )
              # man1 = "d, v", funcs1 = "x, abs(x)", steps1 = "1,2,3,4",
              # man1 = "w", funcs1 = "x", steps1 = "0.2, 0.8")
              

list2env(input, envir = environment())
conditions <- expand.grid(man1 = manipulation1, man2=manipulation2)

conditions$d  <- conditions$man1*Einfluss_man1_d + 
  conditions$man2*Einfluss_man2_d
conditions$v  <- conditions$man1*Einfluss_man1_v + 
  conditions$man2*Einfluss_man2_v
conditions$sd <- input$sd
conditions$sv <- input$sv
conditions$w <- input$w


stim <- rep(c(-1, 1), each=input$n_sim)


sim <- conditions %>% group_by(man1, man2) %>%
  reframe(X = rnorm(2*input$n_sim, mean=c(rep(c(d/2,-d/2), each=n_sim)), input$sd),
          V = rnorm(2*input$n_sim, v, input$sv),
          stim = rep(c(1, -1), each=n_sim)) %>% 
  mutate(resp = (-1)^(X < input$c),
         correct = as.numeric(resp==stim),
         conf = (1-input$w)*abs(X-input$c) + input$w *V)
ggplot(sim, aes(x=X, linetype=as.factor(man2), color=as.factor(man1),
                group=interaction(stim, man1, man2)))+
  scale_color_discrete(name=input$name_man1)+
  scale_linetype_discrete(name=input$name_man2)+
  geom_density()
p_d_man1 <- ggplot(sim, aes(x=X, linetype=as.factor(stim), color=as.factor(man1)))+
  scale_color_discrete(name=input$name_man1)+
  scale_linetype_discrete(name="Stimuluskategorie")+
  geom_density()
p_d_man2 <- ggplot(sim, aes(x=X, linetype=as.factor(stim), color=as.factor(man2)))+
  scale_color_discrete(name=input$name_man2)+
  scale_linetype_discrete(name="Stimuluskategorie")+
  geom_density()
ggarrange(p_d_man1, p_d_man2, nrow=2)
p_v_man1 <- ggplot(sim, aes(x=V, linetype=as.factor(stim), color=as.factor(man1)))+
  scale_color_discrete(name=input$name_man1)+
  scale_linetype_discrete(name="Stimuluskategorie")+
  geom_density()
p_v_man2 <- ggplot(sim, aes(x=V, linetype=as.factor(stim), color=as.factor(man2)))+
  scale_color_discrete(name=input$name_man2)+
  scale_linetype_discrete(name="Stimuluskategorie")+
  geom_density()
ggarrange(p_v_man1, p_v_man2, nrow=2)
ggarrange(p_d_man1, p_d_man2,p_v_man1, p_v_man2, layou)
gridExtra::grid.arrange(p_d_man1, p_v_man1, p_d_man2, p_v_man2, nrow=2)
# D = eval(parse(text=input$discriminability))
sim


  # gen_plot <- reactive({
  #   
  # })
  
  output$pathsplot <- renderPlot({
    input$recalc
    # pars1 <- as.character(trimws(unlist(strsplit(input$man1,","))))
    # func1 <- as.character(trimws(unlist(strsplit(input$funcs1,","))))
    # funcs1 <- list()
    # for (i in 1:length(func1)) {
    #   eval(parse(text = paste('funcs1[[',i,']]', #pars1[i], 
    #                           ' <- function(x) { return(' , func1[i] , ')}', sep='')))
    # }
    # for (i in 1:length(func1)) {
    #   eval(parse(text = paste('funcs1[[',i,']]', #pars1[i], 
    #                           ' <- function(x) { return(' , func1[i] , ')}', sep='')))
    # }
    # funcs1

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

