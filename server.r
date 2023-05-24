# paramNames <- c("z", "sz", "a", "v", "sv", "tau",
#                 "delta_t", "input$n_sim", "max_rt")
input <- list(c = 0.05, sd=1, sv=1, w=0.5, n_sim= 100,
              manipulation1 = "0, 1, 2, 3",
              Einfluss_man1_d = 2, Einfluss_man1_v=0.5,
              name_man1 = "Orientierung"
              )
              



  # gen_plot <- reactive({
  #   
  # })
server <- function(input, output, session) {
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
    {  
      require(dplyr)
      require(tidyr)
      require(ggplot2)
      require(gridExtra)
      palette1 = "OrRd"
      #list2env(input, envir = environment())
      manipulation1 <- as.numeric(trimws(unlist(strsplit(input$manipulation1,","))))
      conditions <- expand.grid(man1 = manipulation1)
      
      conditions$d  <- conditions$man1*input$Einfluss_man1_d 
      conditions$v  <- conditions$man1*input$Einfluss_man1_v 
      conditions$sd <- input$sd
      conditions$sv <- input$sv
      conditions$w <- input$w
      
      
      stim <- rep(c(-1, 1), each=input$n_sim)
      
      
      sim <- conditions %>% group_by(man1) %>%
        reframe(X = rnorm(2*input$n_sim, mean=c(rep(c(d/2,-d/2), each=input$n_sim)), input$sd),
                V = rnorm(2*input$n_sim, v, input$sv),
                stim = rep(c(1, -1), each=input$n_sim)) %>% 
        mutate(resp = (-1)^(X < input$c),
               correct = as.numeric(resp==stim),
               conf = (1-input$w)*abs(X-input$c) + input$w *V)
      agg_sim <- sim %>% group_by(stim, man1) %>%
        summarise(mean_resp=mean(resp),
                  mean_X = mean(X), 
                  mean_V = mean(V),.groups = "drop") %>%
        mutate(xfactor = factor(paste(man1,stim,  sep="."), 
                                levels=c(paste(rev(manipulation1), -1, sep="."),
                                         paste(manipulation1, 1, sep="."))))

      p_d_man1 <- ggplot(sim, aes(x=X, linetype=as.factor(stim), color=as.factor(man1)))+
        scale_color_brewer(name=input$name_man1, palette=palette1,
                           limits = factor(c("a", "b", manipulation1)), breaks=manipulation1)+
        scale_linetype_discrete(name="Stimuluskategorie")+
        geom_vline(xintercept=input$c, col="black", linewidth=1.2)+
        geom_density()+ylab("Dichte")+xlab("Evidenz")+
        geom_vline(data=agg_sim, aes(xintercept=mean_X,linetype=factor(stim), color=as.factor(man1) ))+
        theme_minimal()+xlim(c(-5.2, 5.2))
      p_v_man1 <- ggplot(sim, aes(x=V, linetype=as.factor(stim), color=as.factor(man1)))+
        scale_color_brewer(name=input$name_man1, palette=palette1,
                           limits = factor(c("a", "b", manipulation1)), breaks=manipulation1)+
        scale_linetype_discrete(name="Stimuluskategorie")+
        geom_density()+ylab("Dichte")+xlab("Sichtbarkeit")+
        geom_vline(data=agg_sim, aes(xintercept=mean_V,linetype=factor(stim), color=as.factor(man1) ))+
        theme_minimal()+xlim(c(-3.2, 5.2))
      
      p_dec_man1 <- ggplot(agg_sim, aes(x=xfactor, fill=as.factor(man1), y=mean_resp))+
        scale_fill_brewer(name=input$name_man1, palette=palette1,
                           limits = factor(c("a", "b", manipulation1)), breaks=manipulation1)+
        scale_x_discrete(name="Stimuluskategorie", breaks=paste(round(length(input$manipulation1)/2), c("-1", "1"), sep="."),
                         labels=c("-1",  "1"))+
        geom_bar(stat="identity", position=position_dodge())+
        theme_minimal()+ylim(c(-1,1))+
        ylab("Mittlere Antwort")+ggtitle(paste("Mittlere Antwort in Abhängigkeit von Stimulus und", input$name_man1))

      mean_conf <- sim %>% group_by( man1, correct) %>%
        summarise(mean_conf=mean(conf), .groups = "drop") 
      p_conf_man1 <- ggplot(sim, aes(x=conf, linetype=factor(correct, levels=c(1,0)), color=as.factor(man1)))+
        scale_color_brewer(name=input$name_man1, palette=palette1,
                           limits = factor(c("a", "b", manipulation1)), breaks=manipulation1)+
        scale_linetype_discrete(name="Korrekt", breaks=c(1, 0), labels=c("Richtig", "Falsch"))+
        geom_density()+ggtitle("Verteilung der Konfidenzvariable")+ylab("Dichte")+
        geom_vline(data=mean_conf, aes(xintercept=mean_conf,linetype=factor(correct, levels=c(1,0)), color=as.factor(man1) ))+
        theme_minimal()+xlab("Konfidenzvariable")+xlim(c(-1.6, 4.7))

      
      p_conf_mean <- ggplot(mean_conf, aes(x=man1, linetype=factor(correct, levels=c(1,0)), y= mean_conf))+
        scale_linetype_discrete(name="Korrekt", breaks=c(1, 0), labels=c("Richtig", "Falsch"))+
        geom_line()+xlab(input$name_man1)+ggtitle(paste("Mittlere Konfidenz in Abhängigkeit von", input$name_man1))+
        ylab("Mittlere Konfidenz")+
        theme_minimal()

      # ggarrange(plotlist= list(p_d_man1, p_v_man1,p_dec_man1, p_conf_man1, p_conf_mean), 
      #           nrow=2, widths = c(0.3, 0.4, 0.3))
      
      gridExtra::grid.arrange(p_d_man1, p_v_man1,p_dec_man1, p_conf_man1, p_conf_mean,
                              layout_matrix=matrix(c(1,1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5), nrow=6))
    }  
  })
}

