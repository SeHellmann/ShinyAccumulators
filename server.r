
# input <- list(c = 0.05, sd=1, sv=1, w=0.5, n_sim= 100, 
#               manipulation1 = "0, 1, 2, 3",manipulation2 = "1, 3",
#               Einfluss_man1_d = 2, Einfluss_man1_v=0.5, 
#               Einfluss_man2_d=1, Einfluss_man2_v=1,
#               name_man1 = "Orientierung", name_man2="Kontrast"
#               )

server <- function(input, output, session) {
  output$pathsplot <- renderPlot({
    require(dplyr)
    require(tidyr)
    require(ggplot2)
    library(ggpubr)
    
    require(gridExtra)
    palette1 = "OrRd"
    palette2 = "GnBu"
    {
      manipulation1 <- as.numeric(trimws(unlist(strsplit(input$manipulation1,","))))
      manipulation2 <- as.numeric(trimws(unlist(strsplit(input$manipulation2,","))))
      conditions <- expand.grid(man1 = manipulation1, man2=manipulation2)
      
      conditions$d  <- conditions$man1*input$Einfluss_man1_d + 
        conditions$man2*input$Einfluss_man2_d
      conditions$v  <- conditions$man1*input$Einfluss_man1_v + 
        conditions$man2*input$Einfluss_man2_v
      conditions$sd <- input$sd
      conditions$sv <- input$sv
      conditions$w <- input$w
      
      
      
      sim <- conditions %>% group_by(man1, man2) %>%
        reframe(X = rnorm(2*input$n_sim, mean=d/2, input$sd),
                V = rnorm(2*input$n_sim, v, input$sv)) %>% 
        mutate(correct = as.numeric(X > 0),
               conf = (1-input$w)*abs(X) + input$w *V)
      
      agg_sim <- sim %>% group_by(man2, man1) %>%
        summarise(mean_resp=mean(correct),
                  mean_X = mean(X), 
                  mean_V = mean(V),.groups = "drop") 
      # mutate(xfactor = factor(paste(man1,man2,  sep="."), 
      #                         levels=c(paste(rev(manipulation1), manipulation2, sep="."),
      #                                  paste(manipulation1, manipulation2, sep="."))))
      p_d_man <- ggplot(sim, aes(x=X, linetype=as.factor(man2), color=as.factor(man1)))+
        scale_color_brewer(name=input$name_man1, palette=palette1,
                           limits=factor(c(-2*min(manipulation1), -min(manipulation1), manipulation1)),
                           breaks=manipulation1)+
        scale_linetype_discrete(name=input$name_man2)+
        geom_vline(xintercept=0, col="black", linewidth=1.2)+
        geom_density()+ylab("Dichte")+xlab("Evidenz")+
        geom_vline(data=agg_sim, aes(xintercept=mean_X,linetype=as.factor(man2), color=as.factor(man1) ))+
        theme_minimal()+xlim(c(-3.2, 7.2))
  
      
      p_v_man <- ggplot(sim, aes(x=V, linetype=as.factor(man2), color=as.factor(man1)))+
        scale_color_brewer(name=input$name_man1, palette=palette1,
                           limits=factor(c(-2*min(manipulation1), -min(manipulation1), manipulation1)),
                           breaks=manipulation1)+
        scale_linetype_discrete(name=input$name_man2)+
        geom_density()+ylab("Dichte")+xlab("Sichtbarkeit")+
        geom_vline(data=agg_sim, aes(xintercept=mean_V,linetype=as.factor(man2), color=as.factor(man1) ))+
        theme_minimal()+xlim(c(-2.2, 7.2))
  
      
      
      
      p_dec_man <- ggplot(agg_sim, aes(x=as.factor(man2), fill=as.factor(man1), y=mean_resp))+
        scale_fill_brewer(name=input$name_man1, palette=palette1,
                          limits=factor(c(-2*min(manipulation1), -min(manipulation1), manipulation1)),
                          breaks=manipulation1)+
        scale_x_discrete(name=input$name_man2, 
                         breaks=as.character(manipulation2), labels=as.character(manipulation2))+
        geom_bar(stat="identity", position=position_dodge())+
        theme_minimal()+ylim(c(0,1))+
        ylab("Mittlere Genauigkeit")+ggtitle(paste("Mittlere Genauigkeit nach Manipulationen"))
  
      mean_conf <- sim %>% group_by( man1, man2, correct) %>%
        summarise(mean_conf=mean(conf),
                  sd_conf = sd(conf),.groups = "drop") 
      
  
      pd = position_dodge(0.1)
      p_conf_mean1 <- ggplot(mean_conf, aes(x=man2, color=as.factor(man1),
                                            linetype=factor(correct, levels=c(1,0)), 
                                            y= mean_conf), group=interaction(man1, correct))+
        geom_errorbar(aes(ymin=mean_conf-sd_conf, ymax=mean_conf+sd_conf), 
                      width=(max(manipulation2)-min(manipulation2))/6, position=pd)+
        scale_color_brewer(name=input$name_man1, palette=palette1,
                           limits=factor(c(-2*min(manipulation1), -min(manipulation1), manipulation1)),
                           breaks=manipulation1)+
        scale_x_continuous(name=input$name_man2, 
                           breaks=manipulation2, labels=as.character(manipulation2))+
        scale_linetype_discrete(name="Korrekt", breaks=c(1, 0), labels=c("Richtig", "Falsch"))+
        geom_line(position = pd, linewidth=1.4)+geom_point(position=pd, size=1.2)+
        ggtitle(paste("Mittlere Konfidenz nach Manipulationen"))+
        ylab("Mittlere Konfidenz")+
        theme_minimal()
  
      p_conf_mean2 <- ggplot(mean_conf, aes(x=man1, color=as.factor(man2),
                                            linetype=factor(correct, levels=c(1,0)), 
                                            y= mean_conf), group=interaction(man1, correct))+
        geom_errorbar(aes(ymin=mean_conf-sd_conf, ymax=mean_conf+sd_conf), 
                      width=(max(manipulation1)-min(manipulation1))/6, position=pd)+
        scale_color_brewer(name=input$name_man2, palette=palette2, 
                           limits=factor(c(-min(manipulation2), -2*min(manipulation2), manipulation2)),
                           breaks=manipulation2)+
        scale_x_continuous(name=input$name_man1, 
                           breaks=manipulation1, labels=as.character(manipulation1))+
        scale_linetype_discrete(name="Korrekt", breaks=c(1, 0), labels=c("Richtig", "Falsch"))+
        geom_line(position = pd, linewidth=1.4)+geom_point(position=pd, size=1.2)+
        ggtitle(paste("Mittlere Konfidenz nach Manipulationen"))+
        ylab("Mittlere Konfidenz")+
        theme_minimal()
  
      # ggarrange(plotlist= list(p_d_man1, p_v_man1,p_dec_man1, p_conf_man1, p_conf_mean), 
      #           nrow=2, widths = c(0.3, 0.4, 0.3))
      temp_p <- ggarrange(p_d_man, p_v_man, nrow=2, common.legend = TRUE, legend="right")
      # temp_p2 <- ggarrange(p_dec_man, p_conf_mean1, nrow=1, common.legend=TRUE, legend="bottom")
      # temp_p2  
    }    
    gridExtra::grid.arrange(p_dec_man, temp_p, p_conf_mean1,p_conf_mean2,
                            # layout_matrix=matrix(c(1,1,2,2,2,2,3,3,3,4,4,4, 3, 3,3,4,4,4), nrow=6))
                            layout_matrix=matrix(c(1,1,2,2,2,2,3,3,3,4,4,4), nrow=6))
  })
}

