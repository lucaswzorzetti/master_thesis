### Tidied scrypt ###

# Loading the packages and importing data ---------------------------------
  #Loading the packages
    library(ggplot2) #Graphs
    library(dplyr)   #to tidy data
    library(ggpubr)  #extra functions to ggplot2
    library(lme4)    #Many types of models - lme, glm, glmm, etc
    library(stargazer) #To make tables
    library(nlme)  #Fit and compare Gaussian linear and nonlinear mixed-effects models
    library(car) #Statistic functions like Anova
    library(scales) #scales for ggplot2
    library(MASS) #transformations and statistical functions
    library(knitr) #tables
    library(predictmeans) #functions for diagnostics of models

  #Importing 
    geral <- read.table("planilhageral_atualizada2.txt", header = T, colClasses = c(
      "factor", "factor","factor","factor","character", "numeric", "numeric","numeric",
      "numeric","numeric","numeric","factor", "numeric","numeric","numeric","numeric",
      "numeric", "logical", "integer", "integer", "numeric","numeric","numeric","numeric",
      "numeric","numeric", "numeric","numeric","numeric"
    ))
    str(geral)
    View(geral)
    
    geral$presas_consumidas_gravacao[114] <- 2
    geral <- mutate(geral, taxacrescimento = 
                      (((varbiom+biomassant)/biomassant)^(1/sobrev))) #growth rate  
    geral <- geral %>% mutate(biomassa_mg = biomassant*1000) # Converting to Biomass in miligrams
    geral <- geral %>% mutate(dif_temp_cap = ifelse(test = is.na(tempocap2), #Difference on two capture times
                                                    yes = NA,
                                                    no = (tempocap2-tempocap1)),
                              dif_temp_cap_mod = ifelse(test = is.na(tempocap2),
                                                        yes = NA,
                                                        no = ifelse(test = (tempocap2-tempocap1) < 0,
                                                                    yes = (tempocap2-tempocap1)*-1,
                                                                    no = (tempocap2-tempocap1))))
    geral <- geral %>% mutate(prop_cap = presas_consumidas_gravacao/3) #Proportion of captured prey
    
    ## Separating the Taxa ##
      belostomatidae <- filter(geral, suborfam == "Belostomatidae")
      notonectidae <-  filter(geral, suborfam == "Notonectidae")
      anisoptera <-  filter(geral, suborfam == "Anisoptera")
      zygoptera <-  filter(geral, suborfam == "Zygoptera")
      
      pare


# Comparing biomasses between Taxa ----------------------------------------
      biomassas_todos <- geral %>% ggplot(aes(x = suborfam, y = biomassa_mg, fill = suborfam))+
        geom_point(size = 5, alpha = 0.5, shape = 21)+
        scale_x_discrete(limits = c("Belostomatidae", "Anisoptera",
                                    "Zygoptera", "Notonectidae")) +
        xlab("Taxa") + ylab("Biomass [mg]")+
        theme_classic() + theme(legend.position = "none",
                                axis.text = element_text(face = "bold",
                                                         size = 12, colour = "black"),
                                axis.title.x = element_blank(),
                                axis.title.y = element_text(face = "bold",
                                                            size = 18,
                                                            margin = margin(r = 10)))
      biomassas_todos
        #saving
          jpeg(filename = "biomassas.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          biomassas_todos
          dev.off()
      
      
      #Size
      tamanho_todos <- geral %>% ggplot(aes(x = suborfam, y = compr, fill = suborfam))+
        geom_point(size = geral$larg*20, alpha = 0.5, shape = 21)+
        scale_x_discrete(limits = c("Belostomatidae", "Anisoptera",
                                    "Zygoptera", "Notonectidae")) +
        xlab("Taxa") + ylab("Width [cm]")+
        theme_classic() + theme(legend.position = "none",
                                axis.text = element_text(face = "bold",
                                                         size = 12, colour = "black"),
                                axis.title.x = element_blank(),
                                axis.title.y = element_text(face = "bold",
                                                            size = 18,
                                                            margin = margin(r = 10))
        )
      tamanho_todos
        #saving
          jpeg(filename = "tamanho_todos.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
         tamanho_todos
          dev.off()

# Models of First Capture Time --------------------------------------------
      #Belostomatidae 
      belo_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                   data = belostomatidae, na.action = na.omit)
      summary(belo_temcap1_lme_int) #com interação
      belo_temcap1_table <- Anova(belo_temcap1_lme_int, type = "III")
      belo_temcap1_table
      
      shapiro.test(resid(belo_temcap1_lme_int)) 
      
        #Figure
          belo_temcap1 <- model_line(belostomatidae, log10(belostomatidae$biomassa_mg), log10(belostomatidae$tempocap1), 
                                     "Time of 1º capture [s] \n log10 scale", belo_temcap1_lme, "Belostomatidae") +
                            scale_x_continuous(breaks = c(1, 1.30, 1.48, 1.6), labels = c(10, 20, 30, 40),
                                               limits = c(0.8, 1.8)) +
                            scale_y_continuous(breaks = c(1, 2, 3, 4), labels = c(10, 100, 1000, 10000),
                                               limits = c(0.8, 4.5))+
                            annotation_logticks()
          belo_temcap1
          
          #saving
          jpeg(filename = "temcap1_belo.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          belo_temcap1
          dev.off()
          
      #Anisoptera
      aniso_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                        data = anisoptera, na.action = na.omit)
      summary(aniso_temcap1_lme_int)
          
      aniso_temcap1_lme_int_table <- Anova(aniso_temcap1_lme_int, type = "III") #interaction confirmed
      aniso_temcap1_lme_int_table
          
      plot(aniso_temcap1_lme_int)
      View(sort(cooks.distance(aniso_temcap1_lme_int)))
      plot(sort(cooks.distance(aniso_temcap1_lme_int)))
          
      shapiro.test(resid(aniso_temcap1_lme_int))
          
      anisoptera$tempocap1[1] <- NA #cook D near 0.6, should I withdraw? it increases shapiro value
          
          #Figure
          aniso_temcap1 <- model_line(anisoptera, log10(anisoptera$biomassa_mg), log10(anisoptera$tempocap1), 
                                      "Time of first capture [s] \n log10 scale",
                                      aniso_temcap1_lme, "Anisoptera")+
            scale_x_continuous(breaks = c(1, 1.4, 1.7, 2, 2.18), labels = c(10, 25, 50, 100, 150),
                               limits = c(0.8, 2.2)) +
            scale_y_continuous(breaks = c(1, 1.48, 1.7, 2, 2.48, 2.7, 3), labels = c(10, 30, 50, 100, 300, 500, 1000),
                               limits = c(0.8, 3))+
            annotation_logticks()
          
          aniso_temcap1
          
          jpeg(filename = "temcap1_aniso.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          aniso_temcap1
          dev.off()      
      
      
      #Zygoptera
      zygo_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                   data = zygoptera, na.action = na.omit)
      summary(zygo_temcap1_lme_int)
      
      Anova(zygo_temcap1_lme_int, type = "III") #no interaction
      
      zygo_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                               data = zygoptera, na.action = na.omit)
      summary(zygo_temcap1_lme)
      
      zygo_temcap1_lme_table <- Anova(zygo_temcap1_lme)
      zygo_temcap1_lme_table
      
      plot(zygo_temcap1_lme)
      shapiro.test(resid(zygo_temcap1_lme))
      
      sort(cooks.distance(zygo_temcap1_lme))
      plot(sort(cooks.distance(zygo_temcap1_lme)))
      
        #Figure
          zygo_temcap1 <- model_line(zygoptera, log10(zygoptera$biomassa_mg), log10(zygoptera$tempocap1), 
                                     "Time of first capture [s], log10 scale", zygo_temcap1_lme,
                                     "Zygoptera")+
            scale_x_continuous(breaks = c(1, 1.3, 1.48), labels = c(10, 20, 30),
                               limits = c(0.8, 1.50)) +
            scale_y_continuous(breaks = c(1, 2, 3, 3.7), labels = c(10, 100, 1000, 5000),
                               limits = c(0.8, 4))+
            annotation_logticks()
          zygo_temcap1
          
          jpeg(filename = "temcap1_zygo.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          zygo_temcap1
          dev.off()
      
      #Notonectidae
      noto_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                       data = notonectidae, na.action = na.omit)
      summary(noto_temcap1_lme_int) 
      Anova(noto_temcap1_lme_int, type = "III") #no interaction
          
      noto_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                   data = notonectidae, na.action = na.omit)
      summary(noto_temcap1_lme)
          
      noto_temcap1_lme_table <- Anova(noto_temcap1_lme)
      noto_temcap1_lme_table
      plot(noto_temcap1_lme)
          
      shapiro.test(resid(noto_temcap1_lme))
          
      plot(sort(cooks.distance(noto_temcap1_lme)))
          
      notonectidae$tempocap1[13] <- NA #outlier removing
          
          #figure
          noto_temcap1 <- model_line(notonectidae, log10(notonectidae$biomassa_mg),
                                     log10(notonectidae$tempocap1), 
                                     "Time of first capture [s] \n log10 scale", noto_temcap1_lme, "Notonectidae")+
            scale_x_continuous(breaks = c(0.84, 1, 1.18,1.30), labels = c(7 ,10, 15, 20),
                               limits = c(0.8, 1.3)) +
            scale_y_continuous(breaks = c(1, 2, 3, 4), labels = c(10, 100, 1000, 10000),
                               limits = c(0.8, 4.5))+
            annotation_logticks()
          noto_temcap1
          
          jpeg(filename = "temcap1_noto.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          noto_temcap1
          dev.off()
          
          
          
    
          
      #Tabela com os resultados da Anova
          belo_temcap1_table
          aniso_temcap1_lme_int_table
          zygo_temcap1_lme_table
          noto_temcap1_lme_table
          
          stargazer(belo_temcap1_table,
                    aniso_temcap1_lme_int_table,
                    zygo_temcap1_lme_table,
                    noto_temcap1_lme_table,
                    type = "text", summary = FALSE,
                    title = c("Belostomatidae", "Anisoptera", "Zygoptera", "Notonectidae"))
          
          
          
          
          pare

      
      
      
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

# Models of Difference between Capture Times ------------------------------


          