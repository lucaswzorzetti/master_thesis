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
      library(MuMIn) #R²m and R²c
      library(gnlm) #Non linear regressions
      library(AICcmodavg) #tables of model selection
      library(effsize) #effect size

    #Importing 
      geral <- read.table("planilhageral_atualizada2.txt", header = T, colClasses = c(
        "factor", "factor","factor","factor","character", "numeric", "numeric","numeric",
        "numeric","numeric","numeric","factor", "numeric","numeric","numeric","numeric",
        "numeric", "logical", "integer", "integer", "numeric","numeric","numeric","numeric",
        "numeric","numeric", "numeric","numeric","numeric"
      ))
      str(geral)
    
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
      geral <- geral %>% mutate(dif_temp_manip = ifelse(test = is.na(tempomanip2), #Difference on two capture times
                                                        yes = NA,
                                                        no = (tempomanip2-tempomanip1)))
      geral <- geral %>% mutate(prop_cap = presas_consumidas_gravacao/3) #Proportion of captured prey
      
      ## Separating the Taxa ##
        belostomatidae <- filter(geral, suborfam == "Belostomatidae")
        notonectidae <-  filter(geral, suborfam == "Notonectidae")
        anisoptera <-  filter(geral, suborfam == "Anisoptera")
        zygoptera <-  filter(geral, suborfam == "Zygoptera")
        
        stop
        
        geral %>%  group_by(bloco, suborfam, tratamento) %>% 
          summarise(n = n()) %>% 
          ggplot()+
          geom_bar(aes(y = n, x = bloco, fill = suborfam, colour = tratamento),
                   stat="identity") +
          xlab("Dia de Início da Amostra \n [referência: início do experimento]") + ylab("Quantidade de amostras feitas")+
          theme_classic(base_size = 22) + guides(fill = guide_legend(title="Grupo Taxonômico")) +
          scale_fill_discrete(labels = c("Libélula", "Barata d'água", "Notonecto",
                                         "Donzelinha")) +
          scale_x_discrete(labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                      11, 12)) +
          scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14))+
          scale_colour_manual(values = c("black", "black"))
# Previous tests ----------------------------------------------------------
        #To verify if the covariable vary with treatment
          t.test(belostomatidae$biomassa_mg~belostomatidae$tratamento)
          t.test(anisoptera$biomassa_mg~anisoptera$tratamento)
          t.test(zygoptera$biomassa_mg~zygoptera$tratamento)
          t.test(notonectidae$biomassa_mg~notonectidae$tratamento)
          
        #Verificando se a biomassa varia com os blocos
          belostomatidae %>% select(bloco, tratamento, biomassa_mg) %>%
            group_by(bloco, tratamento) %>%
            mutate(med = mean(biomassa_mg)) %>% summarise(mean(biomassa_mg)) #sei não...
          #ao que parece, a maioria é "pareada", mas alguns são bem discrepantes
          
          belostomatidae %>% select(bloco, tratamento, biomassa_mg) %>%
            group_by(bloco, tratamento) %>%
            mutate(med = mean(biomassa_mg)) %>% summarise(med = mean(biomassa_mg)) %>% 
            ggplot(aes(x = bloco, y = med))+geom_point()

# Comparing biomasses between Taxa ----------------------------------------
  #Biomass
    biomass_all <- geral %>% ggplot(aes(x = suborfam, y = log10(biomassa_mg), fill = suborfam))+
            geom_point(size = 5, alpha = 0.5, shape = 21)+
            scale_x_discrete(limits = c("Belostomatidae", "Anisoptera",
                                        "Zygoptera", "Notonectidae")) +
            xlab("Taxa") + ylab("Biomass [mg]\nlog10 scale")+
            scale_y_continuous(breaks = c(0.69897, 1, 1.3979, 1.6989, 2, 2.3010, 2.544),
                               labels = c(5, 10, 25, 50, 100, 200, 350))+
            theme_classic() + theme(legend.position = "none",
                                    axis.text.x = element_text(face = "bold",
                                                             size = 16, colour = "black"),
                                    axis.text.y = element_text(size = 18, colour = "black"),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_text(face = "bold",
                                                                size = 20,
                                                                margin = margin(r = 10)))
    biomass_all
          
      #saving .jpeg
            jpeg(filename = "biomassas.jpg", width = 2350, height = 1900, 
                 units = "px", pointsize = 12, quality = 100,
                 bg = "white",  res = 300)
            biomass_all
            dev.off()
          
          
  #Size
    size_all <- geral %>% ggplot(aes(x = suborfam, y = compr, fill = suborfam))+
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
    size_all
    
      #saving
        jpeg(filename = "tamanho_todos.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          size_all
          dev.off()
# Models of First Capture Time --------------------------------------------
  #Belostomatidae 
    #Previous tests
      #Levene Test for homocedasticity
      leveneTest(belostomatidae$tempocap1, center=mean, group = belostomatidae$tratamento)
      
      #Effect size for Treatment
      cohen.d(d = belostomatidae$tempocap1, f = belostomatidae$tratamento, na.rm = T)
      
    #Interaction model
      belo_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                       data = belostomatidae, na.action = na.omit, REML = F)
          summary(belo_temcap1_lme_int) #inter sig
          
          Anova(belo_temcap1_lme_int, type = "III") #inter sig
          
          shapiro.test(resid(belo_temcap1_lme_int)) #normality of residuals
          
          plot(sort(cooks.distance(belo_temcap1_lme_int))) #Cook's distance
          
    #Simple model
      belo_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg)+tratamento + (1|bloco),
                                   data = belostomatidae, na.action = na.omit, REML = F)
          summary(belo_temcap1_lme) #
          
          Anova(belo_temcap1_lme, type = "III") #
          
          shapiro.test(resid(belo_temcap1_lme)) #normality of residuals
          
          plot(sort(cooks.distance(belo_temcap1_lme))) #Cook's distance
          
    #Without Treatment
      belo_temcap1_lme_notrat <- lmer(log(tempocap1) ~ log(biomassa_mg) + (1|bloco),
                                      data = belostomatidae, na.action = na.omit, 
                                      REML = F)
          summary(belo_temcap1_lme_notrat) #
          
          Anova(belo_temcap1_lme_notrat, type = "III") #
          
          shapiro.test(resid(belo_temcap1_lme_notrat)) #normality of residuals
          
          plot(sort(cooks.distance(belo_temcap1_lme_notrat))) #Cook's distance
      
    #Model selection
      #Confirming interaction
        anova(belo_temcap1_lme_int, belo_temcap1_lme)
        
      # R2m
        r.squaredGLMM(belo_temcap1_lme_int)
        r.squaredGLMM(belo_temcap1_lme)
        r.squaredGLMM(belo_temcap1_lme_notrat)
     
      #aictab
        aictab(c(belo_temcap1_lme, belo_temcap1_lme_int, belo_temcap1_lme_notrat),
                 c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
          
    #Figure
      belo_temcap1 <- model_line(belostomatidae, log10(belostomatidae$biomassa_mg),
                                 log10(belostomatidae$tempocap1), 
                                 "Time of 1º capture [s]\nlog 10 scale",
                                 belo_temcap1_lme, "Belostomatidae") +
            scale_x_continuous(breaks = c(1, 1.30, 1.48, 1.6), labels = c(10, 20, 30, 40),
                               limits = c(0.8, 1.6)) +
            scale_y_continuous(breaks = c(1, 2, 3, 4), labels = c(10, 100, 1000, 10000),
                               limits = c(0.8, 4.5)) +
            theme(axis.text = element_text(size = 18, colour = "black"),
                  legend.position = "none") +
            annotation_logticks()
      belo_temcap1
          
        #saving
          jpeg(filename = "temcap1_belo.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          belo_temcap1
          dev.off()
          
  #Anisoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(anisoptera$tempocap1, center=mean, group = anisoptera$tratamento)
          
      #Effect size for Treatment
          cohen.d(d = anisoptera$tempocap1, f = anisoptera$tratamento, na.rm = T)
          
    #Interaction model
      aniso_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                        data = anisoptera, na.action = na.omit, REML = F)
      
        summary(aniso_temcap1_lme_int)  
        
        Anova(aniso_temcap1_lme_int, type = "III")
        
        shapiro.test(resid(aniso_temcap1_lme_int))
        
        plot(sort(cooks.distance(aniso_temcap1_lme_int)))
        
    #Simple model
      aniso_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                data = anisoptera, na.action = na.omit, REML = F)
      
        summary(aniso_temcap1_lme_int)
        
        Anova(aniso_temcap1_lme)
        
        shapiro.test(resid(aniso_temcap1_lme))
        
        plot(sort(cooks.distance(aniso_temcap1_lme)))
        
        
    #Without Treatment
      aniso_temcap1_lme_notrat <- lmer(log(tempocap1) ~ log(biomassa_mg) + (1|bloco),
                                data = anisoptera, na.action = na.omit, REML = F)
      
        summary(aniso_temcap1_lme_notrat)
        
        Anova(aniso_temcap1_lme_notrat)
        
        shapiro.test(resid(aniso_temcap1_lme_notrat))
        
        plot(sort(cooks.distance(aniso_temcap1_lme_notrat)))
      
      
    #Model selection
      #Confirming interaction
        anova(aniso_temcap1_lme_int, aniso_temcap1_lme)
        
      # R2m
        r.squaredGLMM(aniso_temcap1_lme_int)
        r.squaredGLMM(aniso_temcap1_lme)
        r.squaredGLMM(aniso_temcap1_lme_notrat)
    
      #aictab
        aictab(c(aniso_temcap1_lme, aniso_temcap1_lme_int, aniso_temcap1_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Outliers
        anisoptera$tempocap1[1] <- NA #cook d > 0.7
        
    #Figure
      aniso_temcap1 <- model_line(anisoptera, log10(anisoptera$biomassa_mg), log10(anisoptera$tempocap1), 
                                    "Time of first capture [s] \n log10 scale",
                                    aniso_temcap1_lme, "Anisoptera")+
          scale_x_continuous(breaks = c(1, 1.4, 1.7, 2, 2.18), labels = c(10, 25, 50, 100, 150),
                             limits = c(0.8, 2.2)) +
          scale_y_continuous(breaks = c(1, 1.48, 1.7, 2, 2.48, 2.7, 3), labels = c(10, 30, 50, 100, 300, 500, 1000),
                             limits = c(0.8, 3)) +
          theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none") +
          annotation_logticks()
        
      aniso_temcap1
      
        #saving
          jpeg(filename = "temcap1_aniso.jpg", width = 2350, height = 1900, 
              units = "px", pointsize = 12, quality = 100,
              bg = "white",  res = 300)
          aniso_temcap1
          dev.off() 
          
        
  #Zygoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(zygoptera$tempocap1, center=mean, group = zygoptera$tratamento)
        
      #Effect size for Treatment
        cohen.d(d = zygoptera$tempocap1, f = zygoptera$tratamento, na.rm = T)
        
    #Interaction model
      zygo_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                       data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_temcap1_lme_int)
        
        Anova(zygo_temcap1_lme_int, type = "III")
        
        shapiro.test(resid(zygo_temcap1_lme_int))
        
        plot(sort(cooks.distance(zygo_temcap1_lme_int)))
      
    #Simple model
      zygo_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                               data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_temcap1_lme)
        
        Anova(zygo_temcap1_lme)
        
        shapiro.test(resid(zygo_temcap1_lme))
        
        plot(sort(cooks.distance(zygo_temcap1_lme)))
      
    #Without Treatment
      zygo_temcap1_lme_notrat <- lmer(log(tempocap1) ~ log(biomassa_mg) + (1|bloco),
                               data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_temcap1_lme_notrat)
        
        Anova(zygo_temcap1_lme_notrat)
        
        shapiro.test(resid(zygo_temcap1_lme_notrat))
        
        plot(sort(cooks.distance(zygo_temcap1_lme_notrat)))
          
    #Model selection
      #Verifying interaction
        anova(zygo_temcap1_lme_int, zygo_temcap1_lme)
        
      # R2m
        r.squaredGLMM(zygo_temcap1_lme_int)
        r.squaredGLMM(zygo_temcap1_lme)
        r.squaredGLMM(zygo_temcap1_lme_notrat)
          
      #aictab
        aictab(c(zygo_temcap1_lme, zygo_temcap1_lme_int, zygo_temcap1_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
          
    #Figure
        zygo_temcap1 <- model_line_noline(zygoptera, log10(zygoptera$biomassa_mg), log10(zygoptera$tempocap1), 
                                          "Time of first capture [s] \n log10 scale",
                                          "Body Size [mg] log10scale",
                                          title = "Zygoptera")+
          scale_x_continuous(breaks = c(1, 1.3, 1.48), labels = c(10, 20, 30),
                             limits = c(0.8, 1.50)) +
          scale_y_continuous(breaks = c(1, 2, 3, 3.7), labels = c(10, 100, 1000, 5000),
                             limits = c(0.8, 4)) +
          theme(axis.text = element_text(size = 18, colour = "black"),
                legend.position = "none") +
          annotation_logticks()
        zygo_temcap1  
        
      #saving       
        jpeg(filename = "temcap1_zygo.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        zygo_temcap1
        dev.off()
     
  #Notonectidae
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(notonectidae$tempocap1, center=mean, group = notonectidae$tratamento)
        
      #Effect size for Treatment
        cohen.d(d = notonectidae$tempocap1, f = notonectidae$tratamento, na.rm = T)
        
    #Interaction model
      noto_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                     data = notonectidae, na.action = na.omit, REML = F)
        summary(noto_temcap1_lme_int) 
        
        Anova(noto_temcap1_lme_int, type = "III") 
        
        shapiro.test(resid(noto_temcap1_lme_int))
        
        plot(sort(cooks.distance(noto_temcap1_lme_int)))
        
    #Simple model
      noto_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                 data = notonectidae, na.action = na.omit, REML = F)
        summary(noto_temcap1_lme)
        
        Anova(noto_temcap1_lme)
        
        shapiro.test(resid(noto_temcap1_lme))
        
        plot(sort(cooks.distance(noto_temcap1_lme)))
        
    #Without Treatment
      noto_temcap1_lme_notrat <- lmer(log(tempocap1) ~ log(biomassa_mg) + (1|bloco),
                                      data = notonectidae, na.action = na.omit, REML = F)
        summary(noto_temcap1_lme_notrat)
        
        Anova(noto_temcap1_lme_notrat)
        
        shapiro.test(resid(noto_temcap1_lme_notrat))
        
        plot(sort(cooks.distance(noto_temcap1_lme_notrat)))
         
    #Model selection
      #Veryfying interaction
        anova(noto_temcap1_lme_int, noto_temcap1_lme)
        
      # R2m
        r.squaredGLMM(noto_temcap1_lme_int)
        r.squaredGLMM(noto_temcap1_lme)
        r.squaredGLMM(noto_temcap1_lme_notrat)
        
      #aictab
        aictab(c(noto_temcap1_lme, noto_temcap1_lme_int, noto_temcap1_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
      noto_temcap1 <- model_line_noline(notonectidae, log10(notonectidae$biomassa_mg),
                                         log10(notonectidae$tempocap1), 
                                         "Time of first capture [s] \n log10 scale", noto_temcap1_lme, "Notonectidae")+
          scale_x_continuous(breaks = c(0.84, 1, 1.18,1.30), labels = c(7 ,10, 15, 20),
                             limits = c(0.8, 1.3)) +
          scale_y_continuous(breaks = c(1, 2, 3, 4), labels = c(10, 100, 1000, 10000),
                             limits = c(0.8, 4.5)) +
          theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none") +
          annotation_logticks()
      noto_temcap1
      
      #saving   
        jpeg(filename = "temcap1_noto.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        noto_temcap1
        dev.off() 
        
      #Outliers
        notonectidae$tempocap1[13] <- NA # C.d >0.7
        
        
  #Table of models
        stargazer( belo_temcap1_lme_int, aniso_temcap1_lme_int,
                   zygo_temcap1_lme_notrat, noto_temcap1_lme,
                   align = TRUE,
                   type = "text",
                   title = "1° Capture Time Model results", ci = TRUE,
                   ci.level = 0.95, model.numbers = FALSE,
                   notes = "Confidence Interval of 95 percent",
                   column.labels = c("Belostomatidae", "Anisoptera",
                                     "Zygoptera", "Notonectidae"),
                   covariate.labels = c("Log(Biomass)", "Treatment: Warmed",
                                        "Log(Biomass) : Treatment",
                                        "Constant"), 
                   dep.var.labels = "1° Capture Time",
                   add.lines = list(c("R² marginal",
                                      round(r.squaredGLMM(belo_temcap1_lme_int)[1,1],
                                            digits = 4),
                                      round(r.squaredGLMM(aniso_temcap1_lme_int)[1,1],
                                            digits = 4),
                                      round(r.squaredGLMM(zygo_temcap1_lme_notrat)[1,1],
                                            digits = 4),
                                      round(r.squaredGLMM(noto_temcap1_lme)[1,1],
                                            digits = 4)),
                                    c("R² conditional",
                                      round(r.squaredGLMM(belo_temcap1_lme_int)[1,2], 
                                            digits = 4),
                                      round(r.squaredGLMM(aniso_temcap1_lme_int)[1,2],
                                            digits = 4),
                                      round(r.squaredGLMM(zygo_temcap1_lme_notrat)[1,2],
                                            digits = 4),
                                      round(r.squaredGLMM(noto_temcap1_lme)[1,2],
                                            digits = 4))))
        
# Models of Difference between Capture Times ------------------------------
  #Belostomatidae: All individuals captured only one prey
        
  #Notonectidae: Only 2 individuals captured more than one prey
        
  #Anisoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(anisoptera$dif_temp_cap, center=mean, group = anisoptera$tratamento)
        
      #Effect size for Treatment
        cohen.d(d = anisoptera$dif_temp_cap, f = anisoptera$tratamento, na.rm = T)
        
    #Interaction model
      aniso_diftemcap_lme_int <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                        data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_diftemcap_lme_int)
        
        Anova(aniso_diftemcap_lme_int, type = "III") #no inter
        
        shapiro.test(resid(aniso_diftemcap_lme_int))
        
        plot(sort(cooks.distance(aniso_diftemcap_lme_int)))
        
      #proportion (cap2/cap1)  
        aniso_propcaps_lme_int <- lmer(log(tempocap2/tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                        data = anisoptera, na.action = na.omit, REML = F)
          summary(aniso_propcaps_lme_int)
          
          Anova(aniso_propcaps_lme_int, type = "III") #no inter
          
          shapiro.test(resid(aniso_propcaps_lme_int))
          
          plot(sort(cooks.distance(aniso_propcaps_lme_int)))
        
    #Simple model
      aniso_diftemcap_lme <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                  data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_diftemcap_lme)
        
        Anova(aniso_diftemcap_lme)
        
        shapiro.test(resid(aniso_diftemcap_lme))
        
        plot(sort(cooks.distance(aniso_diftemcap_lme)))
        
          anisoptera$dif_temp_cap[16] <- NA #cookd > 1.9
        
        #proportion cap2/cap1
          aniso_propcaps_lme <- lmer(log(tempocap2/tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                   data = anisoptera, na.action = na.omit, REML = F)
            summary(aniso_propcaps_lme)
            
            Anova(aniso_propcaps_lme)
            
            shapiro.test(resid(aniso_propcaps_lme))
            
            plot(sort(cooks.distance(aniso_propcaps_lme)))
          
    #Without Treatment
      aniso_diftemcap_lme_notrat <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg)+ (1|bloco),
                                         data = anisoptera, na.action = na.omit, REML = F)  
        summary(aniso_diftemcap_lme_notrat)
        
        Anova(aniso_diftemcap_lme_notrat)
        
        shapiro.test(resid(aniso_diftemcap_lme_notrat))
        
        plot(sort(cooks.distance(aniso_diftemcap_lme_notrat)))
        
        #proportion cap2/cap1
          aniso_propcaps_lme_notrat <- lmer(log(tempocap2/tempocap1) ~ log(biomassa_mg) + (1|bloco),
                                            data = anisoptera, na.action = na.omit, REML = F)
            summary(aniso_propcaps_lme_notrat)
            
            Anova(aniso_propcaps_lme_notrat)
            
            shapiro.test(resid(aniso_propcaps_lme_notrat))
            
            plot(sort(cooks.distance(aniso_propcaps_lme_notrat)))
            
    #Model selection
      #Veryfying interaction
        anova(aniso_diftemcap_lme_int, aniso_diftemcap_lme)
            
      # R2m
        r.squaredGLMM(aniso_diftemcap_lme_int)
        r.squaredGLMM(aniso_diftemcap_lme)
        r.squaredGLMM(aniso_diftemcap_lme_notrat)
        
      #aictab
        aictab(c(aniso_diftemcap_lme, aniso_diftemcap_lme_int, aniso_diftemcap_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
      aniso_diftemcap <- model_line_1line(anisoptera, log10(anisoptera$biomassa_mg),
                                            log10neg(anisoptera$dif_temp_cap), 
                                            "Difference of Capture Times [s]\n log10 scale",
                                            noto_temcap1_lme, "Anisoptera")+
          scale_x_continuous(breaks = c(1, 1.48, 1.7, 2),
                             labels = c(10, 30, 50, 100), limits = c(0.9, 2.17)) +
          scale_y_continuous(breaks = c(-3.3, -2, -1, 0, 1, 2, 3, 4),
                             labels = c(-2000, -100, -10, 0, 10, 100, 1000, 10000))+
          geom_hline(yintercept = 0, linetype =3)+
          theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")+
          annotation_logticks()
        aniso_diftemcap
        
      #saving
        jpeg(filename = "diftemcap_aniso.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        aniso_diftemcap
        dev.off()
        
        
  #Zygoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(zygoptera$dif_temp_cap, center=mean, group = zygoptera$tratamento) 
        
      #Effect size for Treatment
        cohen.d(d = zygoptera$dif_temp_cap, f = zygoptera$tratamento, na.rm = T)
        
    #Interaction model
      zygo_diftemcap_lme_int <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                        data = zygoptera, na.action = na.omit, REML = F) 
        summary(zygo_diftemcap_lme_int)
        
        Anova(zygo_diftemcap_lme_int)
        
        shapiro.test(resid(zygo_diftemcap_lme_int))
        
        plot(sort(cooks.distance(zygo_diftemcap_lme_int)))
        
        #Proportion cap2/cap1
        zygo_propcaps_lme_int <- lmer(log(tempocap2/tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                      data = zygoptera, na.action = na.omit, REML = F)
          summary(zygo_propcaps_lme_int)
          
          Anova(zygo_propcaps_lme_int, type = "III")
          
          shapiro.test(resid(zygo_propcaps_lme_int))
          
          plot(sort(cooks.distance(zygo_propcaps_lme_int)))
        
        
    #Simple model
      zygo_diftemcap_lme <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                 data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_diftemcap_lme)
        
        Anova(zygo_diftemcap_lme)
        
        shapiro.test(resid(zygo_diftemcap_lme))
        
        plot(sort(cooks.distance(zygo_diftemcap_lme)))
          
        #proportion tempocap2/tempocap1
          zygo_propcaps_lme <- lmer(log(tempocap2/tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                    data = zygoptera, na.action = na.omit, REML = F)
            summary(zygo_propcaps_lme)
            
            Anova(zygo_propcaps_lme)
            
            shapiro.test(resid(zygo_propcaps_lme))
            
            plot(sort(cooks.distance(zygo_propcaps_lme)))
          
    #Without Treatment
      zygo_diftemcap_lme_notrat <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg) + (1|bloco),
                                        data = zygoptera, na.action = na.omit, REML = F) 
        summary(zygo_diftemcap_lme_notrat)
        
        Anova(zygo_diftemcap_lme_notrat)
        
        shapiro.test(resid(zygo_diftemcap_lme_notrat))
        
        plot(sort(cooks.distance(zygo_diftemcap_lme_notrat)))
            
    #Model selection
      #Veryfying interaction
        anova(zygo_diftemcap_lme_int, zygo_diftemcap_lme)
        
      # R2m
        r.squaredGLMM(zygo_diftemcap_lme_int)
        r.squaredGLMM(zygo_diftemcap_lme)
        r.squaredGLMM(zygo_diftemcap_lme_notrat) 
        
      #aictab
        aictab(c(zygo_diftemcap_lme, zygo_diftemcap_lme_int, zygo_diftemcap_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
        zygo_diftemcap <- model_line_noline(zygoptera, log10(zygoptera$biomassa_mg),
                                            log10neg(zygoptera$dif_temp_cap), 
                                            "Difference of Capture Times [s]\n log10 scale",
                                            noto_temcap1_lme, "Zygoptera")+
          scale_x_continuous(breaks = c(1, 1.3, 1.48),
                             labels = c(10, 20, 30)) +
          scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3, 4),
                             labels = c(-1000, -100, -10, 0, 10, 100, 1000, 10000))+
          geom_hline(yintercept = 0, linetype = 3) +
          theme(axis.text = element_text(size = 18, colour = "black"),
                legend.position = "none") +
          annotation_logticks()
        zygo_diftemcap 
        
        #saving
          jpeg(filename = "diftemcap_zygo.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          zygo_diftemcap
          dev.off()
        

          
# Models of Handling Time -------------------------------------------------
  #Belostomatidae: no measures
          
  #Anisoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(anisoptera$tempomanip1, center=mean, group = anisoptera$tratamento) 
          
      #Effect size for Treatment
        cohen.d(d = anisoptera$tempomanip1, f = anisoptera$tratamento, na.rm = T)
          
    #Interaction model
      aniso_temp_lme_int <- lmer(log(tempomanip1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                   data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_temp_lme_int)  #no interaction
        
        Anova(aniso_temp_lme_int, type = "III")
        
        shapiro.test(resid(aniso_temp_lme_int))
        
        plot(sort(cooks.distance(aniso_temp_lme_int)))
        
    #Simple model
      aniso_temp_lme <- lmer(log(tempomanip1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                               data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_temp_lme)
        
        Anova(aniso_temp_lme, type = "II")
        
        shapiro.test(resid(aniso_temp_lme))
        
        plot(sort(cooks.distance(aniso_temp_lme)))
        
    #Without Treatment
      aniso_temp_lme_notrat <- lmer(log(tempomanip1) ~ log(biomassa_mg) + (1|bloco),
                               data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_temp_lme_notrat)
        
        Anova(aniso_temp_lme_notrat, type = "II")
        
        shapiro.test(resid(aniso_temp_lme_notrat))
        
        plot(sort(cooks.distance(aniso_temp_lme_notrat)))
        
        anisoptera$tempomanip1[16] <- NA #Cd >0.5
        anisoptera$tempomanip1[12] <- NA #Cd >0.7
        anisoptera$tempomanip1[18] <- NA #Cd >0.5
        
    #Model selection
      #Veryfying interaction
        anova(aniso_temp_lme_int, aniso_temp_lme)
        
      # R2m
        r.squaredGLMM(aniso_temp_lme_int)
        r.squaredGLMM(aniso_temp_lme)
        r.squaredGLMM(aniso_temp_lme_notrat)
        
      #aictab
        aictab(c(aniso_temp_lme, aniso_temp_lme_int, aniso_temp_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
        aniso_temp <- model_line_1line(anisoptera, log10(anisoptera$biomassa_mg),
                                       log10(anisoptera$tempomanip1), 
                                       "Handling Time [s]\n log10 scale",
                                       noto_temcap1_lme, "Anisoptera")+
          scale_x_continuous(breaks = c(1, 1.3, 1.48, 1.6, 1.7, 2),
                             labels = c(10, 20, 30, 40, 50, 100), limits = c(0.9, 2.17)) +
          scale_y_continuous(breaks = c(0.7, 1, 1.7, 2),
                             labels = c(5, 10, 50, 100))+
          annotation_logticks() +
          theme(axis.text = element_text(size = 18, colour = "black"),
                legend.position = "none")
        aniso_temp
        
        #saving
          jpeg(filename = "temp_aniso.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          aniso_temp
          dev.off()  
          
  #Zygoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(zygoptera$tempomanip1, center=mean, group = zygoptera$tratamento)
          
      #Effect size for Treatment
        cohen.d(d = zygoptera$tempomanip1, f = zygoptera$tratamento, na.rm = T)
          
    #Interaction model
      zygo_temp_lme_int <- lmer(log(tempomanip1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                  data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_temp_lme_int) 
        
        Anova(zygo_temp_lme_int, type = "III") #no inter
        
        shapiro.test(resid(zygo_temp_lme_int))
        
        plot(sort(cooks.distance(zygo_temp_lme_int)))
          
    #Simple model
      zygo_temp_lme <- lmer(log(tempomanip1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                  data = zygoptera, na.action = na.omit, REML = F) 
        summary(zygo_temp_lme)
        
        Anova(zygo_temp_lme, type = "II")
        
        shapiro.test(resid(zygo_temp_lme))
        
        plot(sort(cooks.distance(zygo_temp_lme)))
        
    #Without Treatment
      zygo_temp_lme_notrat <- lmer(log(tempomanip1) ~ log(biomassa_mg) + (1|bloco),
                                   data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_temp_lme_notrat)
        
        Anova(zygo_temp_lme_notrat, type = "II")
        
        shapiro.test(resid(zygo_temp_lme_notrat))
        
        plot(sort(cooks.distance(zygo_temp_lme_notrat)))
      
    #Model selection
      #Veryfying interaction
        anova(zygo_temp_lme_int, zygo_temp_lme)
        
      # R2m
        r.squaredGLMM(zygo_temp_lme_int)
        r.squaredGLMM(zygo_temp_lme)
        r.squaredGLMM(zygo_temp_lme_notrat)
          
      #aictab
        aictab(c(zygo_temp_lme, zygo_temp_lme_int, zygo_temp_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
          
    #Figure
      zygo_temp <- model_line_1line(zygoptera, log10(zygoptera$biomassa_mg),
                                      log10(zygoptera$tempomanip1), 
                                      "Handling Time [s]\n log10 scale",
                                      noto_temcap1_lme, "Zygoptera")+
          scale_x_continuous(breaks = c(0.7, 1, 1.3, 1.48),
                             labels = c(5, 10, 20, 30)) +
          scale_y_continuous(breaks = c(0.7, 1, 1.3, 1.7, 2),
                             labels = c(5, 10, 20, 50, 100))+
          theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none") +
          annotation_logticks()
       zygo_temp
        
      #saving
        jpeg(filename = "temp_zygo.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        zygo_temp
        dev.off()  
        
        
  #Notonectidae
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(notonectidae$tempomanip1, center=mean, group = notonectidae$tratamento) 
        
      #Effect size for Treatment
        cohen.d(d = notonectidae$tempomanip1, f = notonectidae$tratamento, na.rm = T)
        
    #Interaction model
      noto_temp_lme_int <- lmer((tempomanip1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                  data = notonectidae, na.action = na.omit, REML = F)
        summary(noto_temp_lme_int)
        
        Anova(noto_temp_lme_int, type = "III")#no inter
        
        shapiro.test(resid(noto_temp_lme_int))
        
        plot(sort(cooks.distance(noto_temp_lme_int)))
        
    #Simple model
      noto_temp_lme <- lmer((tempomanip1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                              data = notonectidae,na.action = na.omit, REML = F)
        
        summary(noto_temp_lme)
        
        Anova(noto_temp_lme)
        
        shapiro.test(resid(noto_temp_lme))
        
        plot(sort(cooks.distance(noto_temp_lme)))
        
    #Without Treatment
      noto_temp_lme_notrat <- lmer((tempomanip1) ~ log(biomassa_mg) + (1|bloco),
                              data = notonectidae,na.action = na.omit, REML = F)
      
        summary(noto_temp_lme_notrat)
        
        Anova(noto_temp_lme_notrat)
        
        shapiro.test(resid(noto_temp_lme_notrat))
        
        plot(sort(cooks.distance(noto_temp_lme_notrat)))
        
        notonectidae$tempomanip1[14] <- NA #cook d > 1.3
        notonectidae$tempomanip1[13] <- NA # cook d >0.7
        notonectidae$tempomanip1[29] <- NA
        notonectidae$tempomanip1[17] <- NA
        
        
        
    #Model selection
      #Veryfying interaction
        anova(noto_temp_lme_int, noto_temp_lme)
        
      # R2m
        r.squaredGLMM(noto_temp_lme_int)  
        r.squaredGLMM(noto_temp_lme) 
        r.squaredGLMM(noto_temp_lme_notrat) 
        
      #aictab
        aictab(c(noto_temp_lme, noto_temp_lme_int, noto_temp_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
      noto_temp <- model_line_noline(notonectidae, log10(notonectidae$biomassa_mg),
                                       (notonectidae$tempomanip1), 
                                       "Handling Time [s]",
                                       noto_temcap1_lme, "Notonectidae")+
          scale_x_continuous(breaks = c(0.845, 1, 1.18),
                             labels = c(7, 10, 15), limits = c(0.845, 1.18)) +
        theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none") +
          annotation_logticks(sides = "b")
      noto_temp
        
      #saving
        jpeg(filename = "temp_noto.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        noto_temp
        dev.off() 
        
           
        
# Models of Total Consumption ----------------------------------------------
  #Belostomatidae
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(log10((belostomatidae$Totalpresascorrigido+1)), center=mean, group = belostomatidae$tratamento)
        
      #Effect size for Treatment
        cohen.d(d = belostomatidae$Totalpresascorrigido, f = belostomatidae$tratamento, na.rm = T)
        
    #Interaction model
      belo_pres_lme_int <- lmer(log10((belostomatidae$Totalpresascorrigido+1)) ~ (biomassa_mg)*tratamento +
                                    (1|bloco), data = belostomatidae, na.action = na.omit, REML = F)
        summary(belo_pres_lme_int) 
        
        Anova(belo_pres_lme_int, type = "III")
        
        shapiro.test(resid(belo_pres_lme_int))
        
        plot(sort(cooks.distance(belo_pres_lme_int)))
        
        belostomatidae$Totalpresascorrigido[29] <- NA
        
        
    #Simple model
      belo_pres_lme <-  lmer(log10((belostomatidae$Totalpresascorrigido+1)) ~ (biomassa_mg) + tratamento + 
                                 (1|bloco), data = belostomatidae, na.action = na.omit, REML = F) 
        summary(belo_pres_lme)
        
        Anova(belo_pres_lme) 
        
        shapiro.test(resid(belo_pres_lme))
        
        plot(sort(cooks.distance(belo_pres_lme)))
        
    #Without Treatment
      belo_pres_lme_notrat <- lmer(log10((belostomatidae$Totalpresascorrigido+1)) ~ (biomassa_mg)+ 
                                     (1|bloco), data = belostomatidae, na.action = na.omit, REML = F)
        summary(belo_pres_lme_notrat)
        
        Anova(belo_pres_lme_notrat)
        
        shapiro.test(resid(belo_pres_lme_notrat))
        
        plot(sort(cooks.distance(belo_pres_lme_notrat)))
      
    #Model selection
      #Veryfying interaction
        anova(belo_pres_lme_int, belo_pres_lme)
        
      # R2m
        r.squaredGLMM(belo_pres_lme_int)
        r.squaredGLMM(belo_pres_lme)
        r.squaredGLMM(belo_pres_lme_notrat)
        
      #aictab
        aictab(c(belo_pres_lme, belo_pres_lme_int, belo_pres_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
      belo_pres <- model_line(belostomatidae, log10(belostomatidae$biomassa_mg),
                                (log10((belostomatidae$Totalpresascorrigido+1))),
                                "N° prey consumed/day\n log10 scale", belo_pres_lme_int, "Belostomatidae")+
          theme(legend.position = c(0.8, 0.8)) + geom_hline(yintercept = 0, linetype = 3)+
          scale_x_continuous(breaks = c(1, 1.301, 1.7, 2, 2.477),
                             labels = c(10, 20, 50, 100, 300))+ annotation_logticks()+
          scale_y_continuous(breaks = c(0, 0.17609, 0.30102, 0.39794, 0.47712),
                           labels = c(0, 0.5, 1, 1.5, 2)) + 
          theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")
      belo_pres
      
      #saving  
        jpeg(filename = "pres_belo.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        belo_pres
        dev.off()
       
        
  #Anisoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(anisoptera$Totalpresascorrigido, center=mean, group = anisoptera$tratamento)
        
      #Effect size for Treatment
        cohen.d(d = anisoptera$Totalpresascorrigido, f = anisoptera$tratamento, na.rm = T)
        
    #Interaction model
      aniso_pres_lme_int <- lmer(log(Totalpresascorrigido) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                   data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_pres_lme_int)
        
        Anova(aniso_pres_lme_int, type = "III")
        
        shapiro.test(resid(aniso_pres_lme_int))
        
        plot(sort(cooks.distance(aniso_pres_lme_int)))
        
    #Simple model
      aniso_pres_lme <- lmer(log(Totalpresascorrigido) ~ log(biomassa_mg) + tratamento + (1|bloco),
                               data = anisoptera, na.action = na.omit, REML = F)
        
        summary(aniso_pres_lme)
        
        Anova(aniso_pres_lme) 
        
        shapiro.test(resid(aniso_pres_lme))
        
        plot(sort(cooks.distance(aniso_pres_lme)))
        
        anisoptera$Totalpresascorrigido[28] <- NA #cook d >0.8
        anisoptera$Totalpresascorrigido[30] <- NA #cook d > 0.7
        
    #Without Treatment
      aniso_pres_lme_notrat <- lmer(log(Totalpresascorrigido) ~ log(biomassa_mg) + (1|bloco),
                               data = anisoptera, na.action = na.omit, REML = F)  
        summary(aniso_pres_lme_notrat)
        
        Anova(aniso_pres_lme_notrat)
        
        shapiro.test(resid(aniso_pres_lme_notrat))
        
        plot(sort(cooks.distance(aniso_pres_lme_notrat)))
      
    #Model selection
      #Veryfying interaction
        anova(aniso_pres_lme_int, aniso_pres_lme)
        
      # R2m
       r.squaredGLMM(aniso_pres_lme_int) 
       r.squaredGLMM(aniso_pres_lme)
       r.squaredGLMM(aniso_pres_lme_notrat)
        
      #aictab
        aictab(c(aniso_pres_lme, aniso_pres_lme_int, aniso_pres_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
      aniso_pres <- model_line(anisoptera, log10(anisoptera$biomassa_mg), log10(anisoptera$Totalpresascorrigido),
                                 "N° prey consumed/day\n log10 scale",
                                 aniso_pres_lme_int, "Anisoptera") + annotation_logticks(sides = "bl")+
          scale_x_continuous(breaks = c(0.698, 1, 1.301, 1.698, 2),
                             labels = c(5, 10, 20, 50, 100))+
          scale_y_continuous(breaks = c(0.1, 0.2, 0.3010299),
                           labels = c(1.26, 1.58, 2)) +
          theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none") 
        aniso_pres 
        
      #saving
        jpeg(filename = "pres_aniso.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        aniso_pres
        dev.off()
       
       
  #Zygoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(zygoptera$Totalpresascorrigido, center=mean, group = zygoptera$tratamento)
        
      #Effect size for Treatment
        cohen.d(d = zygoptera$Totalpresascorrigido, f = zygoptera$tratamento, na.rm = T)
        
    #Interaction model
      zygo_pres_lme_int <- lmer(Totalpresascorrigido ~ biomassa_mg*tratamento + (1|bloco),
                                  data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_pres_lme_int)
        
        Anova(zygo_pres_lme_int, type = "III") 
        
        shapiro.test(resid(zygo_pres_lme_int))
        
        plot(sort(cooks.distance(zygo_pres_lme_int)))
        
    #Simple model
      zygo_pres_lme <- lmer(Totalpresascorrigido ~ log(biomassa_mg) + tratamento + (1|bloco),
                              data = zygoptera, na.action = na.omit, REML = F)
        
        summary(zygo_pres_lme)
        
        Anova(zygo_pres_lme, type = "II")
        
        shapiro.test(resid(zygo_pres_lme))
        
        plot(sort(cooks.distance(zygo_pres_lme)))
        
    #Without Treatment
      zygo_pres_lme_notrat <- lmer(Totalpresascorrigido ~ log(biomassa_mg)+ (1|bloco),
                              data = zygoptera, na.action = na.omit, REML = F)
        
        summary(zygo_pres_lme_notrat)
        
        Anova(zygo_pres_lme_notrat, type = "II")
        
        shapiro.test(resid(zygo_pres_lme_notrat))
        
        plot(sort(cooks.distance(zygo_pres_lme_notrat)))  
        
    #Model selection
      #Veryfying interaction
        anova(zygo_pres_lme_int, zygo_pres_lme)
        
      # R2m
        r.squaredGLMM(zygo_pres_lme_int)
        r.squaredGLMM(zygo_pres_lme)
        r.squaredGLMM(zygo_pres_lme_notrat)
        
      #aictab
       aictab(c(zygo_pres_lme, zygo_pres_lme_int, zygo_pres_lme_notrat),
              c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)")) 
        
    #Figure
      zygo_pres <-  model_line(zygoptera, log10(zygoptera$biomassa_mg), (zygoptera$Totalpresascorrigido),
                                       "N° prey consumed/day", zygo_pres_lme,
                                       "Zygoptera")+ annotation_logticks(sides = "b")+
        scale_x_continuous(breaks = c(0.69897, 1, 1.301, 1.4771),
                           labels = c(5, 10, 20, 30)) +
        theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")
        
      zygo_pres
      
      #saving 
       jpeg(filename = "pres_zygo.jpg", width = 2350, height = 1900, 
            units = "px", pointsize = 12, quality = 100,
            bg = "white",  res = 300)
       zygo_pres
       dev.off()
        
  #Notonectidae
    #Previous tests
      #Levene Test for homocedasticity
       leveneTest(notonectidae$Totalpresascorrigido, center=mean, group = notonectidae$tratamento)
       
      #Effect size for Treatment
       cohen.d(d = notonectidae$Totalpresascorrigido, f = notonectidae$tratamento, na.rm = T)
       
    #Interaction model
      noto_pres_lme_int <- lmer(Totalpresascorrigido ~ log(biomassa_mg)*tratamento + 
                                   (1|bloco), data = notonectidae,
                                 na.action = na.omit, REML = F)
        summary(noto_pres_lme_int)
       
        Anova(noto_pres_lme_int, type = "III") 
        
        shapiro.test(resid(noto_pres_lme_int))
        
        plot(sort(cooks.distance(noto_pres_lme_int)))
        
    #Simple model
      noto_pres_lme <- lmer((Totalpresascorrigido) ~ log(biomassa_mg) + tratamento + 
                                (1|bloco), data = notonectidae, na.action = na.omit, REML = F)
        
        summary(noto_pres_lme)
        
        Anova(noto_pres_lme, type = "II")
        
        shapiro.test(resid(noto_pres_lme))
        
        plot(sort(cooks.distance(noto_pres_lme)))
        
        notonectidae$Totalpresascorrigido[16] <- NA #cook d > 0.74
        notonectidae$Totalpresascorrigido[8] <- NA #only without it normalizes, c d ~0.5
       
    #Without Treatment
      noto_pres_lme_notrat <- lmer(Totalpresascorrigido ~ log(biomassa_mg) + 
                                (1|bloco), data = notonectidae, na.action = na.omit, REML = F)
        summary(noto_pres_lme_notrat)
        
        Anova(noto_pres_lme_notrat)
        
        shapiro.test(resid(noto_pres_lme_notrat))
        
        plot(sort(cooks.distance(noto_pres_lme_notrat)))
       
    #Model selection
      #Veryfying interaction
        anova(noto_pres_lme_int, noto_pres_lme)
       
      # R2m
        r.squaredGLMM(noto_pres_lme_int)
        r.squaredGLMM(noto_pres_lme)
        r.squaredGLMM(noto_pres_lme_notrat)
       
      #aictab
        aictab(c(noto_pres_lme, noto_pres_lme_int, noto_pres_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
       
    #Figure
      noto_pres <- model_line_noline(notonectidae, log10(notonectidae$biomassa_mg),
                                       notonectidae$Totalpresascorrigido,
                                       "N° prey consumed/day", noto_pres_lme, "Notonectidae")+
          scale_x_continuous(breaks = c(0.845, 1, 1.176, 1.301),
                             labels = c(7, 10, 15, 20), limits = c(0.8,1.35)) +
          scale_y_continuous(breaks = c(0.8, 1, 1.25, 1.5, 1.75, 2),
                             labels = c(0.8, 1, 1.25, 1.5, 1.75, 2))+
        theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")
      noto_pres
      
      #saving  
        jpeg(filename = "pres_noto.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        noto_pres
        dev.off() 
 
        
        
               
# Models of Growth rate ---------------------------------------------------
  #Belostomatidae
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest((((belostomatidae$taxacrescimento)^-7)), center=mean, group = belostomatidae$tratamento)
        
        boxcox(belostomatidae$taxacrescimento ~ belostomatidae$tratamento, lambda = c(-15,0)) #-0.5
      #Effect size for Treatment
        cohen.d(d = belostomatidae$taxacrescimento, f = belostomatidae$tratamento, na.rm = T)
        
    #Interaction model
      belo_cresc_lme_int <- lmer((taxacrescimento^-7) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                   data = belostomatidae, na.action = na.omit, REML = F)
        summary(belo_cresc_lme_int)
        
        Anova(belo_cresc_lme_int, "III") 
        
        shapiro.test(resid(belo_cresc_lme_int)) 
        
        plot(sort(cooks.distance(belo_cresc_lme_int))) 
        
        #belostomatidae$taxacrescimento[17] <- NA #cook d > 0.58 #even in this way, not normalized
        
    #Simple model
      belo_cresc_lme <- lmer((taxacrescimento^-7) ~ biomassa_mg + tratamento + (1|bloco),
                               data = belostomatidae, na.action = na.omit, REML = F)
        summary(belo_cresc_lme)
        
        Anova(belo_cresc_lme)
        
        shapiro.test(resid(belo_cresc_lme))
        
        plot(sort(cooks.distance(belo_cresc_lme)))
        
    #Without Treatment
      belo_cresc_lme_notrat <- lmer((taxacrescimento^-7) ~ biomassa_mg + (1|bloco),
                               data = belostomatidae, na.action = na.omit, REML = F)
        summary(belo_cresc_lme_notrat)
        
        Anova(belo_cresc_lme_notrat)
        
        shapiro.test(resid(belo_cresc_lme_notrat))
        
        plot(sort(cooks.distance(belo_cresc_lme_notrat)))
        
    #Model selection
      #Veryfying interaction
        anova(belo_cresc_lme_int, belo_cresc_lme)
        
      # R2m
        r.squaredGLMM(belo_cresc_lme_int) 
        r.squaredGLMM(belo_cresc_lme)
        r.squaredGLMM(belo_cresc_lme_notrat)
        
        
      #aictab
        aictab(c(belo_cresc_lme, belo_cresc_lme_int, belo_cresc_lme_notrat),
                      c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
    #Figure
      belo_cresc <-  model_line(belostomatidae, log10(belostomatidae$biomassa_mg), 
                                  (-(belostomatidae$taxacrescimento^-7)),
                                  ynome = expression(paste("Growth rate [(-)boxcox scale ", lambda, "= -7]")), 
                                  model = belo_cresc_lme_int,
                                  title = "Belostomatidae") + annotation_logticks(sides = "b") +
          geom_hline(yintercept = -1, linetype = 3)+
        scale_x_continuous(breaks = c(1, 1.301, 1.7, 2, 2.477),
                           labels = c(10, 20, 50, 100, 300)) +
        scale_y_continuous(breaks = c(-0.09486, -0.1593663, -0.2790816, -0.5131581 ,-0.7106813, -0.8705602, -1, -1.160144, -1.330765),
                           labels = c(1.40, 1.3, 1.2, 1.1, 1.05, 1.02, 1, 0.98, 0.96))+
        theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")
      belo_cresc
        
      
      #saving     
      jpeg(filename = "growth_belos.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      belo_cresc
      dev.off() 
        
 
  #Anisoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(anisoptera$taxacrescimento, center=mean, group = anisoptera$tratamento)
      
      #Effect size for Treatment
        cohen.d(d = anisoptera$taxacrescimento, f = anisoptera$tratamento, na.rm = T)
      
    #Interaction model
      aniso_cresc_lme_int <- lmer((taxacrescimento) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                    data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_cresc_lme_int)
        
        Anova(aniso_cresc_lme_int, "III")
        
        shapiro.test(resid(aniso_cresc_lme_int))
        
        plot(sort(cooks.distance(aniso_cresc_lme_int)))
        
    #Simple model
      aniso_cresc_lme <- lmer((taxacrescimento) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_cresc_lme)
        
        Anova(aniso_cresc_lme)
        
        shapiro.test(resid(aniso_cresc_lme))
        
        plot(sort(cooks.distance(aniso_cresc_lme)))
        
        anisoptera$taxacrescimento[4] <- NA #
        anisoptera$taxacrescimento[16] <- NA # cook d > 1.15
      
    #Without Treatment
      aniso_cresc_lme_notrat <- lmer((taxacrescimento) ~ biomassa_mg + (1|bloco),
                                       data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_cresc_lme_notrat)
      
        Anova(aniso_cresc_lme_notrat)
        
        shapiro.test(resid(aniso_cresc_lme_notrat))
        
        plot(sort(cooks.distance(aniso_cresc_lme_notrat)))
      
    #Model selection
      #Veryfying interaction
        anova(aniso_cresc_lme_int, aniso_cresc_lme)
      
      # R2m
        r.squaredGLMM(aniso_cresc_lme_int)
        r.squaredGLMM(aniso_cresc_lme)
        r.squaredGLMM(aniso_cresc_lme_notrat)
        
      #aictab
        aictab(c(aniso_cresc_lme, aniso_cresc_lme_int, aniso_cresc_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
      
    #Figure
      aniso_cresc <-  model_line_1line(anisoptera, log10(anisoptera$biomassa_mg), 
                                                (anisoptera$taxacrescimento),
                                                ynome = "Growth rate", 
                                                model = belo_cresc_lme_int,
                                                title = "Anisoptera") +
          geom_hline(yintercept = 1, linetype = 3)+ annotation_logticks(sides = "b")+
          scale_x_continuous(breaks = c(0.698, 1, 1.301, 1.698, 2, 2.301),
                             labels = c(5, 10, 20, 50, 100, 200))+
        theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")
          
      aniso_cresc 
        
      #saving
      jpeg(filename = "growth_aniso.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      aniso_cresc
      dev.off()
        

  #Zygoptera
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(zygoptera$taxacrescimento, center=mean, group = zygoptera$tratamento)
      
      #Effect size for Treatment
        cohen.d(d = zygoptera$taxacrescimento, f = zygoptera$tratamento, na.rm = T)
      
    #Interaction model
      zygo_cresc_lme_int <- lmer(log(taxacrescimento) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                 data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_cresc_lme_int)
        
        Anova(zygo_cresc_lme_int, "III")
        
        shapiro.test(resid(zygo_cresc_lme_int))

        plot(sort(cooks.distance(zygo_cresc_lme_int)))
      
    #Simple model
      zygo_cresc_lme <- lmer(log(taxacrescimento) ~ log(biomassa_mg) + tratamento + (1|bloco),
                               data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_cresc_lme)
        
        Anova(zygo_cresc_lme)
        
        shapiro.test(resid(zygo_cresc_lme))

        plot(sort(cooks.distance(zygo_cresc_lme)))
        
    #Without Treatment
      zygo_cresc_lme_notrat <- lmer(log(taxacrescimento) ~ log(biomassa_mg) + (1|bloco),
                               data = zygoptera, na.action = na.omit, REML = F)
        summary(zygo_cresc_lme_notrat)
        
        Anova(zygo_cresc_lme_notrat)
        
        shapiro.test(resid(zygo_cresc_lme_notrat))
        
        plot(sort(cooks.distance(zygo_cresc_lme_notrat)))
        
        zygoptera$taxacrescimento[24] <- NA #cook d > 0.84
    
    #Model selection
      #Veryfying interaction
        anova(zygo_cresc_lme_int, zygo_cresc_lme)
      
      # R2m
        r.squaredGLMM(zygo_cresc_lme_int)
        r.squaredGLMM(zygo_cresc_lme)
        r.squaredGLMM(zygo_cresc_lme_notrat)
      
      #aictab
        aictab(c(zygo_cresc_lme, zygo_cresc_lme_int, zygo_cresc_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
      
    #Figure
      zygo_cresc <-  model_line_1line(zygoptera, log10(zygoptera$biomassa_mg), 
                                        log10(zygoptera$taxacrescimento),
                                        ynome = "Growth rate \n log10 scale", 
                                        model = zygo_cresc_lme,
                                        title = "Zygoptera")+
          scale_x_continuous(breaks = c(0.778, 1, 1.301, 1.477),
                             labels = c(6, 10, 20, 30))+
          scale_y_continuous(breaks = c(-0.0223,-0.0088, 0, 0.0086, 0.0212, 0.0334, 0.0414),
                             labels = c(0.95, 0.98, 1, 1.02, 1.05, 1.08, 1.10)) +
          annotation_logticks()+
          geom_hline(yintercept = 0, linetype = 3)+
        theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")
      zygo_cresc     
        
      #saving
        jpeg(filename = "growth_zygo.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        zygo_cresc
        dev.off()
        
      
  #Notonectidae
    #Previous tests
      #Levene Test for homocedasticity
        leveneTest(notonectidae$taxacrescimento, center=mean, group = notonectidae$tratamento)
        
      #Effect size for Treatment
        cohen.d(d = notonectidae$taxacrescimento, f = notonectidae$tratamento, na.rm = T)
        
    #Interaction model
      noto_cresc_lme_int <- lmer(taxacrescimento ~ log(biomassa_mg)*tratamento + (1|bloco),
                                   data = zygoptera, na.action = na.omit, REML = F)
        summary(noto_cresc_lme_int)
        
        Anova(noto_cresc_lme_int, "III")
        
        shapiro.test(resid(noto_cresc_lme_int))
        
        plot(sort(cooks.distance(noto_cresc_lme_int)))
        
    #Simple model
      noto_cresc_lme <- lmer(taxacrescimento ~ log(biomassa_mg) + tratamento + (1|bloco),
                               data = zygoptera, na.action = na.omit, REML = F)
        
        summary(noto_cresc_lme)
        
        Anova(noto_cresc_lme)
        
        shapiro.test(resid(noto_cresc_lme))
        
        plot(sort(cooks.distance(noto_cresc_lme)))
        
    #Without Treatment
      noto_cresc_lme_notrat <- lmer(taxacrescimento ~ log(biomassa_mg) + (1|bloco),
                                      data = zygoptera, na.action = na.omit, REML = F)
        summary(noto_cresc_lme_notrat)
        
        Anova(noto_cresc_lme_notrat)
        
        shapiro.test(resid(noto_cresc_lme_notrat))
        
        plot(sort(cooks.distance(noto_cresc_lme_notrat)))
        
    #Model selection
      #Veryfying interaction
        anova(noto_cresc_lme_int, noto_cresc_lme)
        
      # R2m
        r.squaredGLMM(noto_cresc_lme_int)
        r.squaredGLMM(noto_cresc_lme)
        r.squaredGLMM(noto_cresc_lme_notrat)
        
      #aictab
        aictab(c(noto_cresc_lme, noto_cresc_lme_int, noto_cresc_lme_notrat),
               c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))  
        
    #Figure
      noto_cresc <-  model_line_1line(notonectidae, log10(notonectidae$biomassa_mg), 
                                        (notonectidae$taxacrescimento),
                                        ynome = "Growth rate", 
                                        model = noto_cresc_lme,
                                        title = "Notonectidae")+
          scale_x_continuous(breaks = c(0.698, 1, 1.301, 1.477, 1.602),
                             labels = c(5, 10, 20, 30, 40))+
          scale_y_continuous() +
        geom_hline(yintercept = 1, linetype = 3)+
          annotation_logticks(sides = "b")+
        theme(axis.text = element_text(size = 18, colour = "black"),
              legend.position = "none")
      noto_cresc 
        
      #saving
        jpeg(filename = "growth_noto.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        noto_cresc
        dev.off()   
        

        