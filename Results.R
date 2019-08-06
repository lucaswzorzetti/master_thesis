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
        
# Previous tests ----------------------------------------------------------
        #To verify if the covariable vary with treatment
          t.test(belostomatidae$biomassa_mg~belostomatidae$tratamento)
          t.test(anisoptera$biomassa_mg~anisoptera$tratamento)
          t.test(zygoptera$biomassa_mg~zygoptera$tratamento)
          t.test(notonectidae$biomassa_mg~notonectidae$tratamento)
        

# Comparing biomasses between Taxa ----------------------------------------
  #Biomass
    biomass_all <- geral %>% ggplot(aes(x = suborfam, y = biomassa_mg, fill = suborfam))+
            geom_point(size = 5, alpha = 0.5, shape = 21)+
            scale_x_discrete(limits = c("Belostomatidae", "Anisoptera",
                                        "Zygoptera", "Notonectidae")) +
            xlab("Taxa") + ylab(expression(paste("Biomass [mg]")))+
            theme_classic() + theme(legend.position = "none",
                                    axis.text = element_text(face = "bold",
                                                             size = 12, colour = "black"),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_text(face = "bold",
                                                                size = 18,
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
          #Previous tests
            #Levene Test for homocedasticity
            #Effect size for Treatment
          #Interaction model
          #Simple model
          #Without Treatment
          #Model selection
            #Veryfying interaction
            # R2m
            #aictab
          #Figure
            #saving
          
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
                             limits = c(0.8, 3))+
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
                             limits = c(0.8, 4))+
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
      noto_temcap1 <- model_line_1line(notonectidae, log10(notonectidae$biomassa_mg),
                                         log10(notonectidae$tempocap1), 
                                         "Time of first capture [s] \n log10 scale", noto_temcap1_lme, "Notonectidae")+
          scale_x_continuous(breaks = c(0.84, 1, 1.18,1.30), labels = c(7 ,10, 15, 20),
                             limits = c(0.8, 1.3)) +
          scale_y_continuous(breaks = c(1, 2, 3, 4), labels = c(10, 100, 1000, 10000),
                             limits = c(0.8, 4.5)) +
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
          geom_hline(yintercept = 0, linetype = 3)+
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
          geom_hline(yintercept = 0, linetype =3)+
          annotation_logticks()
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
      noto_temp_lme_int <- lmer(log(tempomanip1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                  data = notonectidae, na.action = na.omit, REML = F)
        summary(noto_temp_lme_int)
        
        Anova(noto_temp_lme_int, type = "III")#no inter
        
        shapiro.test(resid(noto_temp_lme_int))
        
        plot(sort(cooks.distance(noto_temp_lme_int)))
        
    #Simple model
      noto_temp_lme <- lmer(log(tempomanip1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                              data = notonectidae,na.action = na.omit, REML = F)
        
        summary(noto_temp_lme)
        
        Anova(noto_temp_lme)
        
        shapiro.test(resid(noto_temp_lme))
        
        plot(sort(cooks.distance(noto_temp_lme)))
        
    #Without Treatment
      noto_temp_lme_notrat <- lmer(log(tempomanip1) ~ log(biomassa_mg) + (1|bloco),
                              data = notonectidae,na.action = na.omit, REML = F)
      
        summary(noto_temp_lme_notrat)
        
        Anova(noto_temp_lme_notrat)
        
        shapiro.test(resid(noto_temp_lme_notrat))
        
        plot(sort(cooks.distance(noto_temp_lme_notrat)))
        
        notonectidae$tempomanip1[14] <- NA #cook d > 1.3
        notonectidae$tempomanip1[29] <- NA
        
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
                                       log10(notonectidae$tempomanip1), 
                                       "Handling Time [s]\n log10 scale",
                                       noto_temcap1_lme, "Notonectidae")+
          scale_x_continuous(breaks = c(0.7, 1, 1.18, 1.4),
                             labels = c(5, 10, 15,  25), limits = c(0.7, 1.4)) +
          scale_y_continuous(breaks = c(2.48, 2.7, 3, 3.3, 3.7, 4),
                             labels = c(300, 500, 1000, 2000, 5000, 10000))+
          annotation_logticks()
      noto_temp
        
      #saving
        jpeg(filename = "temp_noto.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        noto_temp
        dev.off() 
        
           