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
    library(MuMIn) #R²m and R²c
    library(gnlm) #Non linear regressions
    library(AICcmodavg) #tabelas de seleção de modelos
    library(effsize)

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
    geral <- geral %>% mutate(dif_temp_manip = ifelse(test = is.na(tempomanip2), #Difference on two capture times
                                                      yes = NA,
                                                      no = (tempomanip2-tempomanip1)))
    geral <- geral %>% mutate(prop_cap = presas_consumidas_gravacao/3) #Proportion of captured prey
    
    ## Separating the Taxa ##
      belostomatidae <- filter(geral, suborfam == "Belostomatidae")
      notonectidae <-  filter(geral, suborfam == "Notonectidae")
      anisoptera <-  filter(geral, suborfam == "Anisoptera")
      zygoptera <-  filter(geral, suborfam == "Zygoptera")
      
      pare


# testes prévios ----------------------------------------------------------
    #para verificar se a covariavel nao varia com o tratamento
      t.test(belostomatidae$biomassa_mg~belostomatidae$tratamento)
      t.test(anisoptera$biomassa_mg~anisoptera$tratamento)
      t.test(zygoptera$biomassa_mg~zygoptera$tratamento)
      t.test(notonectidae$biomassa_mg~notonectidae$tratamento)
      #todos OK
      

# Comparing biomasses between Taxa ----------------------------------------
      biomassas_todos <- geral %>% ggplot(aes(x = suborfam, y = biomassa_mg, fill = suborfam))+
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
                                   data = belostomatidae, na.action = na.omit, REML = F) #Usei Maximum Likelihood para facilitar a comparação dos modelos
      summary(belo_temcap1_lme_int) #com interação
      belo_temcap1_table <- Anova(belo_temcap1_lme_int, type = "III")
      belo_temcap1_table
      
      print(belo_temcap1_table, digits =4)
      
      r.squaredGLMM(belo_temcap1_lme_int)
      
      shapiro.test(resid(belo_temcap1_lme_int)) 
      
      leveneTest(belostomatidae$tempocap1, center=mean, group = belostomatidae$tratamento) #testando a homocedasticidade (hipotese nula -> p>0.05)
      
      t.test(belostomatidae$biomassa_mg~belostomatidae$tratamento) #teste para mostrar que a covariavel não varia com o tratamento
                                                      #o p-value > 0.05 para aceitar a hipotese nula (não há diferenças)
      
      AIC(belo_temcap1_lme_int)
      
        #Seleção de modelos
        
        belo_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg)+tratamento + (1|bloco),
                                     data = belostomatidae, na.action = na.omit, REML = F)
        belo_temcap1_lme_notrat <- lmer(log(tempocap1) ~ log(biomassa_mg) + (1|bloco),
                                 data = belostomatidae, na.action = na.omit, REML = F)
        
        aictab(c(belo_temcap1_lme, belo_temcap1_lme_int, belo_temcap1_lme_notrat),
                   c("Log(Biomass) + Treatment", "log(Biomass):Treatment", "Log(Biomass)"))
        
        #omega 2 <- 1-var(residuals(belo_temcap1_lme_int))/(var(model.response(model.frame(belo_temcap1_lme_int))))
        
        cohen.d(d = belostomatidae$tempocap1, f = belostomatidae$tratamento, na.rm = T)
        
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
          
      ###Anisoptera
      aniso_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                        data = anisoptera, na.action = na.omit, REML = F)
      summary(aniso_temcap1_lme_int)
          
      aniso_temcap1_lme_int_table <- Anova(aniso_temcap1_lme_int, type = "III") #interaction confirmed
      aniso_temcap1_lme_int_table
      
      r.squaredGLMM(aniso_temcap1_lme_int)
      
          
      plot(aniso_temcap1_lme_int)
      
      plot(sort(cooks.distance(aniso_temcap1_lme_int)))
          
      shapiro.test(resid(aniso_temcap1_lme_int))
      
      leveneTest(anisoptera$tempocap1, center=mean, group = anisoptera$tratamento) #homocedasticidade
      
      t.test(anisoptera$biomassa_mg~anisoptera$tratamento) #covar n varia com o trat
          
      anisoptera$tempocap1[1] <- NA #cook D near 0.6, should I withdraw? it increases shapiro value
      
          #seleção de modelos
          aniso_temcap1_lme <- 
            
          aniso_temcap1_lme_notrat <- 
          
         
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
                                   data = zygoptera, na.action = na.omit, REML = F)
      summary(zygo_temcap1_lme_int)
      
      Anova(zygo_temcap1_lme_int, type = "III") #no interaction
      
      zygo_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                               data = zygoptera, na.action = na.omit)
      summary(zygo_temcap1_lme)
      
      zygo_temcap1_lme_table <- Anova(zygo_temcap1_lme)
      zygo_temcap1_lme_table
      r.squaredGLMM(zygo_temcap1_lme)
      
      plot(zygo_temcap1_lme)
      shapiro.test(resid(zygo_temcap1_lme))
      
      leveneTest(zygoptera$tempocap1, center=mean, group = zygoptera$tratamento) #homocedasticidade
      
      sort(cooks.distance(zygo_temcap1_lme))
      plot(sort(cooks.distance(zygo_temcap1_lme)))
      
      cohen.d(d = zygoptera$tempocap1, f = zygoptera$tratamento, na.rm = T)
      
        #modelo sem tratamento
        zygo_temcap1_lme_notreat <- lmer(log(tempocap1) ~ log(biomassa_mg) + (1|bloco),
                                         data = zygoptera, na.action = na.omit)
        zygo_temcap1_lme_notreat_table <- Anova(zygo_temcap1_lme_notreat)
        zygo_temcap1_lme_notreat_table
        
        r.squaredGLMM(zygo_temcap1_lme_notreat)
        shapiro.test(resid(zygo_temcap1_lme_notreat))
        
      
      
      
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
          
          jpeg(filename = "temcap1_zygo.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          zygo_temcap1
          dev.off()
      
      #Notonectidae
      noto_temcap1_lme_int <- lmer(log(tempocap1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                       data = notonectidae, na.action = na.omit, REML = F)
      summary(noto_temcap1_lme_int) 
      Anova(noto_temcap1_lme_int, type = "III") #no interaction
          
      noto_temcap1_lme <- lmer(log(tempocap1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                   data = notonectidae, na.action = na.omit)
      summary(noto_temcap1_lme)
          
      noto_temcap1_lme_table <- Anova(noto_temcap1_lme)
      noto_temcap1_lme_table
      r.squaredGLMM(noto_temcap1_lme)
      
      plot(noto_temcap1_lme)
          
      shapiro.test(resid(noto_temcap1_lme))
          
      plot(sort(cooks.distance(noto_temcap1_lme)))
      
      leveneTest(notonectidae$tempocap1, center=mean, group = notonectidae$tratamento) #homocedasticidade
          
      notonectidae$tempocap1[13] <- NA #outlier removing
      notonectidae$tempocap1[17] <- NA
      notonectidae$tempocap1[11] <- NA
      notonectidae$tempocap1[14] <- NA
      
      cohen.d(d = notonectidae$tempocap1, f = notonectidae$tratamento, na.rm = T)
      
        #modelo sem tratamento
        noto_temcap1_lme_notrat <- lmer(log(tempocap1) ~ log(biomassa_mg)+ (1|bloco),
                                         data = notonectidae, na.action = na.omit)
        noto_temcap1_lme_notrat_table <- Anova(noto_temcap1_lme_semtrat)
        noto_temcap1_lme_notrat_table
        
        shapiro.test(resid(noto_temcap1_lme_notrat)) #vou deixar como efeito marginal e 
        r.squaredGLMM(noto_temcap1_lme_notrat)       #fazer com o trat mesmo
        
        sort(cooks.distance(noto_temcap1_lme_notrat))
        plot(sort(cooks.distance(noto_temcap1_lme_notrat)))
        
          
          #figure
          noto_temcap1 <- model_line_1line(notonectidae, log10(notonectidae$biomassa_mg),
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
  #Belostomatidae: All individuals captured only one prey
          
  #Notonectidae: Only 2 individuals captured more than one prey
          
  #Anisoptera
          aniso_diftemcap_lme_int <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                       data = anisoptera, na.action = na.omit, REML = F)
          summary(aniso_diftemcap_lme_int)
          Anova(aniso_diftemcap_lme_int, type = "III") #no inter
          
          aniso_diftemcap_lme <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                   data = anisoptera, na.action = na.omit)
          summary(aniso_diftemcap_lme)
          Anova(aniso_diftemcap_lme)
          
          r.squaredGLMM(aniso_diftemcap_lme)
          
          cohen.d(d = anisoptera$dif_temp_cap, f = anisoptera$tratamento, na.rm = T)
          
          #Diagnostics
          shapiro.test(resid(aniso_diftemcap_lme))
          plot(aniso_diftemcap_lme)
           
          plot(sort(cooks.distance(aniso_diftemcap_lme)))  # one outlier
          
          leveneTest(anisoptera$dif_temp_cap, center=mean, group = anisoptera$tratamento) #homocedasticidade
          
          anisoptera$dif_temp_cap[16] <- NA # outlier: cd >1.5
          
          #modelo sem tratamento
          aniso_diftemcap_lme_notrat <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg)+ 
                                               (1|bloco),
                                          data = anisoptera, na.action = na.omit)
          aniso_diftemcap_lme_notrat_table <- Anova(aniso_diftemcap_lme_notrat)
          aniso_diftemcap_lme_notrat_table
          
          shapiro.test(resid(aniso_diftemcap_lme_notrat))
          plot(sort(cooks.distance(aniso_diftemcap_lme_notrat)))
          
          r.squaredGLMM(aniso_diftemcap_lme_notrat)
          
          
          
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
              zygo_diftemcap_lme_int <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                          data = zygoptera, na.action = na.omit, REML = F)
              summary(zygo_diftemcap_lme_int)
              Anova(zygo_diftemcap_lme_int, type = "III") #no inter
              
              zygo_diftemcap_lme<- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                    data = zygoptera, na.action = na.omit)
              summary(zygo_diftemcap_lme)
              
              Anova(zygo_diftemcap_lme)
              
              r.squaredGLMM(zygo_diftemcap_lme)
              
              shapiro.test(resid(zygo_diftemcap_lme)) #normal
              plot(zygo_diftemcap_lme)
              
              cohen.d(d = anisoptera$dif_temp_cap, f = anisoptera$tratamento, na.rm = T)
              
              plot(sort(cooks.distance(zygo_diftemcap_lme))) #one outlier (>1)
              
              leveneTest(zygoptera$dif_temp_cap, center=mean, group = zygoptera$tratamento) #homocedasticidade
              
              
              #zygoptera$dif_temp_cap[2] <- NA #para caso de precisar
              
              #modelo sem tratamento
              zygo_diftemcap_lme_notrat <- lmer(logneg(dif_temp_cap) ~ log(biomassa_mg) +
                                                  (1|bloco),
                                                data = zygoptera, na.action = na.omit)
              zygo_diftemcap_lme_notrat_table <- Anova(zygo_diftemcap_lme_notrat)
              zygo_diftemcap_lme_notrat_table
              
              shapiro.test(resid(zygo_diftemcap_lme_notrat))
              
              r.squaredGLMM(zygo_diftemcap_lme_notrat)
              
              
              
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

#              
              
          
          
          
          
          
          
          

          

# Models of Proportion of Captured Prey -----------------------------------
  #Belostomatidae
    belo_prop_bnlm <- bnlr(y = cbind(belostomatidae$presas_consumidas_gravacao,
                                     3-belostomatidae$presas_consumidas_gravacao), 
                           mu = ~belostomatidae$tratamento, pmu = c(0,1))
    Anova(belo_prop_bnlm)
                
    
    gnlmm(llik = )
                
                
                
                
                
                

                
# Models of Handling Time -------------------------------------------------
  #Belostomatidae: no measures
                
  #Anisoptera
    aniso_temp_lme_int <- lmer(log(tempomanip1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                      data = anisoptera, na.action = na.omit, REML = F)
                summary(aniso_temp_lme_int)  #sem interação
                
                Anova(aniso_temp_lme_int, type = "III")
                
                aniso_temp_lme <- lmer(log(tempomanip1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                      data = anisoptera, na.action = na.omit)
                summary(aniso_temp_lme)
                
                Anova(aniso_temp_lme, type = "II")
                
                r.squaredGLMM(aniso_temp_lme)
                
                shapiro.test(resid(aniso_temp_lme))
                
                plot(sort(cooks.distance(aniso_temp_lme)))
                
                cohen.d(d = anisoptera$tempomanip1, f = anisoptera$tratamento, na.rm = T)
                
                leveneTest(anisoptera$tempomanip1, center=mean, group = anisoptera$tratamento) #homocedasticidade
                
                
                #modelo sem tratamento
                aniso_temp_lme_notrat <- lmer(log(tempomanip1) ~ log(biomassa_mg) +
                                                (1|bloco),
                                              data = anisoptera, na.action = na.omit)
                Anova(aniso_temp_lme_notrat)
                
                shapiro.test(resid(aniso_temp_lme_notrat))
                plot(sort(cooks.distance(aniso_temp_lme)))
                
                r.squaredGLMM(aniso_temp_lme_notrat)
                
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
     zygo_temp_lme_int <- lmer(log(tempomanip1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                              data = zygoptera, na.action = na.omit, REML = F)
                    summary(zygo_temp_lme_int) 
                    
                    Anova(zygo_temp_lme_int, type = "III") #no inter
                    
      zygo_temp_lme <- lmer(log(tempomanip1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                            data = zygoptera, na.action = na.omit, REML = F)
      
                    Anova(zygo_temp_lme)
                    
                    r.squaredGLMM(zygo_temp_lme)
                    
                    plot(zygo_temp_lme)
                    shapiro.test(resid(zygo_temp_lme))
                    
                    plot(sort(cooks.distance(zygo_temp_lme)))
                    
                    leveneTest(zygoptera$tempomanip1, center=mean, group = zygoptera$tratamento) #homocedasticidade
                    
                    cohen.d(d = zygoptera$tempomanip1, f = zygoptera$tratamento, na.rm = T)
                    
                  #modelo sem tratamento
                    zygo_temp_lme_notrat <- lmer(log(tempomanip1) ~ log(biomassa_mg)+ (1|bloco),
                                          data = zygoptera, na.action = na.omit, REML = F)
                    Anova(zygo_temp_lme_notrat)
                    
                    shapiro.test(resid(zygo_temp_lme_notrat))
                    plot(sort(cooks.distance(zygo_temp_lme_notrat)))
                    
                    r.squaredGLMM(zygo_temp_lme_notrat)
                    
                    
                    
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
    noto_temp_lme_int <- lmer(log(tempomanip1) ~ log(biomassa_mg)*tratamento + (1|bloco),
                             data = notonectidae, na.action = na.omit, REML = F)
                        summary(noto_temp_lme_int)
                        
                        Anova(noto_temp_lme_int, type = "III")#no inter
                        
    noto_temp_lme <- lmer(log(tempomanip1) ~ log(biomassa_mg) + tratamento + (1|bloco),
                         data = notonectidae,na.action = na.omit, REML = F)
                        
                        summary(noto_temp_lme)
                        
                        Anova(noto_temp_lme)
                        
                        r.squaredGLMM(noto_temp_lme)
                        
                        plot(noto_temp_lme)
                        
                        shapiro.test(resid(noto_temp_lme))
                        
                        leveneTest(notonectidae$tempomanip1, center=mean, group = notonectidae$tratamento) #homocedasticidade
                        
                        plot(sort(cooks.distance(noto_temp_lme))) #one point ~0.89
                        
                        notonectidae$tempomanip1[14] <- NA #precisa no sem trat
                        notonectidae$tempomanip1[29] <- NA
                        
                        cohen.d(d = notonectidae$tempomanip1, f = notonectidae$tratamento, na.rm = T)
                        
                #modelo sem tratamento - dexa quieto
                    noto_temp_lme_notrat <- lmer(log(tempomanip1) ~ log(biomassa_mg) + (1|bloco),
                                              data = notonectidae,na.action = na.omit, REML = F)
                    Anova(noto_temp_lme_notrat)
                    
                    shapiro.test(resid(noto_temp_lme_notrat))
                    
                    plot(sort(cooks.distance(noto_temp_lme_notrat)))
                    
                    
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
                        
                        
                



# Models of Difference between handling times (T2-T1) ---------------------
  #Belostomatidae: no second capture
  #Notonectidae: Only 2 captured more than one prey
                          
  #Anisoptera
      aniso_diftemp_lme_int <- lmer(dif_temp_manip ~ biomassa_mg*tratamento +
                                      (1|bloco), data = anisoptera, na.action = na.omit, REML = F)
      aniso_diftemp_lme_int                    
                          
      summary(aniso_diftemp_lme_int) 
      Anova(aniso_diftemp_lme_int, "III")
      
      aniso_diftemp_lme <- lmer(dif_temp_manip ~ biomassa_mg + tratamento +
                                  (1|bloco), data = anisoptera, na.action = na.omit, REML = F)
      summary(aniso_diftemp_lme)
      Anova(aniso_diftemp_lme)
      
      r.squaredGLMM(aniso_diftemp_lme)
                          
      plot(aniso_diftemp_lme)
      shapiro.test(resid(aniso_diftemp_lme))
      
      plot(sort(cooks.distance(aniso_diftemp_lme)))
      
        #Figure
            aniso_diftemp <- model_line_semlog(anisoptera, anisoptera$biomassa_mg,
                                          anisoptera$dif_temp_manip, 
                                          "Difference between Handling Times [s]",
                                          noto_temcap1_lme, "Anisoptera")+
              scale_x_continuous() +
              scale_y_continuous()+
              geom_hline(yintercept = 0, linetype =3)
            aniso_diftemp 
            #saving
            jpeg(filename = "diftemp_aniso.jpg", width = 2350, height = 1900, 
                 units = "px", pointsize = 12, quality = 100,
                 bg = "white",  res = 300)
            aniso_diftemp
            dev.off()
                         
                          
  #Zygoptera
      zygo_diftemp_lme_int <- lmer(logneg(dif_temp_manip) ~ log(biomassa_mg)*tratamento +
                                     (1|bloco), data = zygoptera, na.action = na.omit, REML = F)
      summary(zygo_diftemp_lme_int)
      
      Anova(zygo_diftemp_lme_int, "III")
      
      zygo_diftemp_lme <- lmer(logneg(dif_temp_manip) ~ log(biomassa_mg) + tratamento +
                                     (1|bloco), data = zygoptera, na.action = na.omit)
      summary(zygo_diftemp_lme)
      Anova(zygo_diftemp_lme)
      
      r.squaredGLMM(zygo_diftemp_lme)
      
      shapiro.test(resid(zygo_diftemp_lme))
      plot(sort(cooks.distance(zygo_diftemp_lme)))
      
        #Figure
            zygo_diftemp <- model_line(zygoptera, log10(zygoptera$biomassa_mg),
                                       log10neg(zygoptera$dif_temp_manip), 
                                        "Difference between Handling Times [s]\n log10 scale",
                                        noto_temcap1_lme, "Zygoptera")+
              scale_x_continuous(breaks = c(0.7 ,1, 1.3, 1.477),
                                 labels = c(5, 10, 20, 30)) +
              scale_y_continuous(breaks = c(-2, -1, 0, 1, 2),
                                 labels = c(-100, -10, 0, 10, 100))+
              geom_hline(yintercept = 0, linetype =3)+ annotation_logticks()
            zygo_diftemp
            #saving
            jpeg(filename = "diftemp_zygo.jpg", width = 2350, height = 1900, 
                 units = "px", pointsize = 12, quality = 100,
                 bg = "white",  res = 300)
            zygo_diftemp
            dev.off()
 

# Models of Consumption of First Day --------------------------------------


                
                

# Models of Total Consumption ----------------------------------------------
    #Belostomatidae
            belo_pres_lme_int <- lmer(Totalpresascorrigido ~ log(biomassa_mg)*tratamento +
                                       (1|bloco), data = belostomatidae, na.action = na.omit, REML = F)
            summary(belo_pres_lme_int) 
            
            Anova(belo_pres_lme_int, type = "III") #no inter
            
            belo_pres_lme <-  lmer(Totalpresascorrigido ~ log(biomassa_mg) + tratamento + 
                                    (1|bloco), data = belostomatidae, na.action = na.omit, REML = F) 
            summary(belo_pres_lme)
            
            Anova(belo_pres_lme)
            
            r.squaredGLMM(belo_pres_lme)
            
            shapiro.test(resid(belo_pres_lme))
            
            leveneTest(belostomatidae$Totalpresascorrigido, center=mean, group = belostomatidae$tratamento) #homocedasticidade #opa, esse ai nao deu
            
            plot(belo_pres_lme)
            
            plot(sort(cooks.distance(belo_pres_lme))) #sem outliers
            
            cohen.d(d = belostomatidae$Totalpresascorrigido, f = belostomatidae$tratamento, na.rm = T)
            
            #figura
            belo_pres <- model_line(belostomatidae, log10(belostomatidae$biomassa_mg),
                                    belostomatidae$Totalpresascorrigido,
                                    "N° prey consumed/day", belo_pres_lme_int, "Belostomatidae")+
              theme(legend.position = c(0.8, 0.8)) + geom_hline(yintercept = 0, linetype = 3)+
              scale_x_continuous(breaks = c(1, 1.301, 1.7, 2, 2.477),
                                 labels = c(10, 20, 50, 100, 300))+ annotation_logticks()
            belo_pres
            
            jpeg(filename = "pres_belo.jpg", width = 2350, height = 1900, 
                 units = "px", pointsize = 12, quality = 100,
                 bg = "white",  res = 300)
            belo_pres
            dev.off()
        
            
            
        #Anisoptera
            aniso_pres_lme_int <- lmer(log10(Totalpresascorrigido) ~ log10(biomassa_mg)*tratamento + (1|bloco),
                                       data = anisoptera, na.action = na.omit, REML = F)
            summary(aniso_pres_lme_int)
            
            Anova(aniso_pres_lme_int, type = "III") #com inter
            
            r.squaredGLMM(aniso_pres_lme_int)
            
            plot(aniso_pres_lme_int)
            shapiro.test(resid(aniso_pres_lme_int))
            
            leveneTest(anisoptera$Totalpresascorrigido, center=mean, group = anisoptera$tratamento) #homocedasticidade
            
            plot(sort(cooks.distance(aniso_pres_lme_int)))
            
            aniso_pres_lme <- lmer(Totalpresascorrigido ~ log(biomassa_mg) + tratamento + (1|bloco),
                                  data = anisoptera, na.action = na.omit, REML = F)
            
            summary(aniso_pres_lme)
            
            Anova(aniso_pres_lme)
            
            plot(aniso_pres_lme)
            
            shapiro.test(resid(aniso_pres_lme))
            
            plot(sort(cooks.distance(aniso_pres_lme)))
            
            anisoptera$Totalpresascorrigido[28] <- NA #outlier: ~1
            anisoptera$Totalpresascorrigido[30] <- NA # >0.5 só com ele fica marginalmente normal
            
            cohen.d(d = anisoptera$Totalpresascorrigido, f = anisoptera$tratamento, na.rm = T)
            
            #Figura
            aniso_pres <- model_line(anisoptera, log10(anisoptera$biomassa_mg), (anisoptera$Totalpresascorrigido),
                                     "N° prey consumed/day",
                                     aniso_pres_lme_int, "Anisoptera") + annotation_logticks()+
              scale_x_continuous(breaks = c(0.698, 1, 1.301, 1.698, 2),
                                 labels = c(5, 10, 20, 50, 100))
            aniso_pres
            
            jpeg(filename = "pres_aniso.jpg", width = 2350, height = 1900, 
                 units = "px", pointsize = 12, quality = 100,
                 bg = "white",  res = 300)
            aniso_pres
            dev.off()
            
        #Zygoptera
            zygo_pres_lme_int <- lmer(Totalpresascorrigido ~ biomassa_mg*tratamento + (1|bloco),
                                      data = zygoptera, na.action = na.omit, REML = F)
            summary(zygo_pres_lme_int)
            
            Anova(zygo_pres_lme_int, type = "III") #tem inter sem logs
            
            r.squaredGLMM(zygo_pres_lme_int)
            
            plot(sort(cooks.distance(zygo_pres_lme_int)))
            shapiro.test(resid(zygo_pres_lme_int))
            
            leveneTest(zygoptera$Totalpresascorrigido, center=mean, group = zygoptera$tratamento) #homocedasticidade
            
            zygo_pres_lme <- lmer(Totalpresascorrigido ~ log(biomassa_mg) + tratamento + (1|bloco),
                                 data = zygoptera, na.action = na.omit, REML = F)
            
            summary(zygo_pres_lme)
            
            Anova(zygo_pres_lme, type = "II")
            
            shapiro.test(resid(zygo_pres_lme))
            
            cohen.d(d = zygoptera$Totalpresascorrigido, f = zygoptera$tratamento, na.rm = T)
            
            #Figura
            zygo_pres <-  model_line_semlog(zygoptera, zygoptera$biomassa_mg, (zygoptera$Totalpresascorrigido+1),
                                     "N° prey consumed/day", zygo_pres_lme,
                                     "Zygoptera")
            zygo_pres
            
            jpeg(filename = "pres_zygo.jpg", width = 2350, height = 1900, 
                 units = "px", pointsize = 12, quality = 100,
                 bg = "white",  res = 300)
            zygo_pres
            dev.off()
            
            
            
            #Notonectidae
            noto_pres_lme_int <- lmer(Totalpresascorrigido ~ log(biomassa_mg)*tratamento + 
                                        (1|bloco), data = notonectidae,
                                      na.action = na.omit, REML = F)
            summary(noto_pres_lme_int)
            
            Anova(noto_pres_lme_int, type = "III") #inter nao signi
            
            noto_pres_lme <- lmer(Totalpresascorrigido ~ log(biomassa_mg) + tratamento + 
                                    (1|bloco), data = notonectidae, na.action = na.omit, REML = F)
            
            summary(noto_pres_lme)
            
            Anova(noto_pres_lme, type = "II")
            
            r.squaredGLMM(noto_pres_lme)
            
            shapiro.test(resid(noto_pres_lme))
            
            leveneTest(notonectidae$Totalpresascorrigido, center=mean, group = notonectidae$tratamento) #homocedasticidade
            
            plot(sort(cooks.distance(noto_pres_lme)))
            
            notonectidae$Totalpresascorrigido[16] <- NA #outlier > 1.5
            notonectidae$Totalpresascorrigido[8] <- NA #não djanta, só tirando que normaliza
            
            cohen.d(d = notonectidae$Totalpresascorrigido, f = notonectidae$tratamento, na.rm = T)
            
            #modelo sem trat
            noto_pres_lme_notrat <- lmer(Totalpresascorrigido ~ log(biomassa_mg) + 
                                    (1|bloco), data = notonectidae, na.action = na.omit, REML = F)
            Anova(noto_pres_lme_notrat)
            
            #Figura
            noto_pres <- model_line_noline(notonectidae, log10(notonectidae$biomassa_mg),
                                    notonectidae$Totalpresascorrigido,
                                    "N° prey consumed/day", noto_pres_lme, "Notonectidae")+
              scale_x_continuous(breaks = c(0.845, 1, 1.176, 1.301),
                                 labels = c(7, 10, 15, 20), limits = c(0.8,1.35)) +
              scale_y_continuous(breaks = c(0.8, 1, 1.25, 1.5, 1.75, 2),
                                 labels = c(0.8, 1, 1.25, 1.5, 1.75, 2))
            noto_pres
            
            jpeg(filename = "pres_noto.jpg", width = 2350, height = 1900, 
                 units = "px", pointsize = 12, quality = 100,
                 bg = "white",  res = 300)
            noto_pres
            dev.off()
            

                
                

# Models of Growth rate ---------------------------------------------------
  #Belostomatidae
      belo_cresc_lme_int <- lmer(log(taxacrescimento) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                 data = belostomatidae, na.action = na.omit, REML = F)
      summary(belo_cresc_lme_int)
      
      Anova(belo_cresc_lme_int, "III")
      
      r.squaredGLMM(belo_cresc_lme_int)
      
      shapiro.test(resid(belo_cresc_lme_int)) #de bom tamanho já
      plot(sort(cooks.distance(belo_cresc_lme_int))) 
      
      leveneTest(log(belostomatidae$taxacrescimento), center=mean, group = belostomatidae$tratamento) #homocedasticidade
      
      belo_cresc_lme <- lmer(taxacrescimento ~ biomassa_mg + tratamento + (1|bloco),
                             data = belostomatidae, na.action = na.omit, REML = F)
      summary(belo_cresc_lme)
      Anova(belo_cresc_lme)
      
      shapiro.test(resid(belo_cresc_lme))
      plot(sort(cooks.distance(belo_cresc_lme)))
      
      cohen.d(d = belostomatidae$taxacrescimento, f = belostomatidae$tratamento, na.rm = T)
      
      #Figure
      belo_cresc <-  model_line(belostomatidae, log10(belostomatidae$biomassa_mg), 
                                log10(belostomatidae$taxacrescimento),
                                ynome = "Growth rate, log10 scale", 
                                model = belo_cresc_lme_int,
                                title = "Belostomatidae") + annotation_logticks() +
        geom_hline(yintercept = 0, linetype = 3)
      belo_cresc
        #saving
        jpeg(filename = "growth_belos.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        belo_cresc
        dev.off()

  #Anisoptera
        aniso_cresc_lme_int <- lmer(taxacrescimento ~ log(biomassa_mg)*tratamento + (1|bloco),
                                    data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_cresc_lme_int)
        
        Anova(aniso_cresc_lme_int, "III")
        
        aniso_cresc_lme <- lmer(log(taxacrescimento) ~ biomassa_mg + tratamento + (1|bloco),
                                    data = anisoptera, na.action = na.omit, REML = F)
        summary(aniso_cresc_lme)
        Anova(aniso_cresc_lme)
        
        r.squaredGLMM(aniso_cresc_lme)
        
        shapiro.test(resid(aniso_cresc_lme))
        
        leveneTest(anisoptera$taxacrescimento, center=mean, group = anisoptera$tratamento) #homocedasticidade
        
        plot(sort(cooks.distance(aniso_cresc_lme)))

        anisoptera$taxacrescimento[4] <- NA
        anisoptera$taxacrescimento[16] <- NA
        
        cohen.d(d = anisoptera$taxacrescimento, f = anisoptera$tratamento, na.rm = T)
        
        #modelo sem tratamento
          aniso_cresc_lme_notrat <- lmer(log(taxacrescimento) ~ biomassa_mg + (1|bloco),
                                         data = anisoptera, na.action = na.omit, REML = F)
          Anova(aniso_cresc_lme_notrat)
          
          shapiro.test(resid(aniso_cresc_lme_notrat))
          plot(sort(cooks.distance(aniso_cresc_lme_notrat)))
          
          r.squaredGLMM(aniso_cresc_lme_notrat)
        
        
        #Figure
        aniso_cresc <-  model_line_semlog_1line(anisoptera, anisoptera$biomassa_mg, 
                                  log10(anisoptera$taxacrescimento),
                                  ynome = "Growth rate [proportion] \n log10 scale", 
                                  model = belo_cresc_lme_int,
                                  title = "Anisoptera")+
          scale_y_continuous(breaks = c(-0.0044, 0, 0.0043, 0.0086, 0.0128, 0.017, 0.0212, 0.0253),
                             labels = c(0.99, 1, 1.01, 1.02, 1.03, 1.04, 1.05, 1.06)) +
          geom_hline(yintercept = 0, linetype = 3)
        aniso_cresc

          #saving
          jpeg(filename = "growth_aniso.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          aniso_cresc
          dev.off()
          
  #Zygoptera
      zygo_cresc_lme_int <- lmer(log(taxacrescimento) ~ log(biomassa_mg)*tratamento + (1|bloco),
                                 data = zygoptera, na.action = na.omit, REML = F)
      summary(zygo_cresc_lme_int)
      
      Anova(zygo_cresc_lme_int, "III")
      
      zygo_cresc_lme <- lmer(log(taxacrescimento) ~ log(biomassa_mg) + tratamento + (1|bloco),
                                 data = zygoptera, na.action = na.omit, REML = F)
      summary(zygo_cresc_lme)
      Anova(zygo_cresc_lme)
      
      r.squaredGLMM(zygo_cresc_lme)
      
      plot(zygo_cresc_lme)
      
      shapiro.test(resid(zygo_cresc_lme))
      
      leveneTest(zygoptera$taxacrescimento, center=mean, group = zygoptera$tratamento) #homocedasticidade
      
      plot(sort(cooks.distance(zygo_cresc_lme)))
      
      zygoptera$taxacrescimento[24] <- NA #outlier > 0.8
      
      cohen.d(d = zygoptera$taxacrescimento, f = zygoptera$tratamento, na.rm = T)
      
      #modelo sem tratamento
      zygo_cresc_lme_notrat <- lmer(log(taxacrescimento) ~ log(biomassa_mg) +
                                      (1|bloco),
                             data = zygoptera, na.action = na.omit, REML = F)
      Anova(zygo_cresc_lme_notrat)
      
      shapiro.test(resid(zygo_cresc_lme_notrat))
      
      plot(sort(cooks.distance(zygo_cresc_lme_notrat)))
      
      r.squaredGLMM(zygo_cresc_lme_notrat)
      
      
      
      #Figure
          zygo_cresc <-  model_line_1line(zygoptera, log10(zygoptera$biomassa_mg), 
                                            log10(zygoptera$taxacrescimento),
                                            ynome = "Growth rate [proportion] \n log10 scale", 
                                            model = zygo_cresc_lme,
                                            title = "Zygoptera")+
            scale_x_continuous(breaks = c(0.778, 1, 1.301, 1.477),
                               labels = c(6, 10, 20, 30))+
            scale_y_continuous(breaks = c(-0.0223,-0.0088, 0, 0.0086, 0.0212, 0.0334, 0.0414),
                               labels = c(0.95, 0.98, 1, 1.02, 1.05, 1.08, 1.10)) +
            annotation_logticks()+
            geom_hline(yintercept = 0, linetype = 3)
          zygo_cresc     
            
          #saving
          jpeg(filename = "growth_zygo.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          zygo_cresc
          dev.off()  

        
  #Notonectidae
      noto_cresc_lme_int <- lmer(taxacrescimento ~ log(biomassa_mg)*tratamento + (1|bloco),
                                 data = zygoptera, na.action = na.omit, REML = F)
      summary(noto_cresc_lme_int)
      
      Anova(noto_cresc_lme_int, "III")
      
      noto_cresc_lme <- lmer(taxacrescimento ~ log(biomassa_mg) + tratamento + (1|bloco),
                             data = zygoptera, na.action = na.omit)
      
      summary(noto_cresc_lme)
      Anova(noto_cresc_lme)
      
      r.squaredGLMM(noto_cresc_lme)
      
      plot(noto_cresc_lme)
      shapiro.test(resid(noto_cresc_lme))
      
      leveneTest(notonectidae$taxacrescimento, center=mean, group = notonectidae$tratamento) #homocedasticidade
      
      plot(sort(cooks.distance(noto_cresc_lme)))
      
      cohen.d(d = notonectidae$taxacrescimento, f = notonectidae$tratamento, na.rm = T)
      
      #modelo sem tratamento
      noto_cresc_lme_notrat <- lmer(taxacrescimento ~ log(biomassa_mg) + (1|bloco),
                             data = zygoptera, na.action = na.omit, REML = F)
      
      Anova(noto_cresc_lme_notrat)
      
      r.squaredGLMM(noto_cresc_lme_notrat)
      
      shapiro.test(resid(noto_cresc_lme_notrat))
      
      plot(sort(cooks.distance(noto_cresc_lme_notrat)))
      
      #Figure
        noto_cresc <-  model_line_1line(notonectidae, log10(notonectidae$biomassa_mg), 
                                  (notonectidae$taxacrescimento),
                                  ynome = "Growth rate [proportion]", 
                                  model = noto_cresc_lme,
                                  title = "Notonectidae")+
          scale_x_continuous(breaks = c(0.698, 1, 1.301, 1.477, 1.602),
                             labels = c(5, 10, 20, 30, 40))+
          scale_y_continuous() +
          annotation_logticks()
        noto_cresc 
         
        #saving
        jpeg(filename = "growth_noto.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        noto_cresc
        dev.off()   
            

      
      
      
      
      
      
      
      
      
                