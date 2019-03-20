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

  #Importing 
    geral <- read.table("planilhageral_atualizada2.txt", header = T, colClasses = c(
      "factor", "factor","factor","factor","character", "numeric", "numeric","numeric",
      "numeric","numeric","numeric","factor", "numeric","numeric","numeric","numeric",
      "numeric", "logical", "integer", "integer", "numeric","numeric","numeric","numeric",
      "numeric","numeric", "numeric","numeric","numeric"
    )) blabla
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
      

# Models of First Capture Time --------------------------------------------
      #Belostomatidae 
      belo_temcap1_lme_int <- lmer(tempocap1 ~ log(biomassa_mg)*tratamento + (1|bloco),
                                   data = belostomatidae, na.action = na.omit)
      summary(belo_temcap1_lme_int) #com interação
      belo_temcap1_table <- Anova(belo_temcap1_lme_int, type = "III")
      belo_temcap1_table
      
      shapiro.test(resid(belo_temcap1_lme_int)) 
        #Figura
          model_line(belostomatidae, belostomatidae$biomassa_mg, belostomatidae$tempocap1, 
                     "Time of first capture [s], log10 scale", belo_temcap1_lme, "Belostomatidae") +
              geom_hline(yintercept = 0, linetype = 2) 
      
      
          #Notonectidae
      noto_temcap1_lme_int <- lme(log(tempocap1) ~ log(biomassa_mg)*tratamento,
                                  random = ~1|bloco, data = notonectidae, na.action = na.omit,
                                  weights = varIdent(form = ~ 1 | tratamento))
      summary(noto_temcap1_lme_int) #sem inter
      Anova(noto_temcap1_lme_int, type = "III")
      
      noto_temcap1_lme <- lme(log(tempocap1) ~ log(biomassa_mg) + tratamento,
                              random = ~1|bloco, data = notonectidae, na.action = na.omit,
                              weights = varIdent(form = ~ 1 | tratamento))
      summary(noto_temcap1_lme)
      Anova(noto_temcap1_lme)
      plot(noto_temcap1_lme)
      shapiro.test(resid(noto_temcap1_lme))
      plot(lm(log(notonectidae$tempocap1)~log(notonectidae$biomassa_mg)+notonectidae$tratamento))
      
      notonectidae$tempocap1[13] <- NA
      
      #figura
      noto_temcap1 <- model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$tempocap1, 
                                 "Time of first capture [s], log10 scale", noto_temcap1_lme, "Notonectidae")+
        geom_hline(yintercept = 0, linetype = 2)+
        annotation_logticks() + theme(legend.position = c(0.8, 0.8))
      noto_temcap1
      
      jpeg(filename = "temcap1_noto.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      noto_temcap1
      dev.off()
      
      #Anisoptera
      aniso_temcap1_lme_int <- lme(log(tempocap1) ~ log(biomassa_mg)*tratamento,
                                   random = ~1|bloco, data = anisoptera, na.action = na.omit,
                                   weights = varIdent(form = ~ 1 | tratamento))
      summary(aniso_temcap1_lme_int)
      Anova(aniso_temcap1_lme_int, type = "III") #tem inter
      
      plot(aniso_temcap1_lme_int)
      plot(lm(log(anisoptera$tempocap1)~log(anisoptera$biomassa_mg)+anisoptera$tratamento))
      shapiro.test(resid(aniso_temcap1_lme_int))
      
      #Figura
      aniso_temcap1 <- model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$tempocap1, 
                                  "Time of first capture [s], log10 scale", aniso_temcap1_lme, "Anisoptera")+
        geom_hline(yintercept = 0, linetype = 2)+
        annotation_logticks() + theme(legend.position = c(0.8, 0.8))
      aniso_temcap1
      
      jpeg(filename = "temcap1_aniso.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      aniso_temcap1
      dev.off()
      
      #Zygoptera
      zygo_temcap1_lme_int <- lme(log(tempocap1) ~ log(biomassa_mg)*tratamento,
                                  random = ~1|bloco, data = zygoptera, na.action = na.omit,
                                  weights = varIdent(form = ~ 1 | tratamento))
      summary(zygo_temcap1_lme_int)
      
      Anova(zygo_temcap1_lme_int, type = "III")
      
      zygo_temcap1_lme <- lme(log(tempocap1) ~ log(biomassa_mg) + tratamento,
                              random = ~1|bloco, data = zygoptera, na.action = na.omit,
                              weights = varIdent(form = ~ 1 | tratamento))
      summary(zygo_temcap1_lme)
      Anova(zygo_temcap1_lme)
      plot(zygo_temcap1_lme)
      shapiro.test(resid(zygo_temcap1_lme))
      plot(lm(log(zygoptera$tempocap1)~log(zygoptera$biomassa_mg)+zygoptera$tratamento))
      
      #Figura
      zygo_temcap1 <- model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$tempocap1, 
                                 "Time of first capture [s], log10 scale", zygo_temcap1_lme, "Zygoptera")+
        geom_hline(yintercept = 0, linetype = 2)+
        annotation_logticks() + theme(legend.position = c(0.8, 0.8))
      zygo_temcap1
      
      jpeg(filename = "temcap1_zygo.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      zygo_temcap1
      dev.off()

      
      
      
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    