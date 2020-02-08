####Pacotes a carregar####
library(ggplot2) #Gráficos
library(dplyr)   #to tidy data
library(ggpubr)  #extra functions to ggplot2
library(lme4)    #Many types of models - lme, glm, glmm, etc
library(stargazer) #To make tables
library(nlme)  #Fit and compare Gaussian linear and nonlinear mixed-effects models
library(car) #Statistic functions like Anova
library(MASS) #transformations and statistical functions
library(knitr) #tables
library(predictmeans) #functions for diagnostics of models
library(MuMIn) #R²m and R²c
library(AICcmodavg) #tables of model selection
library(effsize) #effect size
library(PairedData) #trabalhar com dados pareados

####Dados####
  geral <- read.table("planilhageral_atualizada_new.limpa.txt", header = T, colClasses = c(
    "factor", "factor","factor","factor","character", "factor", "numeric", "numeric",
    "numeric","numeric",
    "numeric","numeric","numeric","factor", "numeric","numeric","numeric","numeric",
    "numeric", "logical", "integer", "integer", "numeric","numeric","numeric","numeric",
    "numeric","numeric", "numeric","numeric","numeric"
  ))
  str(geral)

  #calculo de variáveis e mudanças na tabela geral
    geral <- geral %>% mutate(biomassa_mg = biomassant*1000, #gr para mg
                              taxacrescimento = 
                                (((varbiom+biomassant)/biomassant)^(1/sobrev)),
                              tempocap1 = ifelse(test = is.na(tempocap1),
                                                 yes = 5400, #antes tinha marcado quem não pegou durante a gravação como NA
                                                 no = tempocap1),
                              tempocap2 = ifelse(test = tempocap1==5400, #5400 segundos -> 1 h 30 min gravação
                                                 yes = NA, #caso o anterior não capturou, não faz sentido ter a 2a
                                                 no = ifelse(test = is.na(tempocap2), 
                                                             yes = 5400-tempocap1, #caso tenha pego o primeiro
                                                             no = tempocap2)),
                              dif_temp_cap = ifelse(test = is.na(tempocap2),
                                                    yes = NA,
                                                    no = tempocap2 - tempocap1))
geral <- geral %>% filter(suborfam == "Anisoptera" |#excluindo os adultos de Belostomatidae
                          suborfam =="Zygoptera" | #para apenas trabalhar com ninfas
                          suborfam =="Notonectidae" | 
                          (suborfam =="Belostomatidae" & biomassa_mg < 100))
geral <- geral[-c(10, 56),] #dado excluido por problemas na medição
View(geral)

  #Deixando apenas o essencial
    geral_arrumado <- geral %>% dplyr::select(suborfam, bloco, amostra, number, tratamento,
                                   presa_mean, tempocap1, tempocap2,
                                   compr, larg, biomassa_mg,
                                   fezmuda, sobrev, presas_consumidas_gravacao,
                                   Totalpresascorrigido, tempomanip1, tempomanip2,
                                   taxacrescimento, dif_temp_cap)
   
  #Dividindo pelo tratamento para facilitar para alinhar os pares depois
    geral_am <- geral_arrumado %>%  filter(tratamento == "Ambiente")
    geral_aq <- geral_arrumado %>%  filter(tratamento == "Aquecido")  
     
  #planilha de efeito do aquecimento (diferença das variáveis)
    efeito_aq <- left_join(x = geral_am, y = geral_aq, by = "number", #
                           suffix = c(".am", ".aq")) %>% mutate(suborfam = suborfam.aq, bloco = bloco.aq,
                                                                par = number,
                                                                dif_compr = compr.aq-compr.am, dif_larg = larg.aq-larg.am,
                                                                dif_biomass = biomassa_mg.aq - biomassa_mg.am,
                                                                ef_temcap1 = tempocap1.aq-tempocap1.am,
                                                                ef_temcap2 = tempocap2.aq-tempocap2.am,
                                                                ef_sobrev = sobrev.aq-sobrev.am,
                                                                ef_cons_grav = presas_consumidas_gravacao.aq-presas_consumidas_gravacao.am,
                                                                ef_cons_total = Totalpresascorrigido.aq-Totalpresascorrigido.am,
                                                                ef_tempomanip1 = tempomanip1.aq-tempomanip1.am,
                                                                ef_tempomanip2 = tempomanip2.aq-tempomanip2.am,
                                                                ef_growth_rate = taxacrescimento.aq-taxacrescimento.am,
                                                                ef_satiety = dif_temp_cap.aq-dif_temp_cap.am,
                                                                biom_mean = ((biomassa_mg.aq +biomassa_mg.am)/2)
                           )
    
    #Dividindo por grupo taxonomico
      belostomatidae_ef <- filter(efeito_aq, suborfam == "Belostomatidae")
      notonectidae_ef <-  filter(efeito_aq, suborfam == "Notonectidae")
      anisoptera_ef <-  filter(efeito_aq, suborfam == "Anisoptera")
      zygoptera_ef <-  filter(efeito_aq, suborfam == "Zygoptera")
    

#### Análise dos dados - [pareada apenas e com modelos lme] ####
####Exploração geral dos dados ####
  # Diferença de biomassa entre os pares
      
      efeito_aq %>% group_by(suborfam) %>% 
        summarise(pares = n(), avg = mean((dif_biomass)), desvio = sd(dif_biomass),
                  IC_min_95 = avg - desvio, IC_max_95 = avg+desvio)
    #Visualmente
      efeito_aq %>% group_by(suborfam) %>%
        summarise(avg = mean((dif_biomass)), desvio = sd(dif_biomass), 
                  IC_min_95 = (avg - 1.96*desvio), IC_max_95 = (avg+1.96*desvio)) %>% 
        ggplot(aes(x = suborfam, y = avg, fill = suborfam, colour = suborfam))+
        geom_errorbar(aes(ymin = IC_min_95, ymax = IC_max_95), size = 2,
                      linetype = 1 ,show.legend = F)+ #intervalo de confiança de 95%
        geom_point(size = 15, shape = 21, stroke = 3, colour = "black")+
        geom_hline(yintercept = 0) +
        theme_classic(base_size = 24) +
        ylab("Diferença de Biomassa do Par [mg]") + xlab("") +
        scale_fill_discrete(name = "Grupo Taxonomico")
      
    #Estatisticamente
      ggqqplot(efeito_aq$dif_biomass) + facet_wrap(~efeito_aq$suborfam) #qqplots
      
      #normalidade
      efeito_aq %>% group_by(suborfam) %>% summarise(shapiro_W = shapiro.test(dif_biomass)[[1]][1],
                                                     shapiro_p = shapiro.test(dif_biomass)[[2]][1],
                                                     is.normal = ifelse(test = shapiro_p >= 0.05,
                                                                     yes = "Yes",
                                                                     no = "No"),
                                                     teste.recom =
                                                       ifelse(test = is.normal == "Yes",
                                                              yes = "T.test",
                                                              no = "Wilcoxon"))
        #testando se a diferença é estatisticamente igual a 0
          efeito_aq %>% group_by(suborfam) %>% 
            summarise(pares = n(), is.normal = shapiro.test(dif_biomass)[[2]][1] >= 0.05,
                      t_v = ifelse(is.normal == T,
                                     yes = t.test(dif_biomass)[[1]][1],
                                     no = wilcox.test(dif_biomass)[[1]][1]),
                      df = ifelse(is.normal == T,
                                  yes = t.test(dif_biomass)[[2]][1],
                                  no = NA),
                      p_value = ifelse(is.normal == T, #o warning é pq tem dois valores coincidentemente iguais
                                       yes = t.test(dif_biomass)[[3]][1], #já checado
                                       no = wilcox.test(dif_biomass)[[3]][1]),
                      p_less = ifelse(is.normal == T, #o warning é pq tem dois valores coincidentemente iguais
                                      yes = t.test(dif_biomass,
                                                   alternative = "less")[[3]][1], #já checado
                                      no = wilcox.test(dif_biomass,
                                                       alternative = "less")[[3]][1]),
                      p_greater = ifelse(is.normal == T, #o warning é pq tem dois valores coincidentemente iguais
                                         yes = t.test(dif_biomass,
                                                      alternative = "greater")[[3]][1], #já checado
                                         no = wilcox.test(dif_biomass,
                                                          alternative = "greater")[[3]][1])
                      ) #tudo OK
          

  #####Taxa de Crescimento ####
    ##Belostomatidae
      #Effect Size
        efeito_aq %>% group_by(suborfam) %>% summarise(cohen_d = ((mean(taxacrescimento.am)-mean(taxacrescimento.aq))/sd(taxacrescimento.am)))

      #Normalidade da diferença  
        ggdensity(belostomatidae_ef$ef_growth_rate) #parece normal
        ggqqplot(belostomatidae_ef$ef_growth_rate) #parece bastante normal
        
        shapiro.test(belostomatidae_ef$ef_growth_rate) #é normal
        
        
                                                       