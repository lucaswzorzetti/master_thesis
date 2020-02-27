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
library(bbmle) #AICc tables
library(fitdistrplus) #to fit different distributions

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
geral <- geral[-c(10, 56),] #dados excluidos por problemas na medição
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
      
      belostomatidae <- filter(geral_arrumado, suborfam == "Belostomatidae")
      notonectidae <-  filter(geral_arrumado, suborfam == "Notonectidae")
      anisoptera <-  filter(geral_arrumado, suborfam == "Anisoptera")
      zygoptera <-  filter(geral_arrumado, suborfam == "Zygoptera")
    

#### Análise dos dados - [pareada apenas e com modelos lme] ####
####Exploração geral dos dados ####
  #Linha do tempo - para verificar a distribuição temporal das amostras
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
      
  #N amostral dos taxa e entre os blocos
      efeito_aq %>% group_by(suborfam, bloco) %>% summarise(n = n()) 
      
      efeito_aq %>% ggplot(aes(x = bloco)) + geom_bar() + facet_wrap(~suborfam)+
        theme_classic(base_size = 22) + xlab("Dia de início do experimento") +
        ylab("Número de pares")
      
      efeito_aq %>% ggplot(aes(x = bloco, y = biom_mean, fill = suborfam)) + 
        geom_boxplot()+geom_point() + facet_wrap(~suborfam, scales = "free_y")+
        theme_classic(base_size = 22) + xlab("Dia de início do experimento") +
        ylab("Biomassa média do par [mg]")
      
      geral %>% ggplot(aes(x = bloco, y = biomassa_mg, fill = suborfam)) + 
        geom_boxplot()+geom_point() + facet_wrap(~suborfam, scales = "free_y")+
        theme_classic(base_size = 22) + xlab("Dia de início do experimento") +
        ylab("Biomassa [mg]")
      
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
      
      efeito_aq %>% group_by(suborfam) %>% 
        ggplot(aes(x = suborfam, y = dif_biomass, fill = suborfam))+
        geom_boxplot()+
        geom_point(size = 2, shape = 21, fill = "black", show.legend = F)+
        geom_hline(yintercept = 0) +
        theme_classic(base_size = 22) +
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
          
        #Testando com anova, por tratamento e dia de início
          Anova(aov(biomassa_mg~tratamento + bloco, data = belostomatidae))
          Anova(aov(biomassa_mg~tratamento + bloco, data = anisoptera))
          Anova(aov(biomassa_mg~tratamento + bloco, data = zygoptera))
          Anova(aov(biomassa_mg~tratamento + bloco, data = notonectidae))

  #####Taxa de Crescimento ####
   #Effect Size
    efeito_aq %>% group_by(suborfam) %>% summarise(cohen_d = ((mean(taxacrescimento.am)-mean(taxacrescimento.aq))/sd(taxacrescimento.am)))
              
    ####Belostomatidae ####
      #Normalidade da diferença  
        ggdensity(belostomatidae$taxacrescimento) #parece normal
        ggqqplot(belostomatidae$taxacrescimento) #parece bastante normal
        
        shapiro.test(belostomatidae$taxacrescimento) #é normal
        
      #Full model
        belos_cresc_int <- lme(taxacrescimento ~ tratamento*biomassa_mg,
                           random = ~ 1|bloco, method = "ML",
                           data = belostomatidae)
          summary(belos_cresc)
          Anova(belos_cresc, type = "III")
        
        plot.lme(belos_cresc)
        plot(belostomatidae_ef$biom_mean, resid(belos_cresc))
        plot(belostomatidae_ef$bloco, resid(belos_cresc)) #variancias diferentes
        
        qqnorm(belos_cresc, ~ranef(., level=0))

        leveneTest((belostomatidae_ef$ef_growth_rate), center=mean,
                   group = belostomatidae_ef$bloco) #homocedasticidade entre os blocos
        
      #Modelos
        #hipótese1: aquec e biom.
        g_mfull <- lme(ef_growth_rate ~ biom_mean, random = ~1|bloco,
                    data = belostomatidae_ef, method = "ML") 
          summary(g_mfull)
          Anova(g_mfull) #biomassa significativa
          r.squaredGLMM(g_mfull)
          
        #hipótese2: a biom não interfere no efeito do aquecimento
        g_m1 <- lme(ef_growth_rate ~ 1, random = ~1|bloco,
                    data = belostomatidae_ef, method = "ML")
          summary(g_m1)
          
        
          #Protocolo: 1° testar a hip principal, se não for significativa,
          # fazer o modelo nulo e compará-los com critério de info (AICc)
        
      #Avaliando o modelo
        g_mfull <- lme(ef_growth_rate ~ biom_mean, random = ~1|bloco,
                         data = belostomatidae_ef, method = "REML")
          summary(g_mfull)
        
          ggdensity(resid(g_mfull))
          
          ggqqplot(resid(g_mfull)) #normalidade dos resíduos
          shapiro.test(resid(g_mfull)) #normality of residuals
          
          CookD(g_mfull)
          
          Anova(g_mfull)
          
           
      #Fazendo um plot
        belostomatidae_ef %>% ggplot(aes(x = biom_mean, y = ef_growth_rate)) +
                geom_point(size = 3) +
          geom_hline(yintercept = 0, linetype = 2) +
          geom_smooth(method = "lm", colour = "blue") +
          ylab("Efeito do Aumento de Temperatura \n sobre a Taxa de Crescimento") + 
          xlab("Média de Biomassa do Par [mg]") + ggtitle("Belostomatidae")+
                theme_classic(base_size = 22) +
          theme(plot.title = element_text(hjust = 0.5))#com as linhas dos blocos (dia de inicio)
        
        #sem covar
        ggpaired(data = belostomatidae_ef,
                 cond1 = "taxacrescimento.am", cond2 = "taxacrescimento.aq",
                 fill = "condition",
                 line.color = "darkgray", line.size = 0.4,
                 palette = c("green", "red"), point.size = 2, xlab = "",
                 ylab = "Taxa de Crescimento",
                 size.base = 22) + 
          stat_compare_means(paired = TRUE, method = "t.test", method.args = list(alternative = "greater")) + 
          scale_x_discrete(labels = c(" Temperatura Ambiente", "Temperatura Ambiente + 4°C")) +
          theme(legend.position = "none")
        
    ###Anisoptera ####
      #normalidade dos dados
        ggdensity(anisoptera_ef$ef_growth_rate) 
        ggqqplot(anisoptera_ef$ef_growth_rate) 
        
        shapiro.test(anisoptera_ef$ef_growth_rate) #
        
        fit_w  <- fitdist(anisoptera_ef$ef_growth_rate, "geometric")
        
      #Full model
        ag_mfull <- lme(ef_growth_rate ~ log(biom_mean), random = ~1|bloco,
                        data = anisoptera_ef, method = "ML")
          plot.lme(ag_mfull)
          
          fitdistr(resid(ag_mfull), densfun="t")
          library(glmm)
        glmer(ef_growth_rate ~ (biom_mean) + (1|bloco),
             data = anisoptera_ef, family = gaussian(link = "inverse"))
          
          
          
          
          
        summary(lmer(taxacrescimento ~ biomassa_mg + (1|bloco) + (1|number),
                     data = anisoptera))
        
          ggqqplot(resid(ag_mnull))
         
        
        
        
###### novo velho jeito ####
#### Taxa Crescimento ####
  #Belostomatidae
    belo_cresc_int <- lme(taxacrescimento ~ biomassa_mg*tratamento, random = ~1|bloco,
                          data = belostomatidae, method = "ML") 
      summary(belo_cresc_int)  
      Anova(belo_cresc_int, type = "III") #inter signi
      
      #pressupoições
      qqPlot(resid(belo_cresc_int)) #residuos normais
      leveneTest(resid(belos_cresc_int)~belostomatidae$tratamento)
      
      
    
    
    
    
    
    
    
                                                       