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
library(ggeffects) #para plotar os lmes (o ggplot2 não consegue)



####Dados #####
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
geral <- geral[-c(55, 101),] #dados excluidos por problemas na medição
View(geral)

#Dividindo por grupo taxonomico
belostomatidae <- filter(geral, suborfam == "Belostomatidae")
notonectidae <-  filter(geral, suborfam == "Notonectidae")
anisoptera <-  filter(geral, suborfam == "Anisoptera")
zygoptera <-  filter(geral, suborfam == "Zygoptera")

a
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
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14))+
  scale_colour_manual(values = c("black", "black")) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = 8, linetype = 2) +
  geom_hline(yintercept = 6, linetype = 2)+
  geom_hline(yintercept = 4, linetype = 2)+
  geom_hline(yintercept = 2, linetype = 2)

#N amostral dos taxa e entre os blocos
geral %>% group_by(suborfam, bloco) %>% summarise(n = n()) 

#Quantidade de amostra por dia de experimento
geral %>% ggplot(aes(x = bloco)) + geom_bar() + facet_wrap(~suborfam)+
  theme_classic(base_size = 22) + xlab("Dia de início do experimento") +
  ylab("Número de pares")

#Biomassa dos indivíduos em cada dia de experimento
geral %>% ggplot(aes(x = bloco, y = biomassa_mg, fill = suborfam)) + 
  geom_boxplot()+geom_point() + facet_wrap(~suborfam, scales = "free_y")+
  theme_classic(base_size = 22) + xlab("Dia de início do experimento") +
  ylab("Biomassa [mg]")


#Testando com anova, por tratamento e dia de início
Anova(aov(biomassa_mg~tratamento + bloco, data = belostomatidae))
Anova(aov(biomassa_mg~tratamento + bloco, data = anisoptera))
Anova(aov(biomassa_mg~tratamento + bloco, data = zygoptera))
Anova(aov(biomassa_mg~tratamento + bloco, data = notonectidae))



###### Analises ####
#### Taxa Crescimento ####
  #Belostomatidae
    belo_cresc_int <- lme(taxacrescimento ~ biomassa_mg*tratamento, random = ~1|bloco,
                          data = belostomatidae, method = "ML") 
    summary(belo_cresc_int)  
    Anova(belo_cresc_int, type = "III") #inter signi
    plot(belo_cresc_int)
    
    #pressupoições
    qqPlot(resid(belo_cresc_int)) #residuos normais
    leveneTest(resid(belo_cresc_int)~belostomatidae$bloco) #dia de inicio com igual variancia
    leveneTest(resid(belo_cresc_int)~belostomatidae$tratamento) #sem homocedas
    
    #corrigindo a falta de homocedasticidade
    belo_cresc_int2 <- lme(taxacrescimento ~ biomassa_mg*tratamento, random = ~1|bloco,
                      data = belostomatidae, method = "ML", weights = varIdent(form = ~1|tratamento))
      summary(belo_cresc_int)
      Anova(belo_cresc_int, type = "III")
      
      plot(belo_cresc_int)
      leveneTest(resid(belo_cresc_int)~belostomatidae$bloco) # 
      
      r.squaredGLMM(belo_cresc_int)
      
      CookD(belo_cresc_int2)
      
    #plot
      predictions <- ggpredict(belo_cresc_int2, terms = c("biomassa_mg", "tratamento"))
      predictions
      
      ambiente <- predictions %>% filter(group == "Ambiente")
      aquecido <- predictions %>% filter(group == "Aquecido")
      
      ggplot() +
        geom_line(data = ambiente, aes(x = x, y = predicted),
                           color = "green", size = 3) +
        geom_line(data = aquecido, aes(x = x, y = predicted),
                  color = "red", size = 3) +
        geom_ribbon(data = ambiente, 
                    aes(x = x, ymin = predicted - std.error,
                        ymax = predicted + std.error), 
                    fill = "lightgrey", alpha = 0.5) +
        geom_ribbon(data = aquecido, 
                    aes(x = x, ymin = predicted - std.error,
                        ymax = predicted + std.error), 
                    fill = "lightcoral", alpha = 0.5) +
        geom_point(data = belostomatidae, size = 3,
                   aes(x = biomassa_mg,
                       y = taxacrescimento,
                       colour = tratamento))+
        scale_color_manual(values = c("green", "red"),
                           labels = c("Temperatura Ambiente",
                                      "Temperatura Ambiente + 4°C"))+
        labs(color = "Tratamento")+
        xlab("Biomassa [mg]") + ylab("Taxa de Crescimento") +
        geom_hline(yintercept = 1, linetype =2)+
        theme(legend.title = element_text("Tratamento"))+
        theme_classic(base_size = 22)
  
      
  #Anisoptera
    aniso_cresc_int <- lme(taxacrescimento ~ biomassa_mg*tratamento, random = ~1|bloco,
                            data = anisoptera, method = "ML", na.action = na.omit,
                           weights = varIdent(form = ~1|tratamento)) 
      summary(aniso_cresc_int)  
      Anova(aniso_cresc_int, type = "III") #inter
      plot(aniso_cresc_int)
      
    aniso_cresc <- lme(taxacrescimento ~ biomassa_mg + tratamento, random = ~1|bloco,
                       data = anisoptera,  na.action = na.omit,
                       weights = varIdent(form = ~1|tratamento))
      summary(aniso_cresc)
      Anova(aniso_cresc, type = "II")
      plot(aniso_cresc)
      
      shapiro.test(resid(aniso_cresc))
      
      CookD(aniso_cresc)
      
      r.squaredGLMM(belo_cresc)
      
      AICctab(aniso_cresc, aniso_cresc_int, base = T, weights = T)
      
      anisoptera$taxacrescimento[4] <- NA #cookD > 1
      anisoptera$taxacrescimento[16] <- NA #cookD > 0.5
      
      
    #pressupoições
      qqPlot(resid(aniso_cresc)) #residuos normais
      leveneTest(resid(belo_cresc)~belostomatidae$bloco) #dia de inicio com igual variancia
      leveneTest(resid(belo_cresc)~belostomatidae$tratamento) #sem homocedas
      
    #plot                  
      plot_lucas(aniso_cresc_int, dados = anisoptera, yaxis = anisoptera$taxacrescimento)          
                  
                  
                  
                  
                  
                  
                  