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
geral <- read.table("planilhageral_atualizada3.txt", header = T, colClasses = c(
  "factor", "factor","factor","factor","character", "factor", "numeric", "numeric",
  "numeric","numeric",
  "numeric","numeric","numeric","factor", "numeric","numeric","numeric","numeric",
  "numeric", "logical", "integer", "integer", "numeric","numeric","numeric","numeric",
  "numeric","numeric", "numeric","numeric","numeric"
))
str(geral)

geral$presas_consumidas_gravacao[114] <- 2 #marcação errada
geral <- mutate(geral, taxacrescimento = 
                  (((varbiom+biomassant)/biomassant)^(1/sobrev))) #growth rate  
geral <- geral %>% mutate(biomassa_mg = biomassant*1000) # Converting to Biomass in miligrams
geral <- geral %>% mutate(tempocap1 = ifelse(test = is.na(tempocap1),
                                             yes = 5400,
                                             no = tempocap1),
                          tempocap2 = ifelse(test = tempocap1==5400,
                                             yes = NA,
                                             no = ifelse(test = is.na(tempocap2), #Difference on two capture times
                                                yes = 5400-tempocap1, #confirmar depois
                                                no = tempocap2)),
                          dif_temp_cap = ifelse(test = is.na(tempocap2),
                                                yes = NA,
                                                no = tempocap2 - tempocap1))
geral <- geral %>% mutate(prop_cap = presas_consumidas_gravacao/3) #Proportion of captured prey
geral <- geral %>% filter(suborfam == "Anisoptera" | suborfam =="Zygoptera" | suborfam =="Notonectidae" | (suborfam =="Belostomatidae" & biomassa_mg < 100))
geral <- geral %>% mutate(ppsr = biomassa_mg/presa_mean)
geral <- geral[-c(31, 46, 48, 54, 91, 105), ] #Tirando dados não pareados

#Creating a table with the effect of treatment
geral_am <- geral %>%  filter(tratamento == "Ambiente") %>% select(suborfam, bloco, amostra, number,
                                                                   presa_mean, tempocap1, tempocap2,
                                                                   compr, larg, biomassa_mg,
                                                                   fezmuda, sobrev, presas_consumidas_gravacao,
                                                                   Totalpresascorrigido, tempomanip1, tempomanip2,
                                                                   taxacrescimento, dif_temp_cap, ppsr)
geral_aq <- geral %>% filter(tratamento == "Aquecido") %>% select(suborfam, bloco, amostra, number,
                                                               presa_mean, tempocap1, tempocap2,
                                                               compr, larg, biomassa_mg,
                                                               fezmuda, sobrev, presas_consumidas_gravacao,
                                                               Totalpresascorrigido, tempomanip1, tempomanip2,
                                                               taxacrescimento, dif_temp_cap, ppsr)
View(geral_am)
View(geral_aq)

#tentando juntar para calcular o efeito
efeito_aq <- left_join(x = geral_am, y = geral_aq, by = "number",
                       suffix = c(".am", ".aq")) %>% mutate(suborfam = suborfam.aq, bloco = bloco.aq,
                                                            dif_compr = compr.aq-compr.am, dif_larg = larg.aq-larg.am,
                                                            dif_biomass = biomassa_mg.aq - biomassa_mg.am,
                                                            dif_ppsr = ppsr.aq-ppsr.am,
                                                            ef_temcap1 = tempocap1.aq-tempocap1.am,
                                                            ef_temcap2 = tempocap2.aq-tempocap2.am,
                                                            ef_sobrev = sobrev.aq-sobrev.am,
                                                            ef_cons_grav = presas_consumidas_gravacao.aq-presas_consumidas_gravacao.am,
                                                            ef_cons_total = Totalpresascorrigido.aq-Totalpresascorrigido.am,
                                                            ef_tempomanip1 = tempomanip1.aq-tempomanip1.am,
                                                            ef_tempomanip2 = tempomanip2.aq-tempomanip2.am,
                                                            ef_growth_rate = taxacrescimento.aq-taxacrescimento.am,
                                                            ef_satiety = dif_temp_cap.aq-dif_temp_cap.am,
                                                            ppsr_mean = ((ppsr.aq + ppsr.am)/2),
                                                            biom_mean = ((biomassa_mg.aq +biomassa_mg.am)/2)
                                                            ) %>% 
                                                      select(suborfam, bloco, amostra.am, amostra.aq, dif_compr,
                                                             dif_larg, dif_biomass,
                                                             dif_ppsr, ef_temcap1, ef_temcap2, ef_sobrev,
                                                             ef_cons_grav,
                                                             ef_cons_total, ef_tempomanip1, ef_tempomanip2,
                                                             ef_growth_rate, ef_satiety, ppsr_mean, biom_mean)
                
View(efeito_aq)

## Separating the Taxa ##
belostomatidae <- filter(geral, suborfam == "Belostomatidae")
notonectidae <-  filter(geral, suborfam == "Notonectidae")
anisoptera <-  filter(geral, suborfam == "Anisoptera")
zygoptera <-  filter(geral, suborfam == "Zygoptera")

#For the effects ##
belostomatidae_ef <- filter(efeito_aq, suborfam == "Belostomatidae")
notonectidae_ef <-  filter(efeito_aq, suborfam == "Notonectidae")
anisoptera_ef <-  filter(efeito_aq, suborfam == "Anisoptera")
zygoptera_ef <-  filter(efeito_aq, suborfam == "Zygoptera")

stop

# Previous tests ----------------------------------------------------------
#To verify if the covariable vary with treatment
t.test(belostomatidae$biomassa_mg~belostomatidae$tratamento)
t.test(anisoptera$biomassa_mg~anisoptera$tratamento)
t.test(zygoptera$biomassa_mg~zygoptera$tratamento)
t.test(notonectidae$biomassa_mg~notonectidae$tratamento)

#dif biomass
efeito_aq %>% group_by(suborfam) %>%  summarise(avg = mean(abs(dif_biomass)), desvio = sd(dif_biomass), IC_min_95 = avg - desvio, IC_max_95 = avg+desvio)

efeito_aq %>% group_by(suborfam) %>%
  summarise(avg = mean(dif_biomass), desvio = sd(dif_biomass), 
            IC_min_95 = (avg - desvio), IC_max_95 = (avg+desvio)) %>% 
  ggplot(aes(x = suborfam, y = avg, fill = suborfam))+geom_point(size = 10, shape = 21)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = IC_min_95, ymax = IC_max_95, colour = suborfam), size = 2) + theme_classic()

#em cada um dos taxa - biomassa
t.test(x = belostomatidae_ef$dif_biomass, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")
t.test(x = anisoptera_ef$dif_biomass, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")
t.test(x = zygoptera_ef$dif_biomass, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")
t.test(x = notonectidae_ef$dif_biomass, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")

bla
#em cada um, com ppsr
t.test(x = belostomatidae_ef$dif_ppsr, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")
t.test(x = anisoptera_ef$dif_ppsr, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")
t.test(x = zygoptera_ef$dif_ppsr, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")
t.test(x = notonectidae_ef$dif_ppsr, paired = FALSE, conf.level = 0.95,
       alternative = "two.sided")
stop

#


geral %>% select(suborfam, bloco, tratamento, biomassa_mg, ppsr) %>%
  group_by(suborfam, bloco, tratamento) %>%
  mutate(med = mean(ppsr)) %>% summarise(med = mean(ppsr)) %>% 
  ggplot(aes(x = bloco, y = med, colour = tratamento)) + geom_point() 



View(geral %>% select(suborfam, bloco, tratamento, amostra, biomassa_mg) %>%
  group_by(bloco, tratamento) )

geral %>% group_by(bloco) %>% summarise(n = n()) #quantos em cada bloco
geral %>% group_by(suborfam, tratamento) %>% summarise(n = n()) #quantos em cd trat e cd taxa
table_parear <- geral %>% group_by(suborfam, bloco, tratamento) %>% summarise(n = n()) #
View(table_parear)



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

#ppsr
ppsr_all <- geral %>% ggplot(aes(x = suborfam, y = ppsr, fill = suborfam))+
  geom_point(size = 5, alpha = 0.5, shape = 21)+
  scale_x_discrete(limits = c("Belostomatidae", "Anisoptera",
                              "Zygoptera", "Notonectidae")) +
  xlab("Taxa") + ylab("PPSR")+
  theme_classic() + theme(legend.position = "none",
                          axis.text.x = element_text(face = "bold",
                                                     size = 16, colour = "black"),
                          axis.text.y = element_text(size = 18, colour = "black"),
                          axis.title.x = element_blank(),
                          axis.title.y = element_text(face = "bold",
                                                      size = 20,
                                                      margin = margin(r = 10)))
ppsr_all

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

##Teste de modelos
#belos
t.test(x = belostomatidae_ef$ef_sobrev, paired = FALSE, alternative = "greater")
t.test(x = belostomatidae_ef$ef_temcap1, paired = FALSE, alternative = "less")
t.test(x = belostomatidae_ef$ef_cons_total, paired = F, alternative = "less") #consumo menor sob aquecimento
t.test(x = belostomatidae_ef$ef_growth_rate, paired = F, alternative = "greater") #sign
t.test(x = belostomatidae_ef$ef_cons_grav, paired = F)

#aniso
t.test(x = anisoptera_ef$ef_sobrev, paired = F, alternative = "less")
t.test(x = anisoptera_ef$ef_temcap1, paired = F, alternative = "less") #significativo
t.test(x = anisoptera_ef$ef_satiety, paired = F, alternative = "less")
t.test(x = anisoptera_ef$ef_cons_total, paired = F, alternative = "greater") #sign
t.test(x = anisoptera_ef$ef_tempomanip1, paired = F, alternative = "less")  #sign
t.test(x = anisoptera_ef$ef_growth_rate, paired = F, alternative = "less")
t.test(x = anisoptera_ef$ef_cons_grav, paired = F, alternative = "greater") #sign

#zygo
t.test(x = zygoptera_ef$ef_sobrev, paired = F, alternative = "less") #sign
t.test(x = zygoptera_ef$ef_temcap1, paired = F, alternative = "greater") 
t.test(x = zygoptera_ef$ef_satiety, paired = F, alternative = "less") #quaase
t.test(x = zygoptera_ef$ef_cons_total, paired = F, alternative = "greater") 
t.test(x = zygoptera_ef$ef_tempomanip1, paired = F, alternative = "less") #sig
t.test(x = zygoptera_ef$ef_growth_rate, paired = F, alternative = "greater") #sig 
t.test(x = zygoptera_ef$ef_cons_grav, paired = F, alternative = "greater") #sig

#noto
t.test(x = notonectidae_ef$ef_sobrev, paired = F, alternative = "greater") #sign
t.test(x = notonectidae_ef$ef_temcap1, paired = F, alternative = "less") 
t.test(x = notonectidae_ef$ef_satiety, paired = F, alternative = "greater") 
t.test(x = notonectidae_ef$ef_cons_total, paired = F, alternative = "less") 
t.test(x = notonectidae_ef$ef_tempomanip1, paired = F, alternative = "greater") 
t.test(x = notonectidae_ef$ef_growth_rate, paired = F, alternative = "less") #sign
t.test(x = notonectidae_ef$ef_cons_grav, paired = F, alternative = "less") 



efeito_aq %>% group_by(suborfam) %>% ggplot(aes(x = suborfam, y = ef_growth_rate,
                                                fill = suborfam))+
  geom_boxplot() + geom_hline(yintercept = 0) + theme_classic()


test <- lm(ef_growth_rate ~ 1,
   data = belostomatidae_ef)
summary(test)




#