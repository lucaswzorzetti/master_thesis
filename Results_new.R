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
library(PairedData)
library(magrittr)

#Importing 
geral <- read.table("planilhageral_atualizada4.txt", header = T, colClasses = c(
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


geral_arrumado <- geral %>% select(suborfam, bloco, amostra, number, tratamento,
                         presa_mean, tempocap1, tempocap2,
                         compr, larg, biomassa_mg,
                         fezmuda, sobrev, presas_consumidas_gravacao,
                         Totalpresascorrigido, tempomanip1, tempomanip2,
                         taxacrescimento, dif_temp_cap, ppsr)

## Separating the Taxa ##
belostomatidae <- filter(geral_arrumado, suborfam == "Belostomatidae")
notonectidae <-  filter(geral_arrumado, suborfam == "Notonectidae")
anisoptera <-  filter(geral_arrumado, suborfam == "Anisoptera")
zygoptera <-  filter(geral_arrumado, suborfam == "Zygoptera")

#Creating a table with the effect of treatment
geral_am <- geral_arrumado %>%  filter(tratamento == "Ambiente")
geral_aq <- geral_arrumado %>% filter(tratamento == "Aquecido") 

#tentando juntar para calcular o efeito
efeito_aq <- left_join(x = geral_am, y = geral_aq, by = "number", #misturou as familias
                       suffix = c(".am", ".aq")) %>% mutate(suborfam = suborfam.aq, bloco = bloco.aq,
                                                            par = number,
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
                                                            )# %>% 
                                                      #select(suborfam, bloco, par, amostra.am, amostra.aq, dif_compr,
                                                             #dif_larg, dif_biomass,
                                                             #dif_ppsr, ef_temcap1, ef_temcap2, ef_sobrev,
                                                            # ef_cons_grav,
                                                            # ef_cons_total, ef_tempomanip1, ef_tempomanip2,
                                                             #ef_growth_rate, ef_satiety, ppsr_mean, biom_mean)
                
View(efeito_aq)



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

geral %>% group_by(bloco, suborfam) %>% summarise(n = n()) #quantos em cada bloco
geral %>% group_by(suborfam, tratamento) %>% summarise(n = n()) #quantos em cd trat e cd taxa
table_parear <- geral %>% group_by(suborfam, bloco, tratamento) %>% summarise(n = n()) #
View(table_parear)


# Comparing biomasses between Taxa ----------------------------------------
#Descrevendo o tamanho
geral %>% group_by(suborfam) %>% summarise(compr_medio = mean(compr),
                                           desvio_compr = sd(compr),
                                           larg_media = mean(larg),
                                           desvio_larg = sd(larg),
                                           biom_media = mean(biomassa_mg), 
                                           desvio_biom = sd(biomassa_mg))

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

####Teste de modelos ####






#belos
shapiro.test(anisoptera_ef$ef_cons_total)

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



#Verificando o efeito da média de biomassa sobre o efeito do aq
#Belostomatidae
teste1 <- lm(ef_growth_rate ~ biom_mean,
     data = belostomatidae_ef)
summary(teste1)
Anova(teste1, type = "II")


test <- lm(ef_growth_rate ~ 1,
   data = belostomatidae_ef)
summary(test)




#Testando com o pacote Paired Data
pair_tempocap1 <- paired(belostomatidae_ef$tempocap1.am, belostomatidae_ef$tempocap1.aq)
pair_tempocap1

plot(pair_tempocap1, type = "profile") 


par_tempocap1 <- data.frame(ambiente = belostomatidae_ef$tempocap1.am,
                    aquecido = belostomatidae_ef$tempocap1.aq)

ggpaired(data = par_tempocap1, cond1 = "ambiente",
         cond2 = "aquecido", fill = "condition")
##
ggpaired(data = anisoptera_ef, cond1 = "tempomanip1.am",
         cond2 = "tempomanip1.aq", fill = "condition")

##Taxa de crescimento dos significativos
ggpaired(data = belostomatidae_ef, cond1 = "taxacrescimento.am",
         cond2 = "taxacrescimento.aq", fill = "condition",
         line.color = "gray", line.size = 0.4,
         palette = "npg")
ggpaired(data = notonectidae_ef, cond1 = "taxacrescimento.am",
         cond2 = "taxacrescimento.aq", fill = "condition")


#Crescimento
wilcox.test(belostomatidae_ef$taxacrescimento.am, belostomatidae_ef$taxacrescimento.aq,
            paired = TRUE, alternative = "less", exact = TRUE) #sign
wilcox.test(anisoptera_ef$taxacrescimento.am, anisoptera_ef$taxacrescimento.aq,
            paired = TRUE, alternative = "less", exact = TRUE)
wilcox.test(zygoptera_ef$taxacrescimento.am, zygoptera_ef$taxacrescimento.aq,
            paired = TRUE, alternative = "less", exact = TRUE)
wilcox.test(notonectidae_ef$taxacrescimento.am, notonectidae_ef$taxacrescimento.aq,
            paired = TRUE, alternative = "less", exact = TRUE)


#tentativa de um modelo linear pareado

model_par <- lm(signed_rank(ef_growth_rate) ~ 1,
   belostomatidae_ef)

summary(model_par)
plot(model_par)


model_par2 <- lm((taxacrescimento.aq-taxacrescimento.am)~biom_mean,
                 belostomatidae_ef)
summary(model_par2)

ggpaired(data = belostomatidae_ef, cond1 = "taxacrescimento.am",
         cond2 = "taxacrescimento.aq", fill = "condition")


### testando com o crescimento de belostomatidae
#efeito do bloco (dia de início do experimento)
ggplot(belostomatidae_ef, aes(x = ef_growth_rate)) + geom_density() + facet_wrap(~bloco)
ggplot(belostomatidae_ef, aes(x = ef_temcap1)) + geom_density() + facet_wrap(~bloco)



belos_mod_cresc <- lmer(ef_growth_rate~ 1 + (1|bloco),
                        data = belostomatidae_ef)
belos_mod_cresc2 <- lm(ef_growth_rate~1,
                         data = belostomatidae_ef)
belos_mod_cresc3 <- lmer(ef_growth_rate ~ biom_mean + (1|bloco),
                         data = belostomatidae_ef)
library(bbmle)
AICctab(belos_mod_cresc, belos_mod_cresc2, belos_mod_cresc3, base = T, weights = T)

anova(belos_mod_cresc2, belos_mod_cresc)

plotresid(belos_mod_cresc)

summary(belos_mod_cresc)
r.squaredGLMM(belos_mod_cresc)
Anova(belos_mod_cresc)

plot(belos_mod_cresc)
