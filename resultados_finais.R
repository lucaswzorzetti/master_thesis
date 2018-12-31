### Passando mais a limpo ainda meus resultados ##
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(effsize)

#Importing and wrangling data
geral <- read.table("planilhageral.txt", header = T)
View(geral)

geral <- mutate(geral, taxacrescimento = (((varbiom+biomassant)/biomassant)^(1/sobrev)))
geral <- mutate(geral, varbiomcorrigida = (varbiom.perdia/biomassant))
geral <- mutate(geral, ln.varbiom = log(varbiomcorrigida), ln.taxacap1 = log(taxacap1),
                ln.tempomanip1 = log(tempomanip1), ln.consumo = log(Totalpresascorrigido),
                ln.cresc = log(taxacrescimento))
View(geral)

##### Separando os Taxa #####
belostomatidae <- filter(geral, suborfam == "Belostomatidae")
notonectidae <-  filter(geral, suborfam == "Notonectidae")
anisoptera <-  filter(geral, suborfam == "Anisoptera")
zygoptera <-  filter(geral, suborfam == "Zygoptera")

#Extraindo equações alometricas
alo_belo <- lm(biomassant ~ I(compr*larg*pi), data = belostomatidae)
alo_belo
summary(alo_belo)
plot(alo_belo)
belostomatidae %>% ggplot(aes(x = compr, y = biomassant)) + geom_point() + geom_smooth(method = "lm")


#Modelos de taxa de crescimento
ggarrange(ggdensity(belostomatidae, x = "taxacrescimento", fill = "tratamento") + ggtitle("Belostomatidae"),
          ggdensity(notonectidae, x = "taxacrescimento", fill = "tratamento") + ggtitle("Notonectidae"),
          ggdensity(anisoptera, x = "taxacrescimento", fill = "tratamento") + ggtitle("Anisoptera"),
          ggdensity(zygoptera, x = "taxacrescimento", fill = "tratamento") + ggtitle("Zygoptera"),
          nrow = 2, ncol = 2)

belo_model <- lm(taxacrescimento ~ biomassant + tratamento, data = belostomatidae)
belo_model

belo_model_log <-  lm(log(taxacrescimento) ~ log(biomassant) + tratamento + fezmuda + Totalpresascorrigido, data = belostomatidae)
belo_model_log

anova(belo_model, belo_model_log)
AIC(belo_model)
AIC(belo_model_log) #modelo log é melhor

step(belo_model_log) #incluir tudo

summary(belo_model_log)





