
# Importando e arrumando os dados -----------------------------------------
### Passando mais a limpo ainda meus resultados ##
library(tidyverse)
library(ggpubr)
library(Rmisc)
library(effsize)
library(lme4)
library(lmtest)

#Importing and wrangling data
geral <- read.table("planilhageral.txt", header = T)
View(geral)

geral <- mutate(geral, taxacrescimento = (((varbiom+biomassant)/biomassant)^(1/sobrev)))
geral <- mutate(geral, varbiomcorrigida = (varbiom.perdia/biomassant))
geral <- mutate(geral, ln.varbiom = log(varbiomcorrigida), ln.taxacap1 = log(taxacap1),
                ln.tempomanip1 = log(tempomanip1), ln.consumo = log(Totalpresascorrigido),
                ln.cresc = log(taxacrescimento))
geral <- geral %>% mutate(biomassa_convertida = BiomassaTotalconsumidaprimeirodia) #transformar para biomassa com equações alometricas de Benke et al 1999

View(geral)

## Separando os Taxa ##
belostomatidae <- filter(geral, suborfam == "Belostomatidae")
notonectidae <-  filter(geral, suborfam == "Notonectidae")
anisoptera <-  filter(geral, suborfam == "Anisoptera")
zygoptera <-  filter(geral, suborfam == "Zygoptera")



# Modelos de taxa de crescimento ------------------------------------------
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

#Belostomatidae

step(belo_model_log) #incluir tudo

#A função para regressão é “lm” e não requer pacote estatístico (variavel resposta ~ variável preditora)
belo_model_log <-  lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco + fezmuda, data = belostomatidae)
belo_model_log

step(belo_model_log) #incluir tudo

#Sumário dos resultados do modelo
summary(belo_model_log)

#Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(belo_model_log))   ##teste de shapiro wilk (normalidade)

# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha)
plot(rstudent(belo_model_log) ~ fitted(belo_model_log), pch = 19)
abline(h = 0, lty = 2)

#Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
plot(log(belostomatidae$taxacrescimento)~log(belostomatidae$biomassant))
abline(belo_model_log,lty=2)

plot(belo_model_log)

belostomatidae$taxacrescimento[17] <- NA #resolveu muito!

anova(belo_model_log) #bloco não teve efeito (p = 0.73) e biomassa e tratamento tiveram

#Com lme
belo_model_log <-  lmer(log(taxacrescimento) ~ log(biomassant) + tratamento + (1|bloco), data = belostomatidae)
belo_model_log
summary(belo_model_log)

#Modelos sucessivos
belo_model <- lm(taxacrescimento ~ biomassant + tratamento + bloco + fezmuda, data = belostomatidae)
belo_model_log <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco + fezmuda, data = belostomatidae)
belo_model_log1 <- lm(log(taxacrescimento) ~ log(biomassant) + bloco, data = belostomatidae)
belo_model_log2 <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco, data = belostomatidae)

summary(belo_model)
summary(belo_model_log)

lrtest(belo_model, belo_model_log, belo_model_log1, belo_model_log2) #melhor o de log

###Notonectidae

