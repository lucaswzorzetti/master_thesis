
# Importando e arrumando os dados -----------------------------------------
### Passando mais a limpo ainda meus resultados ##
library(tidyverse)
library(ggpubr)
library(Rmisc)
library(effsize)
library(lme4)
library(lmtest)
library(stargazer) #para tabelas
library(RVAideMemoire)

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
belo_model_log <-  lm(log(taxacrescimento) ~ log(biomassant) + tratamento + Error(bloco), data = belostomatidae)
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

#Com aov/lmer
belo_aov_int <-  glmer(taxacrescimento ~ biomassant*tratamento + (1|bloco), data = belostomatidae, family = Gamma())
belo_aov <- glmer(taxacrescimento ~ biomassant + tratamento + (1|bloco), data = belostomatidae, family = Gamma())
belo_aov_nulo <- glmer(taxacrescimento ~ 1 + (1|bloco), data = belostomatidae, family = Gamma())

summary(belo_aov_int) #interação significativa -> o efeito do tam corp na tax cresc é diferente entre os tratamentos
summary(belo_aov)

anova(belo_aov, belo_aov_int) #modelo intera é melhor que sem
anova(belo_aov_int, belo_aov_nulo) #modelo inter é melhor que o nulo

summary(belo_aov_int)

plotresid(belo_aov_int, shapiro = T)

shapiro.test(resid(belo_aov_int))

aquecido_belo <- belostomatidae %>% filter(tratamento == "Aquecido")
ambiente_belo <- belostomatidae %>% filter(tratamento == "Ambiente")

aq_belo <- lmer(log(taxacrescimento) ~ log(biomassant) + (1|bloco), data = aquecido_belo)
am_belo <- lmer(log(taxacrescimento) ~ log(biomassant) + (1|bloco), data = ambiente_belo)

summary(aq_belo)
summary(am_belo)

shapiro.test(resid(belo_model_log_aov))

#Modelos sucessivos
belo_model <- lm(taxacrescimento ~ biomassant + tratamento + bloco + fezmuda, data = belostomatidae)
belo_model_log <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco + fezmuda, data = belostomatidae)
belo_model_log1 <- lm(log(taxacrescimento) ~ log(biomassant) + bloco, data = belostomatidae)
belo_model_log2 <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco, data = belostomatidae)

summary(belo_model)
summary(belo_model_log)

lrtest(belo_model, belo_model_log, belo_model_log1, belo_model_log2) #melhor o completo mesmo
step(belo_model_log) #incluir tudo

###Notonectidae
noto_model <- lm(taxacrescimento ~ biomassant + tratamento + bloco, data = notonectidae)
noto_model_log <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco, data = notonectidae)

lrtest(noto_model, noto_model_log)
step(noto_model_log)

summary(noto_model_log)

#Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(noto_model_log))   ##teste de shapiro wilk (normalidade)

# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha)
plot(rstudent(noto_model_log) ~ fitted(noto_model_log), pch = 19)
abline(h = 0, lty = 2)

#Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
plot(log(notonectidae$taxacrescimento)~log(notonectidae$biomassant))
abline(noto_model_log,lty=2)

plot(noto_model_log)

notonectidae$taxacrescimento[16] <- NA
notonectidae$taxacrescimento[1] <- NA

anova(noto_model_log) #Só a biomassa se correlacionou, o resto não

####Anisoptera
aniso_model <- lm(taxacrescimento ~ biomassant + tratamento + bloco, data = anisoptera)
aniso_model_log <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco, data = anisoptera)

lrtest(aniso_model, aniso_model_log)
step(aniso_model_log)

summary(aniso_model_log)

#Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(aniso_model_log))   ##teste de shapiro wilk (normalidade)

# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha)
plot(rstudent(aniso_model_log) ~ fitted(aniso_model_log), pch = 19)
abline(h = 0, lty = 2)

#Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
plot(log(anisoptera$taxacrescimento)~log(anisoptera$biomassant))
abline(aniso_model_log,lty=2)

plot(aniso_model_log)

anisoptera$taxacrescimento[4] <- NA
anisoptera$taxacrescimento[16] <- NA
anisoptera$taxacrescimento[21] <- NA

anova(aniso_model_log) #o bloco está fazendo efeito!

####Zygoptera
zygo_model <- lm(taxacrescimento ~ biomassant + tratamento + bloco, data = zygoptera)
zygo_model_log <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + fezmuda + bloco, data = zygoptera)

lrtest(zygo_model, zygo_model_log)
step(zygo_model_log)

summary(zygo_model_log)

#Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(zygo_model_log))   ##teste de shapiro wilk (normalidade)

# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha)
plot(rstudent(zygo_model_log) ~ fitted(zygo_model_log), pch = 19)
abline(h = 0, lty = 2)

#Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
plot(log(zygoptera$taxacrescimento)~log(zygoptera$biomassant))
abline(zygo_model_log,lty=2)

plot(zygo_model_log)

zygoptera$taxacrescimento[24] <- NA
zygoptera$taxacrescimento[32] <- NA

anova(zygo_model_log)



# modelos de taxa de captura ----------------------------------------------
##Belostomatidae
belo_model <- lm(taxacap1 ~ biomassant + tratamento + bloco + fezmuda, data = belostomatidae)
belo_model_log <-  lm(log(taxacap1) ~ log(biomassant) + tratamento + bloco + fezmuda, data = belostomatidae)

lrtest(belo_model, belo_model_log)
step(belo_model_log) #incluir tudo

#Sumário dos resultados do modelo
summary(belo_model_log)

#Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(belo_model_log))   ##teste de shapiro wilk (normalidade)

# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha)
plot(rstudent(belo_model_log) ~ fitted(belo_model_log), pch = 19)
abline(h = 0, lty = 2)

#Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
plot(log(belostomatidae$taxacap1)~log(belostomatidae$biomassant))
abline(belo_model_log,lty=2)

plot(belo_model_log)

belostomatidae$taxacap1[]

anova(belo_model_log)

###
