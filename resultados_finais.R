
# Importando e arrumando os dados -----------------------------------------
  ### Passando mais a limpo ainda meus resultados ##
    library(tidyverse) #vários pacotes como o ggplot2 e o dplyr
    library(ggpubr)
    library(Rmisc)
    library(effsize)
    library(lme4) #glm, glmm, lme, etc
    library(lmtest)
    library(stargazer) #para tabelas
    library(RVAideMemoire)
    library(nlme)
    library(car)

  #Importing and wrangling data
    geral <- read.table("planilhageral.txt", header = T)
    View(geral)
    
    geral <- mutate(geral, taxacrescimento = 
                      (((varbiom+biomassant)/biomassant)^(1/sobrev))) #taxa de crescimento
    geral <- mutate(geral, varbiomcorrigida = 
                      (varbiom.perdia/biomassant)) #variação de biomassa relativizada
    geral <- mutate(geral, ln.varbiom = log(varbiomcorrigida),  #logs (desnecessário?)
                    ln.taxacap1 = log(taxacap1),
                    ln.tempomanip1 = log(tempomanip1),
                    ln.consumo = log(Totalpresascorrigido),
                    ln.cresc = log(taxacrescimento))
    geral <- geral %>% mutate(biomassa_convertida = 
                                BiomassaTotalconsumidaprimeirodia) #transformar para
                              #biomassa com equações alometricas de Benke et al 1999
    geral <- geral %>% mutate(biomassa_mg = biomassant*1000)
    
    View(geral)
  
  ## Separando os Taxa ##
    belostomatidae <- filter(geral, suborfam == "Belostomatidae")
    notonectidae <-  filter(geral, suborfam == "Notonectidae")
    anisoptera <-  filter(geral, suborfam == "Anisoptera")
    zygoptera <-  filter(geral, suborfam == "Zygoptera")


# Equações alometricas ----------------------------------------------------
#Extraindo equações alometricas
alo_belo <- lm(biomassant ~ I(compr*larg*pi), data = belostomatidae)
alo_belo

summary(alo_belo)
plot(alo_belo)
belostomatidae %>% ggplot(aes(x = compr, y = biomassant)) + geom_point() + geom_smooth(method = "lm")


# Modelos de taxa de crescimento ------------------------------------------
  #Distribuição de frequencia
    ggarrange(ggdensity(belostomatidae, x = "taxacrescimento", fill = "tratamento")
              + ggtitle("Belostomatidae"),
              ggdensity(notonectidae, x = "taxacrescimento", fill = "tratamento") 
              + ggtitle("Notonectidae"),
              ggdensity(anisoptera, x = "taxacrescimento", fill = "tratamento") 
              + ggtitle("Anisoptera"),
              ggdensity(zygoptera, x = "taxacrescimento", fill = "tratamento") 
              + ggtitle("Zygoptera"),
              nrow = 2, ncol = 2)


  # Belostomatidae ----------------------------------------------------------
    #lm para testar outliers
        belo_cresc_lm <-  lm(log(taxacrescimento) ~ log(biomassa_mg) + 
                                tratamento, data = belostomatidae)
        belo_cresc_lm #coeficientes
  
      #Sumário dos resultados do modelo
        summary(belo_cresc_lm)
  
      #Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
      shapiro.test(rstudent(belo_cresc_lm))
  
      # Análise visual para homogeneidade dos resíduos 
      #(visualmente eles devem se distribuir igualmente abaixo e acima da linha)
      plot(rstudent(belo_cresc_lm) ~ fitted(belo_cresc_lm), pch = 19)
      abline(h = 0, lty = 2)
  
      #Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
      plot(log(belostomatidae$taxacrescimento)~log(belostomatidae$biomassant))
      abline(belo_cresc_lm,lty=2)
  
      plot(belo_cresc_lm) #inspeção dos resíduos
  
      belostomatidae$taxacrescimento[17] <- NA #Se precisar


    #Modelando com lme
      belo_cresc_lme_int <-  lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento,
                                 random = ~1|bloco,
                                 weights = varIdent(form = ~ 1 | tratamento),
                                 data = belostomatidae)

      summary(belo_cresc_lme_int) # interação significativa -> o efeito do tam corp 
                            # na tax cresc é diferente entre os tratamentos
      
      Anova(belo_cresc_lme_int, type = "III") #III é porque tem interação

      plot(belo_cresc_lme_int)
      
      #Figura do modelo
      belo_cresc <-  model_line(belostomatidae, belostomatidae$biomassa_mg, belostomatidae$taxacrescimento,
                                ynome = "Taxa de Crescimento [proporção] em escala logaritmica", model = belo_cresc_lme_int) +
        geom_hline(yintercept = 1, linetype = 2)
      belo_cresc 
      
      plotresid(belo_cresc_lme_int)
      
      #Testes de normalidade dos residuos
      shapiro.test(resid(belo_cresc_lme_int))


  # Notonectidae ------------------------------------------------------------
    noto_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento,
                              weights = varIdent(form = ~ 1 | tratamento),
                              random = ~1|bloco, data = notonectidae)
    summary(noto_cresc_lme_int) #Não tem interação
    
    Anova(noto_cresc_lme_int, type = "III") #interação não significativa mesmo
    
    noto_cresc_lme <- lme(log(taxacrescimento) ~ log(biomassa_mg) + tratamento,
                          weights = varIdent(form = ~ 1 | tratamento),
                          random = ~1|bloco, data = notonectidae)
    
    summary(noto_cresc_lme)
    
    Anova(noto_cresc_lme, type = "II") #apenas biomassa significativa

    plot(noto_cresc_lme)
    
    #verificando outliers
    plot(lm(log(taxacrescimento) ~ log(biomassa_mg) + tratamento, data = notonectidae)) #1 é outlier
    
    notonectidae$taxacrescimento[1] <- NA
    
    #Verificando normalidade dos resíduos
    shapiro.test(resid(noto_cresc_lme)) #ao que parece os resíduos não são normais
    
    #Figura
    model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$taxacrescimento,
               "Taxa de Crescimento [proporção] em escala logaritmica", model = noto_cresc_lme)+
      geom_hline(yintercept = 1, linetype = 2)
    
    
    
####Anisoptera
aniso_model <- lm(taxacrescimento ~ biomassant + tratamento + bloco, data = anisoptera)
aniso_model_log <- lm(log(taxacrescimento) ~ log(biomassant) + tratamento + bloco, data = anisoptera)

  # Anisoptera --------------------------------------------------------------


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
