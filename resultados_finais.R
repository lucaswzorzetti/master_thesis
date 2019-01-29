
# Importando e arrumando os dados -----------------------------------------
  ### Passando mais a limpo ainda meus resultados ##
    library(tidyverse) #vários pacotes como o ggplot2 e o dplyr
    library(ggpubr)
    library(ggplot2)
    library(dplyr)
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
    geral <- geral %>% mutate(totalpresasperdiamg = Totalpresascorrigido/biomassa_mg)
    
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
      noto_cresc <- model_line(notonectidae, notonectidae$biomassa_mg,
                               notonectidae$taxacrescimento,
                 "Taxa de Crescimento [proporção] em escala logaritmica", 
                 model = noto_cresc_lme)+
        geom_hline(yintercept = 1, linetype = 2)
    
    
  # Anisoptera --------------------------------------------------------------
  
    aniso_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco,
                             data = anisoptera)
    summary(aniso_cresc_lme_int) #Não tem interação
    
    Anova(aniso_cresc_lme_int, type = "III")
    
    aniso_cresc_lme <- lme(log(taxacrescimento) ~ log(biomassa_mg) + tratamento,
                           random = ~1|bloco,
                           data = anisoptera)
    
    summary(aniso_cresc_lme) #apenas biomassa e o intercepto significativos
    
    Anova(aniso_cresc_lme, type = "II") 
    
    #verificando outliers
      plot(lm(log(taxacrescimento) ~ log(biomassa_mg) + tratamento, data = anisoptera))
  
      anisoptera$taxacrescimento[4] <- NA
      anisoptera$taxacrescimento[16] <- NA
      
      shapiro.test(resid(aniso_cresc_lme)) #continua não normal
    
    #figura
      aniso_cresc <- model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$taxacrescimento, 
                 "Taxa de Crescimento [proporção] em escala logaritmica",
                 model = aniso_cresc_lme)+
        geom_hline(yintercept = 1, linetype = 2)
      aniso_cresc 
      
    
  # Zygoptera ---------------------------------------------------------------
    zygo_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento,
                              random = ~1|bloco, data = zygoptera)
    
    summary(zygo_cresc_lme_int) #sem interação
    
    Anova(zygo_cresc_lme_int, type = "III")
    
    zygo_cresc_lme <- lme(log(taxacrescimento) ~ log(biomassa_mg) + tratamento,
                              random = ~1|bloco, data = zygoptera)
    summary(zygo_cresc_lme)
    
    Anova(zygo_cresc_lme, type = "II")
    
    #verificando outliers
    plot(lm(log(taxacrescimento) ~ log(biomassa_mg) + tratamento, data = zygoptera))
    
    zygoptera$taxacrescimento[24] <- NA
    
    shapiro.test(resid(zygo_cresc_lme)) #no qqplot parece normal, mas o teste não confirma isso
    
    #Figura
    model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$taxacrescimento, 
               "Taxa de Crescimento [proporção] em escala logaritmica", zygo_cresc_lme)+
      geom_hline(yintercept = 1, linetype = 2)
    

  # Geral -------------------------------------------------------------------
    geral_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento + 
                                 log(biomassa_mg)*suborfam,
                               random = ~1|bloco, data = geral) #add efeito do taxon depois
    summary(geral_cresc_lme_int) #sem interação
    Anova(geral_cresc_lme_int, type = "III")
    
    geral_cresc_lme_int2 <- lme(log(taxacrescimento) ~ log(biomassa_mg) + tratamento + 
                                 log(biomassa_mg)*suborfam,
                               random = ~1|bloco, data = geral)
    summary(geral_cresc_lme_int2)
    
    Anova(geral_cresc_lme_int2, type = "III")
    
    plot(geral_cresc_lme_int2)
    
    
    
    #figura
    model_line(geral,  geral$biomassa_mg, geral$taxacrescimento, 
               "Taxa de Crescimento [proporção] em escala logaritmica", geral_cresc_lme_int2)+
      geom_hline(yintercept = 1, linetype = 2)
    
    
    

# Modelos de Taxa de Captura ----------------------------------------------
  #Belostomatidae
    belo_cap_lme_int <- lme(log(taxacap1) ~ log(biomassa_mg)*tratamento,
                            random = ~1|bloco, data = belostomatidae, na.action = na.omit)
    belo_cap_lme_int
    
    summary(belo_cap_lme_int)
    
    Anova(belo_cap_lme_int, type = "III") #Interação sign (mas não são poucos os dados?)
    
    plot(belo_cap_lme_int)
    
    #Verificando outliers
      plot(lm(log(taxacap1) ~ log(biomassa_mg) + tratamento, data = belostomatidae))
    
      shapiro.test(resid(belo_cap_lme_int))
    
    #Figura
      model_line(belostomatidae, belostomatidae$biomassa_mg, belostomatidae$taxacap1, 
                 "Taxa de Captura []", belo_cap_lme_int) #pensar bem, estranho...
      
    belostomatidae %>% group_by(tratamento) %>% select(taxacap1, tratamento) %>% 
      summarise(n = n()) #não funciona
      
  #Notonectidae
    noto_cap_lme_int <- lme(log(taxacap1) ~ log(biomassa_mg)*tratamento,
                            random = ~1|bloco, data = notonectidae, na.action = na.omit)
    noto_cap_lme_int   
    
    summary(noto_cap_lme_int) #não tem interação
    
    Anova(noto_cap_lme_int, type = "III")
    
    noto_cap_lme <- lme(log(taxacap1) ~ log(biomassa_mg) + tratamento,
                        random = ~1|bloco, data = notonectidae, na.action = na.omit)
    
    summary(noto_cap_lme)
    
    Anova(noto_cap_lme, type = "II") #nada tem efeito
    
    plot(lm(log(taxacap1) ~ log(biomassa_mg) + tratamento, data = notonectidae))
    
    shapiro.test(resid(noto_cap_lme))
    
    #Figura
      model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$taxacap1, 
                 "taxa de Captura []", noto_cap_lme)
      
  #Anisoptera
    aniso_cap_lme_int <- lme(log(taxacap1) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = anisoptera, na.action = na.omit)

    summary(aniso_cap_lme_int) #interação significativa
    
    Anova(aniso_cap_lme_int, type = "III")
    
    plot(lm(log(taxacap1) ~ log(biomassa_mg) + tratamento, data = anisoptera))
    
    shapiro.test(resid(aniso_cap_lme_int))
    
    #Figura
      model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$taxacap1,
                 "Taxa de Captura []",
                 aniso_cap_lme_int)
    
  #Zygoptera
    zygo_cap_lme_int <- lme(log(taxacap1) ~ log(biomassa_mg)*tratamento,
                            random = ~1|bloco, data = zygoptera, na.action = na.omit)

    zygo_cap_lme_int
    summary(zygo_cap_lme_int) #inter nao significativa
    
    Anova(zygo_cap_lme_int, type = "III")
    
    zygo_cap_lme <- lme(log(taxacap1) ~ log(biomassa_mg) + tratamento,
                        random = ~1|bloco, data = zygoptera, na.action = na.omit)
    
     summary(zygo_cap_lme)   
     
     Anova(zygo_cap_lme, type = "II") #nada significativo
     
     plot(lm(log(taxacap1) ~ log(biomassa_mg) + tratamento, data = zygoptera))
     
     shapiro.test(resid(zygo_cap_lme))
     
     #Figura
      model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$taxacap1, 
                 "Taxa de Captura []", zygo_cap_lme)
     
# Modelos de Taxa de Consumo (total presas corrigido) ----------------------------------------------
  #Belostomatidae
    belo_pres_lme_int <- lme(log(totalpresasperdiamg) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = belostomatidae)
    summary(belo_pres_lme_int) #inter não significativa
    
    Anova(belo_pres_lme_int, type = "III")
      
    belo_pres_lme <-  lme(log(totalpresasperdiamg) ~ log(biomassa_mg) + tratamento,
                          random = ~1|bloco, data = belostomatidae) 
    summary(belo_pres_lme)
    
    Anova(belo_pres_lme)
    
    plot(lm(totalpresasperdiamg ~ log(biomassa_mg) + tratamento, data = belostomatidae))
    
    shapiro.test(resid(belo_pres_lme)) #bem normal
    
    #figura
      model_line(belostomatidae, belostomatidae$biomassa_mg, belostomatidae$totalpresasperdiamg,
                 "Número de Presas Consumidas / dia", belo_pres_lme)
      
  #Notonectidae
      noto_pres_lme_int <- lme(log(totalpresasperdiamg) ~ log(biomassa_mg)*tratamento,
                               random = ~1|bloco, data = notonectidae)
      summary(noto_pres_lme_int)
      
      Anova(noto_pres_lme_int, type = "III") #inter nao signi
      
      noto_pres_lme <- lme(log(totalpresasperdiamg) ~ log(biomassa_mg) + tratamento,
                           random = ~1|bloco, data = notonectidae, na.action = na.omit)
      
      summary(noto_pres_lme)
      
      Anova(noto_pres_lme, type = "II")
      
      plot(lm(log(totalpresasperdiamg) ~ log(biomassa_mg)+ tratamento, data = notonectidae))
      
      shapiro.test(resid(noto_pres_lme))
      
      notonectidae$totalpresasperdiamg[16] <- NA
      notonectidae$totalpresasperdiamg[8] <- NA
      
        #Figura
          model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$totalpresasperdiamg,
                    "Total de Presas consumidas/dia", noto_pres_lme)
      
  #Anisoptera
    aniso_pres_lme_int <- lme(log(totalpresasperdiamg) ~ log(biomassa_mg)*tratamento,
                              random = ~1|bloco, data = anisoptera, na.action = na.omit)
    summary(aniso_pres_lme_int)
    
    Anova(aniso_pres_lme_int, type = "III") #sem inter
    
    aniso_pres_lme <- lme(log(totalpresasperdiamg) ~ log(biomassa_mg) + tratamento,
                          random = ~1|bloco, data = anisoptera, na.action = na.omit)
    
    summary(aniso_pres_lme)
    
    Anova(aniso_pres_lme)
    
    plot(lm(log(totalpresasperdiamg) ~ log(biomassa_mg) + tratamento, data = anisoptera))
    
    shapiro.test(resid(aniso_pres_lme_int))
    
    anisoptera$totalpresasperdiamg[28] <- NA #pesquisar melhor isso
    anisoptera$totalpresasperdiamg[30] <- NA
    
      #Figura
        model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$totalpresasperdiamg,
                   "Taxa de consumo [individuo/dia/mg predador]",
                   aniso_pres_lme_int)
        
  #Zygoptera
    zygo_pres_lme_int <- lme(totalpresasperdiamg ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = zygoptera, na.action = na.omit)
    summary(zygo_pres_lme_int)
    
    Anova(zygo_pres_lme_int, type = "III")
    
    zygo_pres_lme <- lme(totalpresasperdiamg ~ log(biomassa_mg) + tratamento,
                         random = ~1|bloco, data = zygoptera, na.action = na.omit)
    
    summary(zygo_pres_lme)
    
    Anova(zygo_pres_lme, type = "II")
    
    plot(lm(totalpresasperdiamg ~ log(biomassa_mg) + tratamento, data = zygoptera))
    
    shapiro.test(resid(zygo_pres_lme))
    
    zygoptera$totalpresasperdiamg[9] <- NA
    zygoptera$totalpresasperdiamg[4] <- NA
    
      #Figura
        model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$totalpresasperdiamg,
                   "Taxa de consumo [individuo/dia/mg predador]", zygo_pres_lme)
        
  #Geral
    geral_pres_lme_int <- lme(totalpresasperdiamg ~ log(biomassa_mg)*tratamento +
                                suborfam*tratamento + suborfam*log(biomassa_mg),
                              random = ~1|bloco, data = geral, na.action = na.omit
                              )    
    summary(geral_pres_lme_int)
    
    Anova(geral_pres_lme_int, type = "III")
    
    shapiro.test(resid(geral_pres_lme_int))
    
      #figura
        model_line(geral, geral$biomassa_mg, geral$totalpresasperdiamg,
                   "Taxa de consumo [individuo/dia/mg predador]", geral_pres_lme_int)
    
# Modelos de Tempo de Manipulação da Presa --------------------------------
  #Belostomatidae - não pode tempo de manip - não foi medido
       
  #Notonectidae
        noto_temp_lme_int <- lme(log(tempomanip1) ~ log(biomassa_mg)*tratamento,
                                 random = ~1|bloco, data = notonectidae,
                                 na.action = na.omit)
        summary(noto_temp_lme_int) #sem interação
        
        Anova(noto_temp_lme_int, type = "III")
        
        noto_temp_lme <- lme(log(tempomanip1) ~ log(biomassa_mg) + tratamento,
                             random = ~1|bloco, data = notonectidae,
                             na.action = na.omit)
        
        summary(noto_temp_lme)
        
        Anova(noto_temp_lme)
        
        plot(lm(log(tempomanip1) ~ log(biomassa_mg) + tratamento, data = notonectidae))
        
        shapiro.test(resid(noto_temp_lme))
        
        
          #Figura
            model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$tempomanip1,
                       "Tempo de manipulação da presa [segundos]",
                       noto_temp_lme)
  #Anisoptera
    aniso_temp_lme_int <- lme(log(tempomanip1) ~ log(biomassa_mg)*tratamento,
                              random = ~1|bloco, data = anisoptera,
                              na.action = na.omit)
    summary(aniso_temp_lme_int)  #sem interação
    
    Anova(aniso_temp_lme_int, type = "III")
    
    aniso_temp_lme <- lme(log(tempomanip1) ~ log(biomassa_mg) + tratamento,
                          random = ~1|bloco, data = anisoptera,
                          na.action = na.omit)
    summary(aniso_temp_lme)
    
    Anova(aniso_temp_lme, type = "II")
    
    plot(lm(log(tempomanip1) ~ log(biomassa_mg) + tratamento, data = anisoptera))
            
    shapiro.test(resid(aniso_temp_lme))    
    
      #Figura
        model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$tempomanip1,
                   "Tempo de manipulação da presa [segundos]",
                   aniso_temp_lme)
        
  #Zygoptera
    zygo_temp_lme_int <- lme(log(tempomanip1) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = zygoptera,
                             na.action = na.omit)
    summary(zygo_temp_lme_int) #sem interação
    
    Anova(zygo_temp_lme_int, type = "III")
    
    zygo_temp_lme <- lme(log(tempomanip1) ~ log(biomassa_mg) +tratamento,
                             random = ~1|bloco, data = zygoptera,
                             na.action = na.omit)
    summary(zygo_temp_lme)
    
    Anova(zygo_temp_lme, type = "II")
    
    plot(lm(log(tempomanip1) ~ log(biomassa_mg) +tratamento, data = zygoptera))
    
    shapiro.test(resid(zygo_temp_lme))
    
      #Figura
        model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$tempomanip1,
                   "Tempo de manipulação da presa [segundos]",
                   zygo_temp_lme)
    
# Modelos de sobrevivência ------------------------------------------------
ggdensity(data = geral, x = geral$sobrev, fill = geral$tratamento)


      

# Outras análises ---------------------------------------------------------
geral %>% group_by(tratamento) %>%  ggplot(aes(x = suborfam, y = biomassa_mg, 
                                               fill = suborfam)) +
        geom_boxplot() + theme_classic()
  
      
 geral %>% group_by(suborfam, tratamento) %>% 
        summarise(avg = mean(biomassa_mg), n_obs = n(),
                  dp = sd(biomassa_mg), ep = dp/sqrt(n_obs),
                  ic_baixo = avg - 1.96*ep,
                  ic_cima = avg +1.96*ep) %>% 
        ggplot(mapping = aes(x = suborfam, y = biomassa_mg, fill = tratamento))+
        geom_errorbar(stat = "identity", mapping = aes(ymax = ic_cima, ymin = ic_baixo),
                      position = position_dodge(width = 0.9), width = 0.5)+
        geom_point(stat = "identity", size = 7,mapping = aes(shape = tratamento, colour = tratamento),
                   position = position_dodge(width = 0.9)) +
        theme_classic() + ylab("Biomassa [mg]") + scale_colour_manual(values = c("deeppink", "darkblue"))
porvaloro      
      