
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
    library(fitdistrplus)
    library(nlme)
    library(car)
    library(MuMIn)
    library(scales)
    library(knitr)
    library(MASS)

  #Importing and wrangling data
    geral <- read.table("planilhageral_atualizada2.txt", header = T, colClasses = c(
      "factor", "factor","factor","factor","character", "numeric", "numeric","numeric",
      "numeric","numeric","numeric","factor", "numeric","numeric","numeric","numeric",
      "numeric", "logical", "integer", "integer", "numeric","numeric","numeric","numeric",
      "numeric","numeric", "numeric","numeric","numeric"
    ))
    View(geral)
    str(geral)
    
    
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
    geral <- geral %>% mutate(totalpresasperdiamg = Totalpresascorrigido/biomassa_mg,
                              totalpresasperdiag = Totalpresascorrigido/biomassant)
    geral <- geral %>% mutate(taxacap_hor_NA = ifelse(taxacap_hor == 0, NA, taxacap_hor), #colocando NAs para facilitar analise
                              taxacap_min_NA = ifelse(taxacap_min == 0, NA, taxacap_min),
                              consumo1dia_NA = ifelse(consumo1dia == 0, NA, consumo1dia))
    geral <- geral %>% mutate(tempocapmedio = ifelse(test = is.na(tempocap1), yes = NA,
                                                     no = ifelse(test = is.na(tempocap2), yes = tempocap1,
                                                                 no = ifelse(test = is.na(tempocap3), 
                                                                             yes = ((tempocap1+tempocap2)/2),
                                                                             no = ((tempocap1+tempocap2+tempocap3)/3)))))
    
    geral <- geral %>% mutate(consumo_rel_1dia = (consumo1dia/1000)/biomassant,
                              consumo_rel_1diaNA = ifelse(test = consumo_rel_1dia > 0,
                                                        yes = consumo_rel_1dia,
                                                        no = NA)) #mg presa/gr predador
    geral <- geral %>% mutate(dif_temp_cap = ifelse(test = is.na(tempocap2),
                                                    yes = NA,
                                                    no = (tempocap2-tempocap1)),
                              dif_temp_cap_mod = ifelse(test = is.na(tempocap2),
                                                        yes = NA,
                                                        no = ifelse(test = (tempocap2-tempocap1) < 0,
                                                                    yes = (tempocap2-tempocap1)*-1,
                                                                    no = (tempocap2-tempocap1))))
    geral <- geral %>% mutate(prop_cap = presas_consumidas_gravacao/3)
    geral$presas_consumidas_gravacao[114] <- 2
    
    View(geral)
  
  ## Separando os Taxa ##
    belostomatidae <- filter(geral, suborfam == "Belostomatidae")
    notonectidae <-  filter(geral, suborfam == "Notonectidae")
    anisoptera <-  filter(geral, suborfam == "Anisoptera")
    zygoptera <-  filter(geral, suborfam == "Zygoptera")

as
# Equações alometricas ----------------------------------------------------
#Extraindo equações alometricas
alo_belo <- lm(log(biomassant) ~ log(compr), data = belostomatidae)
alo_belo

summary(alo_belo)
plot(alo_belo)
belostomatidae %>% ggplot(aes(x = compr, y = biomassant)) + geom_point() + geom_smooth(method = "lm")

geral %>% ggplot(aes(x = log(compr), y = log(biomassa_mg), fill = suborfam, shape = suborfam)) +
  geom_smooth(method = "lm") +
  geom_point(aes(fill=factor(suborfam)), size=3) +
  theme_classic() + ylab("Log(Biomassa)") +
  xlab("Log(Comprimento)") + 
  scale_x_continuous(limits = c(-1.1, 1), breaks = seq(-2, 2, by = 0.5))+
  scale_y_continuous(limits = c(1, 6), breaks = seq(0, 6, by = 0.5))


# Gráfico com as Biomassa de todos ----------------------------------------
biomassas_todos <- geral %>% ggplot(aes(x = suborfam, y = biomassa_mg, fill = suborfam))+
                    geom_point(size = 5, alpha = 0.5, shape = 21)+
                    scale_x_discrete(limits = c("Belostomatidae", "Anisoptera",
                                                "Zygoptera", "Notonectidae")) +
                    xlab("Taxa") + ylab("Biomass [mg]")+
                    theme_classic() + theme(legend.position = "none",
                                            axis.text = element_text(face = "bold",
                                                                     size = 12, colour = "black"),
                                            axis.title.x = element_blank(),
                                            axis.title.y = element_text(face = "bold",
                                                                        size = 18,
                                                                        margin = margin(r = 10))
                                            )
biomassas_todos

  #tamanho
    tamanho_todos <- geral %>% ggplot(aes(x = suborfam, y = compr, fill = suborfam))+
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
    tamanho_todos

af

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
                                 data = belostomatidae, na.action = na.omit)

      summary(belo_cresc_lme_int) # interação significativa -> o efeito do tam corp 
                            # na tax cresc é diferente entre os tratamentos
      
      belo_cresc_anova <- Anova(belo_cresc_lme_int, type = "III") #III é porque tem interação
      belo_cresc_anova

      plot(belo_cresc_lme_int)
      
      #Figura do modelo
        belo_cresc <-  model_line(belostomatidae, belostomatidae$biomassa_mg, 
                                  belostomatidae$taxacrescimento,
                                  ynome = "Growth rate, log10 scale", 
                                  model = belo_cresc_lme_int,
                                  title = "Belostomatidae") +
          geom_hline(yintercept = 1, linetype = 2)
        belo_cresc 
        
        plotresid(belo_cresc_lme_int)
        
        jpeg(filename = "growth_belos.jpg", width = 2350, height = 1900, 
        units = "px", pointsize = 12, quality = 100,
        bg = "white",  res = 300)
        belo_cresc
        dev.off()
        
        png(filename = "growth_rate_belostomatide.png", res = 600, width = 480, height = 480)
      
      #Testes de normalidade dos residuos
        shapiro.test(resid(belo_cresc_lme_int))
        
        #Tabela
        belo_cresc_lme_int_r2 <- r.squaredGLMM(belo_cresc_lme_int)
        stargazer( belo_cresc_lme_int, align = TRUE,
                   title = "Belostomatidae Growth Rate Model results", ci = TRUE,
                   ci.level = 0.95, 
                   column.labels = c("Interaction", "Biomass + Treatment",
                                     "Only Biomass"),
                   type = "text", add.lines = list(c("R² marginal",
                                                     round(belo_cresc_lme_int_r2[1,1],
                                                           digits = 4),
                                                     round(belo_cresc_lme_int_r2[1,1],
                                                           digits = 4),
                                                     round(belo_cresc_lme_int_r2[1,1],
                                                           digits = 4)),
                                                   c("R² conditional",
                                                     round(belo_cresc_lme_int_r2[1,2], 
                                                           digits = 4),
                                                     round(belo_cresc_lme_int_r2[1,2],
                                                           digits = 4),
                                                     round(belo_cresc_lme_int_r2[1,2],
                                                           digits = 4))))


  # Notonectidae ------------------------------------------------------------
    noto_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento,
                              weights = varIdent(form = ~ 1 | tratamento),
                              random = ~1|bloco, data = notonectidae)
    summary(noto_cresc_lme_int) #Não tem interação
    
    Anova(noto_cresc_lme_int, type = "III") #interação não significativa mesmo
    
    noto_cresc_lme <- lme(log(taxacrescimento) ~ log(biomassa_mg) + tratamento,
                          weights = varIdent(form = ~ 1 | tratamento),
                          random = ~1|bloco, data = notonectidae, na.action = na.omit)
    noto_cresc_lme_semtrat <- lme(log(taxacrescimento) ~ log(biomassa_mg),
                                  random = ~1|bloco, data = notonectidae, na.action = na.omit)
    
    summary(noto_cresc_lme)
    summary(noto_cresc_lme_semtrat)
    
    noto_cresc_anova <- Anova(noto_cresc_lme, type = "II") #apenas biomassa significativa
    noto_cresc_anova
    
    
    plot(noto_cresc_lme)
    
    #verificando outliers
      plot(lm(log(taxacrescimento) ~ log(biomassa_mg) + tratamento, data = notonectidae)) #1 é outlier
      
      notonectidae$taxacrescimento[1] <- NA
    
    #Verificando normalidade dos resíduos
      shapiro.test(resid(noto_cresc_lme)) #ao que parece os resíduos não são normais
    
    #Figura
      noto_cresc <- model_line(notonectidae, notonectidae$biomassa_mg,
                               notonectidae$taxacrescimento,
                 "Growth rate, log10 scale", 
                 model = noto_cresc_lme,
                 title = "Notonectidae")+
        geom_hline(yintercept = 1, linetype = 2)
      noto_cresc
      
      jpeg(filename = "growth_noto.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      noto_cresc
      dev.off()
    
  # Anisoptera --------------------------------------------------------------
  
    aniso_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco,
                             data = anisoptera)
    summary(aniso_cresc_lme_int) #Não tem interação
    
    Anova(aniso_cresc_lme_int, type = "III")
    
    aniso_cresc_lme <- lme(log(taxacrescimento) ~ log(biomassa_mg) + tratamento,
                           random = ~1|bloco,
                           data = anisoptera, na.action = na.omit,
                           weights = varIdent(form = ~ 1 | tratamento))
    plot(aniso_cresc_lme)
    
    summary(aniso_cresc_lme) #apenas biomassa e o intercepto significativos
    
    aniso_cresc_anova <- Anova(aniso_cresc_lme, type = "II") 
    aniso_cresc_anova
    
    #verificando outliers
      plot(lm(log(taxacrescimento) ~ log(biomassa_mg) + tratamento, data = anisoptera))
  
      anisoptera$taxacrescimento[4] <- NA
      anisoptera$taxacrescimento[16] <- NA
      
      shapiro.test(resid(aniso_cresc_lme)) #continua não normal
    
    #figura
      aniso_cresc <- model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$taxacrescimento, 
                 "Growth rate, log10 scale",
                 model = aniso_cresc_lme,
                 title = "Anisoptera")+
        geom_hline(yintercept = 1, linetype = 2)
      aniso_cresc 
      
      jpeg(filename = "growth_aniso.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      aniso_cresc
      dev.off()
      
    
  # Zygoptera ---------------------------------------------------------------
    zygo_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento,
                              random = ~1|bloco, data = zygoptera)
    
    summary(zygo_cresc_lme_int) #sem interação
    
    Anova(zygo_cresc_lme_int, type = "III")
    
    zygo_cresc_lme <- lme(log(taxacrescimento) ~ log(biomassa_mg) + tratamento,
                              random = ~1|bloco, data = zygoptera,
                          weights = varIdent(form = ~ 1 | tratamento),
                          na.action = na.omit)
                          
    summary(zygo_cresc_lme)
    
    zygo_cresc_anova <- Anova(zygo_cresc_lme, type = "II")
    zygo_cresc_anova
    
    #verificando outliers
    plot(lm(log(taxacrescimento) ~ log(biomassa_mg) + tratamento, data = zygoptera))
    
    zygoptera$taxacrescimento[24] <- NA
    zygoptera$taxacrescimento[32] <- NA
    
    shapiro.test(resid(zygo_cresc_lme)) #no qqplot parece normal, mas o teste não confirma isso
    
    plot(zygo_cresc_lme)
    
    #Figura
    zygo_cresc <- model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$taxacrescimento, 
               "Growth rate, log10 scale",
               zygo_cresc_lme, "Zygoptera")+
      geom_hline(yintercept = 1, linetype = 2)
    zygo_cresc
    
    jpeg(filename = "growth_zygo.jpg", width = 2350, height = 1900, 
         units = "px", pointsize = 12, quality = 100,
         bg = "white",  res = 300)
    zygo_cresc
    dev.off()
    

  # Geral -------------------------------------------------------------------
    geral_cresc_lme_int <- lme(log(taxacrescimento) ~ log(biomassa_mg)*tratamento + 
                                 log(biomassa_mg)*suborfam,
                               random = ~1|bloco, data = geral,
                               weights = varIdent(form = ~ 1 | tratamento),
                               na.action = na.omit) #add efeito do taxon depois
    summary(geral_cresc_lme_int) #sem interação
    Anova(geral_cresc_lme_int, type = "III")
    
    geral_cresc_lme_int2 <- lme(log(taxacrescimento) ~ log(biomassa_mg) +
                                  tratamento + suborfam,
                               random = ~1|bloco, data = geral,
                               weights = varIdent(form = ~ 1 | tratamento),
                               na.action = na.omit)
    summary(geral_cresc_lme_int2)
    
    Anova(geral_cresc_lme_int2, type = "III")
    
    plot(geral_cresc_lme_int2)
    
    shapiro.test(resid(geral_cresc_lme_int2)) #nem um pouco normal
    
    plot(lm(log(taxacrescimento) ~ log(biomassa_mg) + tratamento + suborfam, data = geral))
    
    #figura
    model_line(geral,  geral$biomassa_mg, geral$taxacrescimento, 
               "Growth rate, log10 scale", geral_cresc_lme_int2)+
      geom_hline(yintercept = 1, linetype = 2)
    
    
#Tabela comparativa dos modelos de taxa de crescimento
    #os modelos de cada são:
    belo_cresc_lme_int
    noto_cresc_lme
    aniso_cresc_lme
    zygo_cresc_lme
    
    #R2 para lme
    belo_cresc_lme_int_r2 <- r.squaredGLMM(belo_cresc_lme_int)
    noto_cresc_lme_r2 <- r.squaredGLMM(noto_cresc_lme)
    aniso_cresc_lme_r2 <- r.squaredGLMM(aniso_cresc_lme)
    zygo_cresc_lme_r2 <- r.squaredGLMM(zygo_cresc_lme)
    
    #tabela comparativa dos modelos
    stargazer( belo_cresc_lme_int, noto_cresc_lme,
               aniso_cresc_lme, zygo_cresc_lme,
               align = TRUE,
               type = "text",
               title = "Growth Rate Model results", ci = TRUE,
               ci.level = 0.90, model.numbers = FALSE,
               notes = "Confidence Interval of 90 percent",
               column.labels = c("Belostomatidae", "Notonectidae",
                                 "Anisoptera", "Zygoptera"),
               covariate.labels = c("Log(Biomass)", "Treatment: Warmed",
                                    "Log(Biomass) : Treatment",
                                    "Constant"), 
               dep.var.labels = "Log(Growth rate)",
               add.lines = list(c("R² marginal",
                                  round(belo_cresc_lme_int_r2[1,1],
                                        digits = 4),
                                  round(noto_cresc_lme_r2[1,1],
                                        digits = 4),
                                  round(aniso_cresc_lme_r2[1,1],
                                        digits = 4),
                                  round(zygo_cresc_lme_r2[1,1],
                                        digits = 4)),
                                c("R² conditional",
                                  round(belo_cresc_lme_int_r2[1,2], 
                                        digits = 4),
                                  round(noto_cresc_lme_r2[1,2],
                                        digits = 4),
                                  round(aniso_cresc_lme_r2[1,2],
                                        digits = 4),
                                  round(zygo_cresc_lme_r2[1,2],
                                        digits = 4))))
    #Tabela de Anovas
    stargazer( belo_cresc_anova, noto_cresc_anova,
               aniso_cresc_anova, zygo_cresc_anova,
               type = "text",
               align = TRUE)
    stargazer(summary(belo_cresc_lme_int))
    
# Modelos de Taxa de Captura ----------------------------------------------
  #Belostomatidae
    belo_cap_glmm_int <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~ 
                                 biomassa_mg + tratamento + I(biomassa_mg) +
                            (1|bloco), data = belostomatidae, family = binomial)
    belo_cap_glmm_int
    
    summary(belo_cap_glmm_int)
    
    Anova(belo_cap_glmm_int, type = "III") #sem interação
    
    plot(belo_cap_glmm_int)
    
    #sem inter
    belo_cap_glmm <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~ 
                             biomassa_mg + tratamento + (1|bloco),
                           data = belostomatidae, family = binomial)
    summary(belo_cap_glmm)
    
    Anova(belo_cap_glmm, type = "II")
    
    plot(belo_cap_glmm)
    
    r.squaredGLMM(belo_cap_glmm)
    
    #Verificando outliers
      
      shapiro.test(resid(belo_cap_glmm))
      
    #Checando overdispersion
      library(AER)
      dispersiontest(belo_cap_glmm)
    
    #Figura
      
      belo_cap <- model_line_noline(belostomatidae, belostomatidae$biomassa_mg,
                             (belostomatidae$prop_cap), 
                 "Proportion of captures [n/3]", belo_cap_glmm_int,
                 title = "Belostomatidae")
      belo_cap
      
      jpeg(filename = "capture_belos.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      belo_cap
      dev.off()
    
    
      
  #Notonectidae
    noto_cap_glmm_int <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~
                                 biomassa_mg*tratamento +
                                (1|bloco), data = notonectidae, family = binomial)
    noto_cap_glmm_int   
    summary(noto_cap_glmm_int)
    Anova(noto_cap_glmm_int, "III")
    
    noto_cap_glmm <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~
                             biomassa_mg + tratamento +
                             (1|bloco), data = notonectidae, family = binomial)
    noto_cap_glmm
    summary(noto_cap_glmm)
    Anova(noto_cap_glmm)
    
    shapiro.test(resid(noto_cap_glmm)) #resíduos não normais
    
    
    noto_cap_glmm_r2 <- r.squaredGLMM(noto_cap_glmm)
    noto_cap_glmm_r2
    
    
    #Figura
      noto_cap <- model_line_noline(notonectidae, notonectidae$biomassa_mg, (notonectidae$prop_cap), 
                 "Proportion of captures [n/3]", noto_cap_lme, "Notonectidae")
      noto_cap
      
      jpeg(filename = "capture_noto.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      noto_cap
      dev.off()
      
      
      
    #Tabela com os modelos Usar como modelo de tabela para os outros
      stargazer( noto_cap_lme_int, noto_cap_lme, noto_cap_lme_min, align = TRUE,
                title = "Notonectidae Regression Model results", ci = TRUE,
                ci.level = 0.90, 
                column.labels = c("Interaction", "Biomass + Treatment", "Only Biomass"),
                type = "text", add.lines = list(c("R² marginal", round(noto_cap_lme_int_r2[1,1], digits = 4),
                                                  round(noto_cap_lme_r2[1,1], digits = 4), round(noto_cap_lme_min_r2[1,1], digits = 4)),
                                                c("R² conditional", round(noto_cap_lme_int_r2[1,2], digits = 4),
                                                  round(noto_cap_lme_r2[1,2], digits = 4), round(noto_cap_lme_min_r2[1,2], digits = 4))))
      
  #Anisoptera
    aniso_cap_glmm_int <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~ 
                               biomassa_mg*tratamento + (1|bloco), data = anisoptera, na.action = na.omit,
                             family = binomial)

    summary(aniso_cap_lme_int) #sem inter
    
    Anova(aniso_cap_lme_int, type = "III")
    plot(aniso_cap_lme_int)
    
    shapiro.test(resid(aniso_cap_glmm_int))
    
    aniso_cap_glmm <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~ 
                             biomassa_mg + tratamento + (1|bloco), data = anisoptera,
                           na.action = na.omit, family = binomial)
    aniso_cap_glmm
    
    summary(aniso_cap_glmm)
    
    Anova(aniso_cap_glmm, type = "II")
    
    shapiro.test(resid(aniso_cap_glmm))
    
    
    
    #Figura
      aniso_cap <- model_line_noline(anisoptera, anisoptera$biomassa_mg, (anisoptera$taxacap_hor+1),
                 "Proportion of captures [n/3]",
                 aniso_cap_glmm, "Anisoptera")
      aniso_cap
      
      jpeg(filename = "capture_aniso.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      aniso_cap
      dev.off()
      
    
  #Zygoptera
    zygo_cap_glmm_int <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~ 
                                 biomassa_mg*tratamento + (1|bloco), data = zygoptera,
                               na.action = na.omit, family = binomial)

    zygo_cap_glmm_int
    summary(zygo_cap_glmm_int) #inter nao significativa
    
    Anova(zygo_cap_glmm_int, type = "III")
    
    shapiro.test(resid(zygo_cap_glmm_int))
    
    zygo_cap_glmm <- glmer(cbind(presas_consumidas_gravacao, 3-presas_consumidas_gravacao) ~ 
                             biomassa_mg + tratamento + (1|bloco), data = zygoptera, 
                           na.action = na.omit, family = binomial)
    
     summary(zygo_cap_glmm)   
     
     Anova(zygo_cap_glmm, type = "II") #biomassa significativo
     
     plot(zygo_cap_glmm)
     
     shapiro.test(resid(zygo_cap_glmm))
     
     #Figura
     zygo_cap <- model_line_noline(zygoptera, zygoptera$biomassa_mg, (zygoptera$prop_cap), 
                 "Proportion of captures [n/3]", zygo_cap_lme, "Zygoptera") 
     zygo_cap
     
     jpeg(filename = "capture_zygo.jpg", width = 2350, height = 1900, 
          units = "px", pointsize = 12, quality = 100,
          bg = "white",  res = 300)
     zygo_cap
     dev.off()
      
      
  #Geral
      #Proporções  
      proporcao <- geral %>% group_by(tratamento, suborfam) %>% 
        filter(!is.na(taxacap1eq)) %>% summarise(n = n(), proportion = (n/15)*100) %>% 
        ggplot(aes(x = tratamento, y = proportion, fill = tratamento)) + geom_col(color = "black") + 
        facet_grid(cols = vars(suborfam)) + theme_classic() + 
        theme(axis.text.x.bottom = element_blank(), axis.title.x = element_blank(),
              legend.title = element_text(face = "bold", size =25),
              axis.line.x = element_blank(), axis.ticks.x = element_blank(),
              strip.text = element_text(face = "bold", size = 15),
              axis.title.y = element_text(face = "bold", size = 15),
              axis.text.y = element_text(colour = "black", size = 15),
              legend.text = element_text(size = 18),
              legend.position = c(0.4, 0.8))+
        expand_limits(y = 100)+
        labs(fill = "Treatment") + scale_fill_manual(values = c("#66cc33","#cc0000"),
                                                     labels = c("Ambient", "Warmed"))+
        scale_y_continuous(breaks = seq(0, 100, length.out = 9),
                           labels = c("0", "12.5 %", "25 %", "37.5 %", "50 %", "62.5 %",
                                      "75 %", "87.5%", "100 %"))+
        ylab("Predators that eat a Prey on the first day")+
        geom_hline(yintercept = 0, linetype = 2) + geom_hline(yintercept = 100, linetype = 2)
      proporcao
      
      #salvando figura
      jpeg(filename = "proporcao_captura.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      proporcao
      dev.off()
      
    #Tabela dos modelos de taxa de captura
      #os modelos de cada são:
      belo_cap_glmm
      noto_cap_glmm
      aniso_cap_glmm
      zygo_cap_glmm
      
      #R2 para lme
      belo_cap_glmm_r2 <- r.squaredGLMM(belo_cap_glmm)
      noto_cap_glmm_r2 <- r.squaredGLMM(noto_cap_glmm)
      aniso_cap_glmm_r2 <- r.squaredGLMM(aniso_cap_glmm)
      zygo_cap_glmm_r2 <- r.squaredGLMM(zygo_cap_glmm)
      
      #tabela comparativa dos modelos
      stargazer( belo_cap_glmm, noto_cap_glmm,
                 aniso_cap_glmm, zygo_cap_glmm,
                 align = TRUE,
                 title = "Capture rate Model results", ci = TRUE,
                 ci.level = 0.90, model.numbers = FALSE,
                 notes = "Confidence Interval of 90 percent",
                 column.labels = c("Belostomatidae", "Notonectidae",
                                   "Anisoptera", "Zygoptera"),
                 covariate.labels = c("Log(Biomass)", "Treatment: Warmed",
                                      "Constant"), 
                 dep.var.labels = "Proportion of captures [n/3]",
                 add.lines = list(c("R² marginal",
                                    round(belo_cap_glmm_r2[1,1],
                                          digits = 4),
                                    round(noto_cap_glmm_r2[1,1],
                                          digits = 4),
                                    round(aniso_cap_glmm_r2[1,1],
                                          digits = 4),
                                    round(zygo_cap_glmm_r2[1,1],
                                          digits = 4)),
                                  c("R² conditional",
                                    round(belo_cap_glmm_r2[1,2], 
                                          digits = 4),
                                    round(noto_cap_glmm_r2[1,2],
                                          digits = 4),
                                    round(aniso_cap_glmm_r2[1,2],
                                          digits = 4),
                                    round(zygo_cap_glmm_r2[1,2],
                                          digits = 4))))
      
     

# Modelos de Tempo entre as Capturas ---------------------------------------------
  #Belostomatidae (muito pouco capturou, muitos NA's)
      belo_temcap_lme_int <- lme(log(tempocapmedio) ~ log(biomassa_mg)*tratamento,
                                  random = ~1|bloco, data = belostomatidae, na.action = na.omit)
      summary(belo_temcap_lme_int)
      Anova(belo_temcap_lme_int, type = "III") #inter marginal
      shapiro.test(resid(belo_temcap_lme_int))
      plot(belo_temcap_lme_int) #poucos dados não NA's
      
      #figura
        belo_temcap <- model_line(belostomatidae, belostomatidae$biomassa_mg, belostomatidae$tempocapmedio, 
                   "Average time of capture [s], log10 scale", belo_temcap_int, "Belostomatidae")+
          geom_hline(yintercept = 0, linetype = 2)+
          annotation_logticks() + theme(legend.position = c(0.8, 0.8))
        belo_temcap
        
        jpeg(filename = "temcap_belo.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        belo_temcap
        dev.off()
      
  #Notonectidae
      noto_temcap_lme_int <- lme(log(tempocapmedio) ~ log(biomassa_mg)*tratamento,
                                 random = ~1|bloco, data = notonectidae, na.action = na.omit,
                                 weights = varIdent(form = ~ 1 | tratamento))
      
      summary(noto_temcap_lme_int)
      Anova(noto_temcap_lme_int, type = "III") #sem inter
      
      noto_temcap_lme <- lme(log(tempocapmedio) ~ log(biomassa_mg) + tratamento,
                             random = ~1|bloco, data = notonectidae, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
      summary(noto_temcap_lme)
      Anova(noto_temcap_lme)
      shapiro.test(resid(noto_temcap_lme))
      plot(noto_temcap_lme)
      
      plot(lm(log(notonectidae$tempocapmedio)~log(notonectidae$biomassa_mg)+notonectidae$tratamento))
      
      notonectidae$tempocapmedio[13] <- NA
      
        #figura
        noto_temcap <- model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$tempocapmedio, 
                                  "Average time of capture [s], log10 scale", noto_temcap_lme, "Notonectidae")+
          geom_hline(yintercept = 0, linetype = 2)+
          annotation_logticks() + theme(legend.position = c(0.8, 0.8))
        noto_temcap
        
        jpeg(filename = "temcap_noto.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        noto_temcap
        dev.off()
      
  #Anisoptera
      aniso_temcap_lme_int <- lme(dif_temp_cap ~ biomassa_mg*tratamento,
                                  random = ~1|bloco, data = anisoptera, na.action = na.omit,
                                  weights = varIdent(form = ~ 1 | tratamento))
      summary(aniso_temcap_lme_int)
      Anova(aniso_temcap_lme_int, type = "III") #sem inter
      
      aniso_temcap_lme <- lme(dif_temp_cap ~ biomassa_mg + tratamento,
                                  random = ~1|bloco, data = anisoptera, na.action = na.omit,
                                  weights = varIdent(form = ~ 1 | tratamento))
      summary(aniso_temcap_lme)
      Anova(aniso_temcap_lme)
      
      shapiro.test(resid(aniso_temcap_lme))
      plot(aniso_temcap_lme)
      
      plot(lm(log(dif_temp_cap)~log(biomassa_mg)+tratamento, data = anisoptera))
      
      anisoptera$dif_temp_cap[16] <- NA
      
        #figura
          aniso_temcap <- model_line_semlog(anisoptera, anisoptera$biomassa_mg, anisoptera$dif_temp_cap, 
                                    "Difference of capture times (2 and 1) [s], log10 scale", aniso_temcap_lme, 
                                    "Anisoptera")+
            geom_hline(yintercept = 0, linetype = 2)
          aniso_temcap
          
          jpeg(filename = "temcap_aniso.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          aniso_temcap
          dev.off()
      
      
  #Zygoptera
      zygo_temcap_lme_int <- lme(dif_temp_cap ~ biomassa_mg*tratamento,
                                 random = ~1|bloco, data = zygoptera, na.action = na.omit,
                                 weights = varIdent(form = ~ 1 | tratamento))
      summary(zygo_temcap_lme_int)
      Anova(zygo_temcap_lme_int, type = "III") #sem inter
      
      zygo_temcap_lme<- lme(dif_temp_cap ~ biomassa_mg + tratamento,
                                 random = ~1|bloco, data = zygoptera, na.action = na.omit,
                                 weights = varIdent(form = ~ 1 | tratamento))
      summary(zygo_temcap_lme)
      
      Anova(zygo_temcap_lme)
      shapiro.test(resid(zygo_temcap_lme)) #não normal :(
      plot(zygo_temcap_lme)
      
      plot(lm(log(tempocapmedio)~log(biomassa_mg)+tratamento, data = zygoptera))
      
        #figura
          zygo_temcap <- model_line_semlog(zygoptera, zygoptera$biomassa_mg, zygoptera$dif_temp_cap, 
                                     "Difference of capture times (2 and 1) [s], log10 scale", zygo_temcap_lme, 
                                     "Zygoptera")+
            geom_hline(yintercept = 0, linetype = 2)
          zygo_temcap
          
          jpeg(filename = "temcap_zygo.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          zygo_temcap
          dev.off()
          
  #Tabela dos modelos de Tempo medio de captura
          #os modelos de cada são:
          aniso_temcap_lme
          zygo_temcap_lme
          
          #R2 para lme
          aniso_temcap_lme_r2 <- r.squaredGLMM(aniso_temcap_lme)
          zygo_temcap_lme_r2 <- r.squaredGLMM(zygo_temcap_lme)
          
          #tabela comparativa dos modelos
          stargazer( aniso_temcap_lme,
                     zygo_temcap_lme,
                     align = TRUE,
                     title = "Difference of Capture Time Models results", ci = TRUE,
                     ci.level = 0.90, model.numbers = FALSE,
                     notes = "Confidence Interval of 90 percent",
                     column.labels = c("Anisoptera", "Zygoptera"),
                     covariate.labels = c("Biomass", "Treatment: Warmed",
                                          "Constant"), 
                     dep.var.labels = "Diference of Capture Times",
                     add.lines = list(c("R² marginal",
                                        round(aniso_temcap_lme_r2[1,1],
                                              digits = 4),
                                        round(zygo_temcap_lme_r2[1,1],
                                              digits = 4)),
                                      c("R² conditional",
                                        round(aniso_temcap_lme_r2[1,2],
                                              digits = 4),
                                        round(zygo_temcap_lme_r2[1,2],
                                              digits = 4))))  
      
 

# Modelos de Tempo de Primeira Captura ------------------------------------
  #Belostomatidae -> exatamente igual ao médio(só capturaram 1 todos)
          
  #Notonectidae
    noto_temcap1_lme_int <- lme(log(tempocap1) ~ log(biomassa_mg)*tratamento,
                                  random = ~1|bloco, data = notonectidae, na.action = na.omit,
                                  weights = varIdent(form = ~ 1 | tratamento))
    summary(noto_temcap1_lme_int) #sem inter
    Anova(noto_temcap1_lme_int, type = "III")
    
    noto_temcap1_lme <- lme(log(tempocap1) ~ log(biomassa_mg) + tratamento,
                                random = ~1|bloco, data = notonectidae, na.action = na.omit,
                                weights = varIdent(form = ~ 1 | tratamento))
    summary(noto_temcap1_lme)
    Anova(noto_temcap1_lme)
    plot(noto_temcap1_lme)
    shapiro.test(resid(noto_temcap1_lme))
    plot(lm(log(notonectidae$tempocap1)~log(notonectidae$biomassa_mg)+notonectidae$tratamento))
    
    notonectidae$tempocap1[13] <- NA
    
    #figura
    noto_temcap1 <- model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$tempocap1, 
                              "Time of first capture [s], log10 scale", noto_temcap1_lme, "Notonectidae")+
      geom_hline(yintercept = 0, linetype = 2)+
      annotation_logticks() + theme(legend.position = c(0.8, 0.8))
    noto_temcap1
    
    jpeg(filename = "temcap1_noto.jpg", width = 2350, height = 1900, 
         units = "px", pointsize = 12, quality = 100,
         bg = "white",  res = 300)
    noto_temcap1
    dev.off()
    
  #Anisoptera
    aniso_temcap1_lme_int <- lme(log(tempocap1) ~ log(biomassa_mg)*tratamento,
                                 random = ~1|bloco, data = anisoptera, na.action = na.omit,
                                 weights = varIdent(form = ~ 1 | tratamento))
    summary(aniso_temcap1_lme_int)
    Anova(aniso_temcap1_lme_int, type = "III") #tem inter
    
    plot(aniso_temcap1_lme_int)
    plot(lm(log(anisoptera$tempocap1)~log(anisoptera$biomassa_mg)+anisoptera$tratamento))
    shapiro.test(resid(aniso_temcap1_lme_int))
    
    #Figura
    aniso_temcap1 <- model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$tempocap1, 
                               "Time of first capture [s], log10 scale", aniso_temcap1_lme, "Anisoptera")+
      geom_hline(yintercept = 0, linetype = 2)+
      annotation_logticks() + theme(legend.position = c(0.8, 0.8))
    aniso_temcap1
    
    jpeg(filename = "temcap1_aniso.jpg", width = 2350, height = 1900, 
         units = "px", pointsize = 12, quality = 100,
         bg = "white",  res = 300)
    aniso_temcap1
    dev.off()
    
  #Zygoptera
    zygo_temcap1_lme_int <- lme(log(tempocap1) ~ log(biomassa_mg)*tratamento,
                                random = ~1|bloco, data = zygoptera, na.action = na.omit,
                                weights = varIdent(form = ~ 1 | tratamento))
    summary(zygo_temcap1_lme_int)
    
    Anova(zygo_temcap1_lme_int, type = "III")
    
    zygo_temcap1_lme <- lme(log(tempocap1) ~ log(biomassa_mg) + tratamento,
                                random = ~1|bloco, data = zygoptera, na.action = na.omit,
                                weights = varIdent(form = ~ 1 | tratamento))
    summary(zygo_temcap1_lme)
    Anova(zygo_temcap1_lme)
    plot(zygo_temcap1_lme)
    shapiro.test(resid(zygo_temcap1_lme))
    plot(lm(log(zygoptera$tempocap1)~log(zygoptera$biomassa_mg)+zygoptera$tratamento))
    
    #Figura
    zygo_temcap1 <- model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$tempocap1, 
                                "Time of first capture [s], log10 scale", zygo_temcap1_lme, "Zygoptera")+
      geom_hline(yintercept = 0, linetype = 2)+
      annotation_logticks() + theme(legend.position = c(0.8, 0.8))
    zygo_temcap1
    
    jpeg(filename = "temcap1_zygo.jpg", width = 2350, height = 1900, 
         units = "px", pointsize = 12, quality = 100,
         bg = "white",  res = 300)
    zygo_temcap1
    dev.off()
    
  #Tabela geral dos modelos de 1a captura
    #os modelos de cada são:
    noto_temcap1_lme
    aniso_temcap1_lme_int
    zygo_temcap1_lme
    
    #R2 para lme
    noto_temcap1_lme_r2 <- r.squaredGLMM(noto_temcap1_lme)
    aniso_temcap1_lme_int_r2 <- r.squaredGLMM(aniso_temcap1_lme_int)
    zygo_temcap1_lme_r2 <- r.squaredGLMM(zygo_temcap1_lme)
    
    #tabela comparativa dos modelos
    stargazer( noto_temcap1_lme,
               aniso_temcap1_lme_int,
               zygo_temcap1_lme,
               align = TRUE,
               title = "First Capture Time Models results", ci = TRUE,
               ci.level = 0.90, model.numbers = FALSE,
               notes = "Confidence Interval of 90 percent",
               column.labels = c("Notonectidae",
                                 "Anisoptera", "Zygoptera"),
               covariate.labels = c("Log(Biomass)", "Treatment: Warmed",
                                    "Log(Biomass): Treatment",
                                    "Constant"), 
               dep.var.labels = "Log(First Capture Time)",
               add.lines = list(c("R² marginal",
                                  round(noto_temcap1_lme_r2[1,1],
                                        digits = 4),
                                  round(aniso_temcap1_lme_int_r2[1,1],
                                        digits = 4),
                                  round(zygo_temcap1_lme_r2[1,1],
                                        digits = 4)),
                                c("R² conditional",
                                  round(noto_temcap1_lme_r2[1,2],
                                        digits = 4),
                                  round(aniso_temcap1_lme_int_r2[1,2],
                                        digits = 4),
                                  round(zygo_temcap1_lme_r2[1,2],
                                        digits = 4))))
    





# Modelos de Taxa de Consumo (total presas corrigido) ----------------------------------------------
  #Belostomatidae
    belo_pres_lme_int <- lme(log(Totalpresascorrigido + 1) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = belostomatidae, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
    summary(belo_pres_lme_int) #inter não significativa
    
    Anova(belo_pres_lme_int, type = "III")
    
    shapiro.test(resid(belo_pres_lme_int))
    
    plot(belo_pres_lme_int)
      
    belo_pres_lme <-  lme(log(Totalpresascorrigido + 1) ~ log(biomassa_mg) + tratamento,
                          random = ~1|bloco, data = belostomatidae,
                          weights = varIdent(form = ~ 1 | tratamento), na.action = na.omit) 
    summary(belo_pres_lme)
    
    Anova(belo_pres_lme)
    
    shapiro.test(resid(belo_pres_lme))
    
    plot(lm(log(Totalpresascorrigido + 1) ~ log(biomassa_mg) + tratamento, data = belostomatidae))
    
    plot(belo_pres_lme)
    
    belostomatidae$totalpresasperdiag[4] <- NA
    belostomatidae$totalpresasperdiag[16] <- NA
    belostomatidae$totalpresasperdiag[27] <- NA
    
    shapiro.test(resid(belo_pres_lme)) #bem normal
    
    #figura
      belo_pres <- model_line(belostomatidae, belostomatidae$biomassa_mg,
                              (belostomatidae$Totalpresascorrigido+1),
                 "N° prey consumed/day", belo_pres_lme_int, "Belostomatidae")+
        geom_hline(yintercept = 1, linetype = 2) + annotation_logticks() + theme(legend.position = c(0.8, 0.8))
      belo_pres
      
      jpeg(filename = "pres_belo.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      belo_pres
      dev.off()
      
      
  #Notonectidae
      noto_pres_lme_int <- lme(log(Totalpresascorrigido) ~ log(biomassa_mg)*tratamento,
                               random = ~1|bloco, data = notonectidae,
                               weights = varIdent(form = ~ 1 | tratamento),
                               na.action = na.omit)
      summary(noto_pres_lme_int)
      
      Anova(noto_pres_lme_int, type = "III") #inter nao signi
      
      noto_pres_lme <- lme(log(Totalpresascorrigido) ~ log(biomassa_mg) + tratamento,
                           random = ~1|bloco, data = notonectidae, na.action = na.omit,
                           weights = varIdent(form = ~ 1 | tratamento))
      
      summary(noto_pres_lme)
      
      Anova(noto_pres_lme, type = "II")
      
      plot(lm(log(Totalpresascorrigido) ~ log(biomassa_mg)+ tratamento, data = notonectidae))
      
      shapiro.test(resid(noto_pres_lme))
      
      notonectidae$Totalpresascorrigido[16] <- NA
      notonectidae$Totalpresascorrigido[8] <- NA
      
        #Figura
          noto_pres <- model_line(notonectidae, notonectidae$biomassa_mg,
                                  (notonectidae$totalpresasperdiag+1),
                    "N° prey consumed/day", noto_pres_lme, "Notonectidae") +
            annotation_logticks() + theme(legend.position = c(0.8, 0.8))
          noto_pres
          
          jpeg(filename = "pres_noto.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          noto_pres
          dev.off()
          
          
      
  #Anisoptera
    aniso_pres_lme_int <- lme(log(Totalpresascorrigido) ~ log(biomassa_mg)*tratamento,
                              random = ~1|bloco, data = anisoptera, na.action = na.omit,
                              weights = varIdent(form = ~ 1 | tratamento))
    summary(aniso_pres_lme_int)
    
    Anova(aniso_pres_lme_int, type = "III") #sem inter
    
    aniso_pres_lme <- lme(log(Totalpresascorrigido) ~ log(biomassa_mg) + tratamento,
                          random = ~1|bloco, data = anisoptera, na.action = na.omit,
                          weights = varIdent(form = ~ 1 | tratamento))
    
    summary(aniso_pres_lme)
    
    Anova(aniso_pres_lme)
    
    plot(aniso_pres_lme)
    
    plot(lm(log(Totalpresascorrigido) ~ log(biomassa_mg) + tratamento, data = anisoptera))
    
    shapiro.test(resid(aniso_pres_lme))
    
    anisoptera$Totalpresascorrigido[28] <- NA #pesquisar melhor isso (tá dificil de normalizar)
    anisoptera$Totalpresascorrigido[30] <- NA
    anisoptera$Totalpresascorrigido[16] <- NA
    anisoptera$Totalpresascorrigido[29] <- NA
    
      #Figura
        aniso_pres <- model_line(anisoptera, anisoptera$biomassa_mg, (anisoptera$Totalpresascorrigido),
                   "N° prey consumed/day",
                   aniso_pres_lme_int, "Anisoptera") + annotation_logticks()+
                    theme(legend.position = c(0.8, 0.8))
        aniso_pres
        
        jpeg(filename = "pres_aniso.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        aniso_pres
        dev.off()
        
  #Zygoptera
    zygo_pres_lme_int <- lme(log(Totalpresascorrigido+1) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = zygoptera, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
    summary(zygo_pres_lme_int)
    
    Anova(zygo_pres_lme_int, type = "III")
    
    zygo_pres_lme <- lme(log(Totalpresascorrigido+1) ~ log(biomassa_mg) + tratamento,
                         random = ~1|bloco, data = zygoptera, na.action = na.omit,
                         weights = varIdent(form = ~ 1 | tratamento))
    
    summary(zygo_pres_lme)
    
    Anova(zygo_pres_lme, type = "II")
    
    plot(lm(log(Totalpresascorrigido+1) ~ log(biomassa_mg) + tratamento, data = zygoptera))
    
    shapiro.test(resid(zygo_pres_lme))
    
    zygoptera$Totalpresascorrigido[9] <- NA
    zygoptera$Totalpresascorrigido[31] <- NA
    zygoptera$Totalpresascorrigido[27] <- NA
    zygoptera$Totalpresascorrigido[4] <- NA
    zygoptera$Totalpresascorrigido[32] <- NA
    
      #Figura
       zygo_pres <-  model_line(zygoptera, zygoptera$biomassa_mg, (zygoptera$Totalpresascorrigido+1),
                   "N° prey consumed/day", zygo_pres_lme,
                   "Zygoptera") + annotation_logticks()+theme(legend.position = c(0.8, 0.8))
       zygo_pres
       
       jpeg(filename = "pres_zygo.jpg", width = 2350, height = 1900, 
            units = "px", pointsize = 12, quality = 100,
            bg = "white",  res = 300)
       zygo_pres
       dev.off()
       
       
       
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
        
  #Tabela
        #os modelos de cada são:
        belo_pres_lme
        noto_pres_lme
        aniso_pres_lme
        zygo_pres_lme
        
        #R2 para lme
        belo_pres_lme_r2 <- r.squaredGLMM(belo_pres_lme)
        noto_pres_lme_r2 <- r.squaredGLMM(noto_pres_lme)
        aniso_pres_lme_r2 <- r.squaredGLMM(aniso_pres_lme)
        zygo_pres_lme_r2 <- r.squaredGLMM(zygo_pres_lme)
        
        #tabela comparativa dos modelos
        stargazer( belo_pres_lme,
                   noto_pres_lme,
                   aniso_pres_lme,
                   zygo_pres_lme,
                   align = TRUE,
                   title = "Consumption rate (prey/day) Models results", ci = TRUE,
                   ci.level = 0.90, model.numbers = FALSE,
                   notes = "Confidence Interval of 90 percent",
                   column.labels = c("Belostomatidae", "Notonectidae",
                                     "Anisoptera", "Zygoptera"),
                   covariate.labels = c("Log(Biomass)", "Treatment: Warmed",
                                        "Constant"), 
                   dep.var.labels = c("", "Log(Prey/day)", "", ""),
                   add.lines = list(c("R² marginal",
                                      round(belo_pres_lme_r2[1,1],
                                            digits = 4),
                                      round(noto_pres_lme_r2[1,1],
                                            digits = 4),
                                      round(aniso_pres_lme_r2[1,1],
                                            digits = 4),
                                      round(zygo_pres_lme_r2[1,1],
                                            digits = 4)),
                                    c("R² conditional",
                                      round(belo_pres_lme_r2[1,2], 
                                            digits = 4),
                                      round(noto_pres_lme_r2[1,2],
                                            digits = 4),
                                      round(aniso_pres_lme_r2[1,2],
                                            digits = 4),
                                      round(zygo_pres_lme_r2[1,2],
                                            digits = 4))))

# Modelos de Taxa de Consumo (gravação) -----------------------------------
  #Belostomatidae
    belo_cons_lme_int <- lme(log(consumo_rel_1dia+1) ~ log(biomassa_mg)*tratamento,
                     random = ~1|bloco, data = belostomatidae, na.action = na.omit,
                     weights = varIdent(form = ~ 1 | tratamento))    
    summary(belo_cons_lme_int)
    
    Anova(belo_cons_lme_int, type = "III")
    
    belo_cons_lme <- lme(log(consumo_rel_1dia+1) ~ log(biomassa_mg) + tratamento,
                         random = ~1|bloco, data = belostomatidae, na.action = na.omit,
                         weights = varIdent(form = ~ 1 | tratamento))
    summary(belo_cons_lme)
    
    Anova(belo_cons_lme)
    
    plot(belo_cons_lme)
    
    shapiro.test(resid(belo_cons_lme))
    
    plot(lm(log(consumo_rel_1dia + 1) ~ log(biomassa_mg) + tratamento, data = belostomatidae))
    
    belostomatidae$consumo_rel_1dia[1] <- NA #com zeros precisa, com NA não
    belostomatidae$consumo_rel_1dia[11] <- NA
    belostomatidae$consumo_rel_1dia[6] <- NA
    belostomatidae$consumo_rel_1dia[8] <- NA
    belostomatidae$consumo_rel_1dia[26] <- NA
    
    #Figura
      belo_cons <- model_line(belostomatidae, belostomatidae$biomassa_mg, 
                  (belostomatidae$consumo_rel_1dia+1),
                 "Relative Consumption [gr prey/ gr predator, log10 scale",
                  belo_cons_lme, "Belostomatidae")+annotation_logticks()
      belo_cons
      
      jpeg(filename = "cons_belo.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      belo_cons
      dev.off()
      
  #Notonectidae
    noto_cons_lme_int <- lme(log(consumo_rel_1dia + 1) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = notonectidae, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
    summary(noto_cons_lme_int) #sem inter
      
    Anova(noto_cons_lme_int, type = "III")  
    
    noto_cons_lme <- lme(log(consumo_rel_1dia + 1) ~ log(biomassa_mg) + tratamento,
                             random = ~1|bloco, data = notonectidae, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
    summary(noto_cons_lme)
    
    Anova(noto_cons_lme)
    
    shapiro.test(resid(noto_cons_lme))
    
    plot(lm(log(consumo_rel_1dia + 1) ~ log(biomassa_mg) + tratamento, data = notonectidae))
     
    notonectidae$consumo_rel_1dia[23] <- NA 
    notonectidae$consumo_rel_1dia[21] <- NA 
    
    #figura
      noto_cons <- model_line(notonectidae, notonectidae$biomassa_mg, 
                              (notonectidae$consumo_rel_1dia+1),
                              "Relative Consumption [gr prey/ gr predator, log10 scale",
                              noto_cons_lme, "Notonectidae") + annotation_logticks()
      noto_cons
      
      jpeg(filename = "cons_noto.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      noto_cons
      dev.off()
      
  #Anisoptera
     aniso_cons_lme_int <-   lme(log(consumo_rel_1dia) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = anisoptera, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
     summary(aniso_cons_lme_int)
     Anova(aniso_cons_lme_int, type = "III") #sem inter
     
     anisoptera$consumo_rel_1dia[7] <- NA
     
     plot(aniso_cons_lme_int)
     
     aniso_cons_lme <-   lme(log(consumo_rel_1dia) ~ log(biomassa_mg) + tratamento,
                                 random = ~1|bloco, data = anisoptera, na.action = na.omit,
                                 weights = varIdent(form = ~ 1 | tratamento))
     summary(aniso_cons_lme)
     Anova(aniso_cons_lme)
     
     shapiro.test(resid(aniso_cons_lme))
     
     plot(lm(log(consumo_rel_1diaNA) ~ log(biomassa_mg) + tratamento, data = anisoptera))
     
     #Figura
       aniso_cons <- model_line(anisoptera, anisoptera$biomassa_mg, 
                               (anisoptera$consumo_rel_1dia+1),
                               "Relative Consumption [gr prey/ gr predator, log10 scale",
                               aniso_cons_lme, "Anisoptera") + annotation_logticks()
       aniso_cons
       
       jpeg(filename = "cons_aniso.jpg", width = 2350, height = 1900, 
            units = "px", pointsize = 12, quality = 100,
            bg = "white",  res = 300)
       aniso_cons
       dev.off()
      
  #Zygoptera
    zygo_cons_lme_int <- lme(log(consumo_rel_1dia+1) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = zygoptera, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
    summary(zygo_cons_lme_int )  
    Anova(zygo_cons_lme_int, type = "III") #sem inter
    
    zygo_cons_lme<- lme(log(consumo_rel_1dia+1) ~ log(biomassa_mg) + tratamento,
                             random = ~1|bloco, data = zygoptera, na.action = na.omit,
                             weights = varIdent(form = ~ 1 | tratamento))
    summary(zygo_cons_lme)
    Anova(zygo_cons_lme)
    
    plot(zygo_cons_lme)
    
    shapiro.test(resid(zygo_cons_lme))
    
    plot(lm(log(consumo_rel_1dia+1) ~ log(biomassa_mg) + tratamento, data = zygoptera))
    
    zygoptera$consumo_rel_1dia[1] <- NA
    zygoptera$consumo_rel_1dia[19] <- NA   
       
    #Figura
      zygo_cons <- model_line(zygoptera, zygoptera$biomassa_mg, 
                               (zygoptera$consumo_rel_1dia+1),
                               "Relative Consumption [gr prey/ gr predator, log10 scale",
                               zygo_cons_lme, "zygoptera") + annotation_logticks()
      zygo_cons
      
      jpeg(filename = "cons_zygo.jpg", width = 2350, height = 1900, 
           units = "px", pointsize = 12, quality = 100,
           bg = "white",  res = 300)
      zygo_cons
      dev.off()
      
  #Tabela dos modelos de taxa de consumo 1 dia
      #os modelos de cada são:
      belo_cons_lme
      noto_cons_lme
      aniso_cons_lme
      zygo_cons_lme
      
      #R2 para lme
      belo_cons_lme_r2 <- r.squaredGLMM(belo_cons_lme)
      noto_cons_lme_r2 <- r.squaredGLMM(noto_cons_lme)
      aniso_cons_lme_r2 <- r.squaredGLMM(aniso_cons_lme)
      zygo_cons_lme_r2 <- r.squaredGLMM(zygo_cons_lme)
      
      #tabela comparativa dos modelos
      stargazer( belo_cons_lme,
                 noto_cons_lme,
                 aniso_cons_lme,
                 zygo_cons_lme,
                 align = TRUE,
                 title = "Relative Consumption (g prey/g predator) Models results", ci = TRUE,
                 ci.level = 0.90, model.numbers = FALSE,
                 notes = "Confidence Interval of 90 percent",
                 column.labels = c("Belostomatidae", "Notonectidae",
                                   "Anisoptera", "Zygoptera"),
                 covariate.labels = c("Log(Biomass)", "Treatment: Warmed",
                                      "Constant"), 
                 dep.var.labels = c("Log(Consumption+1)", "Log(Consumption)",
                                    "Log(Consumption+1)"),
                 add.lines = list(c("R² marginal",
                                    round(belo_cons_lme_r2[1,1],
                                          digits = 4),
                                    round(noto_cons_lme_r2[1,1],
                                          digits = 4),
                                    round(aniso_cons_lme_r2[1,1],
                                          digits = 4),
                                    round(zygo_cons_lme_r2[1,1],
                                          digits = 4)),
                                  c("R² conditional",
                                    round(belo_cons_lme_r2[1,2], 
                                          digits = 4),
                                    round(noto_cons_lme_r2[1,2],
                                          digits = 4),
                                    round(aniso_cons_lme_r2[1,2],
                                          digits = 4),
                                    round(zygo_cons_lme_r2[1,2],
                                          digits = 4))))
      
       
# Modelos de Tempo de Manipulação da Presa --------------------------------
  #Belostomatidae - não pode tempo de manip - não foi medido
       
  #Notonectidae
        noto_temp_lme_int <- lme(log(tempomanipmedio) ~ log(biomassa_mg)*tratamento,
                                 random = ~1|bloco, data = notonectidae,
                                 na.action = na.omit, weights = varIdent(form = ~ 1 | tratamento))
        summary(noto_temp_lme_int) #sem interação
        
        Anova(noto_temp_lme_int, type = "III")
        
        noto_temp_lme <- lme(log(tempomanipmedio) ~ log(biomassa_mg) + tratamento,
                             random = ~1|bloco, data = notonectidae,
                             na.action = na.omit,  weights = varIdent(form = ~ 1 | tratamento))
        
        summary(noto_temp_lme)
        plot(noto_temp_lme)
        
        Anova(noto_temp_lme)
        
        plot(lm(log(tempomanipmedio+1) ~ log(biomassa_mg) + tratamento, data = notonectidae))
        
        shapiro.test(resid(noto_temp_lme))
        
        notonectidae$tempomanipmedio[14] <- NA #será que é muito outlier??
        notonectidae$tempomanipmedio[29] <- NA #tentar glmm nesses
        notonectidae$tempomanipmedio[17] <- NA
        notonectidae$tempomanipmedio[13] <- NA
        notonectidae$tempomanipmedio[21] <- NA
        
          #Figura
          noto_temp <- model_line(notonectidae, notonectidae$biomassa_mg, notonectidae$tempomanip1,
                       "Average Handling Time [seconds]",
                       noto_temp_lme, "Notonectidae") + annotation_logticks()
          noto_temp
          
          jpeg(filename = "temp_noto.jpg", width = 2350, height = 1900, 
               units = "px", pointsize = 12, quality = 100,
               bg = "white",  res = 300)
          noto_temp
          dev.off()
          
          
          
          
  #Anisoptera
    aniso_temp_lme_int <- lme(log(tempomanipmedio) ~ log(biomassa_mg)*tratamento,
                              random = ~1|bloco, data = anisoptera,
                              na.action = na.omit, weights = varIdent(form = ~ 1 | tratamento))
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
        aniso_temp <- model_line(anisoptera, anisoptera$biomassa_mg, anisoptera$tempomanip1,
                   "Average Handling Time [seconds]",
                   aniso_temp_lme, "Anisoptera") + annotation_logticks()
        aniso_temp
        
        jpeg(filename = "temp_aniso.jpg", width = 2350, height = 1900, 
             units = "px", pointsize = 12, quality = 100,
             bg = "white",  res = 300)
        aniso_temp
        dev.off()
        
  #Zygoptera
    zygo_temp_lme_int <- lme(log(tempomanipmedio) ~ log(biomassa_mg)*tratamento,
                             random = ~1|bloco, data = zygoptera,
                             na.action = na.omit, weights = varIdent(form = ~ 1 | tratamento))
    summary(zygo_temp_lme_int) #com interação
    
    Anova(zygo_temp_lme_int, type = "III")
    plot(zygo_temp_lme_int)
    shapiro.test(resid(zygo_temp_lme_int))
    
    plot(lm(log(tempomanipmedio) ~ log(biomassa_mg)*tratamento, data = zygoptera))
     
      #Figura
       zygo_temp <-  model_line(zygoptera, zygoptera$biomassa_mg, zygoptera$tempomanip1,
                   "Average Handling Time [seconds]",
                   zygo_temp_lme, "Zygoptera") + annotation_logticks()
       zygo_temp
       
       jpeg(filename = "temp_zygo.jpg", width = 2350, height = 1900, 
            units = "px", pointsize = 12, quality = 100,
            bg = "white",  res = 300)
       zygo_temp
       dev.off()
       
  #Tabelas gerais de modelos
       #os modelos de cada são:
       noto_temp_lme
       aniso_temp_lme
       zygo_temp_lme_int
       
       #R2 para lme
       noto_temp_lme_r2 <- r.squaredGLMM(noto_temp_lme)
       aniso_temp_lme_r2 <- r.squaredGLMM(aniso_temp_lme)
       zygo_temp_lme_int_r2 <- r.squaredGLMM(zygo_temp_lme_int)
       
       #tabela comparativa dos modelos
       stargazer( noto_temp_lme,
                  aniso_temp_lme,
                  zygo_temp_lme_int,
                  align = TRUE,
                  title = "Average Handling Time Models results", ci = TRUE,
                  ci.level = 0.90, model.numbers = FALSE,
                  notes = "Confidence Interval of 90 percent",
                  column.labels = c("Notonectidae",
                                    "Anisoptera", "Zygoptera"),
                  covariate.labels = c("Log(Biomass)", "Treatment: Warmed",
                                       "Treatment : Log(Biomass)",
                                       "Constant"), 
                  dep.var.labels = c("Log(Average Handling Time)"),
                  add.lines = list(c("R² marginal",
                                     round(noto_temp_lme_r2[1,1],
                                           digits = 4),
                                     round(aniso_temp_lme_r2[1,1],
                                           digits = 4),
                                     round(zygo_temp_lme_int_r2[1,1],
                                           digits = 4)),
                                   c("R² conditional",
                                     round(noto_temp_lme_r2[1,2],
                                           digits = 4),
                                     round(aniso_temp_lme_r2[1,2],
                                           digits = 4),
                                     round(zygo_temp_lme_int_r2[1,2],
                                           digits = 4))))
       
    
# Modelos de sobrevivência ------------------------------------------------
gghistogram(data = geral, x = geral$sobrev, fill = geral$tratamento)


# Modelos de Taxa de Consumo inicial com base em biomassa --------------------------------------
###Belostomatidae
        belo_cons_lme_int <- lme(log(consumo1dia_NA) ~ log(biomassa_mg)*tratamento,
                                     random = ~1|bloco, data = belostomatidae,
                                   na.action = na.omit)
        summary(belo_cons_lme_int)
        
        Anova(belo_cons_lme_int, type = "III") #Não sei o que fazer nesse caso, como usarei zero?
        
###Notonectidae
        noto_cons_lme_int <- lme(log(consumo1dia_NA) ~ log(biomassa_mg)*tratamento,
                                 random = ~1|bloco, data = notonectidae,
                                 na.action = na.omit, 
                                 weights = varIdent(form = ~ 1 | tratamento))
        summary(noto_cons_lme_int)
        
        Anova(noto_cons_lme_int, type = "III")
        
        
        noto_cons_lme <- lme(log(consumo1dia_NA) ~ log(biomassa_mg)+tratamento,
                                 random = ~1|bloco, data = notonectidae,
                                 na.action = na.omit, 
                                 weights = varIdent(form = ~ 1 | tratamento))
        summary(noto_cons_lme)
        
        Anova(noto_cons_lme, type = "II")
        
        shapiro.test(resid(noto_cons_lme))
        
        plot(noto_cons_lme)
        
###Anisoptera
        aniso_cons_lme_int <- lme(log(consumo1dia_NA) ~ log(biomassa_mg)*tratamento,
                                  random = ~1|bloco, data = anisoptera,
                                  na.action = na.omit, 
                                  weights = varIdent(form = ~ 1 | tratamento))
        summary(aniso_cons_lme_int)
        
        Anova(aniso_cons_lme_int, type = "III") #Marginalmente significativa inter
        
        plot(aniso_cons_lme_int)
        
        shapiro.test(resid(aniso_cons_lme_int))
        
###Zygoptera
        zygo_cons_lme_int <- lme(log(consumo1dia_NA) ~ log(biomassa_mg)*tratamento,
                                 random = ~1|bloco, data = anisoptera,
                                 na.action = na.omit, 
                                 weights = varIdent(form = ~ 1 | tratamento))
        summary(zygo_cons_lme_int)
        
        Anova(zygo_cons_lme_int, type = "III") #inter marginalmente signif
        
        plot(zygo_cons_lme_int)
        
        shapiro.test(resid(zygo_cons_lme_int))
        
      

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
      