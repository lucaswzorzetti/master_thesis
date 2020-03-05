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
      
      cresc_belo <- ggplot() +
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
                       colour = tratamento,
                       shape = tratamento))+
        theme_classic(base_size = 16)+
        theme(legend.position="bottom")+
        scale_shape_discrete(guide = F)+
        scale_color_manual(name = "Condição Experimental",
                           values = c("green", "red"),
                           labels = c("Temperatura Ambiente",
                                      "Temperatura Ambiente + 4°C"))+
        scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35, 40),
                           limits = c(4, 40.000001))+
        xlab("Biomassa [mg]") + ylab("Taxa de Crescimento") +
        geom_hline(yintercept = 1, linetype =2)
      cresc_belo
      
      
      cresc_belo_noleg <- ggplot() +
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
        geom_point(data = belostomatidae, size = 3, show.legend = FALSE,
                   aes(x = biomassa_mg,
                       y = taxacrescimento,
                       colour = tratamento))+
        scale_color_manual(values = c("green", "red"),
                           labels = c("Temperatura Ambiente",
                                      "Temperatura Ambiente + 4°C"))+
        scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35, 40),
                           limits = c(4, 40.000001))+
        labs(color = "Tratamento")+
        xlab("Biomassa [mg]") + ylab("Taxa de Crescimento") +
        geom_hline(yintercept = 1, linetype =2)+
        theme_classic(base_size = 22)
      
      
      cresc_belo_noleg
      
      #salvando (salvar com 600 dpi, e )
        png(filename = "crescimento_belo.jpg", width = 10000, height = 10000, 
             units = "px",
             bg = "white",  res = 600)
        cresc_belo
        dev.off()
      
      
  #Anisoptera
    aniso_cresc_int <- lme((taxacrescimento) ~ biomassa_mg*tratamento, random = ~1|bloco,
                            data = anisoptera, method = "ML", na.action = na.omit,
                           weights = varIdent(form = ~1|tratamento)) 
      summary(aniso_cresc_int)  
      Anova(aniso_cresc_int, type = "III") #inter
      plot(aniso_cresc_int)
      CookD(aniso_cresc_int)
      
      shapiro.test(resid(aniso_cresc_int))
      
    aniso_cresc <- lme(taxacrescimento ~ biomassa_mg + tratamento, random = ~1|bloco,
                       data = anisoptera,  na.action = na.omit,
                       weights = varIdent(form = ~1|tratamento))
      summary(aniso_cresc)
      Anova(aniso_cresc, type = "II")
      plot(aniso_cresc)
      
      shapiro.test(resid(aniso_cresc))
      
      CookD(aniso_cresc)
      
      r.squaredGLMM(aniso_cresc)
      
      AICctab(aniso_cresc, aniso_cresc_int, base = T, weights = T) #inter preferivel pelo AICc
      
      anisoptera$taxacrescimento[4] <- NA #cookD > 1
      anisoptera$taxacrescimento[16] <- NA #preciso apenas no modelo simples
      
      
    #pressupoições
      qqPlot(resid(aniso_cresc)) #
      leveneTest(resid(aniso_cresc)~anisoptera$bloco) #
      leveneTest(resid(aniso_cresc)~anisoptera$tratamento) # homocedastico
      
    #plot                  
      cresc_aniso <- plot_lucas(aniso_cresc, dados = anisoptera,
                                eixo_y = anisoptera$taxacrescimento) +
        ylab("Taxa de Crescimento") +
        scale_x_continuous(breaks = c(0, 25, 50, 75,100, 125, 150, 175)) +
        geom_hline(yintercept = 1, linetype = 2)
      cresc_aniso

  #Zygoptera
      zygo_cresc_int <- lme(taxacrescimento ~ biomassa_mg*tratamento, random = ~1|bloco,
                        data = zygoptera, method = "ML", na.action = na.omit,
                        weights = varIdent(form = ~1|tratamento))
       
        summary(zygo_cresc_int)  
        Anova(zygo_cresc_int, type = "III") #inter
        plot(zygo_cresc_int)
        CookD(zygo_cresc_int)
        shapiro.test(resid(zygo_cresc_int))             
      
      zygo_cresc <- lme(taxacrescimento ~biomassa_mg+tratamento, random = ~1|bloco,
                        data = zygoptera, method = "ML", na.action = na.omit,
                        weights = varIdent(form = ~1|tratamento))       
        summary(zygo_cresc)
        Anova(zygo_cresc, type = "II")
        plot(zygo_cresc)
        CookD(zygo_cresc)
        shapiro.test(resid(zygo_cresc))
      
        r.squaredGLMM(zygo_cresc)
        
        zygoptera$taxacrescimento[18] <- NA #cook D >0.5
        
        AICctab(zygo_cresc, zygo_cresc_int, base = T, weights = T)     
        leveneTest(resid(zygo_cresc)~zygoptera$tratamento, na.action = na.omit())
        
        #plot
          plot_lucas(model = zygo_cresc, dados = zygoptera,
                   eixo_y = zygoptera$taxacrescimento) +
            ylab("Taxa de Crescimento") + geom_hline(yintercept = 1, linetype = 2)
            scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35))
            
  #Notonectidae
    noto_cresc_int <- lme(taxacrescimento ~ biomassa_mg + tratamento,
                          random = ~1|bloco, data = notonectidae,
                          method = "ML", na.action = na.omit,
                          weights = varIdent(form = ~1|tratamento))
      summary(noto_cresc_int)    
      Anova(noto_cresc_int, type = "III")
      plot(noto_cresc_int)
      CookD(noto_cresc_int)
      shapiro.test(resid(noto_cresc_int))
      
    noto_cresc <- lme(taxacrescimento ~biomassa_mg + tratamento, random = ~1|bloco,
                      data = notonectidae, method = "ML", na.action = na.omit,
                      weights = varIdent(form = ~1|tratamento))
      summary(noto_cresc)
      Anova(noto_cresc, type = "II")
      plot(noto_cresc)
      CookD(noto_cresc)
      shapiro.test(resid(noto_cresc))
      
      notonectidae$taxacrescimento[1] <- NA
      #notonectidae$taxacrescimento[15] <- NA  #cookD > 0.5
      
    #plot
        plot_lucas(model = noto_cresc, dados = notonectidae,
                   eixo_y = notonectidae$taxacrescimento) +
          ylab("Taxa de Crescimento") + geom_hline(yintercept = 1, linetype = 2) +
          scale_x_continuous(breaks = c(0, 6, 8, 10, 12, 14, 16, 18, 20),
                             limits = c(6, 21)) +
          scale_y_continuous(limits = c(0.985, 1.03))
      
#### Taxa de Consumo #####
  #Belostomatidae
    belos_cons_int <- lme(Totalpresascorrigido ~biomassa_mg*tratamento,
                          random = ~1|bloco,
                          data = belostomatidae, method = "ML", na.action = na.omit,
                          weights = varIdent(form = ~1|tratamento))
      summary(belos_cons_int)  
      Anova(belos_cons_int, type = "III") 
      plot(belos_cons_int)
      CookD(belos_cons_int)
      shapiro.test(resid(belos_cons_int))
      
    belos_cons <- lme(Totalpresascorrigido ~ biomassa_mg + tratamento,
                      random = ~1|bloco,
                      data = belostomatidae, method = "ML", na.action = na.omit,
                      weights = varIdent(form = ~1|tratamento))
      summary(belos_cons) 
      Anova(belos_cons, type = "III") 
      plot(belos_cons)
      CookD(belos_cons)
      shapiro.test(resid(belos_cons))
      
      qqPlot(resid(belos_cons))
      
    #plot
      plot_lucas(model = belos_cons, dados = belostomatidae,
                 eixo_y = belostomatidae$Totalpresascorrigido) +
        ylab("Taxa de Consumo\n[presas/dia]") +
        scale_x_continuous(breaks = c(0, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,
                                      26, 28, 30, 32, 34, 36, 38))+
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75))

  #Anisoptera
      aniso_cons_int <- lme(Totalpresascorrigido ~ log(biomassa_mg)*tratamento,
                            random = ~1|bloco,
                            data = anisoptera, method = "ML", na.action = na.omit,
                            weights = varIdent(form = ~1|tratamento))
        summary(aniso_cons_int)  
        Anova(aniso_cons_int, type = "III") 
        plot(aniso_cons_int)
        CookD(aniso_cons_int)
        shapiro.test(resid(aniso_cons_int))
      
      aniso_cons <- lme(Totalpresascorrigido ~ log(biomassa_mg) + tratamento,
                        random = ~1|bloco,
                        data = anisoptera, method = "ML", na.action = na.omit,
                        weights = varIdent(form = ~1|tratamento))
        summary(aniso_cons)
        Anova(aniso_cons, type = "II") 
        plot(aniso_cons)
        CookD(aniso_cons) #28 com cookd >1.2
        shapiro.test(resid(aniso_cons))
        
          qqPlot(resid(aniso_cons)) # só um fora
          AICctab(aniso_cons, aniso_cons_int, base = T, weights = T) #melhor com a inter
          
        anisoptera$Totalpresascorrigido[28] <- NA
        
      #plot
        plot_lucas(model = aniso_cons_int, dados = anisoptera,
                   eixo_y = anisoptera$Totalpresascorrigido) +
          ylab("Taxa de Consumo\n[presas/dia]")+
          scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180))+
          scale_y_continuous(breaks = c(1.0, 1.25, 1.5, 1.75, 2.0, 2.25)) +
          ggtitle(expression(paste(italic("Erythrodiplax sp.")))) +
          xlab("Biomassa [mg]")
        
  #Zygoptera
    zygo_cons_int <- lme(Totalpresascorrigido ~ biomassa_mg*tratamento,
                         random = ~1|bloco,
                         data = zygoptera, method = "ML", na.action = na.omit,
                         weights = varIdent(form = ~1|tratamento))
      summary(zygo_cons_int)
      Anova(zygo_cons_int, type = "III")
      plot(zygo_cons_int)
      CookD(zygo_cons_int) #28 com cookd >1.2
      shapiro.test(resid(zygo_cons_int))
      
    zygo_cons <- lme(Totalpresascorrigido ~ biomassa_mg + tratamento,
                        random = ~1|bloco,
                        data = zygoptera, method = "ML", na.action = na.omit,
                        weights = varIdent(form = ~1|tratamento))
      summary(zygo_cons)
      Anova(zygo_cons, type = "II") 
      plot(zygo_cons)
      CookD(zygo_cons)#  7 cook D >0.5
      shapiro.test(resid(zygo_cons))
      
      qqPlot(resid(zygo_cons)) # só um fora
      qqPlot(resid(zygo_cons_int))
      AICctab(zygo_cons, zygo_cons_int, base = T, weights = T)
      
      #zygoptera$Totalpresascorrigido[7] <- NA #melhor ficar com o inter e não tirar
    
      #plot
        plot_lucas(model = zygo_cons_int, dados = zygoptera,
                   eixo_y = zygoptera$Totalpresascorrigido) +
          ylab("Taxa de Consumo\n[presas/dia]") +
          scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16,
                                        16, 18, 20, 22, 24, 26, 28, 30, 32, 34))+
          scale_y_continuous(breaks = c(0, 0.25,0.5, 0.75, 1.0, 1.25,
                                        1.5, 1.75, 2.0, 2.25)) +
          ggtitle(expression(paste(italic("Nehalennia sp.")))) +
          geom_hline(yintercept = 0, linetype = 2) +
          xlab("Biomassa [mg]") #px: 800 x 600
        
  #Notonectidae
    noto_cons_int <- lme(Totalpresascorrigido ~ biomassa_mg*tratamento,
                         random = ~1|bloco,
                         data = notonectidae, method = "ML", na.action = na.omit,
                         weights = varIdent(form = ~1|tratamento))
      summary(noto_cons_int)
      Anova(noto_cons_int, type = "III")
      plot(noto_cons_int)
      CookD(noto_cons_int) # 15 com cookD > 2!!
      shapiro.test(resid(noto_cons_int))
      
    noto_cons <- lme(Totalpresascorrigido ~ biomassa_mg + tratamento,
                     random = ~1|bloco,
                     data = notonectidae, method = "ML", na.action = na.omit,
                     weights = varIdent(form = ~1|tratamento))
      summary(noto_cons)
      Anova(noto_cons, type = "III")
      plot(noto_cons)
      CookD(noto_cons) # 15 com cD > 1.5
      shapiro.test(resid(noto_cons))
      
        qqPlot(resid(noto_cons))
        AICctab(noto_cons, noto_cons_int, base = T, weights = T)
        
        notonectidae$Totalpresascorrigido[15] <- NA
      
      #plot
        plot_lucas(model = noto_cons, dados = notonectidae,
                   eixo_y = notonectidae$Totalpresascorrigido)
      
      
          
      
        
        
        
        
        
        
        
        