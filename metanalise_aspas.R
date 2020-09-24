## Carregar os dados do outro scrypt
library(ape)
library(caper)

#Calculando cohen f^2
##Crescimento
###Belostomatidae
incluso <- lme(taxacrescimento ~ tratamento + biomassa_mg,
    random = ~ 1|bloco, method = "ML",
    data = belostomatidae)
excluso <- lme(taxacrescimento ~ biomassa_mg,
         random = ~ 1|bloco, method = "ML",
         data = belostomatidae)

r.squaredGLMM(incluso)
r.squaredGLMM(excluso)

r.squaredGLMM((incluso))[1,1]
belo_cohen_f2 <- (r.squaredGLMM((incluso))[1,1] - 
               r.squaredGLMM(excluso)[1,1])/(1- r.squaredGLMM((incluso))[1,1])
belo_cohen_f2


###Notonectidae
incluso <- lme(taxacrescimento ~ tratamento + biomassa_mg,
               random = ~ 1|bloco, method = "ML",
               data = notonectidae)
excluso <- lme(taxacrescimento ~ biomassa_mg,
               random = ~ 1|bloco, method = "ML",
               data = notonectidae)

r.squaredGLMM(incluso)
r.squaredGLMM(excluso)

r.squaredGLMM((incluso))[1,1]
noto_cohen_f2 <- (r.squaredGLMM((incluso))[1,1] - 
               r.squaredGLMM(excluso)[1,1])/(1- r.squaredGLMM((incluso))[1,1])
noto_cohen_f2

###Anisoptera
incluso <- lme(taxacrescimento ~ tratamento + biomassa_mg,
               random = ~ 1|bloco, method = "ML",
               data = anisoptera)
excluso <- lme(taxacrescimento ~ biomassa_mg,
               random = ~ 1|bloco, method = "ML",
               data = anisoptera)

r.squaredGLMM(incluso)
r.squaredGLMM(excluso)

r.squaredGLMM((incluso))[1,1]
aniso_cohen_f2 <- (r.squaredGLMM((incluso))[1,1] - 
               r.squaredGLMM(excluso)[1,1])/(1- r.squaredGLMM((incluso))[1,1])
aniso_cohen_f2

###Zygoptera
incluso <- lme(taxacrescimento ~ tratamento + biomassa_mg,
               random = ~ 1|bloco, method = "ML",
               data = zygoptera)
excluso <- lme(taxacrescimento ~ biomassa_mg,
               random = ~ 1|bloco, method = "ML",
               data = zygoptera)

r.squaredGLMM(incluso)
r.squaredGLMM(excluso)

r.squaredGLMM((incluso))[1,1]
zygo_cohen_f2 <- (r.squaredGLMM((incluso))[1,1] - 
               r.squaredGLMM(excluso)[1,1])/(1- r.squaredGLMM((incluso))[1,1])
zygo_cohen_f2

###Resumo
ef_growth <- data.frame(taxon = c("Belostomatidae", "Notonectidae",
                                 "Anisoptera", "Zygoptera"),
                        ef = c(belo_cohen_f2, noto_cohen_f2, 
                               aniso_cohen_f2, zygo_cohen_f2),
                        ic_low = c(0.14762, -0.04592, -0.07091, -0.07832),
                        ic_up = c(2.09852, 0.06769, 0.14299, 0.18661)
                        )
ef_growth



ef_growth %>%  ggplot(aes(x = taxon, y = ef,
                          ymin = ic_low,
                          ymax = ic_up, colour = taxon)) +
  theme_classic() +
  geom_hline(yintercept = 0.35, linetype = 2) +
  geom_hline(yintercept = 0.15, linetype = 2) +
  geom_hline(yintercept = 0.02, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_pointrange() + geom_point(size = 4) +
  ylab("Cohen's f² of the Growth rate")

####### Consumo ####
#### Belostomatidae
incluso <- lme(Totalpresascorrigido ~ biomassa_mg + tratamento,
              random = ~1|bloco,
              data = belostomatidae, method = "ML", na.action = na.omit,
              weights = varIdent(form = ~1|tratamento))
excluso <- lme(Totalpresascorrigido ~ biomassa_mg,
               random = ~1|bloco,
               data = belostomatidae, method = "ML", na.action = na.omit)

belos_cons_f2 <- cohen_f2(excluso, incluso)

###Notonectidae

noto_cons_incluso <- lme(Totalpresascorrigido ~ biomassa_mg + tratamento,
                          random = ~1|bloco,
                          data = notonectidae, method = "ML", 
                          na.action = na.omit)
r.squaredGLMM(noto_cons_incluso)
r.squaredGLMM(noto_cons_excluso)

noto_cons_excluso <- lme(Totalpresascorrigido ~ biomassa_mg,
                         random = ~1|bloco,
                         data = notonectidae, method = "ML", 
                         na.action = na.omit)
noto_cons_f2 <- cohen_f2(noto_cons_excluso, noto_cons_incluso)


#### Aniso
aniso_cons_incluso <- lme(Totalpresascorrigido ~ log(biomassa_mg) + tratamento,
                          random = ~1|bloco,
                          data = anisoptera, method = "ML", na.action = na.omit)
aniso_cons_excluso <- lme(Totalpresascorrigido ~ log(biomassa_mg),
                          random = ~1|bloco,
                          data = anisoptera, method = "ML",
                          na.action = na.omit)

aniso_f2 <- cohen_f2(aniso_cons_excluso, aniso_cons_incluso)


###Zygoptera
zygo_cons_incluso <- lme(Totalpresascorrigido ~ biomassa_mg + tratamento,
                          random = ~1|bloco,
                          data = zygoptera, method = "ML",
                         na.action = na.omit)
r.squaredGLMM(zygo_cons_excluso)
r.squaredGLMM(zygo_cons_incluso)


zygo_cons_excluso <- lme(Totalpresascorrigido ~ biomassa_mg,
                          random = ~1|bloco,
                          data = zygoptera, method = "ML",
                          na.action = na.omit)

zygo_cons_f2 <- cohen_f2(zygo_cons_excluso, zygo_cons_incluso)
zygo_cons_f2




##Resumo
ef_cons <- data.frame(taxon = c("Belostomatidae", "Notonectidae",
                                  "Anisoptera", "Zygoptera"),
                        ef = c(belo_cohen_f2, noto_cohen_f2, 
                               aniso_cohen_f2, zygo_cohen_f2),
                        ic_low = c(0.55666, -0.05848, -0.08182, ),
                        ic_up =  c(4.43027,  0.10069,  0.21997, )
)
ef_cons



