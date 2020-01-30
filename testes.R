#Testando novas analises

belostomatidae <- belostomatidae %>%  mutate(tempocap1_noNA = ifelse(test = is.na(tempocap1),
                                                 yes = 5400,
                                                 no = tempocap1))


belo_temcap1_lme <- lmer(tempocap1_noNA ~ biomassa_mg + tratamento + (1|bloco),
                             data = belostomatidae, na.action = na.omit, REML = F)
summary(belo_temcap1_lme) #inter sig

Anova(belo_temcap1_lme, type = "II") #inter sig

shapiro.test(resid(belo_temcap1_lme)) #normality of residuals

plot(sort(cooks.distance(belo_temcap1_lme)))

belostomatidae_new %>% ggplot(aes(x = bloco, y = biomassa_mg, fill = tratamento)) +geom_point()


belo_temcap1_noNA <- belostomatidae %>% ggplot(aes(x = tratamento, y = tempocap1_noNA,
                                                   fill = tratamento)) +
  geom_boxplot() + geom_point()
  
belo_temcap1_noNA


belostomatidae_new <- belostomatidae %>% filter(biomassa_mg < 100)
View(belostomatidae_new)


belostomatidae_new <- belostomatidae_new %>%  mutate(tempocap1_noNA = ifelse(test = is.na(tempocap1),
                                                                     yes = 5400,
                                                                     no = tempocap1))


belonew_temcap1_lme <- lmer(tempocap1_noNA ~ biomassa_mg + tratamento + (1|bloco),
                         data = belostomatidae_new, na.action = na.omit, REML = F)
summary(belo_temcap1_lme) #inter sig

Anova(belonew_temcap1_lme, type = "II") #inter sig

shapiro.test(resid(belonew_temcap1_lme)) #normality of residuals

plot(sort(cooks.distance(belonew_temcap1_lme)))


belonew_temcap1 <- model_line_semlog(belostomatidae_new, log(belostomatidae_new$biomassa_mg),
                           (log(belostomatidae_new$tempocap1_noNA)), 
                           "Time of 1º capture [s]",
                           belo_temcap1_lme, "Belostomatidae")+
  theme(axis.text = element_text(size = 18, colour = "black"),
        legend.position = "none")
belonew_temcap1
