####Pacotes a carregar####

####Dados####


#### Análise dos dados - pareada apenas e com modelos lme ####
  #####Taxa de Crescimento ####
    ##Belostomatidae
      #Effect Size
        efeito_aq %>% group_by(suborfam) %>% summarise(cohen_d = ((mean(taxacrescimento.am)-mean(taxacrescimento.aq))/sd(taxacrescimento.am)))

      #Normalidade da diferença  
        ggdensity(belostomatidae_ef$ef_growth_rate) #parece normal
        ggqqplot(belostomatidae_ef$ef_growth_rate) #parece bastante normal
        
        shapiro.test(belostomatidae_ef$ef_growth_rate) #normalzaço
        
        
                                                       