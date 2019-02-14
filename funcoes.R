##Funções que acompanham o scrypt do mestrado

  #Função para representar graficamente os modelos
  model_line <- function(dados, xaxis, yaxis, ynome, model){
    ggplot(data = dados, mapping = aes(x=xaxis, 
                                       y=yaxis, fill=tratamento,
                                                shape = tratamento))+
      geom_smooth(method = "lm") + 
      geom_point(aes(fill=factor(tratamento)),
                 size=5) +
      scale_fill_manual(values = c("#66cc33","#cc0000"), labels = c("Ambient", "Warmed"))+
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 10),
                    labels = trans_format("log10", math_format(10^.x)))+
      scale_y_log10(breaks = round(seq(0.91, to = max(yaxis), length.out = 6), digits = 2),
                    labels = round(seq(0.91, to = max(yaxis), length.out = 6), digits = 2))+
      ylab(ynome)+
      xlab("Body Size [mg], log10 scale")+
      labs(fill = "Treatment", shape = "Treatment")+
      theme_classic()+ scale_shape_manual(values = c(21, 22), labels = c("Ambient", "Warmed")) +
      theme(legend.title = element_text(size=20, face = "bold"))+
      theme(legend.text = element_text(size=12))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title.x = element_text(size = 20, face = "bold"),
            axis.title.y=element_text(size=20, face = "bold"))+
      theme(axis.text = element_text(color = "black",size = rel(1.3)))+
      theme(legend.position = c(0.8, 0.8))
  } 

#teste
model_line(belostomatidae, belostomatidae$biomassa_mg, belostomatidae$taxacrescimento,
           ynome = "Taxa de Crescimento", model = belo_cresc_lme_int) +
  geom_hline(yintercept = 1, linetype = 2)

#Detach package
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}


r.squaredGLMM(fit1.lme) #para r2 de lme no pacote ‘MuMIn