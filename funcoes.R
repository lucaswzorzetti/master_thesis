##Funções que acompanham o scrypt do mestrado

  #Função para representar graficamente os modelos
  model_line <- function(dados, xaxis, yaxis, ynome, model, title = ""){
    ggplot(data = dados, mapping = aes(x=xaxis, 
                                       y=yaxis, fill=tratamento,
                                                shape = tratamento))+
      geom_smooth(method = "lm") + 
      geom_point(aes(fill=factor(tratamento)),
                 size=5, alpha = 0.8) +
      scale_fill_manual(values = c("#66cc33","#cc0000"), labels = c("Ambient", "Warmed"))+
      ylab(ynome)+
      xlab("Body Size [mg] \n log10 scale")+
      ggtitle(title)+
      labs(fill = "Treatment", shape = "Treatment")+
      theme_classic()+ scale_shape_manual(values = c(21, 22), labels = c("Ambient", "Warmed")) +
      theme(legend.title = element_text(size=20, face = "bold"))+
      theme(legend.text = element_text(size=12))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title.x = element_text(size = 18, face = "bold"),
            axis.title.y=element_text(size=18, face = "bold"))+
      theme(axis.text = element_text(color = "black",size = rel(1.3)))+
      theme(legend.background = element_rect(fill="transparent"))+
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
  }
  
  model_line_semlog <- function(dados, xaxis, yaxis, ynome, model, title = ""){
    ggplot(data = dados, mapping = aes(x=xaxis, 
                                       y=yaxis, fill=tratamento,
                                       shape = tratamento))+
      geom_smooth(method = "lm") + 
      geom_point(aes(fill=factor(tratamento)),
                 size=5, alpha = 0.8) +
      scale_fill_manual(values = c("#66cc33","#cc0000"), labels = c("Ambient", "Warmed"))+
      ylab(ynome)+
      xlab("Body Size [mg]")+
      ggtitle(title)+
      labs(fill = "Treatment", shape = "Treatment")+
      theme_classic()+ scale_shape_manual(values = c(21, 22), labels = c("Ambient", "Warmed")) +
      theme(legend.title = element_text(size=20, face = "bold"))+
      theme(legend.text = element_text(size=12))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title.x = element_text(size = 18, face = "bold"),
            axis.title.y=element_text(size=18, face = "bold"))+
      theme(axis.text = element_text(color = "black",size = rel(1.3)))+
      theme(legend.background = element_rect(fill="transparent"))+
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
  }
  
  model_line_noline <- function(dados, xaxis, yaxis, ynome, model, title = ""){
    ggplot(data = dados, mapping = aes(x=xaxis, 
                                       y=yaxis, fill=tratamento,
                                       shape = tratamento))+
      geom_point(aes(fill=factor(tratamento)),
                 size=5, alpha=0.8) +
      scale_fill_manual(values = c("#66cc33","#cc0000"), labels = c("Ambient", "Warmed"))+
      ylab(ynome)+
      xlab("Body Size [mg] \n log10 scale")+
      ggtitle(title)+
      labs(fill = "Treatment", shape = "Treatment")+
      theme_classic()+ scale_shape_manual(values = c(21, 22), labels = c("Ambient", "Warmed")) +
      theme(legend.title = element_text(size=20, face = "bold"))+
      theme(legend.text = element_text(size=12))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title.x = element_text(size = 18, face = "bold"),
            axis.title.y=element_text(size=18, face = "bold"))+
      theme(axis.text = element_text(color = "black",size = rel(1.3)))+
      theme(legend.background = element_rect(fill="transparent"))+
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
  }
  
  model_line_1line <- function(dados, xaxis, yaxis, ynome, model, title = ""){
    ggplot(data = dados, mapping = aes(x=xaxis, 
                                       y=yaxis))+
      geom_smooth(method = "lm") + 
      geom_point(aes(fill=factor(tratamento), shape = factor(tratamento)),
                 size=5, alpha = 0.8) +
      scale_fill_manual(values = c("#66cc33","#cc0000"), labels = c("Ambient", "Warmed"))+
      ylab(ynome)+
      xlab("Body Size [mg] \n log10 scale")+
      ggtitle(title)+
      labs(fill = "Treatment", shape = "Treatment")+
      theme_classic()+ scale_shape_manual(values = c(21, 22), labels = c("Ambient", "Warmed")) +
      theme(legend.title = element_text(size=20, face = "bold"))+
      theme(legend.text = element_text(size=12))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title.x = element_text(size = 18, face = "bold"),
            axis.title.y=element_text(size=18, face = "bold"))+
      theme(axis.text = element_text(color = "black",size = rel(1.3)))+
      theme(legend.background = element_rect(fill="transparent"))+
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
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


###Módulo matemático
modulo <- function(x){
  if(is.na(x)){NA}
  else if (x>=0){x}
  else{-x}
}

modulodif <- function(x, y){
  z = x-y
  if(is.na(x)){NA}
  else if(z>=0){z}
  else {-z}
}

#Função para produzir uma escala log para uso com números negativos
log10neg <- function(x){
  ifelse(test = is.na(x), yes = NA,
         no = ifelse(test = x == 0, yes = 0,
                     no = ifelse(test = x>0, yes = log10(x), no = (-log10(-x)))))
}
logneg <- function(x){
  ifelse(test = is.na(x), yes = NA,
         no = ifelse(test = x == 0, yes = 0,
                     no = ifelse(test = x>0, yes = log(x), no = (-log(-x)))))
}




