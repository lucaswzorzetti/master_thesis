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
      xlab("Body Size [mg]\nlog10 scale")+
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
      xlab("Body Size [mg]\nlog10 scale")+
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
  
  model_line_semlog_1line <- function(dados, xaxis, yaxis, ynome, model, title = ""){
    ggplot(data = dados, mapping = aes(x=xaxis, 
                                       y=yaxis))+
      geom_smooth(method = "lm") + 
      geom_point(aes(fill=factor(tratamento), shape = tratamento),
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

logzero <- function(x, b = 10){
  ifelse(test = is.na(x), yes = NA,
         no = ifelse(test = x == 0, yes = 0,
                     no = log(x, base = b)))
}

logplus <- function(x, b = 10){
  ifelse(test = is.na(x), yes = NA,
         no = ifelse(test = x == 0, yes = 0,
                     no = log((x+1), base = b)))
}

##Função signed rank
signed_rank <- function(x){
  sign(x)*rank(abs(x))
}

signed_rank(c(2, -4,5,32,34,12))

#####Nova função gráfica #####

plot_lucas <- function(model, dados, eixo_y){
  predict_fun <- ggpredict(model, terms = c("biomassa_mg", "tratamento"))
  predict_fun
  
  ambiente <- predict_fun %>% filter(group == "Ambiente")
  aquecido <- predict_fun %>% filter(group == "Aquecido")
  
  ggplot() +
    geom_line(data = ambiente, aes(x = x, y = predicted),
              color = "green", size = 3) +
    geom_line(data = aquecido, aes(x = x, y = predicted),
              color = "red", size = 3) +
    geom_ribbon(data = ambiente, 
                aes(x = x, ymin = predicted - std.error,
                    ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.2) +
    geom_ribbon(data = aquecido, 
                aes(x = x, ymin = predicted - std.error,
                    ymax = predicted + std.error), 
                fill = "lightcoral", alpha = 0.2) +
    geom_point(data = dados, aes(x = biomassa_mg,
                                 y = eixo_y,
                                 colour = tratamento,
                                 shape = tratamento), size = 3)+
    theme_classic (base_size = 16)+
    scale_shape_discrete (guide = F)+
    theme(legend.position = "bottom")+
    scale_color_manual(name = "Condição Experimental",
                       values = c("green", "red"),
                       labels = c("Temperatura Ambiente",
                                  "Temperatura Ambiente + 4°C"))+
    xlab("Biomassa [mg]")
  
}
plot_lucas(model = belo_cresc_int2, dados = belostomatidae, eixo_y = belostomatidae$taxacrescimento) 
