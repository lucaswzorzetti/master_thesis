idades <- seq(from=55, to=120, by = 1)
idades

expec_vida(
  ifelse()
)


f_prev <- function(id, tempo_contr, homem = TRUE, m_exp = 72.5, f_exp = 79.6){
  ifelse(test = homem==TRUE, 
         yes = ((tempo_contr*0.31)/(72.5-id))*(1 + ((id+tempo_contr*0.31)/100)),
         no = ((tempo_contr*0.31)/(79.6-id))*(1 + ((id+tempo_contr*0.31)/100)))
}

f_prev(55, 35)


