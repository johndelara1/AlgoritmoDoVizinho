validacaoDosParametros <- function(){
  i = 1
  perigo = FALSE
  while(i <=  6){
    if(t[2,i] > 1 | t[2,i] < 0){
      perigo = TRUE
      print("Perigo")
    }
    print(sprintf("rodando %*s: %s",0,j,i))
    i = i + 1
  }
  return(perigo)
}

validarTabelaComRestricoes <- function(flag, t){
  flag1 = flag
  if(flag[1,6] < t[1,6]){
    if(!validacaoDosParametros()){
      if(!travar_algoritmo()){
        flag = t
      }else{
        
      }
    }
  }
  return(flag)
}

travar_algoritmo(flag, t)

#### _______ Desenvolvendo pra cima _______####
qualRetornoDaCarteira <- function(t){
  i = 1
  while (i <=length(t)-1) {
    if(i == 1){
      multiplica <- t[1 ,i]*t[2 ,i]
    }else{
      multiplica1 <- t[1 ,i]*t[2 ,i]
      multiplica <- rbind(multiplica, multiplica1)
    }
    i = i + 1
  }
  return(sum(multiplica))
}
totalCarteiraEscolhida <- function(t){
  i = 1
  while (i <= length(t)-1) {
    if(i == 1){
      soma <- t[2 ,i]
    }else{
      soma1 <- t[2 ,i]
      soma <- rbind(soma, soma1)
    }
    i = i + 1
  }
  return(sum(soma))
}

#p = parametro
#t = tabela
#tabela <- gerarMelhorTabela(carteiraEscolhida)

fazerPrimeiraLinha  <- function(t, i, j, p){
  if(i != j){
    t[2,i] = t[2,i] + p
    t[2,i+1] = t[2,i+1] - p
    t[1,6] <- qualRetornoDaCarteira(t)
    t[2,6] <- totalCarteiraEscolhida(t)
  }else{
    t[2,j] = t[2,j] + p
    t[2,j+1] = t[2,j+1] - p
    t[1,6] <- qualRetornoDaCarteira(t)
    t[2,6] <- totalCarteiraEscolhida(t)
  }
  return(t)
}


gerarMelhorTabela <- function(carteiraEscolhida){
  nomeDasColunas <- c("A","B","C","D","E", "Retorno Carteira")
  RetornoAnual <-  c(0.42292, 0.25685, 0.04128,  0.05812, 0.01731, 0.20979)
  t <- as.data.frame(rbind(RetornoAnual, carteiraEscolhida))
  colnames(t) <- nomeDasColunas
  return(t)
}

validado_flag_sao_iguais <- function(flag, validado){
  i = 1
  passou = FALSE
  while(i <= length(flag)){
    if(flag[1,i]  != validado[1,i]  | flag[2,i] != validado[2,i]){
      passou = TRUE
    }
    i = i + 1
  }
  return(passou)
}
