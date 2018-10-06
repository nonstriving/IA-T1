source("Estado.R")

## Classe e métodos para o problema dos 3 Missionários e 3 Canibais
QuebraCabeca <- function(desc = NULL, pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("QuebraCabeca", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.QuebraCabeca = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.QuebraCabeca <- function(obj) {
  cat("(M C B): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.QuebraCabeca <- function(atual){
  
  ## Se uma peca nao estiver na sua posicao objetivo,
  ## somar a distancia entre sua posicao nos eixos x
  ## e y e incrementar a heuristica
  
  if(is.null(atual$desc))
    return(Inf)
  h(obj) = P11 + P12 + P21 + P22 + P31 + P32 + P41 + P42 + P51 + P52 + P61 + P62 + P71 + P72 + P81 + P82
  return(sum(h(obj)))
}

geraFilhos.QuebraCabeca <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()

  desc <- obj$desc
  
  bAtual <- as.numeric(desc[16])
  
  bNovo <- as.numeric(bAtual != 1)
  
  ## gera filhos usando todos os operadores  
  if(bAtual == 1){
    
    operadores <- list(c(2,0,bAtual), c(0,2,bAtual), c(1,1,bAtual), c(1,0,bAtual), c(0,1,bAtual))
    
    filhosDesc <- lapply(operadores, function(op) desc-op)
    
  } else {
    
    operadores <- list(c(1,0,bNovo), c(-1,0,bNovo), c(0,1,bNovo), c(0,-1,bNovo))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
  }
  
  ## verifica estados filhos incompatíveis com o problema  
  incompativeis <- sapply(1:length(filhosDesc),
                    function(i) {
                      fDesc <- filhosDesc[[i]]
                      if((any(fDesc[1:2] > 2)) || ## Se algum eixo ultrapassar a posicao 2
                         (any(fDesc[1:2] < 0)))   ## Se algum eixo ultrapassar a posicao 0
                        i ## é incompatível: retorna índice
                      else
                        0 ## senão é compatível
                    })
  
  ## mantém no vetor apenas os que são incompatíveis
  incompativeis <- incompativeis[incompativeis != 0]
  
  ## remove estados filhos incompatíveis
  filhosDesc <- filhosDesc[-incompativeis]
  
  ## gera os objetos QuebraCabeca para os filhos
  for(filhoDesc in filhosDesc){
    filho <- QuebraCabeca(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}