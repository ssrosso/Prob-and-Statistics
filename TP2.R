N <- 1
capital = 100

#a
una_jugada <- function(){
  z <- runif(N)
  for (i in 1:N){
    if(capital>0){
      if(z[i]<=18/38){
        capital <- capital + 1
      }else capital <- capital -1
    }
  }
  return(capital)
}

#b
cantidad_jugadas <- function(){
  c <- 0
  while (capital > 0){
    k <- runif(1)
    if(k<=18/38){
      capital <- capital + 1
    }else{
      capital <- capital - 1}
    c <- c+1
  }
  return(c)
}

#c

veces <- 10000
lista <- rep(0,veces)
promedio <- function(){
  for(i in 1:veces){
    lista[i] <- cantidad_jugadas()
  }
  return(sum(lista)/veces)
}