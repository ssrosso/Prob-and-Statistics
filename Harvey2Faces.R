# Two exercises where a third coin has two faces (P(x) = 1), and based on the conditional results
# this model should determine the special coin, based on #N flips of each coin

N <- 1000

### MONEDAS
x <- runif(N)
y <- runif(N)
moneda1 <- 1*(x<0.5)
moneda2<- 1*(y<0.5)
moneda3 <- rep(1,N)


### EJERCICIO A
prob_cara <- function(moneda1, moneda2, moneda3, N){
  z <- runif(N)
  resultados <- rep(0,N)
  for (i in 1:N){
    if(z[i]<=1/3){
      resultados[i] <- moneda1[i]
    }else if(z[i]<=2/3){
      resultados[i] <- moneda2[i]
    }else resultados[i] <- moneda3[i]
  }
return (sum(resultados)/N)
}


### EJERCICIO B
prob_moneda1y2 <- function(moneda1, moneda2, moneda3, N){
  c <- N
  z <- runif(N)
  resultados <- rep(0,N)
  for (i in 1:N){
    if(z[i]<=1/3){
      monedaRandom <- "moneda1"
    }else if(z[i]<=2/3){
      monedaRandom <- "moneda2"
    }else monedaRandom <- "moneda3"
      if ((monedaRandom == "moneda1") & (moneda1[i]==1)){
        resultados[i] <- 1
      }else if ((monedaRandom == "moneda2") & (moneda2[i]==1)){
        resultados[i] <- 1
      }else if (monedaRandom == "moneda3"){
        resultados[i] <- 0
      }else{
        c <- c-1
      }
  }
return(sum(resultados)/c)  
}
