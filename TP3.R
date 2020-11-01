library(ggplot2)
n = 1000
t= 60
X = rweibull(n, 3, t+5)

Y = rep(0,n)
for(i in 1:n){
  if(X[i] >= 60){
    Y[i] = 50*X[i]-2800
  }else{
    Y[i] = 950
  }
}
z <- 100

SList <- rep(0,1000)
for (j in 1:1000){
  X2 = rweibull(n, 3, t+5)
  y2 = rep(0,z)
  for(i in 1:z){
    if(X2[i] >= 60){
      y2[i] = 50*X[i]-2800
    }else{
      y2[i] = 950
    }
    
  }
  SList[j] = sum(y2)
  
}

     
#hist(SList, main = "Ejercicio7", 
#     xlab = "Costo total", ylab = "",
#     col = "ivory")


#estandarizarYGraficar<-hist(SList, breaks=10, col="gray", xlab="Costo total",
#        main="Histograma con Curva Normal")
#xfit<-seq(min(SList),max(SList),length=40)
#yfit<-dnorm(xfit,mean=mean(SList),sd=sd(SList))
#yfit <- yfit*diff(h$mids[1:2])*length(SList)
#lines(xfit, yfit, col="black", lwd=2)



#ECDF=ecdf(Y)
#plot(ECDF,col="red",lwd=3,xlab="Y",ylab="")
#points(Y,rep(0,n),col="blue",pch=20,cex=2)
#segments(Y,rep(0,n),Y,sapply(Y,ECDF),col="gray",lwd=3)