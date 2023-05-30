
tamaño<- function(n) {
  xbarra <- 4.8  
  desviacion <- 0.5   
  mu <- 5      
  z <- (xbarra - mu) / (desviacion / sqrt(n))
  p<-pnorm(z)  
  return(p)
}
p1<-tamaño(n=5) 
p2<-tamaño(n=15) 
p3<-tamaño(n=50) 
 
p1
p2
p3
 
grafica <- function(n,mensaje){
  xbarra <- 4.8  
  desviacion <- 0.5   
  mu <- 5   
  z <- (xbarra - mu) / (desviacion / sqrt(n))
  p<-pnorm(z) 
  z
  valorp<-pnorm(abs(z))
  valorp
  #Region critica
  alpha<-0.05
  limizq<- qnorm(alpha)
  limdere<- -limizq
  limizq
  limdere
  ejex<-seq(-3,3,0.01)
  alpha=0.05
  #### l�mites de la regi�n de aceptaci�n:
  ejex <- seq(-3,3,0.01)
  ejey <- dnorm(ejex)
  plot(ejex,ejey,type='l', lwd=3)
  polygon(c(3,-3,ejex),c(0,0,ejey),col='red')
  abline(v=0)
  ####################
  ejex <- seq(-3,3,0.01)
  ejey <- dnorm(ejex)
  plot(ejex,ejey,type='l', lwd=3)
  lizq <- qnorm(alpha)
  aux.x <- ejex[ejex<=lizq]
  aux.y <- dnorm(aux.x)
  polygon(c(3,-3,aux.x,lizq),c(0,0,aux.y,0),col='red')
  
  abline(v=0,col=c('purple'))
  abline(v=z,col=c('green'))
  mtext(mensaje,
        side=3, line=3)
  legend("topright",
         inset=0.05,
         legend=c("valor de xbarra","valor de miu",'alpha'),
         
         col=c('green','purple','red'),
         lwd=3) 
}

grafica(n=50,mensaje="Distribución muestral con n=50")
grafica(n=15,mensaje="Distribución muestral con n=15")
grafica(n=5,mensaje="Distribución muestral con n=5")

##intervalo de confianza del 95%

ic90<-1.645
ic95<-1.96

intervalo95<-function(n,ic) {
  error<-(ic*(sigma/sqrt(n)))
  lim1<-xbarra-error
  lim2<-xbarra+error
  resultado<-data.frame(n,xbarra,error,lim1,lim2)
  return(resultado)
  
}
      
intervalo95(n=5,ic=ic95)  
intervalo95(n=15,ic=ic95)
intervalo95(n=50,ic=ic95)

intervalo95(n=5,ic=ic90)  
intervalo95(n=15,ic=ic90)
intervalo95(n=50,ic=ic90)
