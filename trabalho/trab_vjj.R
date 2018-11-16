N<-850
p1<- 0.01
a<-0.05
p2<-0.06
b<-0.10

library(statip)

lote<-function(p1,p2,a,b){
  
  #determinar c
  c <- 0:50
  gl <- 2*(c+1) #graus de liberdade
  q <- qchisq(1-b,gl) / qchisq(a,gl)
  
  aux <- q < p2/p1    #cria um vetor de 0 e 1 para verificar a condição q < p2/p1
  
  c_otimo <- min(c[aux]) #guarda o indice mais baixo onde o valor foi 1
  c_otimo
  
  #determinar n
  gl_1 <- 2 * (c_otimo+1)
  gl_2 <- 2 * (c_otimo+1)
  
  li <- qchisq(1-b,gl_1)/(2*p2)
  ls <- qchisq(a,gl_2)/(2*p1)
  
  n_otimo = ceiling(li)
  n_otimo
  
  valores<-c(c_otimo,n_otimo)
  return(valores)
  
}

valores<-lote(p1,p2,a,b)
c<-valores[1]
n<-valores[2]
c
n

# alinea 2

# Tipo B -  Recorrendo a distribuição binomial  
# Xd ~Binomial(n,p)
# Pb(p) = P(Xd<=c)

p<-c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1)

Pb<- pbinom(c,n,p) # teorico
plot(p,Pb,type="l", main="Curva OC tipo B", ylab="Probabilidade de aceitação", xlab="Proporção de defeituosos no lote")

# alinea 3

M <- 150
Pa <- rep(0,length(p))
k <- 0

for(i in p){
  
  decisao <- rep(0,M)
  
  for(j in 1:M) {
    
    # Simule lote (N)
    lote_simulado <- rbern(N,i)
    lote_simulado
    # Amostra de tamanho n
    amostra_simulado <- sample(lote_simulado,n)
    
    xd <- sum(amostra_simulado)
    
    if(xd <= c) {
      
      decisao[j] <- 1
    }
    
  }
  
  k <- k+1
  Pa[k] <- sum(decisao) / M;
  
}
Pa # empirico
