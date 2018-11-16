# MÉDIA
media=apply(dados_trabalho[,2:5],1,mean)
amplitude=apply(dados_trabalho[,2:5],1,max)-apply(dados_trabalho[,2:5],1,min)
mediamedia=(mean(media)) #--> x barra
amplitudemedia=mean(amplitude) #--> r barra

#CARTA AMPLITUDE R
LCLr= amplitudemedia*0
UCLr=amplitudemedia*2.282

#CARTA AMPLITUDE X
#n= n de dados da amostra
LCLx= mediamedia -0.729 * amplitudemedia
UCLx= mediamedia + 0.729 * amplitudemedia

#CARTA DE CONTROLO R --> variabilidade das amostras
plot(amplitude, xlab = "Amostra", ylab= "Amplitude", type = 'b', pch=19, main="Carta R",ylim=c(0,10))
lines(rep(UCLr,100), col="red")  
lines(rep(LCLr,100), col="red") 
lines(rep(amplitudemedia,100) ,col="red") 
lines(rep(amplitude,1000) )


#CARTA DE CONTROLO X --> média das amostras 
plot(media, xlab = "Média", ylab= "Amostra", type = 'b', pch=18, main="Carta X", ylim= c(7,8))
lines(rep(UCLx,100), col="cyan")  #--> 100 porque são 100 amostras
lines(rep(LCLx,100) ,col="cyan") 
lines(rep(mediamedia,100) ,col="cyan")
# O processo está sob controlo uma vez que a amostras estão toda dentro dos limites de tolerância


#RCPk
LSL <- 6
USL <- 8
sigma <- amplitudemedia/2.059  # --> 2.059 valor tabelado ( isto é uma formula pag.60)
rcpk_min <- min((USL-mediamedia)/(3*sigma),(mediamedia-LSL)/(3*sigma)) # RCPk muito abaixo do RCPk 
#especificado pelo fornecedor. O que significa que o processo produz muitas peças não conforme

########## Parte 2 ################
### Alinea 1 ###
p1 <- 0.01
p2 <- 0.06
alpha <- 0.05
beta <- 0.10
gl <- 2 * (c1+1)

resultados <- 0
plano <- function(p1,p2,alpha,beta){
  c1 <- 1:50
  gl <- 2 * (c1+1)
  q <- qchisq(1-beta, gl)/qchisq(alpha, gl)
  cotimo = min(c1[q <= p2/p1])
  #determinar n otimo
  li = qchisq(1-beta, 2*(cotimo+1))/(2*p2)
  ls = qchisq(alpha, 2*(cotimo+1))/(2*p1)
  notimo = ceiling(li)
  resultados <- list(c = cotimo, n = notimo )
  return(resultados)
}

teste <- plano(p1,p2,alpha,beta)

# Alinea 2
p <- c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.1)
N <- 1000 # Tamanho do lote

# Tipo B, recorrendo a distribuicao binomial Xd - binomial(n,p)
# Pa(p) = P(Xd <= c) ; Xd = Numero de pecas defeituosas em n
PaB = pbinom(teste$c, teste$n, p)
plot(p, PaB, type = "l", col = "blue",main = "Curva OC", 
     xlab = "ProporÃ§Ã£o de defeituosos no lote", 
     ylab = "Probabilidade de aceitaÃ§Ã£o")

# Alinea 3
M <- 150
n <- 10
pa <- rep(0,length(p))
k <- 0
#probabilidade de aceitaÃ§ao do lote para diferentes valores de p
for(i in p){
  D <- rep(0,M)
  for(j in 1:M){
    lote <- rbern(n,0.1)
    amostra <- sample(lote,n)
    if(sum(amostra) >= c){
      D[j] <- 1
    }
  }
  k <- k + 1
  pa[k] <- sum(D)/M
}

