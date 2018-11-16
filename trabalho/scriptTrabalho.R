####################################################################################################
#                                         Projeto de CONEP                                         #
#                                                                                                  #
# Realizado por:                                                                                   #
#               Bárbara Santos  1161033                                                            #
#               Jorge Gabriel   1160929                                                            #
#               Nuno Dinis      1161042                                                            #
#                                                                                                  #
####################################################################################################

# AVISO:
# Antes da execução deste script, deve-se definir o diretório de trabalho para a
# pasta do projeto a fim de evitar problemas de execução.

# DEPENDÊNCIAS:
# Para a execução deste script é necessario ter o package "readxl" bem como as
# as suas dependências instalados no sistema.


#---------------------------------------- Início de Script ----------------------------------------#

#Incluir script com uma função
source("define_plano_amostragem.R")

#limpa o ambiente do R
rm(list=ls())

#Seed do script
set.seed(492365)

#Constantes
A2 <- 0.729
D4 <- 2.282
D3 <- 0

#Parametros de configuração
abr.graf <- 0.25 #Limites do gráfico
limite.itrs <- 5 #Limite de iterações de correção

#import de bibliotecas
library(readxl)

#--------------------------------------------- Início ---------------------------------------------#

#dados guardados no excel
dados_trabalho = read_excel("dados_trabalho.xlsx")
N <- NROW(dados_trabalho[1])
#View(dados_trabalho)

#Media
media.linha <- apply(dados_trabalho[1:N,2:5], 1, mean)
#View(media.linha)
media.global <- mean(unlist(media.linha))
media.global

#Amplitude
amplitude.linha <- apply(dados_trabalho[1:N,2:5], 1, max) - apply(dados_trabalho[1:N,2:5], 1, min)
amplitude.media <- mean(amplitude.linha)

#-------------------------------------------- Carta X ---------------------------------------------#

UCL.x <- media.global + A2 * amplitude.media
LCL.x <- media.global - A2 * amplitude.media
CL.x  <- media.global

plot(media.linha,type = "b", main="Carta de Controlo X", xlab = "Número da Amostra", ylab = "Espessura do Vidro")

lines(rep(UCL.x, N), col="red")
lines(rep(LCL.x, N), col="red")
lines(rep(CL.x, N), col="blue")

# Existem pontos fora dos limites, a carta X tem de ser recalculada

#determina quais os valores fora dos limites
fora.lim1 <- media.linha > UCL.x
fora.lim2 <- media.linha < LCL.x
fora.lim <- fora.lim1 + fora.lim2

#encontra os indices e remove-os
idx.fora <- which(fora.lim == 1)
if(NROW(idx.fora) > 0){
  media.linha <- media.linha[-idx.fora]
}

#Recalcula tudo de novo enquanto houver pontos fora do controlo 
itr <- 0 #Variavel de controlo de iterações

while(sum(fora.lim) > 0 && itr < limite.itrs){
  media.global <- mean(unlist(media.linha))
  
  UCL.x <- media.global + A2 * amplitude.media
  LCL.x <- media.global - A2 * amplitude.media
  CL.x  <- media.global
  
  #determina quais os valores fora dos limites
  fora.lim1 <- media.linha > UCL.x
  fora.lim2 <- media.linha < LCL.x
  fora.lim <- fora.lim1 + fora.lim2
  
  #encontra os indices e remove-os
  idx.fora <- which(fora.lim == 1)
  if(NROW(idx.fora) > 0){
    media.linha <- media.linha[-idx.fora]
  }
  
  itr <- itr+1 #Incrementa a variavel de iteração
}

#Apresenta algumas informações
paste("UCL: ", UCL.x, sep=" ")
paste("LCL: ", LCL.x, sep=" ")
paste("CL: ", CL.x, sep=" ")
paste("Nº de Iterações Necessárias: ", itr, sep=" ")
paste("Pontos fora dos limites: ", sum(fora.lim), sep=" ")

#Apresenta o gráfico
plot(media.linha,type = "b", main=paste("Carta de Controlo X - ",itr,"ª Iteração", sep=" "), xlab = "Número da Amostra", ylab = "Espessura do Vidro", ylim = c(LCL.x-abr.graf,UCL.x+abr.graf))

lines(rep(UCL.x, N), col="red")
lines(rep(LCL.x, N), col="red")
lines(rep(CL.x, N), col="blue")

#-------------------------------------------- Carta R ---------------------------------------------#

UCL.r <- amplitude.media * D4
LCL.r <- amplitude.media * D3
CL.r  <- amplitude.media

plot(amplitude.linha,type = "b", main="Carta de Controlo R", xlab = "Número da Amostra", ylab = "Espessura do Vidro")

lines(rep(UCL.r, N), col="red")
lines(rep(LCL.r, N), col="red")
lines(rep(CL.r, N), col="blue")

# Existem pontos fora dos limites, a carta R tem de ser recalculada

#determina quais os valores fora dos limites
fora.lim1 <- amplitude.linha > UCL.r
fora.lim2 <- amplitude.linha < LCL.r
fora.lim <- fora.lim1 + fora.lim2

#encontra os indices e remove-os
idx.fora <- which(fora.lim == 1)
if(NROW(idx.fora) > 0){
  amplitude.linha <- amplitude.linha[-idx.fora]
}

#Recalcula tudo de novo enquanto houver pontos fora do controlo 
itr <- 0 #Variavel de controlo de iterações

while(sum(fora.lim) > 0 || itr > limite.itrs){
  amplitude.media <- mean(amplitude.linha)
  
  UCL.r <- amplitude.media * D4
  LCL.r <- amplitude.media * D3
  CL.r  <- amplitude.media
  
  #determina quais os valores fora dos limites
  fora.lim1 <- amplitude.linha > UCL.r
  fora.lim2 <- amplitude.linha < LCL.r
  fora.lim <- fora.lim1 + fora.lim2
  
  #encontra os indices e remove-os
  idx.fora <- which(fora.lim == 1)
  if(NROW(idx.fora) > 0){
    amplitude.linha <- amplitude.linha[-idx.fora]
  }
  
  itr <- itr+1 #Incrementa a variavel de iteração
}

#Apresenta algumas informações
paste("UCL: ", UCL.r, sep=" ")
paste("LCL: ", LCL.r, sep=" ")
paste("CL: ", CL.r, sep=" ")
paste("Nº de Iterações Necessárias: ", itr, sep=" ")
paste("Pontos fora dos limites: ", sum(fora.lim), sep=" ")

#Apresenta o gráfico
plot(amplitude.linha,type = "b", main=paste("Carta de Controlo R - ",itr,"ª Iteração", sep=" "), xlab = "Número da Amostra", ylab = "Espessura do Vidro", ylim = c(LCL.r-abr.graf,UCL.r+abr.graf))

lines(rep(UCL.r, N), col="red")
lines(rep(LCL.r, N), col="red")
lines(rep(CL.r, N), col="blue")

#-------------------------------------------- Capcidade ---------------------------------------------#

#RCPk
LSL <- 6
USL <- 8
sigma <- amplitude.media/2.059
rcpk <- min((USL-media.global)/(3*sigma),(media.global-LSL)/(3*sigma))
paste("RCPk:",rcpk,sep=" ")

#-------------------------------------------- Amostragem ---------------------------------------------#

p1 <- 0.01  #p1 -> pior qualidade a que o processo pode operar e que ainda conduz a uma probabilidade elevada de aceitação.
p2 <- 0.06  #p2 -> valor da qualidade a partir do qual se considera que o produto não é aceitável 
a <- 0.050  #a (alfa) -> risco do produtor
b <- 0.100  #b (beta) -> risco do consumidor

nc <- define_plano_amostragem(p1,p2,a,b,N)
