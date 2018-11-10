################################################################################
#                               Projeto de CONEP                               #
#                                                                              #
# Realizado por:                                                               #
#               Bárbara Santos  1161033                                        #
#               Jorge Gabriel   1160929                                        #
#               Nuno Dinis      1161042                                        #
#                                                                              #
################################################################################

# AVISO:
# Antes da execução deste script, deve-se definir o diretório de trabalho para a
# pasta do projeto a fim de evitar problemas de execução.

# DEPENDÊNCIAS:
# Para a execução deste script é necessario ter o package "readxl" bem como as
# as suas dependências instalados no sistema.


#------------------------------ Início de Script ------------------------------#


#limpa o ambiente do R
rm(list=ls())

#Seed do script
set.seed(492365)

#Constantes
A2 <- 0.729
D4 <- 2.282
D3 <- 0

#Define o diretório de trabalho
#dir <- getScriptPath()
#dir
#setwd(dir)

#import de bibliotecas
library(readxl)

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

#Carta X
UCL.x <- media.global + A2 * amplitude.media
LCL.x <- media.global - A2 * amplitude.media
CL.x  <- media.global

plot(media.linha,type = "b", main="Carta de Controlo X", xlab = "Numero da Amostra", ylab = "Espessura do Vidro")
lines(rep(UCL.x, N), col="red")
lines(rep(LCL.x, N), col="red")
lines(rep(CL.x, N), col="blue")

#remover os pontos, calcular TUDO de novo e apresentar o novo grafico
#msm para a amplitude
# ucl amplitude.media * D4
# lcl amplitude.media * D3
# cl  amplitude.media