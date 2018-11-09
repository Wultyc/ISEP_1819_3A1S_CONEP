#limpa o ambiente do R
rm(list=ls())

#Seed do script
set.seed(492365)

#Define o diretório de trabalho
#dir <- getScriptPath()
#dir
#setwd(dir)

#import de bibliotecas
library(readxl)

#dados guardados no excel
dados_trabalho = read_excel("dados_trabalho.xlsx")
#View(dados_trabalho)

#Media de cada amostra
media.linha <- apply(dados_trabalho[1:50,2:4], 1, mean)
#View(media.linha)

#Media Geral
media.global <- mean(unlist(media.linha))
media.global

#Numero de amostras
N <- NROW(media.linha)


LCL <- 6
UCL <- 8
CL <- 7

ucl <- rep(UCL,N)
lcl <- rep(LCL,N)
cl <- rep(CL,N)

plot(media.linha,type = "b", main="Carta de Controlo", xlab = "Numero da Amostra", ylab = "Espessura do Vidro", ylim=c(LCL-0.5, UCL+0.5))
lines(ucl, col="red")
lines(lcl, col="red")
lines(cl, col="blue")

#Existe um ponto fora dos limites
#É necessário remover o ponto 8

#determina quais os valores fora dos limites
fora.lim1 <- media.linha > ucl
fora.lim2 <- media.linha < lcl
fora.lim <- fora.lim1 + fora.lim2

#encontra os indices
idx.fora <- which(fora.lim == 1)

#remove os indices indicados
media.linha <- media.linha[-idx.fora]

#calcula uma nova média global


