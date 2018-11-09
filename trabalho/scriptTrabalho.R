#limpa o ambiente do R
rm(list=ls())

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

plot(media.linha, title="Carta de Controlo", xlab = "Numero da Amostra", ylab = "Espessura do Vidro", ylim=c(LCL-0.5, UCL+0.5))
lines(rep(UCL,N), col="red")
lines(rep(LCL,N), col="red")
lines(rep(CL,N), col="blue")

 


