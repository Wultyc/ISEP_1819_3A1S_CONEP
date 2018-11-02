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
amplitude.linha <- apply(dados_trabalho[,2:4], 1, max) - apply(dados_trabalho[,2:4], 1, min)
#View(dados_trabalho)
#View(amplitude.linha)

#Media de cada amostra
media.linha <- apply(dados_trabalho[1:50,2:4], 1, mean)
#View(media.linha)

#Media Geral
media.global <- mean(unlist(media.linha))
media.global


