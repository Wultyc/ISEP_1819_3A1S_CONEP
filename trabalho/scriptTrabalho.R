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
media.linha <- apply(dados_trabalho[,2:4], 1, mean)
#View(media.linha)

#Media Geral
media.global <- mean(media.linha)


