#Plano de amostragem por atributos
#Constante de aceita��o: c=5
#Tamanho do lote: N=1000
#Tamanho da amostra: n=100
#Xd : N� de pe�as defeituosas em n (na amostra)

N <- 1000
n <- 100
c <- 5
p <- seq(0.005, 0.2, by=0.005) # propor��o de defeitosos no lote

#Curva OC tipo A (Distribui��o hipergeometrica)
nDef <- N*p
nNDef <- N- nDef
PaA <- phyper(c, nDef, nNDef, n) #probabilidade de aceita��o A
lines(p,PaA, col="red")

#Desenho do gr�fico
plot(p,PaA, type="l", main="Curva OC", xlab="Porpo��o de n�o conformes no lote", ylab="P aceita��o do lote")

#Curva OC tipo B (Distribui��o binomial)
PaB <- pbinom(c,n,p) #probabilidade de aceita��o B
lines(p,PaB, col="red") #Desenha a linha sob o grafico j� existente

#Ex 3
#os valores c e n foram escolhidos sem criterio
#a seguir v�o ser calculados os valores de c e n adquados

#p1 -> pior qualidade a que o processo pode operar e que ainda conduz a uma probabilidade elevada de aceita��o.
#p2 -> valor da qualidade a partir do qual se considera que o produto n�o � aceit�vel 
#a (alfa) -> risco do produtor
#b (beta) -> risco do consumidor

p1 <- 0.03
p2 <- 0.1
a <- 0.06
b <- 0.1

#determinar c
c <- 0:50
gl <- 2*(c+1) #graus de liberdade
q <- qchisq(1-b,gl) / qchisq(a,gl)

aux <- q < p2/p1    #cria um vetor de 0 e 1 para verificar a condi��o q < p2/p1

c_otimo <- min(c[aux]) #guarda o indice mais baixo onde o valor foi 1

#determinar n
gl_1 <- 2 * (c_otimo+1)
gl_2 <- 2 * (c_otimo+1)

li <- qchisq(1-b,gl_1)/(2*p2)
ls <- qchisq(a,gl_2)/(2*p1)

n_otimo = ceiling(li)

#Ex 4

c = c_otimo
n = n_otimo
N = 1000
p <- seq(0.005, 0.2, by=0.005) # propor��o de defeitosos no lote

#Nova Curva OC tipo A (Distribui��o hipergeometrica)
nDef <- N*p
nNDef <- N- nDef
PaA_2 <- phyper(c, nDef, nNDef, n) #probabilidade de aceita��o A

lines(p,PaA_2, col="blue")


legend("topright", legend=c("Curva OC tipo A", "Curva OC tipo B", "Curva OC tipo B\ncom valores\notimizados"),fill=c("black", "red", "blue"), bty="n")
