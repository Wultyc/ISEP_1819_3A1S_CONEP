set.seed(12345)
dados <- matrix(data = NA, 100, 36)


for(i in 1:36){
	dados[,i] = rnorm(100,0,1.6)
}

View(dados)

media_amostra = apply(dados, 2, mean)

media = mean(dados)
desvio_padrao = sd(dados)

plot(media_amostra,xlab="Amostra",ylab="Desvio Médio", ylimit=c(-0.5,0.5), type="b")
lines(rep(-0.3,36), col="red")
lines(rep(0.3,36), col="red")
lines(rep(0,36), col="blue")