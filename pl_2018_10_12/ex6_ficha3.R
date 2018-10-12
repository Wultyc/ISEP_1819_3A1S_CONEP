#X é o valor da caracteristica
mean = 10.0
sd = 0.02

#a)
x = 10.03
X = pnorm(x, mean, sd, lower.tail=FALSE)
X

#b)
#P(X <= x) = 0.95

qnorm(0.95, mean, sd) #Quantil onde a probabilidade é 0.95