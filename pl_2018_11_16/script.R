#X1 - binomial (n,p) c1
#x2 - binomial (100,p)
  n1 <- 50
  n2 <- 100
  c1 <- 1
  c2 <- 3
  p<-seq (0.0001, 0.2, by=0.001)
   #probabilidade de x1
  pa1 = pbinom (c1, n1, p)
plot (p, pa1, type="l", main= "Curvas OC - plano de amostragem duplo e simples a azul")

pa2<- dbinom(2,n1,p)*pbinom(1,n2,p)+dbinom(3,n1,p)*pbinom(0,n2,p)
points (p,pa2,col=2, type="l")
pa= pa1+pa2

points (p,pa,col=3, type= "l")

#plano de amostrage simples


n=72
c=2
pasimples= pbinom (c,n,p)
points(p,pasimples,col=4, type="l")