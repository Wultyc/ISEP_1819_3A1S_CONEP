## PL 2018-09-21 Ex1

#a. O vetor 4 8 2. 
x = c(4,8,2);
x

#b. Selecionar o primeiro e terceiro elemento do vetor acima. 
x[3]

#c. O vetor com a sequencia de valores -3 -2 -1 0 1 2 3. 
y = seq(-3,3)
y

#d. O vetor com a sequencia de valores 2.4 3.4 4.4 5.4 6.4 7.4 8.4 9.4 10.4. 
z = seq(2.4,10.4,by=1)
z

#e. O vetor com a sequencia de valores 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35. 
xx = seq(1,35,by=2)
xx

#f. O vetor 1 3 5 7 9 11 14 17 20.
xy1 = seq(1,11,by=2)
xy2 = seq(14,20,by=3)

xy = c(xy1,xy2)
xy

#g. O vetor de sequencia repetida 1 1 1 2 2 2 3 3 3 4 4 4. 
xz = rep(1:4, each=3)
xz

#h. O vetor de elementos repetidos 1 2 3 1 2 3 1 2 3 1 2 3. 
yx = rep(1:3,4)
yx

#i. O vetor alfanumérico "FCP" "SLB" "SCP". 
yy = c("FCP", "SLB", "SCP")
yy

#j. A matriz (m) de dimensao 4x3 e cujos elementos sao os números inteiros de 1 até 12. O preenchimento da matriz deve ser feito por linha. 
yz = matrix(1:12, 4,3, byrow=TRUE)
yz

#	i. Execute os comandos length(m), dim(m), nrow(m), ncol(m), m[2,2], m[,2], m[3,]. 
length(yz) 	#Tamanho/Nº de posições -> 12
dim(yz)	#Dimensões -> 4 3
nrow(yz)	#Nº de Linhas -> 4
ncol(yz)	#Nº de Colunas -> 3
yz[2,2]	#Valor na posiçao [2,2]
yz[,2]	#Valores na Coluna 2
yz[3,]	#Valores na Linha 3

#	ii. Atribua nomes as linhas e as colunas da matriz m [dimnames(m) <- list(c("L1", "L2", "L3", "L4"), c("C1", "C2", "C3"))] 
dimnames(yz) <- list(c("L1", "L2", "L3", "L4"), c("C1", "C2", "C3"))
yz

#	iii. Qual o valor do elemento que corresponde a linha 2 e a coluna 2?
yz[2,2]
#	iv. Determine a soma dos elementos de cada coluna (apply(m, 2, sum)) média dos elementos de cada linha (apply(m, 1, mean)).
zx = apply(yz, 2, sum)
zx
zy = apply(yz, 1, mean)
zy