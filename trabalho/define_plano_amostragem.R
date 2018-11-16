define_plano_amostragem <- function(p1, p2, a, b, N){
  c <- 0:50
  gl <- 2*(c+1) #graus de liberdade
  q <- qchisq(1-b,gl) / qchisq(a,gl)
  
  aux <- q < p2/p1    #cria um vetor de 0 e 1 para verificar a condição q < p2/p1
  
  c_otimo <- min(c[aux]) #guarda o indice mais baixo onde o valor foi 1
  
  #determinar n
  gl_1 <- 2 * (c_otimo+1)
  gl_2 <- 2 * (c_otimo+1)
  
  li <- qchisq(1-b,gl_1)/(2*p2)
  ls <- qchisq(a,gl_2)/(2*p1)
  
  n_otimo = ceiling(li)
  
  r <- c(n_otimo, c_otimo)
  
  return (r)
}