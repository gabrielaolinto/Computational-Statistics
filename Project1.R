#gerador congruencial
gerador <- function(n,seed){ #tamanho da amostra, semente inicial
#if (seed == NULL) {seed = Sys.time()} else {seed = seed}
  y = as.vector(1:n)
  x = as.vector(1:n)
  
  a = 16807 
  m = 2^31 - 1
  
  for (i in 1:n){
    if (i == 1) {
    y[i]= seed  
    x[i] = y[i] / m
    } else {
    y[i] = (a * y[i-1]) %% m
    x[i] = y[i] / m
  }
  }
  
  return(x)
}
inicio<-proc.time()
x<-gerador(100,4000000) # escolhendo uma amostra qualquer n=10
proc.time()-inicio
a <- seq(0, 1,0.1) 
ha <- dunif(a) 
library(ggplot2)
df <- data.frame(x)
g = ggplot(df, aes(x)) + geom_histogram(aes(y=..density..),binwidth=.2, alpha=.8,colour="black", fill="gray")
g + scale_x_continuous(breaks=seq(0,1,0.1))+ geom_line(aes(a[1:10],ha[1:10]),colour='red')+ geom_line(aes(a[2:11],ha[2:11]),colour='red')


#integração monte carlo p normal padrão

dnormn <- function(b,x){
  y = as.vector(1:length(x))
  for (i in 1:length(x)){
  y[i] = b / sqrt(2*pi) * exp(-(x[i] * b)^2 / 2)
 }
 
  s = sum(y)/length(x)
 
 return(s)
}
x<-gerador(10,4000000)
x.teorico<-runif(10,0,1)
qqplot(x.teorico,x,main="QQ-plot")
grid()
abline(0,1,col='red')
#acumuladas 
#empírica
f=ecdf(x);acumul=f(x)
y<- seq(0,1,0.01)
acum<-punif(y)

plot(ecdf(x), do.points=F, verticals=T,main='')  
lines(y,acum,col="red")
legend(0,0.9,lty=c(1,1),c("Empírica", "Uniforme"),lwd=c("1","1"), col=c(1,2), bty="n",cex=0.8)
ks.test(acumul,acum)

#gerando tabela da normal
i=0
simulado = NULL
real = NULL
erro = NULL
for (b in seq(0,4.09,0.01)){
  i = i + 1
  simulado[i] = dnormn(b,x=x)
  real[i] = pnorm(b) - 0.5
  erro[i] = simulado[i]-real[i]
}
l<-seq(0,4.09,0.01)
df2 = data.frame(l,simulado, real, erro)

#montando a tabela da normal
m <- matrix(simulado,nrow=41,ncol=10,byrow=T)

#montando a tabela de erros
e <- matrix(erro,nrow=41,ncol=10,byrow=T)

#juntando as tabelas
t2 <- matrix(nrow=2,ncol=10)
tm<-NULL
for (i in 1:41){
  t2[1,] = m[i,]
  t2[2,] = e[i,]
  rownames(t2) <- c(paste('Simulado',i),paste('Erro',i))
  tm<-rbind(tm,t2)
}

write.table(tm, "C:\\Users\\N4030\\Desktop\\Gabi\\Matérias, Livros e Gabaritos\\Matérias\\Mestrado\\Estatística Computacional\\Trabalhos de entrega\\Trabalho 1\\tabela.csv", sep=";",dec=",", row.names=T)
