set.seed(123)
N=100
U =runif(N)                                            
y = rep(NA,N)
for(i in 1:N){
  if(U[i]>.5){
    y[i] = rnorm(1,3,0.75)
  }else if(U[i]<.5){
    y[i] = rnorm(1,7,1)
  }
}

gama=c()
mu1=sample(y,1)
sigma1=var(y)
mu2=sample(y,1)
sigma2=var(y)
p=0.3

#gráfico nos chutes inciais
dens<-function(x,mu1,sigma1,mu2,sigma2,p){
  (1-p)*dnorm(x,mu1,sqrt(sigma1))+p*dnorm(x,mu2,sqrt(sigma2))
}
x <- seq(0, 20, 0.01)
hist(y, main=expression(paste('Iteração 1, ',mu^(1),'=6.04 ',sigma^(1),'=2.13 ',mu^(2),'=7.25 ',sigma^(2),'=2.13 ',p,'=0.3 ',sep=' ')), breaks=20, prob =TRUE,xlim=c(1,10))
lines(x,dens(x=x,mu1=mu1,sigma1=sigma1,mu2=mu2,sigma2=sigma2,p=p),col=2)

#iterando..
j=0
while(j<50){
  j=j+1
for (i in 1:N){
  gama[i]=(1-p)*dnorm(y[i],mu1,sqrt(sigma1))/((1-p)*dnorm(y[i],mu1,sqrt(sigma1))+p*dnorm(y[i],mu2,sqrt(sigma2)))
}
  mu1=sum((1-gama)*y)/sum(1-gama)
  mu2=sum(gama*y)/sum(gama)
  sigma1=sum((1-gama)*(y-mu1)^2)/sum(1-gama)
  sigma2=sum(gama*(y-mu2)^2)/sum(gama)
  p=sum(gama)/N
  

  mypath <- file.path("C:\\Users\\N4030\\Desktop\\Gabi\\Matérias, Livros e Gabaritos\\Matérias\\Mestrado\\Estatística Computacional\\Trabalhos de entrega\\Trabalho 5\\",paste("ajustes", j, ".jpg", sep = ""))
  jpeg(file=mypath)
  mytitle = paste("iteração", j)
  hist(y, main=paste('Iteração ',j,sep=''), breaks=20, prob =TRUE,xlim=c(1,10),ylim=c(0,0.3))
  lines(x,dens(x=x,mu1=mu1,sigma1=sigma1,mu2=mu2,sigma2=sigma2,p=p),col=2)
  dev.off()
  
}
#grafico na ultima iteraçao
x <- seq(0, 20, 0.01)
hist(y, main=expression(paste('Iteração 50, ',mu^(1),'=2.80 ',sigma^(1),'=0.67 ',mu^(2),'=7.55 ',sigma^(2),'=1.01 ',p,'=0.54 ',sep=' ')), breaks=20, prob =TRUE,xlim=c(1,10),ylim=c(0,0.3))
lines(x,dens(x=x,mu1=mu1,sigma1=sigma1,mu2=mu2,sigma2=sigma2,p=p),col=2)

reais=c(3,0.75,7,1,0.4)
conv=c(2.80,0.67,7.55,1.01,0.54)
mean(abs(reais-conv))

test.glm=glm(visitas~idade,family='poisson',data=dados)
summary(test.glm)
