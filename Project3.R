feval <- function(fc,...){do.call(fc,list(...))} 
#gradiente
grad <- function(f,x,delta=0.0001){
  n <- length(x)
  e <- diag(x=1, n, n) #Matriz identidade
  g <- as.numeric()
  for (i in 1:n){
    f1 <- feval(f, x+delta*e[,i])
    f2 <- feval(f, x)
    g[i] <- (f1-f2)/delta
  }
  return(g)
}

#norma de um vetor
norm_vec <- function(x) sqrt(sum(x^2))

#sessao aurea
s.aurea = function(x,a,b,d,erro){ 
  while((b-a)>erro){
    xa = b-0.618*(b-a)
    xb = a+0.618*(b-a)  
    funa = f(x+xa*d)
    funb = f(x+xb*d)
    if(funa<funb) {
      b=xb
    } else {
      a=xa
    }
  }
  alfa=(a+b)/2
  return(alfa)
}


#metodo gradiente
set.seed(123)
#funcao quadratica
iteracoes=c()
minimo=matrix(nrow=100,ncol=2)
for (k in 1:100){
n=2   #numero de variaveis
x=matrix(data=rnorm(n), nrow=n)
Q=diag(c(1,1.2),n,n) 
f=function(x){t(x)%*%Q%*%x}
d=numeric()
alfa=NULL
erro=0.001

i=0
d=1
  while(norm_vec(d)>=0.001){
    i=i+1
    d = -grad(f,x)
    alfa = s.aurea(x,0,4/norm_vec(x),d,erro)
    x = x+alfa*d
    if (i>=200) break
  }
iteracoes[k]=i  
minimo[k,]=t(x)
}
resultados=data.frame(minimo,iteracoes)
resultados.validos<-subset(resultados,resultados$iteracoes<=199)
length(resultados.validos$iteracoes)
mean(resultados.validos$iteracoes)


#fun??o quadr?tica com mal condicionamento
set.seed(123)
iteracoes=c()
minimo=matrix(nrow=100,ncol=2)
for (k in 1:100){
  n=2   #n?mero de vari?veis
  x=matrix(data=rnorm(n), nrow=n)
  Q=diag(c(1,100),n,n) 
  f=function(x){t(x)%*%Q%*%x}
  d=numeric()
  alfa=NULL
  erro=0.001
  
  i=0
  d=1
  while(norm_vec(d)>=0.001){
    i=i+1
    d = -grad(f,x)
    alfa = s.aurea(x,0,4/norm_vec(x),d,erro)
    x = x+alfa*d
    if (i>=200) break
  }
  iteracoes[k]=i  
  minimo[k,]=t(x)
}
resultados=data.frame(minimo,iteracoes)
resultados.validos<-subset(resultados,resultados$iteracoes<=199)
length(resultados.validos$iteracoes)
mean(resultados.validos$iteracoes)

#fun??o multimodal/n?o-convexa
set.seed(123)
iteracoes=c()
minimo=matrix(nrow=100,ncol=2)
for (k in 1:100){
  
n=2   #n?mero de vari?veis
x=matrix(data=rnorm(n), nrow=n)
f=function(x){-10*exp(-t(x)%*%x)-cos(t(x)%*%x)-sin(t(x)%*%x)+1/5*t(x)%*%x+15}
d=NULL
alfa=NULL
erro=0.001
d=1
i=0
while(norm_vec(d)>=0.001){
  i=i+1
  d = -grad(f,x)
  alfa = s.aurea(x,0,4/norm_vec(x),d,erro)
  x = x+alfa*d
  if (i>=200) break
}
iteracoes[k]=i  
minimo[k,]=t(x)
}
resultados=data.frame(minimo,iteracoes)
resultados.validos<-subset(resultados,resultados$iteracoes<=199)
length(resultados.validos$iteracoes)
mean(resultados.validos$iteracoes)


#PSO - a solu??o com a ajuda do can?ado

#fun??o quadr?tica
set.seed(123)
iteracoes=c()
minimo=matrix(nrow=100,ncol=2)
for (k in 1:100){
  
n=2   
Q=diag(c(1,1.2),n,n) 
f=function(x){t(x)%*%Q%*%x}
phi1=2
phi2=2

x=matrix(data=c(runif(20,0,10),runif(20,0,10)),2,byrow=FALSE)
g=matrix(data=rep(0,40),2) # g deve ser 2x1
gp=matrix(data=rep(0,40),2) 
v=matrix(data=rep(0,40),2)
p=matrix(data=rep(0,40),2)
a=c()
ap=c()

ag=Inf # melhor avalia??o da FO

j=0
while(j<200){
  j=j+1
  if(j==1){
    for (i in 1:20){ 
      a[i]=f(x[,i])
      
      if(a[i]<ag){ # guarda a melhor solu??o em g
        ag=a[i]
        g=x[,i]
      }
      
      ap[i]=a[i]
      p[,i]=x[,i]
      
    }
    gp[,j]=g}
  if(j>1){
    for (i in 1:20){ 
      a[i]=f(x[,i])
      
      p[,i]=ifelse(a[i]<ap[i],x[,i],p[,i])
      ap[i]=ifelse(a[i]<ap[i],a[i],ap[i])
      
      if(a[i]<ag){ # guarda a melhor solu??o em g
        ag=a[i]
        g=x[,i]
      }
    }
    gp[,j]=g}
  
  # fazer o sorteio de u1 e u2 em todas as itera??es
  # e usar u1 e u2 diferentes para cada x[,i]
  
  # n?o sei como fazer matricialmente no R, ent?o coloquei um for
  for(i in 1:20){
    u1=runif(1)
    u2=runif(1)
    v[,i]=v[,i]+phi1*u1*(p[,i]-x[,i])+phi2*u2*(g-x[,i])
  }
  
  x=x+v
#  plot(x[1,],x[2,])
#  Sys.sleep(1)
  if(j>1){ if(abs(f(gp[,j])-f(gp[,j-1]))<0.001){break} }

}


iteracoes[k]=j  
minimo[k,]=g
}
resultados=data.frame(minimo,iteracoes)
resultados.validos<-subset(resultados,resultados$iteracoes<=199)
length(resultados.validos$iteracoes)
mean(resultados.validos$iteracoes)


#fun??o quadr?tica com mal condicionamento
set.seed(123)
iteracoes=c()
minimo=matrix(nrow=100,ncol=2)
for (k in 1:100){
  
  n=2   
  Q=diag(c(1,100),n,n) 
  f=function(x){t(x)%*%Q%*%x}
  phi1=2
  phi2=2
  
  x=matrix(data=c(runif(20,0,10),runif(20,0,10)),2,byrow=FALSE)
  g=matrix(data=rep(0,40),2) 
  gp=matrix(data=rep(0,40),2) 
  v=matrix(data=rep(0,40),2)
  p=matrix(data=rep(0,40),2)
  a=c()
  ap=c()
  
  ag=Inf 
  
  j=0
  while(j<200){
    j=j+1
    if(j==1){
      for (i in 1:20){ 
        a[i]=f(x[,i])
        
        if(a[i]<ag){ 
          ag=a[i]
          g=x[,i]
        }
        
        ap[i]=a[i]
        p[,i]=x[,i]
        
      }
      gp[,j]=g}
    if(j>1){
      for (i in 1:20){ 
        a[i]=f(x[,i])
        
        p[,i]=ifelse(a[i]<ap[i],x[,i],p[,i])
        ap[i]=ifelse(a[i]<ap[i],a[i],ap[i])
        
        if(a[i]<ag){ 
          ag=a[i]
          g=x[,i]
        }
      }
      gp[,j]=g}
    
    for(i in 1:20){
      u1=runif(1)
      u2=runif(1)
      v[,i]=v[,i]+phi1*u1*(p[,i]-x[,i])+phi2*u2*(g-x[,i])
    }
    
    x=x+v
#    plot(x[1,],x[2,])
#    Sys.sleep(1)
    if(j>1){ if(abs(f(gp[,j])-f(gp[,j-1]))<0.001){break} }
    
  }
  
  
  iteracoes[k]=j  
  minimo[k,]=g
}
resultados=data.frame(minimo,iteracoes)
resultados.validos<-subset(resultados,resultados$iteracoes<=199)
length(resultados.validos$iteracoes)
mean(resultados.validos$iteracoes)

#fun??o multimodal/n?o-convexa
set.seed(123)
iteracoes=c()
minimo=matrix(nrow=100,ncol=2)
for (k in 1:100){
  
  n=2   
  Q=diag(c(1,100),n,n) 
  f=function(x){-10*exp(-t(x)%*%x)-cos(t(x)%*%x)-sin(t(x)%*%x)+1/5*t(x)%*%x+15}
  phi1=2
  phi2=2
  
  x=matrix(data=c(runif(20,0,10),runif(20,0,10)),2,byrow=FALSE)
  g=matrix(data=rep(0,40),2) 
  gp=matrix(data=rep(0,40),2) 
  v=matrix(data=rep(0,40),2)
  p=matrix(data=rep(0,40),2)
  a=c()
  ap=c()
  
  ag=Inf 
  
  j=0
  while(j<200){
    j=j+1
    if(j==1){
      for (i in 1:20){ 
        a[i]=f(x[,i])
        
        if(a[i]<ag){ 
          ag=a[i]
          g=x[,i]
        }
        
        ap[i]=a[i]
        p[,i]=x[,i]
        
      }
      gp[,j]=g}
    if(j>1){
      for (i in 1:20){ 
        a[i]=f(x[,i])
        
        p[,i]=ifelse(a[i]<ap[i],x[,i],p[,i])
        ap[i]=ifelse(a[i]<ap[i],a[i],ap[i])
        
        if(a[i]<ag){ 
          ag=a[i]
          g=x[,i]
        }
      }
      gp[,j]=g}
    
    for(i in 1:20){
      u1=runif(1)
      u2=runif(1)
      v[,i]=v[,i]+phi1*u1*(p[,i]-x[,i])+phi2*u2*(g-x[,i])
    }
    
    x=x+v
        plot(x[1,],x[2,])
        Sys.sleep(1)
    if(j>1){ if(abs(f(gp[,j])-f(gp[,j-1]))<0.001){break} }
    
  }
  
  
  iteracoes[k]=j  
  minimo[k,]=g
}
resultados=data.frame(minimo,iteracoes)
resultados.validos<-subset(resultados,resultados$iteracoes<=199)
length(resultados.validos$iteracoes)
mean(resultados.validos$iteracoes)
