#função pede o tempo de atendimento, a taxa de chegada e o tempo total de obervação
fila<-function(tempomedatt=0.3125,taxachegadas=3,tempoobs=10){
  tobs=tempoobs
  temp_esp=NULL
  tempoto=0
  i=0
  tt=0
  att=NULL
  temp_esp[1]=0
  tempot_esp=0
  tchg=NULL
  while(tt<tobs){
    i=i+1
    chgai<-rexp(1,taxachegadas)
    tt=tempoto+chgai
    
    if(tt<tobs){
      tchg[i]=chgai
      att[i]=rexp(1,1/tempomedatt)
      temp_esp[i+1]<-temp_esp[i]-tchg[i]+att[i]
      tempoto=tempoto+tchg[i] 
      if(temp_esp[i]>=0){
        tempot_esp=tempot_esp+temp_esp[i]
      }
      tam_fila<-tempot_esp/tempoto
      esp_fila<-tempot_esp/i
    }}
  return(data.frame(TamanhoMedioFila=tam_fila,TempoMedioEspera=esp_fila))
}
##########################
m=10
n=10
mn=m*n
#EQM via m x n simulações do TamanhoMedioFila
theta = c()
media = c()
for (i in 1:mn){  
   theta[i] <- fila(tempomedatt=0.3125,taxachegadas=10,tempoobs=10)$TamanhoMedioFila 
}
mat <- matrix(theta ,nrow=m,ncol=n,byrow=T)
for (i in 1:m){  
  media[i] = mean(mat[i,])
}
(mediaa = sum(media))
(eqm = sum((theta-media)^2) / length(media))

#EQM via bootstrap do TamanhoMedioFila
theta = matrix(nrow=m,ncol=n,byrow=T)
media = c()
for (i in 1:m){  
  theta[i,] <- sample(mat[1,],replace=T)
  media[i] = mean(theta[i,])
}
(mediaa = sum(media))
(eqm = sum((theta-media)^2) / length(media))

m=10
n=10
mn=m*n
#EQM via m x n simulações do TempoMedioEspera
theta = c()
media = c()
for (i in 1:mn){  
  theta[i] <- fila(tempomedatt=0.3125,taxachegadas=10,tempoobs=10)$TempoMedioEspera 
}
mat <- matrix(theta ,nrow=m,ncol=n,byrow=T)
for (i in 1:m){  
  media[i] = mean(mat[i,])
}
(mediaa = sum(media))
(eqm = sum((theta-media)^2) / length(media))

#EQM via bootstrap do TempoMedioEspera
theta = matrix(nrow=m,ncol=n,byrow=T)
media = c()
for (i in 1:m){  
  theta[i,] <- sample(mat[1,],replace=T)
  media[i] = mean(theta[i,])
}
(mediaa = sum(media))
(eqm = sum((theta-media)^2) / length(media))
