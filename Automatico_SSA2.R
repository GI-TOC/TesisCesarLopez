rm(list=ls())


library(igraph)

VPG <- function(n,indegree,outdegree,Sol,Grafo){
  uve<-{}
  lamda=which(indegree==0)
  for (i in 1:n){
    lamda=setdiff(lamda,uve)            # Conformo el conjunto lamda
    tx=as.numeric(Sol[2,i]%in% uve)     # Verifico si el trabajo actual esta en el conjunto uve
    t=as.numeric(Sol[2,i]%in% lamda)    # Verifico si el trabajo actual esta en el conjunto lamda
    if (t!=1 && tx!=1){                 
      y3=Sol[1,i]
      y1=Sol[2,i]
      tau=outdegree[lamda]
      h=sample(1:length(lamda),1)
      y2=lamda[h]
      y4=Sol[1,which(Sol[2,]==y2)]
      Sol[1,which(Sol[2,]==y2)]=y3
      Sol[2,which(Sol[2,]==y2)]=y1
      Sol[2,i]=y2
      Sol[1,i]=y4
      uve=c(uve,y2)
      suce=which(Mprece[y2,]==1)
      for (k in 1:length(suce)){
        donde=indegree[suce[k]]
        indegree[suce[k]]=donde-1
      }
      lamda=which(indegree==0)
    }else{
      suce=which(Mprece[Sol[2,i],]==1)
      for (k in 1:length(suce)){
        donde=indegree[suce[k]]
        indegree[suce[k]]=donde-1
      }
      lamda=which(indegree==0)
      uve=c(uve,Sol[2,i])
    }
    
  }
  return(Sol)
}
#Funcion para asignar trabajadores a ordenes
Asignacion<- function(Sq_pos,Habilidad,n){
  for (i in 1:n){
    Quien=which(Habilidad[,Sq_pos[2,i]]==1)
    if (length(Quien)>1){
      a=sample(1:length(Quien),1)
      Sq_pos[3,i]=Quien[a]
    }else{
      Sq_pos[3,i]=Quien 
    }
  }
  return(Sq_pos)
}

#Funcion Evaluacion
Eval_Jssop <- function(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq){
  Cmax=sample(0,n,replace=TRUE)
  # Programo el primer trabajo
  Tpt=TP[Sq_pos[2,1]]
  Mqt=Maquina[[Sq_pos[2,1]]]
  PRt=Lmq[[Mqt]]
  aux=c(0,Tpt)
  PRt<-rbind(PRt,aux)
  Lmq[[Mqt]]=PRt
  
  # Programo el primer operario
  Opt = Sq_pos[3,1]
  aop = Lop[[Opt]]
  aop=rbind(aop,c(0,Tpt))
  Lop[[Opt]]=aop
  Cmax[Sq_pos[2,1]]=Tpt
  
  # Programo el j esimo trabajo y operario
  j=2
  while (j<=n){
    q=as.numeric(Mprece[Sq_pos[2,j-1],Sq_pos[2,j]]==1)
    if (q==1){
      Optj = Sq_pos[3,j]                  
      Mqtj=Maquina[[Sq_pos[2,j]]]         
      Tptj=TP[Sq_pos[2,j]]
      Mqant=Maquina[[Sq_pos[2,j-1]]] 
      Acmq=Lmq[[Mqtj]]
      Dmqt=Lmq[[Mqant]]                   
      aopj=Lop[[Optj]]                    
      tam=nrow(Dmqt)
      tam2=nrow(aopj)
      tam3=nrow(Acmq)
      tini=max(Dmqt[tam,2],aopj[tam2,2],Acmq[tam3,2])
      aja=c(tini, tini+Tptj)
      Acmq=rbind(Acmq,aja)                
      aopj=rbind(aopj,aja)
      Cmax[Sq_pos[2,j]]=tini+Tptj
      Lmq[[Mqtj]]=Acmq
      Lop[[Optj]]=aopj
      j=j+1
    }
    else{
      op=as.numeric(Sq_pos[3,j-1]== Sq_pos[3,j])
      if (op==1){
        Optj = Sq_pos[3,j]                  
        Mqtj=Maquina[[Sq_pos[2,j]]]         
        Tptj=TP[Sq_pos[2,j]]
        Mqant=Maquina[[Sq_pos[2,j-1]]] 
        Acmq=Lmq[[Mqtj]]
        Dmqt=Lmq[[Mqant]]                   
        aopj=Lop[[Optj]]                    
        tam=nrow(Dmqt)
        tam2=nrow(aopj)
        tam3=nrow(Acmq)
        tini=max(Dmqt[tam,2],aopj[tam2,2],Acmq[tam3,2])
        aja=c(tini, tini+Tptj)
        Acmq=rbind(Acmq,aja)                
        aopj=rbind(aopj,aja)
        Cmax[Sq_pos[2,j]]=tini+Tptj
        Lmq[[Mqtj]]=Acmq
        Lop[[Optj]]=aopj
        j=j+1
      }else{
        Mqj=Maquina[[Sq_pos[2,j]]]
        Optj = Sq_pos[3,j]
        Acmq=Lmq[[Mqj]]
        aopj=Lop[[Optj]]
        Tptj=TP[Sq_pos[2,j]]
        tam=nrow(Acmq)
        tam2=nrow(aopj)
        tini=max(Acmq[tam,2],aopj[tam2,2])
        aja=c(tini, tini+Tptj)
        Acmq=rbind(Acmq,aja)
        aopj=rbind(aopj,aja)
        Cmax[Sq_pos[2,j]]=tini+Tptj
        Lmq[[Mqj]]=Acmq
        Lop[[Optj]]=aopj
        j=j+1
      }
    }
  }
  Cmax=max(Cmax)
  
  return(Cmax)
}

# Funcion para generar 1 ardilla
Pob_Inicial<-function(n){
  pos<-runif(n,min=-5,max=5)
  secu <- order(pos)
  pos <-sort(pos)
  Ardilla <- matrix(0,nrow=3,ncol=n)
  Ardilla[1,]<-pos
  Ardilla[2,]<-secu
  Ardilla[3,]<-0
  return(Ardilla)}

#Funcion SSA
SSA_2 <- function(n,Gc,n2,n3,Iter,Dg,Tamo,Pdp,n1){
  
  #Creo la poblacion inicial de ardillas
  Manada <- matrix(0,nrow=(3*Tamo),ncol=n+1)
  control<-seq(1,(3*Tamo),by=3)
  for (ii in 1:Tamo){
    jj<-control[ii]
    Sol<-Pob_Inicial(n)
    Sq_pos<-VPG(n,indegree,outdegree,Sol,Grafo)
    Sq_pos<- Asignacion(Sq_pos,Habilidad,n)
    resul<-c(0,0,0)
    Sq_pos<-cbind(Sq_pos,resul)
    Sol_eval<-Eval_Jssop(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq)
    Sq_pos[,n+1]=Sol_eval
    Manada[jj:(jj+2),]<-Sq_pos
  }
  # Organizo la poblacion inicial segun valor de FO (Ascedente)
  Manada <-Manada[order((Manada[,n+1])),]
  
  t=1
  while (t <= Iter){
    
    # Movimiento de las ardillas desde Acorn Nut Tree hacia Hickory Nut Tree
    for (lk in 1:n1){
      r1=runif(1,0,1)
      if (r1<Pdp){
        Manada[control[lk+1],1:n]= Manada[control[lk+1],1:n]+(Dg*Gc*(Manada[1, 1:n]-Manada[control[lk+1],1:n]))
        rm(Sol,Sq_pos)
        Sol=matrix(0,nrow=3,ncol=n)
        Sol[1,]=sort(Manada[control[lk+1],1:n])
        Sol[2,]=order(Manada[control[lk+1],1:n])
        Sol[3,]=0
        Sq_pos<-VPG(n,indegree,outdegree,Sol,Grafo)
        Sq_pos<- Asignacion(Sq_pos,Habilidad,n)
        resul<-c(0,0,0)
        Sq_pos<-cbind(Sq_pos,resul)
        Sol_eval<-Eval_Jssop(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq)
        Sq_pos[,n+1]=Sol_eval
        sd=control[(lk+2)]
        Manada[control[lk+1]:(sd-1),]=Sq_pos
      }else{
        rm(Sol,Sq_pos)
        Sol=matrix(0,nrow=3,ncol=n)
        pos=runif(n,min=-5,max=5)
        Sol[1,]=sort(pos)
        Sol[2,]=order(pos)
        Sol[3,0]=0
        Sq_pos<-VPG(n,indegree,outdegree,Sol,Grafo)
        Sq_pos<- Asignacion(Sq_pos,Habilidad,n)
        resul<-c(0,0,0)
        Sq_pos<-cbind(Sq_pos,resul)
        Sol_eval<-Eval_Jssop(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq)
        Sq_pos[,n+1]=Sol_eval
        sd=control[(lk+2)]
        Manada[control[lk+1]:(sd-1),]=Sq_pos
      }
    }
   
     # Movimiento de las ardillas desde Normal Trees hacia Acorn Normal Tree
    for (kl in 1:n2){
      r2=runif(1,0,1)
      if (r2<Pdp){
        lo=length(control)
        control2=control[5:lo]
        Manada[control2[kl],1:n]= Manada[control2[kl],1:n]+(Dg*Gc*(Manada[4, 1:n]-Manada[control2[kl],1:n]))
        rm(Sol,Sq_pos)
        Sol=matrix(0,nrow=3,ncol=n)
        Sol[1,]=sort(Manada[control2[kl],1:n])
        Sol[2,]=order(Manada[control2[kl],1:n])
        Sol[3,]=0
        Sq_pos<-VPG(n,indegree,outdegree,Sol,Grafo)
        Sq_pos<- Asignacion(Sq_pos,Habilidad,n)
        resul<-c(0,0,0)
        Sq_pos<-cbind(Sq_pos,resul)
        Sol_eval<-Eval_Jssop(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq)
        Sq_pos[,n+1]=Sol_eval
        sd=control2[(kl+1)]
        Manada[control2[kl]:(sd-1),]=Sq_pos
      }else{
        lo=length(control)
        control2=control[5:lo]
        rm(Sol,Sq_pos)
        Sol=matrix(0,nrow=3,ncol=n)
        pos=runif(n,min=-5,max=5)
        Sol[1,]=sort(pos)
        Sol[2,]=order(pos)
        Sol[3,0]=0
        Sq_pos<-VPG(n,indegree,outdegree,Sol,Grafo)
        Sq_pos<- Asignacion(Sq_pos,Habilidad,n)
        resul<-c(0,0,0)
        Sq_pos<-cbind(Sq_pos,resul)
        Sol_eval<-Eval_Jssop(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq)
        Sq_pos[,n+1]=Sol_eval
        sd=control2[(kl+1)]
        Manada[control2[kl]:(sd-1),]=Sq_pos
      }
    }
    
    # Movimiento de las ardillas desde Normal Trees hacia Acorn Normal Tree
    for (kt in 1:n3){
      r3=runif(1,0,1)
      if (r3<Pdp){
        Tami=Tamo+1
        control3=seq(10,(3*Tami),by=3)
        Manada[control3[kt],1:n]= Manada[control3[kt],1:n]+(Dg*Gc*(Manada[1, 1:n]-Manada[control3[kt],1:n]))
        rm(Sol,Sq_pos)
        Sol=matrix(0,nrow=3,ncol=n)
        Sol[1,]=sort(Manada[control3[kt],1:n])
        Sol[2,]=order(Manada[control3[kt],1:n])
        Sol[3,]=0
        Sq_pos<-VPG(n,indegree,outdegree,Sol,Grafo)
        Sq_pos<- Asignacion(Sq_pos,Habilidad,n)
        resul<-c(0,0,0)
        Sq_pos<-cbind(Sq_pos,resul)
        Sol_eval<-Eval_Jssop(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq)
        Sq_pos[,n+1]=Sol_eval
        sd=control3[(kt+1)]
        Manada[control3[kt]:(sd-1),]=Sq_pos
      }else{
        Tami=Tamo+1
        control3=seq(10,(3*Tami),by=3)
        rm(Sol,Sq_pos)
        Sol=matrix(0,nrow=3,ncol=n)
        pos=runif(n,min=-5,max=5)
        Sol[1,]=sort(pos)
        Sol[2,]=order(pos)
        Sol[3,0]=0
        Sq_pos<-VPG(n,indegree,outdegree,Sol,Grafo)
        Sq_pos<- Asignacion(Sq_pos,Habilidad,n)
        resul<-c(0,0,0)
        Sq_pos<-cbind(Sq_pos,resul)
        Sol_eval<-Eval_Jssop(Sq_pos,Habilidad,TP,Maquina,n,Lop,Lmq)
        Sq_pos[,n+1]=Sol_eval
        sd=control3[(kt+1)]
        Manada[control3[kt]:(sd-1),]=Sq_pos
      }
    }
    Manada=Manada[order((Manada[,n+1])),]
    Smin=(10^(-6))/((365)^((t*2.5)/Iter))
    ra=rnorm(1, mean=0, sd=1)
    rb=rnorm(1, mean=0, sd=1)
    for (t1 in 1:n1){
      FSaux=Manada[control[t1+1],1:n]
      FShtaux=Manada[1,1:n]
      acum=0
      for (t2 in 1:n){
        Dato= (FSaux[t2]-FShtaux[t2])^2
        acum=acum+Dato
      }
      Sc=sqrt(acum)
      if (Sc<Smin){
        # ojo generar 3 soluciones bajo levy, evaluar y reemplazar
        
      }
      t=t+1 
    }
  }
  return(Manada[1,n+1])
}

Mejor_Cmax=sample(0,32,replace = TRUE)

for (nn in 2:2){
  N=nn
  print(N)
  Mprece <- switch (N,
                    {as.matrix(read.table("Grafo1.1.txt"))},
                    {as.matrix(read.table("Grafo1.4.txt"))},
                    {as.matrix(read.table("Grafo1.7.txt"))},
                    {as.matrix(read.table("Grafo2.2.txt"))},
                    {as.matrix(read.table("Grafo2.3.txt"))},
                    {as.matrix(read.table("Grafo2.5.txt"))},
                    {as.matrix(read.table("Grafo2.6.txt"))},
                    {as.matrix(read.table("Grafo3.2.txt"))},
                    {as.matrix(read.table("Grafo3.4.txt"))},
                    {as.matrix(read.table("Grafo3.6.txt"))},
                    {as.matrix(read.table("Grafo3.8.txt"))},
                    {as.matrix(read.table("Grafo4.7.txt"))},
                    {as.matrix(read.table("Grafo4.8.txt"))},
                    {as.matrix(read.table("Grafo4.9.txt"))},
                    {as.matrix(read.table("Grafo4.10.txt"))},
                    {as.matrix(read.table("Grafo5.1.txt"))},
                    {as.matrix(read.table("Grafo6.1.txt"))},
                    {as.matrix(read.table("Grafo6.4.txt"))},
                    {as.matrix(read.table("Grafo6.8.txt"))},
                    {as.matrix(read.table("Grafo6.9.txt"))},
                    {as.matrix(read.table("Grafo7.2.txt"))},
                    {as.matrix(read.table("Grafo7.5.txt"))},
                    {as.matrix(read.table("Grafo7.7.txt"))},
                    {as.matrix(read.table("Grafo7.10.txt"))},
                    {as.matrix(read.table("Grafo8.3.txt"))},
                    {as.matrix(read.table("Grafo8.6.txt"))},
                    {as.matrix(read.table("Grafo8.9.txt"))},
                    {as.matrix(read.table("Grafo8.10.txt"))}
  )
  
  Habilidad <- switch (N,
                       {as.matrix(read.table("Habilidad1.1.txt"))},
                       {as.matrix(read.table("Habilidad1.4.txt"))},
                       {as.matrix(read.table("Habilidad1.7.txt"))},
                       {as.matrix(read.table("Habilidad2.2.txt"))},
                       {as.matrix(read.table("Habilidad2.3.txt"))},
                       {as.matrix(read.table("Habilidad2.5.txt"))},
                       {as.matrix(read.table("Habilidad2.6.txt"))},
                       {as.matrix(read.table("Habilidad3.2.txt"))},
                       {as.matrix(read.table("Habilidad3.4.txt"))},
                       {as.matrix(read.table("Habilidad3.6.txt"))},
                       {as.matrix(read.table("Habilidad3.8.txt"))},
                       {as.matrix(read.table("Habilidad4.7.txt"))},
                       {as.matrix(read.table("Habilidad4.8.txt"))},
                       {as.matrix(read.table("Habilidad4.9.txt"))},
                       {as.matrix(read.table("Habilidad4.10.txt"))},
                       {as.matrix(read.table("Habilidad5.1.txt"))},
                       {as.matrix(read.table("Habilidad6.1.txt"))},
                       {as.matrix(read.table("Habilidad6.4.txt"))},
                       {as.matrix(read.table("Habilidad6.8.txt"))},
                       {as.matrix(read.table("Habilidad6.9.txt"))},
                       {as.matrix(read.table("Habilidad7.2.txt"))},
                       {as.matrix(read.table("Habilidad7.5.txt"))},
                       {as.matrix(read.table("Habilidad7.7.txt"))},
                       {as.matrix(read.table("Habilidad7.10.txt"))},
                       {as.matrix(read.table("Habilidad8.3.txt"))},
                       {as.matrix(read.table("Habilidad8.6.txt"))},
                       {as.matrix(read.table("Habilidad8.9.txt"))},
                       {as.matrix(read.table("Habilidad8.10.txt"))}
  )
  
  TP <- switch (N,
                {as.matrix(read.table("TP1.1.txt"))},
                {as.matrix(read.table("TP1.4.txt"))},
                {as.matrix(read.table("TP1.7.txt"))},
                {as.matrix(read.table("TP2.2.txt"))},
                {as.matrix(read.table("TP2.3.txt"))},
                {as.matrix(read.table("TP2.5.txt"))},
                {as.matrix(read.table("TP2.6.txt"))},
                {as.matrix(read.table("TP3.2.txt"))},
                {as.matrix(read.table("TP3.4.txt"))},
                {as.matrix(read.table("TP3.6.txt"))},
                {as.matrix(read.table("TP3.8.txt"))},
                {as.matrix(read.table("TP4.7.txt"))},
                {as.matrix(read.table("TP4.8.txt"))},
                {as.matrix(read.table("TP4.9.txt"))},
                {as.matrix(read.table("TP4.10.txt"))},
                {as.matrix(read.table("TP5.1.txt"))},
                {as.matrix(read.table("TP6.1.txt"))},
                {as.matrix(read.table("TP6.4.txt"))},
                {as.matrix(read.table("TP6.8.txt"))},
                {as.matrix(read.table("TP6.9.txt"))},
                {as.matrix(read.table("TP7.2.txt"))},
                {as.matrix(read.table("TP7.5.txt"))},
                {as.matrix(read.table("TP7.7.txt"))},
                {as.matrix(read.table("TP7.10.txt"))},
                {as.matrix(read.table("TP8.3.txt"))},
                {as.matrix(read.table("TP8.6.txt"))},
                {as.matrix(read.table("TP8.9.txt"))},
                {as.matrix(read.table("TP8.10.txt"))}
  )
  
  
  Maquina <- switch (N,
                     {as.matrix(read.table("Maquina1.1.txt"))},
                     {as.matrix(read.table("Maquina1.4.txt"))},
                     {as.matrix(read.table("Maquina1.7.txt"))},
                     {as.matrix(read.table("Maquina2.2.txt"))},
                     {as.matrix(read.table("Maquina2.3.txt"))},
                     {as.matrix(read.table("Maquina2.5.txt"))},
                     {as.matrix(read.table("Maquina2.6.txt"))},
                     {as.matrix(read.table("Maquina3.2.txt"))},
                     {as.matrix(read.table("Maquina3.4.txt"))},
                     {as.matrix(read.table("Maquina3.6.txt"))},
                     {as.matrix(read.table("Maquina3.8.txt"))},
                     {as.matrix(read.table("Maquina4.7.txt"))},
                     {as.matrix(read.table("Maquina4.8.txt"))},
                     {as.matrix(read.table("Maquina4.9.txt"))},
                     {as.matrix(read.table("Maquina4.10.txt"))},
                     {as.matrix(read.table("Maquina5.1.txt"))},
                     {as.matrix(read.table("Maquina6.1.txt"))},
                     {as.matrix(read.table("Maquina6.4.txt"))},
                     {as.matrix(read.table("Maquina6.8.txt"))},
                     {as.matrix(read.table("Maquina6.9.txt"))},
                     {as.matrix(read.table("Maquina7.2.txt"))},
                     {as.matrix(read.table("Maquina7.5.txt"))},
                     {as.matrix(read.table("Maquina7.7.txt"))},
                     {as.matrix(read.table("Maquina7.10.txt"))},
                     {as.matrix(read.table("Maquina8.3.txt"))},
                     {as.matrix(read.table("Maquina8.6.txt"))},
                     {as.matrix(read.table("Maquina8.9.txt"))},
                     {as.matrix(read.table("Maquina8.10.txt"))}
  )
  #+ Creo grafo y calculo in-out degree
  Grafo <- graph.adjacency(Mprece)
  indegree <-as.vector(degree(Grafo, mode="in"))
  outdegree <- as.vector(degree(Grafo,mode="out"))
  
  #+----------------------------------------------------------------
  # Parametros del problema:
  # n: Numeros de trabajos
  # N.op: Numero de operadores
  # N.mq: Numero de maquinas
  n=length(TP)
  N.op<- dim(Habilidad)[1]
  N.mq<- max(Maquina)
  Lop=list(N.op)
  Lmq=list(N.mq)
  for (i in 1:N.op){
    Lop[[i]]=matrix(0,nrow=2,ncol=2)
  }
  for (i in 1:N.mq){
    Lmq[[i]]=matrix(0,nrow=2,ncol=2)
  }
  #Parametros de SSA
  Tamo=50
  D=0.1533
  L=0.04693
  Teta=atan(D/L)
  Hg=8
  Sf=20
  Dg=(Hg/tan(Teta))/Sf
  n1=3
  Iter=100
  #beta=1.5
  #Pi=3.1416
  #siggma=((gamma(1+beta)*sin((Pi*beta)/2))/(gamma((1+beta)/2)*beta*2^((beta-1)/2)))^(1/beta)
  #----------------------------------------------------

  # Aqui hacer el código para las replicas
  re=1
  Replica=sample(0,3, replace=TRUE)
  while (re <=3){
    Pdp=0.7
    Gc=1.6
    n2=0.4*Tamo
    n3=Tamo-(n1+n2+1)
    Cmax=SSA_2(n,Gc,n2,n3,Iter,Dg,Tamo,Pdp,n1)
    Replica[re]=Cmax
    re=re+1
  }
  Mejor_Cmax[nn]=min(Replica)
}
write.table(Mejor_Cmax,file="Resultados_SSA2.txt")