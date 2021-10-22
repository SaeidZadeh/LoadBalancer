virtu=10
fifo=rep(0,200)
rnd=rep(0,200)
greed=rep(0,200)
ours=rep(0,200)
greedmin=rep(0,200)
oursmin=rep(0,200)
fifomin=rep(0,200)
greedfun=function(n,a)
{
  if(length(n)==1)
    return(c(n,0))
  greedlist=rep(0,virtu)
  tmp=1
  for(i in 1:length(n)){
    greedlist[a[i,tmp]]=greedlist[a[i,tmp]]+n[i]
    tmp=which.min(greedlist[a[i,][a[i,]!=0]])
  }
  return(c(max(greedlist),min(greedlist)))
}
fifofun=function(n,a)
{
  fiflist=rep(0,virtu)
  tmp=rep(0,10)
  for(i in 1:length(n)){
    k=which.min(tmp[a[i,][a[i,]!=0]])
    tmp[a[i,k]]=tmp[a[i,k]]+1
    fiflist[a[i,k]]=fiflist[a[i,k]]+n[i]
  }
  return(c(max(fiflist),min(fiflist)))
}
rndfun=function(n,a)
{
  rndlist=rep(0,virtu)
  for(i in 1:length(n)){
    tmp=sample(a[i,][a[i,]!=0],1)
    tmp=which(a[i,]==tmp)
    rndlist[a[i,tmp]]=rndlist[a[i,tmp]]+n[i]
  }
  return(max(rndlist))
}
sortfun1=function(n,a)
{
  l=rep(0,length(a[,1]))
  for(i in 1:length(n))
    l[i]=length(a[i,][a[i,]!=0])
  l=order(l, decreasing = FALSE)
  a=a[l,]
  m=n[l]
  return(cbind(a,m))
}
sortfun2=function(n,a)
{
  l=rep(0,length(a[,1]))
  m=n
  b=a
  for(i in 1:length(n))
    l[i]=length(a[i,][a[i,]!=0])
  for(i in 1:10){
    k=which(l==i)
    if(length(k)>1){
      kp=min(k)
      w=order(n[k], decreasing = TRUE)
      m[kp+(1:length(w))-1]=n[kp+w-1]
      b[kp+(1:length(w))-1,]=a[kp+w-1,]
    }
  }
  return(cbind(b,m))
}
oursfun=function(n,a)
{
  ourslist=rep(0,virtu)
  if(length(n)==1){
    tmp=which.min(ourslist[a[a!=0]])
    ourslist[a[tmp]]=ourslist[a[tmp]]+n
    return(c(max(ourslist),min(ourslist)))
  }
  if(length(n)!=1){
    mp=sortfun1(n,a)
    m=mp[,11]
    b=mp[,1:10]
    mp=sortfun2(m,b)
    m=mp[,11]
    b=mp[,1:10]
    for(i in 1:length(n)){
      tmp=which.min(ourslist[b[i,][b[i,]!=0]])
      ourslist[b[i,tmp]]=ourslist[b[i,tmp]]+m[i]
    }
    return(c(max(ourslist),min(ourslist)))
  }
}
matfun=function(n)
{
  a=matrix(data=0,nrow=length(n),ncol=10)
  for(i in 1:length(n))
  {
    j=sample(1:10,1)
    b=sample(1:10,j,replace = FALSE)
    for(k in 1:length(b))
      a[i,k]=b[k]
  }
  return(a)
}
for(i in 1:200)
{
  tmpfifo=rep(0,50)
  tmprnd=rep(0,50)
  tmpgreed=rep(0,50)
  tmpours=rep(0,50)
  tmpoursmin=rep(0,50)
  tmpgreedmin=rep(0,50)
  tmpfifomin=rep(0,50)
  for(j in 1:50){
    n=sample(1:50,i,replace = TRUE)
    matdat=matfun(n)
    tmp=fifofun(n,matdat)
    tmpfifo[j]=tmp[1]
    tmpfifomin[j]=tmp[2]
    tmprnd[j]=rndfun(n,matdat)
    tmp=greedfun(n,matdat)
    tmpgreed[j]=tmp[1]
    tmpgreedmin[j]=tmp[2]
    tmp=oursfun(n,matdat)
    tmpours[j]=tmp[1]
    tmpoursmin[j]=tmp[2]
  }
  fifo[i]=mean(tmpfifo)
  greed[i]=mean(tmpgreed)
  rnd[i]=mean(tmprnd)
  ours[i]=mean(tmpours)
  greedmin[i]=mean(tmpgreedmin)
  fifomin[i]=mean(tmpfifomin)
  oursmin[i]=mean(tmpoursmin)
}
min(greed-ours)
windows()
plot(fifo,type='l',ylim=c(0,max(rnd)),col='red',xlab="number of tasks",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","GREEDY","OURS"),col=c("blue", "red","green","black"), lty=1,)
lines(rnd,col='blue')
lines(greed,col='green')
lines(ours,col='black')
windows()
plot(greed-ours,type='l',ylim=c(0,max(fifo-ours)),xlab="number of tasks",ylab="Av. makespan Diff.",col="red")
legend("topleft", c("FIFO-OURS","GREEDY-OURS"),col=c("black","red"), lty=1,)
lines(fifo-ours,col='black')
windows()
plot(greed-greedmin,type='l',ylim=c(min(ours-oursmin),10+max(greed-greedmin)),xlab="number of tasks",ylab="Av. TCD",col="red")
legend("topleft", c("GREEDY","FIFO","OURS"),col=c("red","blue","black"), lty=1,)
lines(ours-oursmin,col="black")
lines(fifo-fifomin,col="blue")

for(i in 1:200)
{
  tmpfifo=rep(0,50)
  tmprnd=rep(0,50)
  tmpgreed=rep(0,50)
  tmpours=rep(0,50)
  tmpoursmin=rep(0,50)
  tmpgreedmin=rep(0,50)
  m=rpois(50,7)+1
  for(j in 1:50){
    n=sample(1:50,m[j],replace = TRUE)
    matdat=matfun(n)
    tmpfifo[j]=fifofun(n,matdat)[1]
    tmprnd[j]=rndfun(n,matdat)
    tmp=greedfun(n,matdat)
    tmpgreed[j]=tmp[1]
    tmpgreedmin[j]=tmp[2]
    tmp=oursfun(n,matdat)
    tmpours[j]=tmp[1]
    tmpoursmin[j]=tmp[2]
  }
  fifo[i]=mean(tmpfifo)
  greed[i]=mean(tmpgreed)
  rnd[i]=mean(tmprnd)
  ours[i]=mean(tmpours)
  greedmin[i]=mean(tmpgreedmin)
  oursmin[i]=mean(tmpoursmin)
}

windows()
plot(fifo,type='p',pch=15,ylim=c(40,max(rnd)+15),col='red',xlab="Index",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","GREEDY","OURS"),col=c("blue", "red","green","black"), lty=1,)
lines(rnd,col='blue',type='p',pch=15)
lines(greed,col='green',type='p',pch=15)
lines(ours,col='black',type='p',pch=15)
abline(h=mean(ours),col='black')
abline(h=mean(fifo),col='red')
abline(h=mean(rnd),col='blue')
abline(h=mean(greed),col='green')

rnd1=rep(0,50)
greed1=rep(0,50)
ours1=rep(0,50)
fifo1=rep(0,50)
fifomin1=rep(0,50)
oursmin1=rep(0,50)
greedmin1=rep(0,50)
trnd=rep(0,50)
tfif=rep(0,50)
tour=rep(0,50)
tgrd=rep(0,50)

start_time <- Sys.time()

tfif[j]=end_time - start_time


for(k in 1:50)
{
  trnd1=rep(0,200)
  tfif1=rep(0,200)
  tour1=rep(0,200)
  tgrd1=rep(0,200)
  for(i in 1:200)
  {
    tmpfifo=rep(0,50)
    tmprnd=rep(0,50)
    tmpgreed=rep(0,50)
    tmpours=rep(0,50)
    tmpoursmin=rep(0,50)
    tmpgreedmin=rep(0,50)
    tmpfifomin=rep(0,50)
    m=rpois(50,7)+1
    trnd2=rep(0,50)
    tfif2=rep(0,50)
    tour2=rep(0,50)
    tgrd2=rep(0,50)
    for(j in 1:50){
      n=sample(1:50,m[j],replace = TRUE)
      matdat=matfun(n)
      start_time <- Sys.time()
      tmp=fifofun(n,matdat)
      tmpfifo[j]=tmp[1]
      tmpfifomin[j]=tmp[2]
      end_time <- Sys.time()
      tfif2[j]=end_time - start_time
      start_time <- Sys.time()
      tmprnd[j]=rndfun(n,matdat)
      end_time <- Sys.time()
      trnd2[j]=end_time - start_time
      start_time <- Sys.time()
      tmp=greedfun(n,matdat)
      tmpgreed[j]=tmp[1]
      tmpgreedmin[j]=tmp[2]
      end_time <- Sys.time()
      tgrd2[j]=end_time - start_time
      start_time <- Sys.time()
      tmp=oursfun(n,matdat)
      tmpours[j]=tmp[1]
      tmpoursmin[j]=tmp[2]
      end_time <- Sys.time()
      tour2[j]=end_time - start_time
    }
    fifo[i]=mean(tmpfifo)
    fifomin[i]=mean(tmpfifomin)
    greed[i]=mean(tmpgreed)
    rnd[i]=mean(tmprnd)
    ours[i]=mean(tmpours)
    greedmin[i]=mean(tmpgreedmin)
    oursmin[i]=mean(tmpoursmin)
    tour1[i]=mean(tour2)
    trnd1[i]=mean(trnd2)
    tgrd1[i]=mean(tgrd2)
    tfif1[i]=mean(tfif2)
  }
  fifo1[k]=mean(fifo)
  fifomin1[k]=mean(fifomin)
  greed1[k]=mean(greed)
  rnd1[k]=mean(rnd)
  ours1[k]=mean(ours)
  oursmin1[k]=mean(oursmin)
  greedmin1[k]=mean(greedmin)
  tour[k]=sum(tour1)
  trnd[k]=sum(trnd1)
  tgrd[k]=sum(tgrd1)
  tfif[k]=sum(tfif1)
}
min(greed1-ours1)
mean(tour)
mean(tfif)
mean(tgrd)
mean(trnd)

windows()
plot(fifo1,type='l',ylim=c(min(ours1),max(rnd1)+5),col='red',xlab="Index",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","GREEDY","OURS"),col=c("blue", "red","green","black"), lty=1,)
lines(rnd1,col='blue',type='l')
lines(greed1,col='green',type='l')
lines(ours1,col='black',type='l')
windows()
plot(greed1-ours1,ylim=c(min(fifo1-ours1),max(greed1-ours1)+2),type='l',xlab="Index",ylab="Av. makespan Diff.",col="red")
legend("topleft", c("FIFO-OURS","GREEDY-OURS"),col=c("black","red"), lty=1,)
lines(fifo1-ours1,col='black',type='l')

windows()
plot(greed1-greedmin1,type='l',ylim=c(min(ours1-oursmin1),max(greed1-greedmin1)+5),xlab="Index",ylab="Av. TCD",col="red")
legend("topleft", c("GREEDY","FIFO","OURS"),col=c("red","blue","black"), lty=1,)
lines(ours1-oursmin1,col="black")
lines(fifo1-fifomin1,col="blue")



##############################200 timestep continuous
fiflist=rep(0,virtu)
greedlist=rep(0,virtu)
rndlist=rep(0,virtu)
ourslist=rep(0,virtu)


fifofun1=function(n,a,fiflist,tmp)
{
  if(length(tmp)==1)
    tmp=rep(0,10)
  for(i in 1:length(n)){
    k=which.min(tmp[a[i,][a[i,]!=0]])
    tmp[a[i,k]]=tmp[a[i,k]]+1
    fiflist[a[i,k]]=fiflist[a[i,k]]+n[i]
  }
  return(c(max(fiflist),min(fiflist),tmp,fiflist))
}
greedfun1=function(n,a,greedlist)
{
  for(i in 1:length(n)){
    tmp=which.min(greedlist[a[i,][a[i,]!=0]])
    greedlist[a[i,tmp]]=greedlist[a[i,tmp]]+n[i]
  }
  return(c(max(greedlist),min(greedlist),greedlist))
}
rndfun1=function(n,a,rndlist)
{
  for(i in 1:length(n)){
    tmp=sample(a[i,][a[i,]!=0],1)
    tmp=which(a[i,]==tmp)
    rndlist[a[i,tmp]]=rndlist[a[i,tmp]]+n[i]
  }
  return(c(max(rndlist),rndlist))
}
oursfun1=function(n,a,ourslist)
{
  if(length(n)==1){
    tmp=which.min(ourslist[a[a!=0]])
    ourslist[a[tmp]]=ourslist[a[tmp]]+n
    return(c(max(ourslist),min(ourslist),ourslist))
  }
  if(length(n)!=1){
    mp=sortfun1(n,a)
    m=mp[,11]
    b=mp[,1:10]
    mp=sortfun2(m,b)
    m=mp[,11]
    b=mp[,1:10]
    for(i in 1:length(n)){
      tmp=which.min(ourslist[b[i,][b[i,]!=0]])
      ourslist[b[i,tmp]]=ourslist[b[i,tmp]]+m[i]
    }
    return(c(max(ourslist),min(ourslist),ourslist))
  }
}

tmp=1
tmp1=1
trnd=rep(0,200)
tfif=rep(0,200)
tour=rep(0,200)
tgrd=rep(0,200)

for(i in 1:200)
{
  tmpfifo=rep(0,50)
  tmprnd=rep(0,50)
  tmpgreed=rep(0,50)
  tmpours=rep(0,50)
  tmpoursmin=rep(0,50)
  tmpgreedmin=rep(0,50)
  tmpfifomin=rep(0,50)
  trnd1=rep(0,50)
  tfif1=rep(0,50)
  tour1=rep(0,50)
  tgrd1=rep(0,50)
  m=rpois(50,7)+1
  for(j in 1:50){
    n=sample(1:50,m[j],replace = TRUE)
    matdat=matfun(n)
    start_time <- Sys.time()
    temp=fifofun1(n,matdat,fiflist,tmp)
    tmpfifo[j]=temp[1]
    tmpfifomin[j]=temp[2]
    tmp=temp[3:12]
    fiflist=temp[13:length(temp)]
    end_time <- Sys.time()
    tfif1[j]=end_time - start_time
    start_time <- Sys.time()
    temp=rndfun1(n,matdat,rndlist)
    tmprnd[j]=temp[1]
    rndlist=temp[2:length(temp)]
    end_time <- Sys.time()
    trnd1[j]=end_time - start_time
    start_time <- Sys.time()
    temp=greedfun1(n,matdat,greedlist)
    tmpgreed[j]=temp[1]
    tmpgreedmin[j]=temp[2]
    greedlist=temp[3:length(temp)]
    end_time <- Sys.time()
    tgrd1[j]=end_time - start_time
    start_time <- Sys.time()
    temp=oursfun1(n,matdat,ourslist)
    tmpours[j]=temp[1]
    tmpoursmin[j]=temp[2]
    ourslist=temp[3:length(temp)]
    end_time <- Sys.time()
    tour1[j]=end_time - start_time
  }
  tour[i]=mean(tour1)
  trnd[i]=mean(trnd1)
  tgrd[i]=mean(tgrd1)
  tfif[i]=mean(tfif1)
  fifo[i]=mean(tmpfifo)
  greed[i]=mean(tmpgreed)
  rnd[i]=mean(tmprnd)
  ours[i]=mean(tmpours)
  greedmin[i]=mean(tmpgreedmin)
  oursmin[i]=mean(tmpoursmin)
  fifomin[i]=mean(tmpfifomin)
}
min(greed-ours)
min(fifo-ours)
min(rnd-ours)
sum(tour)
sum(tfif)
sum(trnd)
sum(tgrd)

windows()
plot(fifo,type='l',ylim=c(min(ours),max(rnd)),col='red',xlab="Index",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","GREEDY","OURS"),col=c("blue", "red","green","black"), lty=1,)
lines(rnd,col='blue',type='l')
lines(greed,col='green',type='l')
lines(ours,col='black',type='l')
windows()
plot(greed-ours,type='l',xlab="Index",col="black",ylab="Av. makespan Diff.")
plot(fifo-ours,type='l',xlab="Index",col="black",ylab="Av. makespan Diff.")
legend("topleft", c("FIFO-OURS","GREEDY-OURS"),col=c("black","red"), lty=1,)
lines(fifo-ours,col='black',type='l')
plot(greed-ours,type='l',ylim=c(min(greed-ours),max(greed-ours)+1),xlab="Index",ylab="Av. makespan Diff.")

windows()
plot(greed-greedmin,type='l',ylim=c(min(ours-oursmin),max(greed-greedmin)+3),xlab="Index",ylab="Av. TCD",col="red")
legend("topleft", c("GREEDY","OURS","FIFO"),col=c("red","black","blue"), lty=1)
lines(ours-oursmin,col="black")
lines(fifo-fifomin,col="blue")


fiflist=rep(0,virtu)
greedlist=rep(0,virtu)
rndlist=rep(0,virtu)
ourslist=rep(0,virtu)

rnd1=rep(0,50)
greed1=rep(0,50)
ours1=rep(0,50)
fifo1=rep(0,50)
oursmin1=rep(0,50)
greedmin1=rep(0,50)
fifomin1=rep(0,50)
tmp=1
tmp1=1
for(k in 1:50)
{
  for(i in 1:200)
  {
    tmpfifo=rep(0,50)
    tmprnd=rep(0,50)
    tmpgreed=rep(0,50)
    tmpours=rep(0,50)
    tmpoursmin=rep(0,50)
    tmpgreedmin=rep(0,50)
    tmpfifomin=rep(0,50)
    m=rpois(50,7)+1
    for(j in 1:50){
      n=sample(1:50,m[j],replace = TRUE)
      matdat=matfun(n)
      temp=fifofun1(n,matdat,fiflist,tmp)
      tmpfifo[j]=temp[1]
      tmpfifomin[j]=temp[2]
      tmp=temp[3:12]
      fiflist=temp[13:length(temp)]
      temp=rndfun1(n,matdat,rndlist)
      tmprnd[j]=temp[1]
      rndlist=temp[2:length(temp)]
      temp=greedfun1(n,matdat,greedlist)
      tmpgreed[j]=temp[1]
      tmpgreedmin[j]=temp[2]
      greedlist=temp[3:length(temp)]
      temp=oursfun1(n,matdat,ourslist)
      tmpours[j]=temp[1]
      tmpoursmin[j]=temp[2]
      ourslist=temp[3:length(temp)]
    }
    fifo[i]=mean(tmpfifo)
    fifomin[i]=mean(tmpfifomin)
    greed[i]=mean(tmpgreed)
    rnd[i]=mean(tmprnd)
    ours[i]=mean(tmpours)
    greedmin[i]=mean(tmpgreedmin)
    oursmin[i]=mean(tmpoursmin)
  }
  fifo1[k]=mean(fifo)
  fifomin1[k]=mean(fifomin)
  greed1[k]=mean(greed)
  rnd1[k]=mean(rnd)
  ours1[k]=mean(ours)
  oursmin1[k]=mean(oursmin)
  greedmin1[k]=mean(greedmin)
}

windows()
plot(fifo1,type='l',ylim=c(min(ours1),max(rnd1)+20),col='red',xlab="Index",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","GREEDY","OURS"),col=c("blue", "red","green","black"), lty=1,)
lines(rnd1,col='blue',type='l')
lines(greed1,col='green',type='l')
lines(ours1,col='black',type='l')
windows()
plot(greed1-ours1,type='l',col="black",xlab="Index",ylab="Av. makespan Diff.")
plot(fifo1-ours1,type='l',col="black",xlab="Index",ylab="Av. makespan Diff.")
plot(rnd1-ours1,type='l',col="black",xlab="Index",ylab="Av. makespan Diff.")

plot(greed1-ours1,type='l',ylim=c(min(greed1-ours1),max(greed1-ours1)),col="red",xlab="Index",ylab="Av. makespan Diff.")

windows()
plot(greed1-greedmin1,type='l',ylim=c(min(ours1-oursmin1),max(fifo1-fifomin1)),xlab="Index",ylab="Av. TCD",col="red")
legend("topleft", c("GREEDY","FIFO","OURS"),col=c("red","blue","black"), lty=1,)
lines(ours1-oursmin1,col="black")
lines(fifo1-fifomin1,col="blue")

plot(greed1-greedmin1,type='l',ylim=c(min(ours1-oursmin1),max(greed1-greedmin1)+2),xlab="Index",ylab="Av. TCD",col="red")
legend("topleft", c("GREEDY","OURS"),col=c("red","black"), lty=1,)
lines(ours1-oursmin1,col="black")
