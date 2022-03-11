library("vioplot")
library("stats")
library("rgl")
library("plot3D")
library("stats")

matfun=function(n,virtu)
{
  a=matrix(data=0,nrow=length(n),ncol=virtu)
  for(i in 1:length(n))
  {
    j=sample(1:virtu,1)
    b=sample(1:virtu,j,replace = FALSE)
    for(k in 1:length(b))
      a[i,k]=b[k]
  }
  return(a)
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
sortfun2=function(n,a,virtu)
{
  l=rep(0,length(a[,1]))
  m=n
  b=a
  for(i in 1:length(n))
    l[i]=length(a[i,][a[i,]!=0])
  for(i in 1:virtu){
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

oursfun1=function(n,a,ourslist,virtu)
{
  if(length(n)==1){
    tmp=which.min(ourslist[a[a!=0]])
    ourslist[a[tmp]]=ourslist[a[tmp]]+n
    return(c(max(ourslist),min(ourslist),ourslist))
  }
  if(length(n)!=1){
    mp=sortfun1(n,a)
    m=mp[,virtu+1]
    b=mp[,1:virtu]
    mp=sortfun2(m,b,virtu)
    m=mp[,virtu+1]
    b=mp[,1:virtu]
    for(i in 1:length(n)){
      tmp=which.min(ourslist[b[i,][b[i,]!=0]])
      ourslist[b[i,tmp]]=ourslist[b[i,tmp]]+m[i]
    }
    return(c(max(ourslist),min(ourslist),ourslist))
  }
}


timegtba=matrix(data=0,nrow=13,ncol=13)
gtba=matrix(data=0,nrow=13,ncol=13)
mingtba=matrix(data=0,nrow=13,ncol=13)
ours=rep(0,20)
oursmin=rep(0,20)
for(kap in 1:13)
{
  virtu=2^(kap-1)
  for(kapp in 1:13)
  {
    medtask=2^(kapp-1)
    ourslist=rep(0,virtu)
    tour=rep(0,20)
    tmpours=matrix(data=0,nrow=20,ncol=20)
    tmpoursmin=matrix(data=0,nrow=20,ncol=20)
    for(j in 1:20){
      ourslist=rep(0,virtu)
      tour1=rep(0,20)
      m=rpois(20,medtask)+1
      for(i in 1:20){
        n=runif(m[i],0,60)
        matdat=matfun(n,virtu)
        start_time <- Sys.time()
        temp=oursfun1(n,matdat,ourslist,virtu)
        end_time <- Sys.time()
        tmpours[i,j]=temp[1]
        tmpoursmin[i,j]=temp[2]
        ourslist=temp[3:length(temp)]
        tour1[i]=(end_time - start_time)
      }
      tour[j]=sum(tour1)
    }
    for(i in 1:20)
    {
      ours[i]=mean(tmpours[i,])
      oursmin[i]=mean(tmpoursmin[i,])
    }
    gtba[kap,kapp]=ours[20]
    mingtba[kap,kapp]=oursmin[20]
    timeour[kap,kapp]=mean(tour)
    print(kapp)
  }
  print(kap)
}
windows()

x1=rep(1,13)
for(i in 2:13)
  x1=c(x1,rep(2^i,13))
y1=2^(1:13)
for(i in 2:13)
  y1=c(y1,2^(1:13))
z1=timeour[1,]
for(i in 2:13)
  z1=c(z1,timeour[i,])
z1=z1*1000
xo=timeour*1000
points3D(log2(y1),log2(x1),log2(z1),phi = 10,theta=35, bty = "g",  type = "p", pch = 1, cex = 0.5,ylab="log2(Number of Proc.)",xlab="log2(Number of Tasks)",zlab="log2(Average Execution Time)",zlim=c(min(xo)-5,max(xo)),col="blue",colkey=TRUE)

z1=gtba[1,]
for(i in 2:13)
  z1=c(z1,gtba[i,])
z1=z1*1000

points3D(log2(y1),log2(x1),z1,phi = 10,theta=35, bty = "g",  type = "p", pch = 1, cex = 0.5,ylab="log2(Number of Proc.)",xlab="log2(Number of Tasks)",zlab="Average Makespan",zlim=c(min(z1),max(z1)))

z1=gtba[1,]-mingtba[1,]
for(i in 2:13)
  z1=c(z1,gtb[i,]-mingtba[i,])
z1=z1*1000

points3D(log2(y1),log2(x1),z1,phi = 10,theta=35, bty = "g",  type = "p", pch = 1, cex = 0.5,ylab="log2(Number of Proc.)",xlab="log2(Number of Tasks)",zlab="Average TCD",zlim=c(min(z1),max(z1)))
i=20
w=vioplot(tmpours[i,]/max(tmpours[i,]),names=c("OBA"),col=c("darkgrey"),ylab=expression("Av.makespan"/"max.makespan"))
