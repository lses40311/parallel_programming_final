rm(list=ls())

core = 1
broader = 2
noise = -1

x = read.table("t4.8k.txt", header = FALSE ,sep = " ")
x = x[2:8001,]
y = read.csv("result.txt",header = FALSE)

d = cbind(x,y) ;

plot(1, type="n", xlab="", ylab="", xlim=c(0, ceiling(max(x[,1]))), ylim=c(0, ceiling(max(x[,2]))))
for(i in c(1:8000)){
  if(d[i,3] == 1){
    points(d[i,1],d[i,2],col = "red" ) ;
  }
  else if(d[i,3] == 2){
    points(d[i,1],d[i,2],col = "blue") ;
  }
  else{
    points(d[i,1],d[i,2],col = "black", pch = 16) ;
  }
}

abc = 1

plot(1, type="n", xlab="", ylab="", xlim=c(0, ceiling(max(x[,1]))), ylim=c(0, ceiling(max(x[,2]))))
for(i in c(1:8000)){
  if(d[i,4]==-1){
    points(d[i,1],d[i,2],col = "black", pch = 20) ;
  }
  else{
    points(d[i,1],d[i,2],col = d[i,4]+1) ;
  }
}

##plot(x[2:8001,] , type = "p",pch=16,col=color[1]) ;