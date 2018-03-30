#IEM计算&生成基本代码
setwd("E:/QD/tools/IEM")
x<-read.csv("Equity score.csv",header = T)
x[x==0]<-NA #把打分0的定义为NA
y<-(1/apply(-x[,2:ncol(x)],1,rank,na.last="keep"))^1.35#降序排序，若升序则改为x,多用降序
z<-t(y)
z[is.na(z)]<-0
kk=matrix(nrow=nrow(z),ncol=ncol(z))
zz=matrix(nrow=nrow(z),ncol=ncol(z))
for (i in 1:nrow(z)) {
  for (j in 1:ncol(z)){
    kk[i,j]<-z[i,j]/sum(z[i,],na.rm = T)*100 #用均值去算的话扩大100倍
    kk[is.nan(kk)]<-0 #把无意义的数定义为0，如0/0会显示NaN
    cnames=paste("b5n0",1:ncol(z),sep="")
    colnames(kk)<-cnames
    zz[i,j]<-paste("dw $","b5n0",j,"=",kk[i,j],",",sep="")#定义变量名，根据自己喜好
  }  
}

head(zz)
zz<-cbind(x[,1],zz)
kk<-cbind(x[,1],kk)#算出的数

cc<-as.data.frame(zz)
class(cc)
dd<-do.call(paste,cc[,2:ncol(cc)])#第二个参数要求串列或数据框，故转换了cc的数据类型
ee<-paste("IF $IOBS/",cc[,1],",THEN,",dd,"ENDIF,",sep="")
final<-cbind(kk,ee)
write.table(final,"D_IEM.csv",sep=",",row.names = FALSE, col.names = F)
