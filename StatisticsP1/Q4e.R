####Histogramas para Variável aleatória X - com aproximação N(0,1)#####

EntX=read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 1/dados/data1a.dat",header = FALSE)
vx<-vector()
mx<-vector()
i<-1
n<-100
N<-2000
for (i in 1:2000) {
  wi <-sample(EntX$V1,n)
  vx[i]<-var(wi) #variância amostral
  mx[i]<-(mean(wi)-2)/(sqrt((var(EntX$V1))/n)) #média amostral
}
#### Criando valores para a legenda do histrograma da média #####
gvx<- format(round(var(mx),4), nsmall=4)
gmx<- format(round(mean(mx),4), nsmall=4)
gmreal<- format(round(mean(EntX$V1),4), nsmall=4)
gvreal<- format(round(var(EntX$V1)/n,4), nsmall=4)

##### Histograma da média de X #####
hist(mx,breaks=30, ylim=c(0,0.8), freq=FALSE,main="Histograma das médias amostrais X",xlab = "X",ylab = "Frequência relativa")
x <- seq(min(mx),max(mx),length=200)
y <- dnorm(x,mean=0,sd=1)
lines(x,y, col="blue",lwd=2,add=TRUE)
legend("topright",c(paste("mu=",gmx,",var=",gvx),paste("mu=0, var=1")), col=c("black","blue"), lwd=2)
#dev.off()#
