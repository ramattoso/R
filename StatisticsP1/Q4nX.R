####Histogramas para Variável aleatória X#####

EntX=read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 1/dados/data1a.dat",header = FALSE)
vx<-vector()
mx<-vector()
SQ<-vector()
i<-1
n<-50
N<-2000
for (i in 1:2000) {
  wi <-sample(EntX$V1,n)
  vx[i]<-var(wi) #variância amostral
  mx[i]<-mean(wi) #média amostral
  SQ[i]<-((n-1)/0.333)*vx[i]
}
#### Criando valores para a legenda do histrograma da média #####
gvx<- format(round(var(mx),4), nsmall=4)
gmx<- format(round(mean(mx),4), nsmall=4)
gmreal<- format(round(mean(EntX$V1),4), nsmall=4)
gvreal<- format(round(var(EntX$V1)/n,4), nsmall=4)

##### Histograma da média de X #####
hist(mx,breaks=30,xlim=c(1,3),ylim=c(0, 5),freq=FALSE,main="Histograma das médias amostrais X",xlab = "X",ylab = "Frequência relativa")
x <- seq(min(mx),max(mx),length=200)
y <- dnorm(x,mean=mean(EntX$V1),sd=sqrt(var(EntX$V1)/n))
lines(x,y, col="blue",lwd=2,add=TRUE)
legend("topright",c(paste("mu=",gmx,",var=",gvx),paste("mu=",gmreal,",var=",gvreal)), col=c("black","blue"), lwd=2)
#dev.off()#

###Fazendo a variância com quiquadrado.
#SQ<-vx
gmv<-format(round(mean(vx),4), nsmall=4)
gvar<- format(round(var(vx),4), nsmall=4)
gmreal<- format(round(0.333,4), nsmall=4)
gvreal<- format(round(2*(n-1)/(((n-1)/0.333)^2),4), nsmall=4)

#hist(SQ, breaks=30, xlim = c(0,10), ylim = c(0,1), freq = FALSE, main = "Histograma da Variância Amostral de X", xlab=expression(paste("Amostras de ", S[X]^2)), ylab="Frequência Relativa")
hist(SQ, breaks=30, xlim = c(0,max(SQ)), freq = FALSE, main = "Aproximação da Variância amostral para uma Qui-Quadrado", xlab=expression(paste(chi^2)), ylab="Frequência Relativa")
t<-seq(0, max(SQ), length=200)
a<-dchisq(t,df=n-1)
lines(t, a, col="blue", lwd=2)
legend("topright",c(paste("mu =",gmv,", var = ", gvar), paste("mu =",gmreal,", var = ", gvreal)), col=c("black","blue"), lwd=2)
#legend("topright",c(paste("mu =",gmv,",var =",gvar)), col=c("black"), lwd=2)

