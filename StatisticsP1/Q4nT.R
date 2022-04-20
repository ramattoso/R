EntT=read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 1/dados/data1c.dat",header = FALSE)
vt<-vector()
mt<-vector()
SQ<-vector()
i<-1
n<-5
N<-2000
for (i in 1:2000) {
  wi <-sample(EntT$V1,n)
  vt[i]<-var(wi)
  mt[i]<-mean(wi)
  SQ[i]<-((n-1)/1.875)*vt[i]
}

#### Criando valores para a legenda do histrograma da média #####
#gvt<- format(round(var(mt),4), nsmall=4)
#gmt<- format(round(mean(mt),4), nsmall=4)
#gmreal<- format(round(mean(EntT$V1),4), nsmall=4)
#gvreal<- format(round(var(EntT$V1)/n,4), nsmall=4)

##### Histograma da média de X #####
#hist(mt,breaks=30,xlim=c(0,5),ylim=c(0,2.5),freq=FALSE,main="Histograma das médias amostrais de T",xlab = "T",ylab = "Frequência relativa")
#x <- seq(min(mt),max(mt),length=200)
#y <- dnorm(x,mean=mean(EntT$V1),sd=sqrt(var(EntT$V1)/n))
#lines(x,y, col="blue",lwd=2,add=TRUE)
#legend("topright",c(paste("mu=",gmt,",var=",gvt),paste("mu=",gmreal,",var=",gvreal)), col=c("black","blue"), lwd=2)
#dev.off()#

###Fazendo a variância com quiquadrado.
#SQ<-vt
gmv<-format(round(mean(vt),4), nsmall=4)
gvar<- format(round(var(vt),4), nsmall=4)
gmvreal<- format(round((1.875),4), nsmall=4)
gvreal<- format(round(2*(n-1)/(((n-1)/1.875)^2),4), nsmall=4)

#hist(SQ, breaks=30, xlim = c(0,12), ylim = c(0,1.2), freq = FALSE, main = "Histograma da Variância Amostral de T", xlab=expression(paste("Amostras de ", S[T]^2)), ylab="Frequência Relativa")
hist(SQ, breaks=30, xlim = c(0,max(SQ)), freq = FALSE, main = "Aproximação da Variância amostral para uma Qui-Quadrado", xlab=expression(paste(chi^2)), ylab="Frequência Relativa")
t<-seq(0, max(SQ), length=200)
lines(t, dchisq(t,df=n-1), col="blue", lwd=2)
legend("topright",c(paste("mu =",gmv,", var = ", gvar), paste("mu =",gmvreal,", var = ", gvreal)), col=c("black","blue"), lwd=2)
#legend("topright",c(paste("mu =",gmv,",var =",gvar)), col=c("black"), lwd=2)


