EntY=read.csv("C:/Users/raque/Dropbox/LNCC/Mat�rias/Estat�stica/Trabalho 1/dados/data1b.dat",header = FALSE)
vy<-vector()
my<-vector()
SQ<-vector()
i<-1
n<-5
N<-2000
for (i in 1:2000) {
  wi <-sample(EntY$V1,n)
  vy[i]<-var(wi) #vari�ncia amostral
  my[i]<-mean(wi) #m�dia amostral
  SQ[i]<-((n-1)/1)*vy[i]
 }

#### Criando valores para a legenda do histrograma da m�dia #####
#gvy<- format(round(var(my),4), nsmall=4)
#gmy<- format(round(mean(my),4), nsmall=4)
#gmreal<- format(round(mean(EntY$V1),4), nsmall=4)
#gvreal<- format(round(var(EntY$V1)/n,4), nsmall=4)

##### Histograma da m�dia de X #####
#hist(my,breaks=30,freq=FALSE,xlim=c(0,3.5),ylim = c(0,3),main="Histograma das m�dias amostrais Y",xlab = "Y",ylab = "Frequ�ncia relativa")
#x <- seq(min(my),max(my),length=200)
#y <- dnorm(x,mean=mean(EntY$V1),sd=sqrt(var(EntY$V1)/n))
#lines(x,y, col="blue",lwd=2,add=TRUE)
#legend("topright",c(paste("mu=",gmy,",var=",gvy),paste("mu=",gmreal,",var=",gvreal)), col=c("black","blue"), lwd=2)
#dev.off()#

###Fazendo a vari�ncia com quiquadrado.
#SQ<-vy
gmv<-format(round(mean(vy),4), nsmall=4)
gvar<- format(round(var(vy),4), nsmall=4)
gmvreal<- format(round(1,4), nsmall=4)
gvreal<- format(round(2*(n-1)/((n-1)/1)^2,4), nsmall=4)

hist(SQ, breaks=30, xlim = c(0,max(SQ)), ylim = c(0,0.25), freq = FALSE, main = "Aproxima��o da Vari�ncia amostral para uma Qui-Quadrado", xlab=expression(paste(chi^2)), ylab="Frequ�ncia Relativa")
#hist(SQ, breaks=30, xlim = c(0,max(SQ)), ylim = c(0,0.05), freq = FALSE, main = "Histograma da Vari�ncia Amostral de Y", xlab=expression(paste("Amostras de ", S[Y]^2)), ylab="Frequ�ncia Relativa")
t<-seq(0, max(SQ), length=200)
lines(t, dchisq(t,df=n-1), col="blue", lwd=2)
legend("topright",c(paste("mu =",gmv,", var = ", gvar), paste("mu =",gmvreal,",var = ", gvreal)), col=c("black","blue"), lwd=2)
#legend("topright",c(paste("mu =",gmv,",var =",gvar)), col=c("black"), lwd=2)

