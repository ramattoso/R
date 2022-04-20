###Letra A###
x<-seq(-5,5,length.out=1000)
y<- dnorm(x,mean=0,sd=1)
plot(x,y, type="l",main="PDF de f(x)", ylab="Frequência relativa")
lines(x,y, col="blue",lwd=2)

###Letra B####
N=5000
j=0
n<-5
t<-qt(0.975,df=n-1)
mu=0


for(i in 1:N)
{
  amostras<-rnorm(n,0,1)
  S<-sd(amostras)
  x<-mean(amostras)
  
  if(mu<(x+t*(S/sqrt(n))))
  {
    if(mu>(x-t*(S/sqrt(n))))
    {
      j<-j+1
    }
  }
}
j

b<-j/N
b

