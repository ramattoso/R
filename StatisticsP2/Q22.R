###Letra A###
y<-seq(1,3,length.out=20)
f<- rep(0.5, 20)
plot(y,f, type="l",main="PDF de f(y)", ylab="Frequência relativa")
lines(y,f, col="blue",lwd=2)

###Letra B####
N=5000
j=0
n<-5
t=qt(0.95,df=n-1)
mu=2


for(i in 1:N)
{
  amostras<-runif(n,1,3)
  sigma<-sd(amostras)
  x<-mean(amostras)
  
  if(mu<(x+t*(sigma/sqrt(n))))
  {
    if(mu>(x-t*(sigma/sqrt(n))))
    {
      j<-j+1
    }
  }
}
j

b<-j/N
b

N=5000
j=0
n<-100
t=qnorm(0.95)
mu=2


for(i in 1:N)
{
  amostras<-runif(n,1,3)
  sigma<-sd(amostras)
  x<-mean(amostras)
  
  if(mu<(x+t*(sigma/sqrt(n))))
  {
    if(mu>(x-t*(sigma/sqrt(n))))
    {
      j<-j+1
    }
  }
}
j

b<-j/N
b

