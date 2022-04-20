### Letra A ###
x <- seq(25, 39, length.out=100)
plot(x, dnorm(x, mean=32, sd=6/sqrt(20)), xlab="x", ylab="f(x)", type="l")

x=seq(35.12112,39,length=200)
y=dnorm(x, mean=32, sd=6/sqrt(20))
polygon(c(35.12112,x,39),c(0,y,0), col = "blue")
polygon(c(35.12112,x,39),c(0,y,0), col = "black", density=c(40,40), angle=c(-45,45))
points(35,0,col="red", pch=4)
abline(h=0, col="black")
dev.off

###Letra B###

qnorm(0.99,mean=32,sd=6/sqrt(20))
.x <- seq(25, 45, length.out=100)
inter<-seq(25, 35.12112, length.out=100)
inter2<-seq(35.12112, 45 , length.out=100)
plot(.x, dnorm(.x, mean=32, sd=6/sqrt(20)),type="l",ylab="f(x)",xlab="x")
lines(.x,dnorm(.x, mean=35, sd=6/sqrt(20)),add=T)
cord.x<-c(12.257,inter,35.12112)
cord.y<-c(0,dnorm(inter,mean=35, sd=6/sqrt(20)),0)
polygon(cord.x,cord.y,col="red")
polygon(cord.x,cord.y,col="darkred",density=c(10, 40),angle=c(-45,45))
cord.x<-c(35.12112,inter2)
cord.y<-c(0,dnorm(inter2,mean=32, sd=6/sqrt(20)))
polygon(cord.x,cord.y,col="blue")
polygon(cord.x,cord.y,col="darkblue",density=c(40, 40),angle=c(30,-30))
abline(h=0, col="black")

#legend("topright",c(paste('oi'),paste('oi')),col=c("black","blue"), lwd=2)
dev.off

### Letra D ###
### A ####
x <- seq(25, 39, length.out=100)
plot(x, dnorm(x, mean=32, sd=6/sqrt(100)), xlab="x", ylab="f(x)", type="l")

x=seq(33.39581,39,length=200)
y=dnorm(x, mean=32, sd=6/sqrt(100))
points(35,0, col="red", pch=4)
polygon(c(33.39581,x,39),c(0,y,0), col = "blue")
polygon(c(33.39581,x,39),c(0,y,0), col = "black", density=c(40,40), angle=c(-45,45))
#polygon(c(35,x,39),c(0,y,0), col = "black", density=c(40,40), angle=c(-45,45))
dev.off

###B###

qnorm(0.99,mean=32,sd=6/sqrt(100))
.x <- seq(25, 45, length.out=200)
inter<-seq(25, 33.39581, length.out=200)
inter2<-seq(33.39581, 45 , length.out=200)
plot(.x, dnorm(.x, mean=32, sd=6/sqrt(100)),type="l",ylab="Frequência relativa",xlab="x")
lines(.x,dnorm(.x, mean=35, sd=6/sqrt(100)),add=T)
cord.x<-c(25,inter,33.39581)
cord.y<-c(0,dnorm(inter,mean=35, sd=6/sqrt(100)),0)
polygon(cord.x,cord.y,col="red")
polygon(cord.x,cord.y,col="darkred",density=c(40, 40),angle=c(-45,45))
cord.x<-c(33.39581,inter2)
cord.y<-c(0,dnorm(inter2,mean=32, sd=6/sqrt(100)))
polygon(cord.x,cord.y,col="blue")
polygon(cord.x,cord.y,col="darkblue",density=c(40, 40),angle=c(30,-30))
abline(h=0, col="black")
dev.off

