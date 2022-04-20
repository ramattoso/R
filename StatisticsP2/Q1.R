####Letra B #####

X<-read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 2/dataT2_2.dat",header = FALSE)
mu<-seq (3.2, 7.6, len=50)
var<-seq (1.5, 5.5, len=50)
n <- length(X$V1)
sum <- 0
lnL <- c()
for(i in 1:length(mu)){
  sum <- sum((X$V1-mu[i])^2)
  for(j in 1:length(var)){
    lnl <- -(n/2)*(log(var[j]*2*pi))-(1/(2*var[j]))*sum 
    lnL <- append(lnL,lnl)
  }
}
grid <- expand.grid(var=var,mu=mu)
grid[2:3,"lnL"] <- 0
grid$lnL <- lnL
f <- matrix(grid$lnL,50)
persp(mu,var,f,theta=30,phi=10, expand=0.5, main="Função Log-Verossimilhança", xlab="mu", ylab="var", zlab="ln(L)",ltheta=120,shade=0.75,ticktype="detailed")
dev.off

###Curva de Nivel###

X<-read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 2/dataT2_2.dat",header = FALSE)
mu<-seq (3.2, 7.6, len=50)
var<-seq (1.5, 5.5, len=50)
n <- length(X$V1)
sum <- 0
lnL <- c()
for(i in 1:length(mu)){
  sum <- sum((X$V1-mu[i])^2)
  for(j in 1:length(var)){
    lnl <- -(n/2)*(log(var[j]*2*pi))-(1/(2*var[j]))*sum 
    lnL <- append(lnL,lnl)
  }
}
grid <- expand.grid(var=var,mu=mu)
grid[2:3,"lnL"] <- 0
grid$lnL <- lnL
função_logverossimilhança <- matrix(grid$lnL,50)
contour(mu,var,função_logverossimilhança,main="Curvas de Nível de ln(L)",xlab="mu",ylab="var",nlevels=100, col = topo.colors(1))
points(5.47,3.72, col="red", pch=4)
dev.off

###Letra C #####
varX<-var(Entrada$V1)*9/10
muX<-mean(Entrada$V1)
