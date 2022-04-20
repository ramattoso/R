#Q3X
Entrada=read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 1/dados/data1a.dat",header = FALSE)
hist(Entrada$V1, freq=FALSE, xlab="X", ylab="Frequência relativa", main="Histograma de X")
y <- rep(0.5, 20)
lines(seq(1, 3, length.out = 20), y, col="blue", add=TRUE)

#Q3Y
Entrada=read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 1/dados/data1b.dat",header = FALSE)
hist(Entrada$V1, prob=TRUE, xlab="Y", ylab="Frequência relativa", main="Histograma de Y")
curve(dexp(x, rate = 1), col = "blue",lwd = 2, add = TRUE)

#Q3T
############################# HISTOGRAMA VA TBinomial ##################################
TBinomial=read.csv("C:/Users/raque/Dropbox/LNCC/Matérias/Estatística/Trabalho 1/dados/data1c.dat",header = FALSE)
histTBinomial <- hist(TBinomial$V1, prob=T, xlab="T", ylab="Frequência Relativa",
                      main = paste("Histograma de T"),
                      breaks = seq(min(TBinomial$V1), max(TBinomial$V1),1),
                      axes = TRUE, plot = T)

xhist<- c(min(histTBinomial$breaks),histTBinomial$breaks, color="black")
yhist<- c(0,histTBinomial$density,0)
xfit <- seq(min(TBinomial$V1),max(TBinomial$V1), 1)
yfit <- dbinom(xfit, 10, 0.25)
points(xfit,yfit, col="blue", lwd=3)

