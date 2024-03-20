print('Hello world?')

install.packages("fitdistrplus")
library(fitdistrplus)

rm(list=ls())

Exp <- function(x, lambda){
  lambda*exp(-lambda*x)
}


vec <- c(1,3,5)
vec

1:5
seq(1,5,by=2)

x <- matrix(1:9, nrow=3, byrow=T)
dim(x)

row.names(x) <- c('a','b','c')
colnames(x) <- c(1,2,3)
x

head(iris)
colnames(iris)

iris[iris$Sepal.Length>1,]

list1 <- list(1:10, matrix(1:12, nrow=4), iris[1:10,])
list1

mode(list1)
length(list1)
names(list1)
names(list1) <- c("vector", "matrix", "dataframe")
list1
list1$matrix[3,3]
list1[[1]]<-NULL
list1

if (iris[1,1]>5){
  print("5보다 큼")
} else {
  print("5보다 작음")
}

x <- iris[,1]
which(x>5)
iris[which(x>5),]
