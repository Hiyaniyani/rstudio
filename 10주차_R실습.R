## 예제1

x1 <- 1:10
x2 <- c(3,5,4,6,5,7,6,8,7,9)
x3 <- c(4,5,6,1,2,3,7,8,9,4)
y <- c(2,6,4,7,3,5,4,4,8,9)

x<-cbind(x1,x2,x3)

cor(x)

vifs <- function(ind1, ind2, dep){
  reg <- lm(dep~ind1+ind2)
  rsq <- summary(reg)$r.squared
  1/(1-rsq)
}

vifs(x2,x3,x1)
vifs(x1,x3,x2)
vifs(x1,x2,x3)

## 예제2

# 데이터 정규화
x_sc <- scale(x)
x_sc

cov(x_sc)

# 고유값 정렬
eig <- eigen(cov(x_sc))
eig

# 고유값이 큰 순서대로 정렬
idx <- order(eig$values, decreasing=T)
idx

eigenvector <- eig$vectors[,idx]

# 데이터를 주성분 공간으로 변환
x_pca <- x_sc %*% eigenvector
x_pca

## 예제3

y_sc <- scale(y)
y_i <- y_sc; x_i <- x_sc
tb <- tp <- 0
pls <- matrix(ncol=ncol(x_sc), nrow=nrow(x_sc))
for (i in 1:ncol(x_sc)){
  y_i <- y_i-tb ; x_i <- x_i-tp
  
  # 공분산을 최대화하는 선형조합 도출
  a <- t(x_i) %*% y_i
  a <- a/sqrt(sum(a^2))
  
  # 선형조합 기반 잠재변수 도출
  pls[,i] <- x_i %*% a
  
  # 선형조합에 대한 회귀계수 산출
  p <- pls[,i] %*% x_i / c(pls[,i] %*% pls[,i])
  b <- pls[,i] %*% y_i / c(pls[,i] %*% pls[,i])
  
  tb <- pls[,i] %*% b
  tp <- pls[,i] %*% p
}

pls
