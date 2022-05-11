mortality <- read.table("taiwan.mortality.txt",header = F)
mortality <- mortality[,-1]
colnames(mortality) <- c(0,1,seq(5,80,5))

total.mortality <- mortality[seq(1,58,3),]
man.mortality <- mortality[seq(2,59,3),]
woman.mortality <- mortality[seq(3,60,3),]

row.names(total.mortality) <- c(2001:2020)
row.names(man.mortality) <- c(2001:2020)
row.names(woman.mortality) <- c(2001:2020)

train.T <- total.mortality[c(1:15),]
test.T <- total.mortality[c(16:20),]



#SVD----
train.T <- log(train.T) # get ln(mxt)
a <- apply(train.T,2,mean) #get ax = average(sum(lm(mxt)) 
for (i in c(1:ncol(train.T))) train.T[,i] = train.T[,i]-a[i]
svd <- svd(train.T,1,1)
kt <- svd$u * svd$d[1]
bx <- svd$v 
plot(bx,type = 'l',x = c(0,1,seq(5,80,5)),xlab = 'age')
plot(kt,type= 'l',x = c(2001:2015), xlab = 'year')

bx.kt <- svd$d[1] * svd$u %*% t(svd$v) # fit value by SVD
for (i in c(1:ncol(bx.kt))) bx.kt[,i] = a[i] + bx.kt[,i] 


#predict 5 years times trend
#auto.arima(kt,test = 'adf',ic = 'aic')
model.svd.kt = arima(kt,order = c(5,1,1))
kt.pred = predict(model.svd.kt, n.ahead = 5)$pred
plot(c(kt,kt.pred),x= c(2001:2020),xlab = 'year',ylab = 'kt',main   = 'kt(2001:2015) & k.pred(2016:2020',type = 'l')


bk <- kt %*% t(bx)
bk.pred <- kt.pred %*% t(bx)

#轉換為原始死亡率之fit與predict
for (i in c(1:ncol(bk.pred))) bk.pred[,i] = bk.pred[,i] + a[i]
bk.pred <- exp(bk.pred)
for (i in c(1:ncol(bk))) bk[,i] = bk[,i] + a[i]
bk <- exp(bk)


colnames(bk.pred) <- c(0,1,seq(5,80,5))
row.names(bk.pred) <- c(2016:2020)
colnames(bk) <- c(0,1,seq(5,80,5))
row.names(bk) <- c(2001:2015)
#data.svd <- rbind(bk,bk.pred)
train.T <- total.mortality[c(1:15),]
data.svd <- rbind(train.T,bk.pred)

#2016~2020
pred.avg.by.age.5year <- apply(bk.pred, 1, mean)
original.avg.by.age.5year <- apply(test.T, 1, mean)


plot(pred.avg.by.age.5year,x= c(2016:2020),
     type = 'l',col = 'red',
     xlab = 'year',ylab='mortality',
     main =  'predic mortality by SVD')
lines(original.avg.by.age.5year,x = c(2016:2020),col = 'black')

#2001~2020
pred.avg.by.age <- apply(data.svd, 1, mean)
original.avg.by.age <- apply(total.mortality, 1, mean)

plot(pred.avg.by.age,x= c(2001:2020),
     type = 'l',col = 'red',
     xlab = 'year',ylab='mortality',
     main = 'predict 2016~2020 mortality by SVD')
lines(original.avg.by.age,x = c(2001:2020),col = 'black')
legend('topright',c('true','predict'),lty = 1,lwd = 1,col = c('black','red'))

#MAPE
100*(abs(pred.avg.by.age[16:20]-original.avg.by.age[16:20]))/original.avg.by.age[16:20]

#PCA----
train.T <- total.mortality[c(1:15),]
test.T <- total.mortality[c(16:20),]

train.T <- log(train.T) # get ln(mxt)
a <- apply(train.T,2,mean) #get ax = average(sum(lm(mxt)) 
for (i in c(1:ncol(train.T))) train.T[,i] = train.T[,i]-a[i]

pca.mortality <- princomp(covmat = cov(scale(train.T)))
summary(pca.mortality)
result <- loadings(pca.mortality)

loading <- result[,1]
loading <- as.matrix(loading,ncol = 1)
scores <- as.matrix(train.T)%*%loading
plot(scores,type = 'l')


mode.pca.kt = arima(scores,order = c(1,1,1))
scores.pred = predict(mode.pca.kt, n.ahead = 5)$pred
plot(c(scores,scores.pred),type = 'l')

fit <- scores%*%t(loading)
pred <- scores.pred%*%t(loading)


for (i in c(1:ncol(pred))) pred[,i] = pred[,i] + a[i]
pred <- exp(pred)
for (i in c(1:ncol(fit))) fit[,i] = fit[,i] + a[i]
fit <- exp(fit)


colnames(pred) <- c(0,1,seq(5,80,5))
row.names(pred) <- c(2016:2020)
colnames(fit) <- c(0,1,seq(5,80,5))
row.names(fit) <- c(2001:2015)
#data.pca <- rbind(fit,pred)
train.T <- total.mortality[c(1:15),]
data.pca <- rbind(train.T,pred)


#2016~2020
pred.avg.age.pca <- apply(pred, 1, mean)
test.avg.age.pca <- apply(test.T, 1, mean)


plot(pred.avg.age.pca,x= c(2016:2020),
     type = 'l',col = 'red',
     xlab = 'year',ylab='mortality',
     main =  'predic mortality by PCA')
lines(test.avg.age.pca,x = c(2016:2020),col = 'black')

#2001~2020
fit.avg.by.age.pca <- apply(data.pca, 1, mean)
original.avg.age.pca <- apply(total.mortality, 1, mean)

plot(fit.avg.by.age.pca,x= c(2001:2020),
     type = 'l',col = 'red',
     xlab = 'year',ylab='mortality',
     main = 'predict 2016~2020 mortality by PCA')
lines(original.avg.age.pca,x = c(2001:2020),col = 'black')
legend('topright',c('true','predict'),lty = 1,lwd = 1,col = c('black','red'))



#MAPE
100*(abs(fit.avg.by.age.pca[16:20]-original.avg.age.pca[16:20]))/original.avg.age.pca[16:20]
