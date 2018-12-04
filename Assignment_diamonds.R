#=============================================================================================
# ASSIGNING THE TRAIN AND TEST DATA
#=============================================================================================
install.packages("ggplot2")
dim(diamonds)
library('ggplot2')
wd <- getwd()
set.seed(0)
rand = sample(1:nrow(diamonds),47198)
train = diamonds[rand, ]
test = diamonds[-rand, ]
nrow(test) # 6742 data in test
nrow(train)# 47198 data in train
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================


testerror<-NULL
ntrain<-NULL


# PASS THE n VALUE WHICH IS SAMPLE SIZE OF TRAIN DATA
for(n in c(10,30,40,60,70,120,150,170,200,220,240,260,280,300,310,330,350)){
set1<-sample(1:nrow(train),n)
train1 = train[set1, ]
# MODEL
m7o <- lm(price ~ carat  + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^6) + I(carat^7), train1)
# PREDICTED VALUE
pred = predict(m7o, newdata=test)
testerror<-c(testerror,(sum((pred-test$price)^2)))
ntrain<-c(ntrain,n)
}

# PLOT TRAIN SAMPLE SIZE VS TEST ERROR
jpeg('rplot.jpg')

plot(ntrain,testerror,type = "n",xlab = "Train  Sample  Size",ylab = "Test  Error",cex.main=1.5, cex.lab=1.5, cex=1.5,
     main = "Test Error Vs Train sample Size for order 7")

smoothingSpline = smooth.spline(ntrain, testerror, spar=0.35)

lines(smoothingSpline,col="blue",lwd=2)

dev.off()

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================


testerror<-NULL
ntrain<-NULL


# PASS THE n VALUE WHICH IS SAMPLE SIZE OF TRAIN DATA
for(n in c(10,30,40,60,70,120,150,170,200,220,240,260,280,300,310,330,350)){
  set1<-sample(1:nrow(train),n)
  train1 = train[set1, ]
  # MODEL
  m7o <- lm(price ~ carat  + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^6) , train1)
  # PREDICTED VALUE
  pred = predict(m7o, newdata=test)
  testerror<-c(testerror,(sum((pred-test$price)^2)))
  ntrain<-c(ntrain,n)
}

# PLOT TRAIN SAMPLE SIZE VS TEST ERROR
jpeg('rplot2.jpg')

plot(ntrain,testerror,type = "n",xlab = "Train  Sample  Size",ylab = "Test  Error",cex.main=1.5, cex.lab=1.5, cex=1.5,
     main = "Test Error Vs Train sample Size for order 6")

smoothingSpline = smooth.spline(ntrain, testerror, spar=0.40)

lines(smoothingSpline,col="blue",lwd=2)

dev.off()

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================


testerror<-NULL
ntrain<-NULL


# PASS THE n VALUE WHICH IS SAMPLE SIZE OF TRAIN DATA
for(n in c(10,30,40,60,70,120,150,170,200,220,240,260,280,300,310,330,350)){
  set1<-sample(1:nrow(train),n)
  train1 = train[set1, ]
  # MODEL
  m7o <- lm(price ~ carat  + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^6) + I(carat^7)+ I(carat^8), train1)
  # PREDICTED VALUE
  pred = predict(m7o, newdata=test)
  testerror<-c(testerror,(sum((pred-test$price)^2)))
  ntrain<-c(ntrain,n)
}

# PLOT TRAIN SAMPLE SIZE VS TEST ERROR
jpeg('rplot3.jpg')

plot(ntrain,testerror,type = "n",xlab = "Train  Sample  Size",ylab = "Test  Error",cex.main=1.5, cex.lab=1.5, cex=1.5,
     main = "Test Error Vs Train sample Size for order 8")

smoothingSpline = smooth.spline(ntrain, testerror, spar=0.40)

lines(smoothingSpline,col="blue",lwd=2)

dev.off()
##=========================================================
#2. ALL REGRESSION LINES IN ONE PLOT

model1<- lm(price ~ carat , train)
model2<-lm(price ~ carat  + I(carat^2) , train)
model3<-lm(price ~ carat  + I(carat^2)+I(carat^3) , train)
model4<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4),train)
model5<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5),train)
model6<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6),train)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , train)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , train)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), train)
model10<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8)++I(carat^9)+I(carat^10) , train)
#PLOTTING THE MODEL OVER THE DATA
jpeg('rplot4.jpg',width = 800, height = 800)
plot(train$carat,train$price,pch=19, cex.main=1.5, cex.lab=1.5, cex=1.5,xlab = "Carat",ylab = "Price",main = "Regression Lines of all Orders")
lines(sort(train$carat), fitted(model1)[order(train$carat)], col='#009999', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model2)[order(train$carat)], col='blue', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model3)[order(train$carat)], col='darkgrey', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model4)[order(train$carat)], col='orchid4', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model5)[order(train$carat)], col='turquoise3', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model6)[order(train$carat)], col='maroon1', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model7)[order(train$carat)], col='red', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model8)[order(train$carat)], col='purple', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model9)[order(train$carat)], col='yellow', type='l',pch=20,lwd=2)
lines(sort(train$carat), fitted(model10)[order(train$carat)], col='orange', type='l',pch=20,lwd=2)
legend("topleft", legend=c("Order 1", "Order 2","Order 3","Order 4","Order 5","Order 6","Order 7","Order 8","Order 9","Order 10"),
       col=c("#009999", "blue","darkgrey","orchid4","turquoise3","maroon1","red","purple","yellow","orange"), lty=1, cex=1.0)

dev.off()

#=========================================================================================================
n=100
set.seed(20)
set1<-sample(1:nrow(train),n,replace = FALSE)
n1 = train[set1, ]
set.seed(40)
set1<-sample(1:nrow(train),n,replace = FALSE)
n2 = train[set1, ]
set.seed(50)
set1<-sample(1:nrow(train),n,replace = FALSE)
n3 = train[set1, ]
set.seed(30)
set1<-sample(1:nrow(train),n,replace = FALSE)
n4 = train[set1, ]
modelcomp<-c(1,2,7,8,9,10)
testRSS<-c(100000000,100000000,100000000,100000000,100000000,200000000)
jpeg('rplot5.jpg',width = 800, height = 800)
plot(modelcomp,testRSS,type="n",pch=19, cex.main=1.5, cex.lab=1.5, cex=1.5,xlab = "Model Complexity",ylab = "Test RSS",main = "Test RSS VS Model Complexity of sample size 100")

df20<-NULL
df20$order<-c(1,2,7,8,9,10)
#q<-ggplot( data=df20,aes(x=order, y=testRSS),xlab="Order",ylab="Test RSS")

model1<- lm(price ~ carat , n1)
model2<-lm(price ~ carat  + I(carat^2) , n1)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n1)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n1)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n1)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n1)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n1<-testRSS
testRSS
#q+  geom_line(colour="orange",lwd=1)+
 # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #      panel.background = element_blank(), axis.line = element_line(colour = "black"))
#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="orange",lwd=2)

model1<- lm(price ~ carat , n2)
model2<-lm(price ~ carat  + I(carat^2) , n2)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n2)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n2)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n2)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n2)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n2<-testRSS

#df20<-data.frame(df20)
#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="blue",lwd=2)
#q+  geom_line(colour="blue",lwd=1)+
 # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #      panel.background = element_blank(), axis.line = element_line(colour = "black"))

model1<- lm(price ~ carat , n3)
model2<-lm(price ~ carat  + I(carat^2) , n3)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n3)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n3)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n3)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n3)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n3<-testRSS

#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="turquoise3",lwd=2)


model1<- lm(price ~ carat , n4)
model2<-lm(price ~ carat  + I(carat^2) , n4)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n4)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n4)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n4)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n4)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n4<-testRSS

#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="maroon1",lwd=2)

df20<-data.frame(df20)

smoothingSpline = smooth.spline(df20$order, df20$n1, spar=0.35)
lines(smoothingSpline,col="orange",lwd=2)

smoothingSpline = smooth.spline(df20$order, df20$n2, spar=0.35)
lines(smoothingSpline,col="blue",lwd=2)

smoothingSpline = smooth.spline(df20$order, df20$n3, spar=0.35)
lines(smoothingSpline,col="turquoise3",lwd=2)

smoothingSpline = smooth.spline(df20$order, df20$n4, spar=0.35)
lines(smoothingSpline,col="maroon1",lwd=2)


legend("topright", legend=c("Sample 1", "Sample2","Sample3","Sample4"),
       col=c("orange", "blue","turquoise3","maroon1"), lty=1, cex=2.0)
dev.off()
#================= for sample size 20 ==================================================
n=20
set.seed(20)
set1<-sample(1:nrow(train),n,replace = FALSE)
n1 = train[set1, ]
set.seed(40)
set1<-sample(1:nrow(train),n,replace = FALSE)
n2 = train[set1, ]
set.seed(50)
set1<-sample(1:nrow(train),n,replace = FALSE)
n3 = train[set1, ]
set.seed(30)
set1<-sample(1:nrow(train),n,replace = FALSE)
n4 = train[set1, ]
modelcomp<-c(1,2,7,8,9,10)
testRSS<-c(10000,10000000,100000,100000000,1000000,200000)
jpeg('rplot6.jpg',width = 800, height = 800)
plot(modelcomp,testRSS,type="n",pch=19,cex.main=1.5, cex.lab=1.5, cex=1.5,xlab = "Model Complexity",ylab = "Test RSS",main = "Test RSS VS Model Complexity of sample size 20")

df20<-NULL
df20$order<-c(1,2,7,8,9,10)
#q<-ggplot( data=df20,aes(x=order, y=testRSS),xlab="Order",ylab="Test RSS")

model1<- lm(price ~ carat , n1)
model2<-lm(price ~ carat  + I(carat^2) , n1)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n1)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n1)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n1)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n1)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n1<-testRSS
testRSS
#q+  geom_line(colour="orange",lwd=1)+
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#      panel.background = element_blank(), axis.line = element_line(colour = "black"))
#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="orange",lwd=2)

model1<- lm(price ~ carat , n2)
model2<-lm(price ~ carat  + I(carat^2) , n2)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n2)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n2)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n2)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n2)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n2<-testRSS

#df20<-data.frame(df20)
#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="blue",lwd=2)
#q+  geom_line(colour="blue",lwd=1)+
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#      panel.background = element_blank(), axis.line = element_line(colour = "black"))

model1<- lm(price ~ carat , n3)
model2<-lm(price ~ carat  + I(carat^2) , n3)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n3)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n3)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n3)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n3)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n3<-testRSS

#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="turquoise3",lwd=2)


model1<- lm(price ~ carat , n4)
model2<-lm(price ~ carat  + I(carat^2) , n4)
model7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , n4)
model8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , n4)
model9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), n4)
model10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , n4)

testRSS<-c(sum(model1$residuals^2),sum(model2$residuals^2),sum(model7$residuals^2),sum(model8$residuals^2),sum(model9$residuals^2),sum(model10$residuals^2))
df20$n4<-testRSS

#smoothingSpline = smooth.spline(df20$order, testRSS, spar=0.35)
#lines(smoothingSpline,col="maroon1",lwd=2)

df20<-data.frame(df20)

smoothingSpline = smooth.spline(df20$order, df20$n1, spar=0.35)
lines(smoothingSpline,col="orange",lwd=2)

smoothingSpline = smooth.spline(df20$order, df20$n2, spar=0.35)
lines(smoothingSpline,col="blue",lwd=2)

smoothingSpline = smooth.spline(df20$order, df20$n3, spar=0.35)
lines(smoothingSpline,col="turquoise3",lwd=2)

smoothingSpline = smooth.spline(df20$order,  df20$n4, spar=0.35)
lines(smoothingSpline,col="maroon1",lwd=2)


legend("topright", legend=c("Sample 1", "Sample2","Sample3","Sample4"),
       col=c("orange", "blue","turquoise3","maroon1"), lty=1, cex=2.0)
dev.off()
#====================================================================================
# 4. RMSE train - RMSE test vs Model Complexitty
nrow(train)
head(train)
set1<-sample(1:nrow(train),5400,replace = FALSE)
train4 = train[set1, ]
order<-c(1,2,3,4,5,6,7,8,9,10)
RMSEtest<-NULL
RMSEtrain<-NULL
# building models for all oders

nmodel1<- lm(price ~ carat , train4)
nmodel2<-lm(price ~ carat  + I(carat^2) , train4)
nmodel3<-lm(price ~ carat  + I(carat^2)+I(carat^3),train4)
nmodel4<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4),train4)
nmodel5<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5),train4)
nmodel6<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6),train4)
nmodel7<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7) , train4)
nmodel8<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) , train4)
nmodel9<-lm(price ~ carat  + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9), train4)
nmodel10<-lm(price ~ carat + I(carat^2)+I(carat^3)+I(carat^4)+I(carat^5)+I(carat^6)+I(carat^7)+I(carat^8) +I(carat^9)+I(carat^10) , train4)

#PLOTTING THE MODEL OVER THE DATA

#plot(train4$carat,train4$price, pch=19, cex.main=1.5, cex.lab=1.5, cex=1.5)
#lines(sort(train4$carat), fitted(m2o)[order(train4$carat)], col='maroon1', type='l', pch=19,lwd=3)

#TRAIN AND TEST ACCURACY
RMSEtrain<-c(sqrt(sum(nmodel1$residuals^2)),sqrt(sum(nmodel2$residuals^2)),sqrt(sum(nmodel3$residuals^2)),
             sqrt(sum(nmodel4$residuals^2)),sqrt(sum(nmodel5$residuals^2)),sqrt(sum(nmodel6$residuals^2)),
             sqrt(sum(nmodel7$residuals^2)),sqrt(sum(nmodel8$residuals^2)),sqrt(sum(nmodel9$residuals^2)),
             sqrt(sum(nmodel10$residuals^2)))

pred = predict(nmodel1, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel2, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel3, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel4, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel5, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel6, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel7, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel8, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel9, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

pred = predict(nmodel10, newdata=test)
RMSEtest<-c(RMSEtest,sqrt(sum((pred-test$price)^2)))

RMSEtest

jpeg('rplot7.jpg')
plot(order,c(1000,100000,100000,100000,100000,1000000,100000,100000,1000000,1000000),type="n",xlab = "Model Complexity",
     ylab="RMSE Test + RMSE Train", pch=19, cex.main=1.5, cex.lab=1.3, cex=1.5,main = "RMSE Train + RMSE Train Vs Model Complexity")
smoothingSpline = smooth.spline(order, RMSEtrain, spar=0.35)
lines(smoothingSpline,col="maroon4",lwd=2)
smoothingSpline = smooth.spline(order, RMSEtest, spar=0.55)
lines(order,RMSEtest,col="turquoise",lwd=2)
legend("topleft",legend=c("RMSE Train", "RMSE Test"),
       col=c("maroon4", "turquoise"), lty=1, cex=1.3)
dev.off()
