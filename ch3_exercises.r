library(MASS)
library(ISLR)
str(Carseats)

##8.
Audo<-read.csv("C://data/Auto.csv",header=T,na.strings="?")
Audo=na.omit(Audo)
summary(Audo)
str(Audo)
cor(Audo$mpg,Audo$horsepower)
attach(Audo)
#(a)
#(1,2)
ex8<-lm(Audo$mpg~Audo$horsepower)
summary(ex8)
#p-값이 0에 가까우므로 상관관계가 있다.
4.906/mean(Audo$mpg) #오차확률 20.92%
#root MSE=4.906, R-squred=0.6059
#(3) 계수 -0.1578이므로 음의 관계이다.
#(4)
predict(ex8, data.frame(horsepower=c(98)), interval="confidence")
predict(ex8, data.frame(horsepower=c(98)), interval="prediction")
#(b)
plot(horsepower,mpg)
abline(ex8)
#(c)
par(mfrow=c(2,2))
plot(ex8) #공분산 문제가 있다.


##9.
#(a)
plot(Audo)
#(b)
cor(Audo[,-9])
#(c)
lm9<-lm(mpg~cylinders+displacement+horsepower+
          weight+acceleration+year+origin)
summary(lm9)
#1. F검정의 p-value가 0에 가까우므로 상관관계가 있다
#2. displacement, weight, year, origin
#3. year가 한 단위씩 증가할때마다 mpg가 0.75씩 증가한다.
#(d)
par(mfrow=c(2,2))
plot(lm9) 
#공분산 문제, 3개의 이상치가 있다. 14번째 관측치는 레버리지가 높지만 이상치는 아니다.
#(e)
lm9_e<-lm(mpg~cylinders*displacement*horsepower*
            weight*acceleration+year*origin)
summary(lm9_e) #year-origin, cyl-hors-acc, cyl-dis-wei-acc
#(f)
lm9_f1<-lm(mpg~log(weight)+year+log(origin))
summary(lm9_f1)
plot(Audo)
par(mfrow=c(2,2))
plot(lm9_f1)


##10.
#(a)
summary(Carseats)
lm10<-lm(Sales~Price+Urban+US,data=Carseats)
summary(lm10)
#(b) Price->음의 상관관계가 있다.
#US->yes, no에 따라 Sales에 영향이 있다. US에 상점이 있다면 1201 units만큼 sales 증가한다.
#(C) Yhat=13.04-0.054Price-0.0219UrbanYes+1.2USYes
#(d) Price, USYes : p-value, F
#(e)
lm10_2<-lm(Sales~Price+US,data=Carseats)
summary(lm10_2) #Yhat=13.03-0.054Price+1.12US
#(f) R2의 값은 비슷하다. 2번째 모델은 모든 변수의 p값과 F검정의 p값이 0에 가깝다.
#(g)
confint(lm10_2)
#(h)
par(mfrow=c(2,2))
plot(lm10_2) 
(2+1)/nrow(Carseats) #0.0075 -> 몇몇의 관측치의 leverage가 이보다 큰 값을 가진다.


##14.
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
#(a) Y=2+2x1+0.3x2+e -> 계수 :beta0,1,2 = 2, 2, 0.3
#(b)
plot(x1,x2)
cor(x1,x2) #0.83 양의 상관관계가 있다
#(c)
lm14<-lm(y~x1+x2)
summary(lm14)    
#y hat=2.13+1.44x1+1.01x2, b1은 H0기각, b2는 기각하지 않는다
#b는 b0와 b1은 각각의 beta와 비스산 값을 가진다. b2는 약간 차이가 있다.
#(d)
lm14_2<-lm(y~x1)
summary(lm14_2) #H0기각한다.
#(e)
lm14_3<-lm(y~x2)
summary(lm14_3) #H0기각한다.
#(f)
#x1과 x2에 상관관계가 있어 다중공산성의 문제가 있기 때문에 모순이 생긴다.
#(g)
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)
lm14_4<-lm(y~x1+x2)
summary(lm14_4)
plot(predict(lm14_4), rstudent(lm14_4))
(2+1)/length(y) #0.0297
par(mfrow=c(2,2))
plot(lm14_4) #하나의 관측치의 레버리지가 높다.

lm14_5<-lm(y~x1)
summary(lm14_5)
par(mfrow=c(2,2))
plot(lm14_5) #특별히 크게 레버리지가 높은 관측치가 없다.

lm14_6<-lm(y~x2)
summary(lm14_6)
par(mfrow=c(2,2))
plot(lm14_6) #하나의 관측치의 레버리지가 높다.

##15.
str(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)
attach(Boston)
lm.all = lm(crim~., data=Boston)
summary(lm.all)