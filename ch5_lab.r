#검증셋 기법
library(ISLR)
set.seed(1)
train<-sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2) #검증set MSE

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2) #검증set MSE

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) #검증set MSE

#다른 훈련셋 선택
set.seed(2)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2) #검증set MSE

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2) #검증set MSE

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) #검증set MSE


#LOO 교차검증
glm.fit=glm(mpg~horsepower,data=Auto) #선형회귀모델
coef(glm.fit)
library(boot)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta #LOOCV 통계량
#다항식적합에 대해 반복
cv.err=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.err[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.err


#k-fold 교차검증
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10


#붓스트랩
#선택된 관측치들을 기반으로 a에 대한 추정치 출력하는 alpha.fn() 생성
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100) #a 추정치
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) #100개 관측치를 랜덤으로 복원추출
#붓스트랩 분석은 이 명령을 여러 번 실행하여 a 추정치, 표준편차 계산
#하지만 boot 함수는 이를 자동으로 해준다.
boot(Portfolio,alpha.fn,R=1000)
#ahat=0.5758, SE(ahat) 붓스트랩 추정치=0.0886

#선형회귀모델의 정확도 추정
boot.fn=function(data,index){
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}
boot.fn(Auto,1:392) 
#boot.fn 함수는 관측치들을 랜덤으로 복원추출하여 절편과 기울기에 대한 붓스트랩 추정치 생성
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
#데이터에 이차모델을 적합
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,
                  subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
