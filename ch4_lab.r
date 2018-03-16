#주식시장 자료
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket) #Error : Direction은 질적 변수
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#로지스틱 회귀
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial) #로지스틱 회귀
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

glm.probs=predict(glm.fit,type="response") #주어진 설명변수 값에 대해 상승할 확률 예측
glm.probs[1:10]
contrasts(Direction) #1이 Up임을 알 수 있다.

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction)
mean(glm.pred==Direction)

train=(Year<2005) #혼동행렬
(145+507)/1250 #52.2% 올바르게 예측했다.
mean(glm.pred==Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,] #검증 set
dim(Smarket.2005)
Direction.2005=Direction[!train] #검증 set

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) #검정오차율

#Lag1, Lag2만을 사용하여 로지스틱 회귀
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,
            subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),
                                   Lag2=c(1.1,-0.8)),type="response")


#선형판별분석
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class,Direction.2005)

sum(lda.pred$posterior[,1]>=.5) #임계치 50%
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1] #이 때의 사후 확률은 하락할 확률 의미
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9) #사후확률이 90%이상인 경우에만 하락 예측


#이차판별분석
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)


#K-최근접이웃
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

#Caraban 보험 자료에 적용
dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X=scale(Caravan[,-86]) #데이터 표준
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred) #KNN 오류율
mean(test.Y!="No")
table(knn.pred,test.Y)

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15


#로지스틱 회귀모델
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,
            subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)