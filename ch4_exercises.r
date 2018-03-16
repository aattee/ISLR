library(ISLR)
library(MASS)

#10
#(a)
str(Weekly)
summary(Weekly)
plot(Weekly)
#(b)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Weekly,family=binomial)
summary(glm.fit)
# Lag2의 p-값이 0.02로 가장 작으므로 통계적으로 유의하다고 볼 수 있다.
#(c)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Weekly$Direction)
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Weekly$Direction)
(54+557)/1089 
#56.1% 올바르게 예측했으므로 훈련 오차율은 43.9%이다.
#하지만 이는 1089개 관측치의 동일한 셋에 대해 모델을 훈련하고 검정했다.
#(d)
train=Weekly$Year<=2008
test=Weekly[!train,]
Direction.test=Weekly$Direction[!train]
dim(test)
glm.fit=glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
glm.probs=predict(glm.fit,test,type="response") #2009~2010년 데이터에 대한 확률
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.test)
(9+56)/(9+5+34+56)
#62.5% 올바르게 예측했다.

#(e)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
table(lda.class,Direction.test)
(9+56)/(9+5+34+56)
#62.5% 올바르게 예측했다.

#(f)
qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.class=predict(qda.fit,test)$class
table(qda.class,Direction.test)
61/(43+61)
#58.65% 올바르게 예측했다.

#(g)
library(class)
train.x = as.matrix(Weekly$Lag2[train])
test.x= as.matrix(Weekly$Lag2[!train])
train.Direction=Weekly$Direction[train]
set.seed(1)
knn.pred=knn(train.x,test.x,train.Direction,k=1)
table(knn.pred,Direction.test)
(21+31)/(21+30+22+31)
#50% 올바르게 예측했다.

#(h)
#LDA와 로지스틱이 62.5%로 예측률이 가장 높다.

#(i)
knn.pred4=knn(train.x,test.x,train.Direction,k=4)
table(knn.pred4,Direction.test)
(20+40)/(20+21+23+40) #57.7%

knn.pred10=knn(train.x,test.x,train.Direction,k=10)
table(knn.pred10,Direction.test)
(20+42)/(20+19+23+42) #59.6%

knn.pred20=knn(train.x,test.x,train.Direction,k=20)
table(knn.pred20,Direction.test)
(19+41)/(19+20+24+41) #57.69%