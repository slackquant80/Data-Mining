# Importing data
wine = read.csv("winequalityCLASS.csv", header = TRUE)

# Factorize for classification
wine$quality = factor(wine$quality)

### Bagging
library(rpart)
library(adabag)
set.seed(1234)
my.control = rpart.control(xval = 0, cp = 0, minsplit = 5)
bag.wine = bagging(quality ~ ., data = wine, mfinal = 100, control = my.control)
# 각각의 분류나무는 교차타당성을 수행하지 않고, 비용복잡 함수에서의 알파 값은 0으로 지정 -> 최대크기 의사결정나무, 각 노드의 최소 데이터 수는 5
# mfinal -> 배깅 방법에서 생성하게 될 분류기의 개수(B = 100)
# 각 100개의 분류기는 'rpart' 분류나무를 이용하여 생성

# Variable importance
print(bag.wine$importance)
importanceplot(bag.wine)

# Error vs. number of trees
evol.wine = errorevol(bag.wine, newdata = wine)
plot.errorevol(evol.wine)

# Making predictions
prob.bag.wine = predict.bagging(bag.wine, newdata = wine)$prob
# 목표변수의 범주값에 대한 예측확률 생성 -> 총 100번의 의사결정나무 중 범주 0, 1로 예측한 횟수를 비율로 표현
head(prob.bag.wine, 5)
cutoff = 0.5    # cutoff
yhat.bag.wine = ifelse(prob.bag.wine[, 2] > cutoff, 1, 0)
# ifelse() -> 1범주에 대한 예측확률값이 기준값 0.5를 상회하면 1 범주로 예측, 그렇지 않으면 0으로 예측

# Evaluation
tab = table(wine$quality, yhat.bag.wine, dnn = c("Observed", "Predicted"))
print(tab)                           # confusion matrix
sum(diag(tab))/sum(tab)              # accuracy
tab[2, 2] / sum(tab[2, ])            # sensitivity
tab[1, 1] / sum(tab[1, ])            # specificity

### Boosting
library(rpart)
library(adabag)
set.seed(1234)
my.control = rpart.control(xval = 0, cp = 0, maxdepth = 4)
boo.wine = boosting(quality ~ ., data = wine, boos = T, mfinal = 100,
                    control = my.control)
# 각 의사결정나무의 최대깊이는 4
# 'boos = T'는 표본추출에 의한 분류기 생성방식을 사용
# 'boos = F' 가중치가 반영된 분류기 생성방식 사용

# Variable importance
print(boo.wine$importance)
importanceplot(boo.wine)

# Error vs. number of trees
evol.wine = errorevol(boo.wine, newdata = wine)
plot.errorevol(evol.wine)

# Making predictions
prob.boo.wine = predict.boosting(boo.wine, newdata = wine)$prob
head(prob.boo.wine, 5)
cutoff = 0.5    # cutoff
yhat.boo.wine = ifelse(prob.boo.wine[, 2] > cutoff, 1, 0)

# Evaluation
tab = table(wine$quality, yhat.boo.wine, dnn = c("Observed", "Predicted"))
print(tab)                           # confusion matrix
sum(diag(tab))/sum(tab)              # accuracy
tab[2, 2] / sum(tab[2, ])            # sensitivity
tab[1, 1] / sum(tab[1, ])            # specificity


### Random Forest
library(randomForest)
set.seed(1234)
rf.wine = randomForest(quality ~., data = wine, ntree = 100, mtry = 5,
                       importance = T, na.action = na.omit)
# ntree : 분류기의 개수(B = 100)
# mtry : 중간노드마다 랜덤하게 선택되는 변수들의 개수 의미
# importance : 변수의 중요도를 계산하는 옵션
# na.action : 결측치 처리 방식... 필요한 경우에만 삭제

# Variable importance
importance(rf.wine, type = 1)
varImpPlot(rf.wine, type = 1)

# Plot error rates
plot(rf.wine, type = 'l')
# x축은 분류기 개수, y축은 OOB 오분류율 'out-of-bag' -> 부트스트랩에 포함되지 못한 데이터 의미
# OOB 데이터는 검증 데이터의 역할과 비슷
# 가장 아래쪽 선 -> 목표변수 중 1 범주에 해당하는 오분류율, 가장 위쪽 선 -> 0 범주에 해당하는 오분류율
# 가운데 검정실선 -> 전체 오분류율... 80개 이상이면 전체 오분류의 결과가 비교적 안정적

# Partial dependence plot
partialPlot(rf.wine, pred.data = wine, x.var = 'alcohol', which.class = 1)
# x.var : 변동시킬 특정 변수명, which.class : 예측값을 계산하는 목표변수의 범주명 의미
# alcohol 변수는 9~12 값을 가지는 범위에서는 값이 커질수록 우수등급 와인이 될 확률이 증가하는 것으로 예측
# 12 초과하면서 값이 커질수록 우수등급 와닌이 될 확률은 정체하거나 오히려 약간 감소

# Making predictions
prob.rf.wine = predict(rf.wine, newdata = wine, type = "prob")
head(prob.rf.wine, 5)
cutoff = 0.5    # cutoff
yhat.rf.wine = ifelse(prob.rf.wine[, 2] > cutoff, 1, 0)

# Evaluation
tab = table(wine$quality, yhat.rf.wine, dnn = c("Observed", "Predicted"))
print(tab)                           # confusion matrix
sum(diag(tab))/sum(tab)              # accuracy
tab[2, 2] / sum(tab[2, ])            # sensitivity
tab[1, 1] / sum(tab[1, ])            # specificity

### 회귀앙상블 사례분석
# 의류 생산성 데이터 이용
# R에서 제공하는 배깅과 부스팅의 패키지인 adabag 에서는 회귀앙상블은 가능하지 않고 분류 앙상블만 가능
# randomForest 라는 패키지는 회귀앙상블 가능

# Importing data
prod = read.csv("productivityREG.csv", header = TRUE)

# Factorizing predictor variables
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$quarter)
prod$day = factor(prod$day)
prod$team = factor(prod$team)
# 3장의 회귀의사결정나무모형과 마찬가지로 램덤포레스트 앙상블 방법에서는 범주형 변수를 가변수로 변환하지 않고 있는 그대로 사용 가능

## Random Forest
library(randomForest)
set.seed(1234)
rf.prod <- randomForest(productivity~., data = prod, ntree = 100, try = 5, importance = T, na.action = na.omit)

# Variable importance
importance(rf.prod, type = 1)
varImpPlot(rf.prod, type = 1)

# Plot error rates
plot(rf.prod, type = "l")
# y축은 MSE : 회귀나무의 개수가 60개 이상이면 전체 평균제곱오차ㅡ이 결과가 비교적 안정적

# Partial dependence plot
partialPlot(rf.prod, pred.data = prod, x.var = 'incentive')

# Making predictions
pred.rf.prod = predict(rf.prod, newdata = prod, type = "response")
head(pred.rf.prod, 5)

# Evaluation
mean((prod$productivity - pred.rf.prod) ^ 2)   # MSE
mean(abs(prod$productivity - pred.rf.prod))    # MAE

# Observed vs. Predicted
plot(prod$productivity, pred.rf.prod, xlab = "Observed Values",
     ylab = "Fitted Values")
abline(0, 1)
