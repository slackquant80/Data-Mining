library(tidyverse)

# Importing data
wine <- read.csv("winequalityCLASS.csv")

# Factorize for classification
wine$quality = factor(wine$quality)
# quality 변수는 1, 0 범주형 변수 --> factor()
# 이렇게 하지 않으면, 정수형 변수로 인식하게 되어 회귀나무 수행될 위험

### Classification Tree
library(rpart)                   # CART 방법을 수행하기 위해 패키지 호출
set.seed(1234)                   # 가지치기 위한 교차타당성을 수행하는 과정에서 무작위 난수 사용

# xval : cross-validation -> 10-fold
# cp : cost-complexity -> 비용복잡함수에서의 알파 값 의미
# cp = 0 -> 오분류율값이 최솟값이 될 때까지 계속 분할하라는 뜻
# cp = 0.1 -> 알파 = 0.1에 해당하는 비용복잡함수가 최솟값이 될 때까지만 계속 분할하라
# minsplit = 20 : rpart 함수에서 중간노드를 분할하기 위한 최소 자료의 수
# 이 값을 크게 할수록 분할하기 어려워지므로 의사결정나무는 더 짧고 단순해짐
my.control = rpart.control(xval = 10, cp = 0, minsplit = 20)

# 목표변수는 quality, 나머지 변수들은 입력변수
# method = "class" -> 분류나무
tree.wine = rpart(quality ~., data = wine, method = "class", control = my.control)

# 생성된 의사결정나무를 간단하게 출력
print(tree.wine)
# 출력결과 해석 -> 교재 참고
# 노드번호, 분할규칙, 해당 노드의 총 관찰값 수, 해당 노드에서 오분류되는 관찰값 수, 
# 해당 노드의 목표변수 예측값의 순서로 정보 제공
# 괄호 안 -> 목표변수의 각 집단별 비율
# 줄 마지막에 * 표시 -> 해당 노드가 최종노드임을 의미

# 나무형태로 그리기
# Display tree
library(rpart.plot)
prp(tree.wine, type = 4, extra = 1, digits = 2, box.palette = "Grays")
# 가지치기 전 -> 대단히 사이즈가 큰 상태 -> 해석 매우 어려움

## 가지치기 수행
# Pruning with c-s.e.
cps = printcp(tree.wine)
# 뿌리노드에서 나무구조가 종료되도록 하려면 'cp' 값은 0.3734062를 사용하면 됨
# 계속 분할해 나가려면 'cp'값은 이보다는 작은 값이어야 함
# 'cp'값이 0.0일 때는 분할의 횟수가 44(nsplit = 44)   -> maximal tree

# xerror -> 6번째 값이 가장 작음  분리횟수가 24인 나무
# 교차타당성(cross-validation) 방법에 의한 오분류율 의미

# 표준오차를 더한 값보다 작은 xerror를 가지는 분리횟수가 더 작은 나무를 선택하는 것 가능 -> 교재참고
# 오분류율이 최소가 아니더라도...

# Pruning with c-s.e.
k = which.min(cps[, "xerror"])
err = cps[k, "xerror"]; se = cps[k, "xstd"]
c = 1   # 1-s.e.
k1 = which(cps[, "xerror"] <= err + c * se)[1]
cp.chosen = cps[k1, "CP"]
tree.pruned.wine = prune(tree.wine, cp = cp.chosen)
print(tree.pruned.wine)

# 0.5 - s.e. 법칙 : c = 0.5로 변경해주면 됨

# Display tree
prp(tree.pruned.wine, type = 4, extra = 1, digits = 2, box.palette = "Grays")
# 교재 참고

## 분류정확도 계산하기
# Making predictions - probability prediction
prob.tree.wine = predict(tree.pruned.wine, newdata = wine, type = "prob")
# type = "prob" 목표변수의 범주값에 대한 예측확률 의미, 회귀나무라면 type = "vector"라고 해줌
head(prob.tree.wine, 5)
cutoff = 0.5  # cutoff
yhat.tree.wine = ifelse(prob.tree.wine[, 2] > cutoff, 1, 0)

# Evaluation  해석 -> 교재 참고
tab = table(wine$quality, yhat.tree.wine, dnn = c("Observed", "Predicted"))
print(tab)                # confusion matrix

sum(diag(tab))/sum(tab)   # accuracy
tab[2, 2]/sum(tab[2,])    # sensitivity
tab[1, 1]/sum(tab[1,])    # specificity

### 회귀의사결정나무 
# Importing data
prod = read.csv("productivityREG.csv", header = TRUE)

# Factorizing predictor variables
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)

### Regression Tree
library(rpart)
set.seed(1234)
my.control = rpart.control(xval = 10, cp = 0.01, minsplit = 30)
tree.prod = rpart(productivity ~ ., data = prod, method = "anova", control = my.control)
print(tree.prod)
# 2장에서는 가변수 생성했어야 하지만, 회귀의사결정나무모형에서는 범주형 변수를 그대로 사용 가능
# 모형 설정 내용 관련 -> 교재 참고 98p

# Display tree
library(rpart.plot)
prp(tree.prod, type = 4, extra = 1, digits = 2, box.palette = "Grays")

## 가지치기 수행하기
# Pruning with c-s.e.
cps = printcp(tree.prod)

# 1-s.e. 법칙 : 교재 101p 참고

# Pruning with c-s.e.
k = which.min(cps[, "xerror"])
err = cps[k, "xerror"]; se = cps[k, "xstd"]
c = 1   # 1-s.e.
k1 = which(cps[, "xerror"] <= err + c * se)[1]
cp.chosen = cps[k1, "CP"]
tree.pruned.prod = prune(tree.prod, cp = cp.chosen)
print(tree.pruned.prod)

# display tree
prp(tree.pruned.prod, type = 4, extra = 1, digits = 2, box.palette = "Grays")

# Making predictions
pred.tree.prod = predict(tree.pruned.prod, newdata = prod, type = 'vector')
head(pred.tree.prod, 5)
# 기존 학습 데이터에 대한 회귀나무 예측값을 구한 것
# newdata에 prod 데이터와 동일한 형식의 새로운 데이터를 사용 -> 새로운 데이터에 대한 예측
# type = 'vector' : 회귀나무에 해당하는 목표변수의 예측결과를 의미

# Evaluation
mean((prod$productivity - pred.tree.prod) ^ 2)     # MSE
mean(abs(prod$productivity - pred.tree.prod))      # MAE

# Observed vs Predicted
plot(prod$productivity, pred.tree.prod, xlab = "Observed Values", ylab = "Fitted Values")
abline(0, 1)
# 회귀의사결정나무에서 최종노드 내 관찰값들의 평균값을 예측값으로 사용 -> 동일한 값을 가진 점이 많음
