# Importing data
prod = read.csv("productivityREG.csv", header = TRUE)

# Factorizing predictor variables
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)

# Partitioning data into train and test sets
set.seed(1234)
train.index = sample(1:nrow(prod), size = 0.7 * nrow(prod), replace = F)
prod.train = prod[train.index, ]  # train data
prod.test = prod[-train.index, ]  # test data

# 모든 입력변수를 포함한 선형회귀모형을 훈련 데이터로 적합 후
# 단계적 변수선택법에 의해 유의한 입력변수만 선택한 최적의 모형을 선택
# 남겨 둔 검증 데이터 prod.test를 적합한 모형에 적용하여 목표변수의 예측값을 계산
fit.reg = lm(productivity ~ ., data = prod.train)
fit.step.reg = step(fit.reg, direction = "both", trace = FALSE)

pred.reg = predict(fit.step.reg, newdata = prod.test, type = "response")
mean((prod.test$productivity - pred.reg) ^ 2)    # MSE
mean(abs(prod.test$productivity - pred.reg))     # MAE

# 회귀나무모형을 훈련 데이터로 성장시킨 후 적절한 크기로 가지치기를 한 다음에 최적의 모형을 선택
# 남겨 둔 검증 데이터 prod.test를 적합한 모형에 적용하여 목표변수의 예측값을 계산
library(rpart)

my.control = rpart.control(xval = 10, cp = 0, minsplit = 5)
fit.tree = rpart(productivity ~ ., data = prod.train, method = "anova", control = my.control)
tmp = printcp(fit.tree)
k = which.min(tmp[, "xerror"])
cp.tmp = tmp[k, "CP"]
fit.prun.tree = prune(fit.tree, cp = cp.tmp)

pred.tree = predict(fit.prun.tree, newdata = prod.test, type = "vector")
mean((prod.test$productivity - pred.tree) ^ 2)      # MSE
mean(abs(prod.test$productivity - pred.tree))       # MAE

### 의류생산성 데이터, 신경망모형
library(neuralnet)
library(dummy)

dvar = c(1:4)    # find nominal variables
prod2 = dummy(x = prod[, dvar])       # transform nominal variables into dummy
prod2 = prod2[, -c(5, 7, 13, 25)]     # delete redundant dummy variables
prod2 = cbind(prod[, -dvar], prod2)   # combine them
for(i in 1:ncol(prod2)) if(!is.numeric(prod2[, i]))
  prod2[, i] = as.numeric(prod2[, i])

set.seed(1234)
train.index = sample(1:nrow(prod2), round(0.7 * nrow(prod2)))
prod2.train = prod2[train.index, ]      # train data
prod2.test = prod2[-train.index, ]      # test data

max1 = apply(prod2.train, 2, max)
min1 = apply(prod2.train, 2, min)

sdat.train = scale(prod2.train, center = min1, scale = max1 - min1)
sdat.train = as.data.frame(sdat.train)

sdat.test = scale(prod2.test, center = min1, scale = max1 - min1)
sdat.test = as.data.frame(sdat.test)

vname = names(sdat.train)
f = as.formula(paste("productivity ~", paste(vname[!vname %in% "productivity"], collapse = " + ")))
fit.nn = neuralnet(f, data = sdat.train, hidden = c(3, 1), linear.output = T)

pred.nn = predict(fit.nn, sdat.test)
pred.nn = pred.nn * (max1[7] - min1[7]) + min1[7]

mean((prod.test$productivity - pred.nn) ^ 2)     # MSE
mean(abs(prod.test$productivity - pred.nn))      # MAE

# 랜덤포레스트
library(randomForest)

fit.rf = randomForest(productivity ~ ., data = prod.train, ntree = 100,
                      mtry = 5, importance = T, na.action = na.omit)

pred.rf = predict(fit.rf, newdata = prod.test, type = "response")
mean((prod.test$productivity - pred.rf) ^ 2)    # MSE
mean(abs(prod.test$productivity - pred.rf))     # MAE

par(mfrow = c(2, 2), pty = "s")
a = min(prod.test$productivity)
b = max(prod.test$productivity)
plot(prod.test$productivity, pred.reg, xlim = c(a, b), ylim = c(a, b),
     xlab = "Observed", ylab = "Predicted", main = "Regression")
abline(a = 0, b = 1, lty = 2)
plot(prod.test$productivity, pred.tree, xlim = c(a, b), ylim = c(a, b),
     xlab = "Observed", ylab = "Predicted", main = "Decision Tree")
abline(a = 0, b = 1, lty = 2)
plot(prod.test$productivity, pred.nn, xlim = c(a, b), ylim = c(a, b),
     xlab = "Observed", ylab = "Predicted", main = "Neural Network")
abline(a = 0, b = 1, lty = 2)
plot(prod.test$productivity, pred.rf, xlim = c(a, b), ylim = c(a, b),
     xlab = "Observed", ylab = "Predicted", main = "Random Forests")
abline(a = 0, b = 1, lty = 2)

par(mfrow = c(1, 1))
# MSE, MAE 기준 랜덤포레스트가 가장 작은 오류값을 생성하였고, 회귀나무모형이 가장 큰 오류값을 생성

##### 와인품질 데이터
# Importing data
wine = read.csv("winequalityCLASS.csv", header = TRUE)

# Determining a cutoff
cutoff = 0.5

# Partitioning data into train and test sets
library(caret)
set.seed(1234)
train.index = createDataPartition(wine$quality, p = 0.7, list = FALSE)
wine.train = wine[train.index, ]     # train data
wine.test = wine[-train.index, ]     # test data

# 로지스틱회귀모형
fit.reg = glm(quality ~ ., family = binomial(link = "logit"),
              data = wine.train)
fit.step.reg = step(fit.reg, direction = "both", trace = FALSE)

p.test.reg = predict(fit.step.reg, newdata = wine.test,
                     type = "response")
yhat.test.reg = ifelse(p.test.reg > cutoff, 1, 0)

tab = table(wine.test$quality, yhat.test.reg,
            dnn = c("Observed", "Predicted"))
print(tab)           # confusion matrix

sum(diag(tab)) / sum(tab)    # accuracy
tab[2, 2] / sum(tab[2, ])    # sensitivity
tab[1, 1] / sum(tab[1, ])    # specificity

# 의사결정나무모형
library(rpart)

my.control = rpart.control(xval = 10, cp = 0, minsplit = 5)
fit.tree = rpart(quality ~ ., data = wine.train, method = "class",
                 control = my.control)
tmp = printcp(fit.tree)
k = which.min(tmp[, "xerror"])
cp.tmp = tmp[k, "CP"]
fit.prun.tree = prune(fit.tree, cp = cp.tmp)

p.test.tree = predict(fit.prun.tree, newdata = wine.test, type = "prob")[, 2]
yhat.test.tree = ifelse(p.test.tree > cutoff, 1, 0)

tab = table(wine.test$quality, yhat.test.tree, dnn = c("Observed", "Predicted"))
print(tab)

sum(diag(tab)) / sum(tab)    # accuracy
tab[2, 2] / sum(tab[2, ])    # sensitivity
tab[1, 1] / sum(tab[1, ])    # specificity

# 신경망모형
library(neuralnet)
library(caret)
set.seed(1234)
train.index = createDataPartition(wine$quality, p = 0.7, list = FALSE)
wine.train = wine[train.index, ]       # train data
wine.test = wine[-train.index, ]       # test data

max1 = apply(wine.train, 2, max)
min1 = apply(wine.train, 2, min)

gdat.train = scale(wine.train, center = min1, scale = max1 - min1)
gdat.train = as.data.frame(gdat.train)

gdat.test = scale(wine.test, center = min1, scale = max1 - min1)
gdat.test = as.data.frame(gdat.test)

gn = names(gdat.train)
f = as.formula(paste("quality ~ ", paste(gn[!gn %in% "quality"], collapse = " + ")))
fit.nn = neuralnet(f, data = gdat.train, hidden = c(2, 1), linear.output = F)
p.test.nn = predict(fit.nn, gdat.test)
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)

tab = table(gdat.test$quality, yhat.test.nn, dnn = c("Observed", "Predicted"))
print(tab)

sum(diag(tab)) / sum(tab)    # accuracy
tab[2, 2] / sum(tab[2, ])    # sensitivity
tab[1, 1] / sum(tab[1, ])    # specificity

# 배깅모형
library(rpart)
library(adabag)

if(!is.factor(wine.train$quality)) wine.train$quality = factor(wine.train$quality)
if(!is.factor(wine.test$quality)) wine.test$quality = factor(wine.test$quality)

my.control = rpart.control(xval = 0, cp = 0, minsplit = 5)
fit.bag = bagging(quality ~ ., data = wine.train, mfinal = 100,
                  control = my.control)
print(fit.bag$importance)

p.test.bag = predict.bagging(fit.bag, newdata = wine.test)$prob[, 2]
yhat.test.bag = ifelse(p.test.bag > cutoff,
                       levels(wine.test$quality)[2], levels(wine.test$quality)[1])

tab = table(wine.test$quality, yhat.test.bag, dnn = c("Observed", "Predicted"))
print(tab)

sum(diag(tab)) / sum(tab)    # accuracy
tab[2, 2] / sum(tab[2, ])    # sensitivity
tab[1, 1] / sum(tab[1, ])    # specificity

# 부스팅모형
library(rpart)
library(adabag)

if(!is.factor(wine.train$quality)) wine.train$quality = factor(wine.train$quality)
if(!is.factor(wine.test$quality)) wine.test$quality = factor(wine.test$quality)

my.control = rpart.control(xval = 0, cp = 0, maxdepth = 4)
fit.boo = boosting(quality ~ ., data = wine.train, boos = T, mfinal = 100,
                  control = my.control)

p.test.boo = predict.boosting(fit.boo, newdata = wine.test)$prob[, 2]
yhat.test.boo = ifelse(p.test.boo > cutoff,
                       levels(wine.test$quality)[2], levels(wine.test$quality)[1])

tab = table(wine.test$quality, yhat.test.boo, dnn = c("Observed", "Predicted"))
print(tab)

sum(diag(tab)) / sum(tab)    # accuracy
tab[2, 2] / sum(tab[2, ])    # sensitivity
tab[1, 1] / sum(tab[1, ])    # specificity

# 랜덤포레스트모형
library(randomForest)

if(!is.factor(wine.train$quality)) wine.train$quality = factor(wine.train$quality)
if(!is.factor(wine.test$quality)) wine.test$quality = factor(wine.test$quality)

fit.rf = randomForest(quality ~ ., data = wine.train, ntree = 100,
                      mtry = 5, importance = T, na.action = na.omit)

p.test.rf = predict(fit.rf, newdata = wine.test, type = "prob")[, 2]
yhat.test.rf = ifelse(p.test.rf > cutoff,
                      levels(wine.test$quality)[2], levels(wine.test$quality)[1])

tab = table(wine.test$quality, yhat.test.rf, dnn = c("Observed", "Predicted"))
print(tab)

sum(diag(tab)) / sum(tab)    # accuracy
tab[2, 2] / sum(tab[2, ])    # sensitivity
tab[1, 1] / sum(tab[1, ])    # specificity

# ROC, AUC 예측 결과
library(ROCR)

# Making predictions
pred.reg = prediction(p.test.reg, wine.test$quality)
perf.reg = performance(pred.reg, "tpr", "fpr")
pred.tree = prediction(p.test.tree, wine.test$quality)
perf.tree = performance(pred.tree, "tpr", "fpr")
pred.nn = prediction(p.test.nn, wine.test$quality)
perf.nn = performance(pred.nn, "tpr", "fpr")
pred.bag = prediction(p.test.bag, wine.test$quality)
perf.bag = performance(pred.bag, "tpr", "fpr")
pred.boo = prediction(p.test.boo, wine.test$quality)
perf.boo = performance(pred.boo, "tpr", "fpr")
pred.rf = prediction(p.test.rf, wine.test$quality)
perf.rf = performance(pred.rf, "tpr", "fpr")

# Drawing ROCs
plot(perf.reg, lty = 1, col = 1, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve")
plot(perf.tree, lty = 2, col = 2, add = TRUE)
plot(perf.nn, lty = 3, col = 3, add = TRUE)
plot(perf.bag, lty = 4, col = 4, add = TRUE)
plot(perf.boo, lty = 5, col = 5, add = TRUE)
plot(perf.rf, lty = 6, col = 6, add = TRUE)
lines(x = c(0, 1), y = c(0, 1), col = "grey")
legend(0.7, 0.4, c("Regression", "Decision Tree", "Neural Network", "Bagging", "Boosting", "Random Forest"),
       lty = 1:6, col = 1:6, cex = 0.6)

# Computing AUCs
performance(pred.reg, "auc")@y.values            # Regression
performance(pred.tree, "auc")@y.values           # Decision Tree
performance(pred.nn, "auc")@y.values             # Neural Network
performance(pred.bag, "auc")@y.values            # Bagging
performance(pred.boo, "auc")@y.values            # Boosting
performance(pred.rf, "auc")@y.values             # Random Forest
