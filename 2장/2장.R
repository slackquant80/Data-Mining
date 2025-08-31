library(tidyverse)

# Importing data
prod <- read.csv("productivityREG.csv", header = TRUE)

# Factorizing predictor variables
prod$quarter <- factor(prod$quarter)
prod$department <- factor(prod$department)
prod$day <- factor(prod$day)
prod$team <- factor(prod$team)

# Fitting a linear regression model
fit.all <- lm(productivity ~ ., data = prod)
fit.step <- step(fit.all, direction = "both")
fit.step$anova
# 2, 3 단계에서 day, wip 변수가 각각 제외되었음 
# -> 두개변수 제외한 8개를 가진 모형이 AIC가 가장 작은 최종선택모형임

summary(fit.step)

# Making predictions
pred.reg = predict(fit.step, newdata = prod, type = "response")
# object : 함수 lm, glm 등에서 생성한 object, newdata : 예측할 변수들로 구성된 데이터 프레임
# type = "response" : 목표값을 예측할 때
print(pred.reg)

# Evaluation
mean((prod$productivity - pred.reg) ^ 2)    # MSE
mean(abs(prod$productivity - pred.reg))     # MAE
# 값이 작을수록 더 예측력이 높다고 할 수 있음

#####################################################################################

# Importing data
wine = read.csv("winequalityCLASS.csv", header = TRUE)

# Fitting a logistic regression model
fit.all = glm(quality ~ ., family = binomial, data = wine)
fit.step = step(fit.all, direction = "both")   # stepwise variable selection
fit.step$anova
# 3개 입력변수가 제외되고 나머지 8개만 남게 됨

summary(fit.step)

# Making predictions
p = predict(fit.step, newdata = wine, type = "response")    # prediction
cutoff = 0.5    # cutoff
yhat = ifelse(p > cutoff, 1, 0)

# Evaluation
tab = table(wine$quality, yhat, dnn = c("Observed", "Predicted"))
print(tab)           # confusion matrix

sum(diag(tab)) / sum(tab)      # accuracy
tab[2, 2] / sum(tab[2,])       # sensitivity
tab[1, 1] / sum(tab[1,])       # specificity