library(neuralnet)

set.seed(130)
ind1 = 1:100
ind2 = ind1 / 100
cos2 = cos(ind2 * 4 * pi)

cdat = data.frame(cbind(ind2, cos2))
# 입력변수와 목표변수로 구성된 데이터 프레임

cos2.nn = neuralnet(cos2 ~ ind2, data = cdat, hidden = 5, linear.output = T)
# 회귀의 문제 : linear.output = T

plot(cos2.nn)

# 예측값 산출
cos.pred = predict(cos2.nn, data.frame(ind2))
plot(ind1, cos.pred)
lines(cos2)

### 의류생산성 데이터 분석
library(dummy)

prod = read.csv("productivityREG.csv", header = TRUE)
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)

# 명목형 변수는 적절하게 더미변수를 생성하여 대체해야 함
# 더미변수 생성
dvar = c(1:4)
prod2 = dummy(x = prod[, dvar])
prod2 = prod2[, -c(5, 7, 13, 25)]          
# (범주수 - 1)개 생성및 활용 위해 1개씩 제거 (dummy 함수 디폴트로 쓰면 범주수만큼 다 생성됨)
prod2 = cbind(prod[, -dvar], prod2)

# 수치형으로 다시 변환하여야 표준화 연산이 가능해짐
for (i in 1:ncol(prod2)) 
  if(!is.numeric(prod2[, i]))
    prod2[, i] = as.numeric(prod2[, i])

# 데이터의 표준화
max1 = apply(prod2, 2, max)
min1 = apply(prod2, 2, min)

# 0과 1 사이의 값으로 바꾸어 줌
sdat = scale(prod2, center = min1, scale = max1 - min1)
sdat = as.data.frame(sdat)
pn = names(sdat)

f = as.formula(paste("productivity~", paste(pn[!pn %in% "productivity"], collapse = " + ")))

set.seed(1234)
# 은닉층 2개 : 첫번째 3개 노드, 두번째 1개 노드
fit.nn = neuralnet(f, data = sdat, hidden = c(3, 1), linear.output = T)
plot(fit.nn)

# 예측값 계산 및 MSE 산출
pred.nn = predict(fit.nn, sdat)
pred.nn = pred.nn * (max1[7] - min1[7]) + min1[7]       # 예측값을 원래의 값으로 환원하는 과정

# Mean Squared Error (MSE)
mean((prod2$productivity - pred.nn) ^ 2)

# 관측값 대 예측값(Observed vs. Fitted) 산점도
plot(prod2$productivity, pred.nn, xlab = "Observed Values", ylab = "Fitted Values")
abline(0, 1)

## 와인품질 데이터 분석
library(neuralnet)
wine = read.csv("winequalityCLASS.csv", header = TRUE)

# 임계점 정의
cutoff = 0.5

# 데이터의 표준화
max1 = apply(wine, 2, max)
min1 = apply(wine, 2, min)
gdat = scale(wine, center = min1, scale = max1 - min1)
gdat = as.data.frame(gdat)
gn = names(gdat)
f = as.formula(paste("quality~", paste(gn[!gn %in% "quality"], collapse = " +")))
set.seed(1234)
fit.nn = neuralnet(f, data = gdat, hidden = c(2, 1), linear.output = F)
# linear.output = F --> 분류!!, 은닉층 2개이고 각각 노드 2개, 1개
plot(fit.nn)

# 예측력 평가
p.nn = predict(fit.nn, gdat)
yhat.nn = ifelse(p.nn > cutoff, 1, 0)
# confusion matrix
tab = table(gdat$quality, yhat.nn, dnn = c("Observed", "Predicted"))
print(tab)

sum(diag(tab))/nrow(gdat)      # accuracy
tab[2, 2] / sum(tab[2, ])      # sensitivity    실제 관측값 1을 모형에서 1로 예측한 비율
tab[1, 1] / sum(tab[1, ])      # specificity    실제 관측값 0을 모형에서 0으로 예측한 비율

print(fit.nn)

############# 더미변수 처리
sex <- c("M", "F", "F", "M", "M", "M", "F")
race <- c("white", "black", "Asian", "Hispanic", "white", "black", "white")
age <- c(23, 33, 23, 43, 89, 36, 11)
y <- c(10, 20, 40, 25, 11, 25, 34)
dat <- data.frame(sex, race, age)

dat

library(dummy)
dummy(dat)
dummy(dat, p = c(1, 3))   # 이렇게 함수 자체 설정으로 처리 가능
