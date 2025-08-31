library(tidyverse)

prod <- read.csv("garments_worker_productivity.csv")
prod$date <- as.Date(prod$date, format = '%m/%d/%Y')

str(prod$quarter)
prod$quarter <- factor(prod$quarter)
str(prod$quarter)

prod$department <- factor(prod$department)
prod$day <- factor(prod$day)
prod$team <- factor(prod$team)

# 결측치는 0 처리
prod[is.na(prod)] <- 0
summary(prod)

attach(prod)
par(mfrow = c(3, 4))
boxplot(target, col = "cyan3", xlab = "Target Productivity")
boxplot(smv, col = "cyan3", xlab = "Standard Minute Value")
boxplot(wip, col = "cyan3", xlab = "Work in Progress")
boxplot(over_time, col = "cyan3", xlab = "Overtime")
boxplot(incentive, col = "cyan3", xlab = "Incentive")
boxplot(idle_time, col = "cyan3", xlab = "Idle Time")
boxplot(idle_men, col = "cyan3", xlab = "Idle Men")
boxplot(numchange, col = "cyan3", xlab = "Number of Changes in Style")
boxplot(numworkers, col = "cyan3", xlab = "Number of Workers")
boxplot(productivity, col = "cyan3", xlab = "Productivity")

dout <- rep(0, 15)
dout2 <- rep(0, 15)
for (i in 6:15) {
  t3 <- quantile(prod[, i], 0.75, na.rm = TRUE)
  t1 <- quantile(prod[, i], 0.25, na.rm = TRUE)
  tq <- IQR(prod[, i], 0.75)
  dout[i] <- t3 + 1.5 * tq
  dout2[i] <- t1 - 1.5 * tq
}

outindex <- matrix(0, 1197, 15)
for (i in 1:1197) {
  for (j in 6:15) {
    if (prod[i, j] > dout[j] || prod[i, j] < dout2[j]) {
      outindex[i, j] <- 1
    }
  }
}

# 이상치가 없는 행만 저장
prod2 <- prod[apply(outindex, 1, sum) == 0, ]

prodnew <- prod2[, -c(1, 11:13)]
head(prodnew)

write.csv(prodnew, "productivityREG.csv", quote = F, row.names = F)
detach(prod)

#######################################################################################

wine <- read.csv("winequality-red.csv")

# 6점 이상은 우수, 6점 미만은 보통
wine <- wine %>%
  mutate(quality_ = case_when(quality >= 6 ~ 1,
                              quality < 6 ~ 0)) %>% 
  select(everything(), -quality) %>% 
  rename(., quality = quality_)
  
# wine$quality <- factor(wine$quality)
attach(wine)

par(mfrow = c(3, 4))
boxplot(fixed, col = "cyan3", xlab = "Fixed Acidity")
boxplot(volatile, col = "cyan3", xlab = "Volatile Acidity")
boxplot(citric, col = "cyan3", xlab = "Citric Acid")
boxplot(residsugar, col = "cyan3", xlab = "Residual Sugar")
boxplot(chlorides, col = "cyan3", xlab = "Chlorides")
boxplot(freeSD, col = "cyan3", xlab = "Free Sulfur Dioxide")
boxplot(totalSD, col = "cyan3", xlab = "Total Sulfur Dioxide")
boxplot(density, col = "cyan3", xlab = "Density")
boxplot(pH, col = "cyan3", xlab = "pH")
boxplot(sulphates, col = "cyan3", xlab = "Sulphates")
boxplot(alcohol, col = "cyan3", xlab = "Alcohol")

# 이상치 제거
dout <- rep(0, 12)
dout2 <- rep(0, 12)
for (i in 1:11) {
  t3 <- quantile(wine[, i], 0.75, na.rm = TRUE)
  t1 <- quantile(wine[, i], 0.25, na.rm = TRUE)
  tq <- IQR(wine[, i], 0.75)
  dout[i] <- t3 + 1.5 * tq
  dout2[i] <- t1 - 1.5 * tq
}

outindex <- matrix(0, 1599, 12)
for (i in 1:1599) {
  for (j in 1:11) {
    if (wine[i, j] > dout[j] || wine[i, j] < dout2[j]) {
      outindex[i, j] <- 1
    }
  }
}

# 이상치가 없는 행만 저장
wine2 <- wine[apply(outindex, 1, sum) == 0, ]
table(wine2$quality)

write.csv(wine2, "winequalityCLASS.csv", quote = F, row.names = F)
detach(wine)

########################################################################################
# 가변수 생성
library(dummy)

prod = read.csv("productivityREG.csv")
# summary(prod)

prod$quarter <- factor(prod$quarter)
prod$department <- factor(prod$department)
prod$day <- factor(prod$day)
prod$team <- factor(prod$team)

summary(prod)

str(prod$department)

# raw data에서 "finishing", "finishing " 다르게 인신되는 부분 처리
prod <- prod %>% 
  mutate(department_ = case_when(prod$department == "finishing" ~ "finishing",
                                 prod$department == "finishing " ~ "finishing",
                                 TRUE ~ "sweing"))
prod$department_ <- factor(prod$department_)
prod$department <- prod$department_

prod <- prod %>% 
  select(everything(), -department_)

str(prod$department)

summary(prod)

# Generate dummy variables
dvar = c(1:4)
prod2 = dummy(x = prod[, dvar])
prod2 = prod2[, -c(5, 7, 13, 25)]
prod = cbind(prod[, -dvar], prod2)
for (i in 1 : ncol(prod))
  if(!is.numeric(prod[, i])) prod[, i] = as.numeric(prod[, i])