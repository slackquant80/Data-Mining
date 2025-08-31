library(arules)
tr1 = read.transactions("basket1.txt", format = "basket", sep = ",")
as(tr1, "data.frame")

# 연관규칙 계산
rules1 = apriori(tr1, parameter = list(supp = 0.4, conf = 0.4))

inspect(rules1)


# Income 데이터 구조
data(Income)
str(Income)

rules = apriori(Income, parameter = list(supp = 0.4, conf = 0.8))
inspect(rules)

library(arulesViz)
plot(rules)
plot(rules, method = "grouped")
plot(rules, method = "graph")
plot(rules, method = "paracoord")
