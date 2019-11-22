# Excercise 1
set.seed(10)

w2 <- sample(c(1:6), 1000, replace=TRUE)
table1 <- table(w1)
barplot(table(w1))
relative = table1/1000

w12 <- w1 + w2
barplot(table(w12))
mean(w12)
var(w12)
summary(w12)

library("tidyverse")

data <- read_delim('data/E1-3-data.csv', " ")

hist(data$B, freq = FALSE) # B is normal distrubuted
lines(density(data$B))

sd(data$B)
