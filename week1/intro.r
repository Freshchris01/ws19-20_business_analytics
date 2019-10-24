#set working directory in files view

#load lib
library(tidyverse)

#import data
data <- read_csv("week1/LaborSupply1988.csv") # is tibble by default

# data insights
glimpse(data)
str(data)
summary(data)

dim(data)
nrow(data)
ncol(data)

names(df)

head(data, n=20) # first n lines
tail(data, n=8)

data[10:12,] #10th to 12th row

#max and min of age
max(data[,4]) #data[,4]=df
min(data[,4])

summary(df$age)

range(df$age)

colMeans(data[,4])

for(i in c(0,1,2,3,4,5,6)){
  test <- data %>% filter(data$kids == i)
  print(colMeans(test))
}

