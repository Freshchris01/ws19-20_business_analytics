# Business Analytics WS 2019/2020
# Generalized Linear Models - Exercise

# Load the library tidyverse
library(tidyverse)

# Load the data from the file admit-train.csv
train = read_csv("admit-train.csv")

names(train) 	# output column names
# "admit" (acceptance for master course) = binary dependent variable. 1 means success of admission. 2 does failure.  
# "gre", "gpa" (points in exams) = independent variables: 
  # GRE: Graduate Record Examinations e (200, 800) 10 point increments
  # GPA: Grade Point Average e (2, 4) (4 is best GPA)
# "rank" (rank of bachelor university ) = independent variables e (1, ..., 4) 1 point increments (1 is best rank)

summary(train$gre)
summary(train$gpa)
summary(train$rank)
summary(as.factor(train$rank)) # how many students per rank

# visualize relationships
plot(admit ~ gre, data=train,pch="+")
plot(admit ~ gpa, data=train,pch="+")
plot(admit ~ rank, data=train,pch="+")

# visualise distributions
hist(train$gre, breaks=25)
hist(train$gpa, breaks=18)

## Generalized Linear Model (GLM) (Logistic Regression) 
mylogit = glm(admit ~ gre + gpa + as.factor(rank), data=train, family=binomial(link="logit"))
#glm: create glm model
# admit ~ gre + gpa + as.factor(rank): relationship to map
# dataset train
#logistic regression model

summary(mylogit)
# From summary
#Residual: compare calculated predicted value to actual datapoint -> close to zero is good!
# introducing dummy variables for the different rank variables. Encodes ranks to variables
# rank 1 is implicit when 0 0 0
#as.factor(rank)2  0 0 1
#as.factor(rank)3  0 1 0
#as.factor(rank)4  1 0 0

# Null model: if a constant explains your dataset , you don't need a model
# Null deviance: deviance when using a null model
# Residual deviance: deviance on our model

# Null deviance should be high when start building a model, do I really need a model
# goal: increase difference of the two deviances

# AIC: how many parameters we have & log likelihood, lower is better; how well is model fitting the data? how complex is the model?

# not very good if iterations are high, it needed a long time to compute a solution -> bad model
###############
##	a)	##	
##############


##############
##	b)	##
##############


##############
##	c)	##	
##############

# Packages: aod 

library(aod)

wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms=4:6)	# similar to F-test in multiple linear regression analysis

##############	 
##	d)	##	
##############

# P-value: it's significatn because it is less than 5% -> reject Hypothesis, because it is significant

rank = c(1,2,3,4)
gre = c(mean(train$gre))
gpa = c(mean(train$gpa))
myinstances = data.frame(gre,gpa,rank)
myinstances

# add predictions to data frame 
myinstances$pAdmit = predict(mylogit, newdata=myinstances, type="response")
myinstances

##############
##	e)	##
##############

MCFad = 1 - (mylogit$deviance/mylogit$null.deviance)
MCFad	

# if model does better than just a constant -> value close to 1
# if model does not explain much at all -> value will be close to 0

##############
##	f)	##
##############

test = read_csv("admit-test.csv") 
preds = predict(mylogit, newdata=test, type='response')
preds

test = test %>% mutate(pred = round(preds)) 
test %>% group_by(admit, pred) %>% summarise(count=n())

# you can also create confusion matrix as below
table(true=test$admit,prediction=round(preds))

##############
##	g)	##
##############

## error rate of Logit-Model ##
incorrectPredictionCount = nrow(test %>% filter(admit!=pred))
totalPredictions = nrow(test)
errorRate = incorrectPredictionCount/totalPredictions
errorRate
# == 0.33 every third student gets predicted wrong
