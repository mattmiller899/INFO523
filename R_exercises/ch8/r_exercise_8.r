# R exercise 8 Decision tree and Ensemble method

# Gapminder Data analysis
# Data Availible from https://www.gapminder.org/data/

install.packages('dplyr')
library('dplyr')
install.packages('tidyverse')
library('tidyverse')

# Set the working directory to this script's directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Import Datasets
# by Indicator name from https://www.gapminder.org/data/

# Adults with HIV (%, age 15-49)
adults_hiv = read.csv("data/Adults_15-49_HIV.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")

# Underweight children (added the fileEncoding, think its a Windows issue)
underweight_children = read.csv("data/Underweight_Children.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")

# Total GDP (PPP$, inflation-adjusted)
gdp_total_ppp = read.csv("data/gdp_total_ppp.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")

# Population aged 40-59 years, female (%)
female_40_59_pop_percent = read.csv("data/female_40_59_pop_percent.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")

hiv_table = adults_hiv %>% gather(key = 'year', value = 'adults_hiv', -country)
underweight_table = underweight_children %>% gather(key = 'year', value = 'underweight_children', -country)
gdp_table = gdp_total_ppp %>% gather(key = 'year', value = 'gdp_total_ppp', -country)
female_table = female_40_59_pop_percent %>% gather(key = 'year', value = 'female_40_59_pop_percent', -country)

total_data = full_join(hiv_table, underweight_table, by = c('country','year'))
total_data = full_join(gdp_table, total_data, by = c('country','year'))
total_data = full_join(female_table, total_data, by = c('country','year'))

# meaningless example just an example of how to get at the data
total_data  %>% group_by(country) %>% summarise(avg = mean(adults_hiv, na.rm = TRUE))

# examlpe of how to drill down 
drilled_down_data_ex = total_data  %>%  group_by(country) %>% filter(year > 1900 & adults_hiv > 0.00 & underweight_children > 0.00)

# Drop year, want to try and classify by country, makes more sense then having year as a factor I think.
drilled_down_data_ex = subset(drilled_down_data_ex, select = -year )

# make country a factor
drilled_down_data_ex$country <- as.factor(drilled_down_data_ex$country)

######

library(DMwR2)
set.seed(1234) 

#build a tree: use all attributes in drilled_down_data_ex dataset to predict country.
#function arguments explained in the comments before: se, minsplit, maxdepth and cp.
ct1 <- rpartXse(country ~ ., drilled_down_data_ex, se=1, cp=0, minsplit = 6, maxdepth = 10)

#build a tree using 0-SE pruning (select the lowest estimated error subtree of the original overly large tree)
ct2 <- rpartXse(country ~ ., drilled_down_data_ex, se=0, cp=0, minsplit = 6, maxdepth = 10)

#plot the trees
install.packages("rpart.plot")
library(rpart.plot)

#visualize all att iris tree
prp(ct1, type=0, extra=101, roundint = FALSE)
#vis prunined tree
prp(ct2, type=1, extra=103, roundint = FALSE)

#now we know how to produce a decision tree for classification
#let’s do it again with training and test data
set.seed(1234)
nrow(drilled_down_data_ex)
# have 261 examples
#131 training and 130 test
rndSample <- sample(1:nrow(drilled_down_data_ex),131)
#training examples
tr <- drilled_down_data_ex[rndSample, ]
#rest is the test examples
ts <- drilled_down_data_ex[-rndSample, ]
#build a tree
ct <- rpartXse(country ~ ., tr, se=0.5)
#use ct to predict class labels for the test examples
ps1 <- predict(ct, ts) #this gives the probably of an instance belong to a class
head(ps1)

ps2 <- predict(ct, ts, type="class") #this gives the class label
head(ps2)

#now lets create a contingency table and see how well the classifier works on test examples
#compare ps2 results (machine predication) with correct labels in test
(cm <- table(ps2, ts$country))

#find the error rate in percentage
## question 3 original
(1-sum(diag(cm))/sum(cm))
# we get 0.8 yikes I don't think this is working

#visualize all att iris tree
#prp(ct, type=0, extra=101, roundint = FALSE)

# show decision tree rules. 
## Question 2:
rpart.rules(ct, roundint=FALSE)




##### Ensemble methods for classification and regression

#AdaBoost: offers bagging and boosting, both are for classification (not regression)
install.packages("adabag")
library(adabag)

#bagging: learn trees on boostrapped samples using all variables
##### Taking forever to run
ct <- bagging(country ~ ., tr, mfinal=10)
ps2 <- predict(ct, ts)
ps2$confusion

# Question 3 final
ps2$error   
#0.7 not great but only 10 rounds of bagging

