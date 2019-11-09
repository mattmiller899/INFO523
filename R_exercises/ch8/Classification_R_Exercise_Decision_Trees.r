library(DMwR2)
set.seed(1234) 
data(iris)

#build a tree: use all attributes in iris dataset to predict Species name.
#function arguments explained in the comments before: se, minsplit, maxdepth and cp.
ct1 <- rpartXse(Species ~ ., iris, se=1, cp=0, minsplit = 6, maxdepth = 10)

#build a tree using 0-SE pruning (select the lowest estimated error subtree of the original overly large tree)
ct2 <- rpartXse(Species ~ ., iris, se=0, cp=0, minsplit = 6, maxdepth = 10)

#plot the trees
install.packages("rpart.plot")
library(rpart.plot)

#the tree can be displayed with a variety of features controlled by the parameters supplied to prp()
#e.g: type=0, only displays labels at terminal nodes, type=1 also for internal nodes
#extra=1 Display the number of observations that fall in the node (per class for class objects;)
#extra=3 Class models: misclassification rate at the node, expressed as the number of incorrect classifications and the number of observations in the node.
#extra=+100  Add 100 to any of the above to also display the percentage of observations in the node.

#visualize all att iris tree
prp(ct1, type=0, extra=101, roundint = FALSE)
#vis prunined tree
prp(ct2, type=1, extra=103, roundint = FALSE)

#now we know how to produce a decision tree for classification
#let’s do it again with training and test data
set.seed(1234)
nrow(iris)
#100 training and 50 test
rndSample <- sample(1:nrow(iris),100)
#training examples
tr <- iris[rndSample, ]
#rest is the test examples
ts <- iris[-rndSample, ]
#build a tree
ct <- rpartXse(Species ~ ., tr, se=0.5)
#use ct to predict class labels for the test examples
ps1 <- predict(ct, ts) #this gives the probably of an instance belong to a class
head(ps1)

ps2 <- predict(ct, ts, type="class") #this gives the class label
head(ps2)

#now lets create a contingency table and see how well the classifier works on test examples
#compare ps2 results (machine predication) with correct labels in test
(cm <- table(ps2, ts$Species))

#find the error rate in percentage
> (1-sum(diag(cm))/sum(cm))



#For numerical prediction, train a regression tree
#Use rpart to learn a regression tree
#regression tree: based on http://uc-r.github.io/regression_trees
install.packages("rsample")
install.packages("ipred")
install.packages("AmesHousing") #dataset

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees> 
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility
set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

#examine the structure of ames_train
str(ames_train)

m1 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  cp   = 0.01,
  method  = "anova")

m1 # format of a decision node: “node), split, n, deviance, yval”

#visualize the tree
prp(m1, type=1, extra=101, roundint = FALSE)

#Peek inside the learning process: use plotcp() function
plotcp(m1)

#In the plot, y-axis is cross validation error, lower x-axis is cost complexity (α or cp) value, upper x-axis is the number of terminal nodes (tree size) 
#We find diminishing returns after 12 terminal nodes.
#You may also notice the dashed line which goes through the point tree size= 9.
#Breiman et al. (1984) suggested that in actual practice, its common to instead use the smallest tree within 1 standard deviation of the minimum cross validation error (aka the 1-SE rule). Thus, we could use a tree with 9 terminal nodes and reasonably expect to experience similar results within a small margin of error.
#We can also examine the effects of pruning by generating a full tree (cp=0). 
  
m2 <- rpart(
   formula = Sale_Price ~ .,
   data    = ames_train,
   method  = "anova", 
   control = list(cp = 0, xval = 10))

plotcp(m2)
abline(v = 12, lty = "dashed") #vertical line at tree size = 12.

#In the plot, We see a much larger tree with over 166 terminal nodes, although cp reduces (neglect able amount), cross=validation error stopped reducing around treesize=12. All those additional splitting were over-fitting.
#To compare the error for each α value, rpart performs a 10-fold cross validation so that the error associated with a given α value is computed on the hold-out validation data.
  
#try to improve on the m1 model
m1$cptable

#tune the model: try out different combinations of minsplit and maxdepth values and compare the models
hyper_grid <- expand.grid(
   minsplit = seq(5, 20, 1),
   maxdepth = seq(8, 15, 1))
head(hyper_grid)

#total number of combinations (trees to be generated)
nrow(hyper_grid)

#use a for-loop to run the combinations one by one and save the trees/models.
models <- list()
for (i in 1:nrow(hyper_grid)) {
    # get minsplit, maxdepth values at row i
    minsplit <- hyper_grid$minsplit[i]
    maxdepth <- hyper_grid$maxdepth[i]
    # train a model and store in the list
    models[[i]] <- rpart(
        formula = Sale_Price ~ .,
        data    = ames_train,
        method  = "anova",
        control = list(minsplit = minsplit, maxdepth = maxdepth))}


# function to get optimal cp
get_cp <- function(x) {
min    <- which.min(x$cptable[, "xerror"])
cp <- x$cptable[min, "CP"] }

# function to get minimum error
get_min_error <- function(x) {
min    <- which.min(x$cptable[, "xerror"])
xerror <- x$cptable[min, "xerror"] }

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)) %>%
    arrange(error) %>%
    top_n(-5, wt = error)

#use the parameter combination that results in the least error to build the final tree
#because there are additional random factors in the tree building process, your results may not be exactly the same as mine. cp=0.01 was the default values used for all trees

optimal_tree <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova",
  control = list(minsplit = 7, maxdepth = 8, cp = 0.01))

pred <- predict(optimal_tree, newdata = ames_test)
pred

#model error: Root Mean Squared Error. on average, our predicted sales prices are about $39,145 off from the actual sales price.
RMSE(pred = pred, obs = ames_test$Sale_Price)

#Single tree models suffer from high variance: different random samples will likely produce different trees
#We see the prediction performance is not very good even after tuning. 
#Model ensembles are a way to reduce performance variations, for decision trees, random forest is a robust approach to improve tree prediction performance
  
##### Ensemble methods for classification and regression
set.seed(1234)
rndSample <- sample(1:nrow(iris),100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]

#AdaBoost: offers bagging and boosting, both are for classification (not regression)
install.packages("adabag")
library(adabag)

#bagging: learn trees on boostrapped samples using all variables
##### Taking forever to run, maybe skip this#####
ct <- bagging(Species ~ ., tr, mfinal=500)
ps2 <- predict(ct, ts)
ps2$confusion

ps2$error   #reduced from 0.06 to 0.04


#boosting:iteratively add new models to the ensemble, each model tries to overcome the errors made by the previous model
ct <- boosting(Species ~ ., tr, mfinal=500)
ps2 <- predict(ct, ts) 
ps2$confusion
ps2$error

#Another algorithm: Bagging, can do regression
#bagging: learn trees on boostrapped samples using all variables
  
#750 trees built to bootstrapped samples using all variables
rfm <- ipred::bagging(Sale_Price ~ ., ames_train, nbag=750)
rfpred <- predict(rfm, ames_test)
#error reduced from 39145 to 34536
RMSE(rfpred, ames_test$Sale_Price)

#### MAYBE DO THIS FOR THE HW EXERCISES #######
#Another algorithm: RandomForest, classification and regression
install.packages("randomForest")
library(randomForest)
#build model based on a forest of 750 trees
rfm <- randomForest(Sale_Price ~ ., ames_train, ntree=750)

#predict test data 
rfpred <- predict(rfm, ames_test)

#error reduced from 39145.39 to 24226.48.
RMSE(rfpred, ames_test$Sale_Price)

#Another algorithm: Gradient boosting: classification and regression. like AdaBoost, but use gradient descent to address the current errors. Can overfit, so using cross-validation is very important (cv.folds)
install.packages("gbm")
library(gbm)
rfm <- gbm(Sale_Price ~ ., data=ames_train, n.trees=750, cv.folds=5)
# Distribution not specified, assuming gaussian ...
rfpred <- predict(rfm, ames_test, n.trees=750)
RMSE(rfpred, ames_test$Sale_Price)
