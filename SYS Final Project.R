# SYS 6018: Final Project
# Xinyang Liu: xl9qw
# Ni Zhuang: nz4gg
# Boh Young Suh: bs6ea

# Read in data
wine <- read.csv("wine.csv")


################################# Data Cleaning #################################

# Remove missing values in points and price
wine <- wine[complete.cases(wine),]

# visualize wine score distribution
pts <- table(wine$points)
barplot(pts, main = "Distribution of Wine Scores", xlab = "Scores", ylab = "Counts", col = "Dark Red")

# Visualize wine price distribution
prc <- table(wine$price)
barplot(prc, main = "Distribution of Wine Prices", xlab = "Prices", ylab = "Counts", col = "Dark Red")

# Visualize wine score vs. price
plot(wine$points, wine$price, main = "Wine Points vs. Prices", xlab = "Points", ylab = "Prices")
cor(wine$points, wine$price) # 0.4598634

# Convert region1 and region2 to character variables
wine$region_1 <- as.character(wine$region_1)
wine$region_2 <- as.character(wine$region_2)
# If NA in region2, fill in with values in region1
for (i in 1:nrow(wine)){
  if (wine$region_2[i] == ""){
    wine$region_2[i] = wine$region_1[i]
  }
}

# Remove all rows that still have missing values, because it is hard to impute the NAs
# and we have relatively large dataset, so removing them seems like a reasonable decision
wine[wine == ""] <- NA
wine <- wine[complete.cases(wine),]

# Factorize or characterize chategorical variables
wine$country <- factor(wine$country)
wine$description <- as.character(wine$description)
wine$designation <- factor(wine$designation)
wine$province <- factor(wine$province)
wine$region_1 <- as.factor(wine$region_1)
wine$region_2 <- as.factor(wine$region_2)
wine$variety <- factor(wine$variety)
wine$winery <- factor(wine$winery)

# Convert 100-based scores to 5-based ratings, and create a new column to store ratings 
wine$ratings <- 0
wine$ratings[which((80 <= wine$points) & (wine$points < 84))] <- 1
wine$ratings[which((84 <= wine$points) & (wine$points < 86))] <- 2
wine$ratings[which((86 <= wine$points) & (wine$points < 87))] <- 3
wine$ratings[which((87 <= wine$points) & (wine$points < 88))] <- 4
wine$ratings[which((88 <= wine$points) & (wine$points < 89))] <- 5
wine$ratings[which((89 <= wine$points) & (wine$points < 90))] <- 6
wine$ratings[which((90 <= wine$points) & (wine$points < 91))] <- 7
wine$ratings[which((91 <= wine$points) & (wine$points < 92))] <- 8
wine$ratings[which((92 <= wine$points) & (wine$points < 93))] <- 9
wine$ratings[which((93 <= wine$points) & (wine$points <= 100))] <- 10

# Group each variable by different levels and see how many observations for each level
# For variables with too many levels but only a few observations for each level,
# reclassify them as "other"
as.data.frame(table(wine$country)) # 7 levels
desgn <- as.data.frame(table(wine$designation)) # too many levels
wine$designation <- as.character(wine$designation)
for (i in 1:nrow(wine)){
  if (wine$designation[i] %in% as.character(desgn$Var1[which(desgn$Freq < 60)])){
    wine$designation[i] <- "Other"
  }
}
wine$designation <- factor(wine$designation)
prov <- as.data.frame(table(wine$province)) # too many levels
wine$province <- as.character(wine$province)
for (i in 1:nrow(wine)){
  if (wine$province[i] %in% as.character(prov$Var1[which(prov$Freq < 15)])){
    wine$province[i] <- "Other"
  }
}
wine$province <- factor(wine$province)
reg1 <- as.data.frame(table(wine$region_1)) # too many levels
wine$region_1 <- as.character(wine$region_1)
for (i in 1:nrow(wine)){
  if (wine$region_1[i] %in% as.character(reg1$Var1[which(reg1$Freq < 320)])){
    wine$region_1[i] <- "Other"
  }
}
wine$region_1 <- factor(wine$region_1)
reg2 <- as.data.frame(table(wine$region_2)) # too many levels
wine$region_2 <- as.character(wine$region_2)
for (i in 1:nrow(wine)){
  if (wine$region_2[i] %in% as.character(reg2$Var1[which(reg2$Freq < 190)])){
    wine$region_2[i] <- "Other"
  }
}
wine$region_2 <- factor(wine$region_2)
var <- as.data.frame(table(wine$variety)) # too many levels
wine$variety <- as.character(wine$variety)
for (i in 1:nrow(wine)){
  if (wine$variety[i] %in% as.character(var$Var1[which(var$Freq < 150)])){
    wine$variety[i] <- "Other"
  }
}
wine$variety <- factor(wine$variety)
winr <- as.data.frame(table(wine$winery)) # too many levels
wine$winery <- as.character(wine$winery)
for (i in 1:nrow(wine)){
  if (wine$winery[i] %in% as.character(winr$Var1[which(winr$Freq < 87)])){
    wine$winery[i] <- "Other"
  }
}
wine$winery <- factor(wine$winery)

# Sample the data into training and testing sets
samp <- sample(nrow(wine), nrow(wine) * 0.75)
train <- wine[samp,]
test <- wine[-samp,]


################################### Non-Linear ##################################

# Random Forest
library(randomForest)
set.seed(1)
# Classification RF for ratings
rf_rate <- randomForest(ratings ~.-X-description-points, data = train, mtry = 3, importance = TRUE)
importance(rf_rate)
#              MeanDecreaseAccuracy     MeanDecreaseGini
# country               12.36382892             594.5131
# designation           -0.14071463            2005.5418
# price                 39.24480695            7989.2853
# province              -0.88022977            1520.4757
# region_1               0.81573708            1836.7764
# region_2              -0.04940548            1577.7744
# variety               -0.75350234            4422.2214
# winery                -0.15532991             997.0567
varImpPlot(rf_rate)
yhat.rf <- predict(rf_rate, newdata = test)
yhat.rf
table(yhat.rf, test$ratings) # very high misclassification rate

# ROC Curve for ratings
# calculating the values for ROC curve
library(pROC)
auc <- multiclass.roc(as.numeric(test$ratings), as.numeric(as.character(yhat.rf)))
# Multi-class area under the curve: 0.7095
roc <- auc[['rocs']]
sapply(2:length(roc),function(i) lines.roc(roc[[i]],col=i))

# Regression RF for price
rf_price <- randomForest(price ~.-X-description-ratings, data = train, mtry = 3, importance = TRUE)
importance(rf_price)
#               %IncMSE IncNodePurity
# country      36.53409      969450.5
# designation  49.21817     1426800.8
# points      192.90792    22117789.7
# province     78.86683     4813505.3
# region_1     50.05493     4114991.1
# region_2     62.38881     7093105.6
# variety     119.68657     7532481.1
# winery       34.83898     1326287.5
varImpPlot(rf_price)
yhat.rf <- predict(rf_price, newdata = test)[,2]
yhat.rf
mean((yhat.rf - test$price)^2) # MSE: 489.0404

# Cross Validation
library(rfUtilities)
rf_rate <- randomForest(ratings ~.-X-description-points, data = wine, mtry = 3, importance = TRUE)
rf_rate.cv <- rf.crossValidation(rf_rate, wine[, c(2,4,6,7,8,9,10,11)], p = 0.10, n = 99)
rf_price <- randomForest(price ~.-X-description-ratings, data = wine, mtry = 3, importance = TRUE)
rf_price.cv <- rf.crossValidation(rf_price, wine[, c(2,4,5,7,8,9,10,11)], p = 0.10, n = 99)
 
       
       
################################### Linear ##################################

###------------------###
###Data Visualization###
###------------------###
library(ggplot2)
ggplot(wine, aes(x=country, y= points)) +  geom_point(size=1, shape=1)
# Canada don't have wine with points higher than 95 
ggplot(wine, aes(x=designation, y= points)) +  geom_point(size=1, shape=1)
# Some designation have higher point range 
ggplot(wine, aes(x=price, y= points)) +  geom_point(size=1, shape=1)
# there is not an obvious linear relationship, this is possibly because wine with low price can also get very decent grades
ggplot(wine, aes(x=province, y= points)) +  geom_point(size=1, shape=1)
# some provinces have wide point range but some do not 
ggplot(wine, aes(x=region_1, y= points)) +  geom_point(size=1, shape=1)
ggplot(wine, aes(x=region_2, y= points)) +  geom_point(size=1, shape=1)
ggplot(wine, aes(x=variety, y= points)) +  geom_point(size=1, shape=1)
ggplot(wine, aes(x=winery, y= points)) +  geom_point(size=1, shape=1)

###----------------###
#### Model building###
###----------------###

#### Basic Linear Model ####
wine.lm <- lm(points~.-X-description-ratings, data = train)
summary(wine.lm) # Multiple R-squared:  0.3629,	Adjusted R-squared:  0.3598 
anova(wine.lm) # SSE residual sum of square 404230 

## Find the residuals (and then use a plot to look for patterns/non-constant variance)
ei<-resid(wine.lm)

##The other residuals can be used to detect influential points and outliers. 
## Find the studentized residuals
ri<-rstandard(wine.lm)

## Find the R-student residuals
ti<-rstudent(wine.lm)

## Normal probabilty plot
qqnorm(rstudent(wine.lm))
qqline(rstudent(wine.lm)) #Relatively normal distribution, although not perfect (tails)

## Residual plot vs. fitted values
yhat <- fitted(wine.lm)
plot(yhat,ti) 

## Residual plots vs. explanatory variables
#Tough because most variables are categorical, so transformations are not really feasible
plot(train$country, ti)
plot(train$designation, ti)
plot(train$price, ti)
plot(train$province, ti)
plot(train$region_1, ti)
plot(train$region_2, ti)
plot(train$variety, ti)
plot(train$winery, ti)
plot(train$ratings, ti)

# The assumptions are violated seriously. 

#### Leverage and Influence ####

## A summary of potential leverage and/or influential points
remove= rownames(summary(influence.measures(wine.lm)))
# train <- train[-which(rownames(train) %in% remove),]

#There are lots of influential points but they are real points that indicate the nature of our data
#We should not remove them

#### Check Multicollinearity ####
# 
library(DAAG)
vif(wine.lm)  # country has very large VIF so we should be careful with this variable 

# test correlation between points and price 
cor.test(train$price, train$points)
# t = 129.92, df = 57961, p-value < 2.2e-16
# cor = 0.474919 


#### Model Selection #### 

## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
s.null <- lm(points~1, data=train[,-c(1,3,12)]) # build the model without "description", "X"(which is index column), 
                                                # "ratings"(derived from points; used as another response variable)
s.full <- lm(points~., data=train[,-c(1,3,12)])

## Forward selection
wine.forward = step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward")
wine.forward
#lm(formula = points ~ price + region_1 + province + variety + 
#   winery + region_2 + designation + country, data = train[,  -c(1,3,12)])
                                                            

## Backward selection
wine.backward = step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")
wine.backward
# lm(formula = points ~ country + designation + price + province + 
# region_1 + region_2 + variety + winery, data = train[, -c(1, 3, 12)])
                                                         
                                                    
## Stepwise selection
wine.stepwise = step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
wine.stepwise
#lm(formula = points ~ price + region_1 + province + variety + 
#   winery + region_2 + designation + country, data = train[, -c(1, 3, 12)])
                                                        

# All three selections are the same. 
# The result model is
# lm(formula = points ~ price + region_1 + province + winery + 
#    variety + region_2 + designation + country, data = train[,-c(1,3,12)])

summary(wine.stepwise) # Multiple R-squared:  0.3629,	Adjusted R-squared:  0.3598 
anova(wine.stepwise)  # 404230

wine.result = wine.stepwise # The result the model is the same as our initial model with every possible variable in it. 
# use the test data to see how well the model performs

yhat.lm <- predict(wine.result, newdata = test[,-5])
yhat.lm
mean((yhat.lm - test$points)^2) # MSE: 6.89653

## Cross Validation 
library(boot)
# for model  wine.result
cv.err=cv.glm(train, wine.result)
cv.err$delta # 

#------------------------------------# 

# All variables are in the model. We ended up with the same model as our initial model, which has every possible variables in it.
# This contradicts with our expections because VIF summary table indicates that variable "country" has high multicollinearity 
# concern. Therefore, we try removing the country from the model. 

wine.remove = lm(formula = points ~.-country, data = train[-c(1,3,12)])


### Model Evaluation ### 
summary(wine.remove) #  Multiple R-squared:  0.3628 >0.3629,	Adjusted R-squared:  0.3597 > 0.3598 
anova(wine.remove) # SSE = 404287 > 404230

## check residuals 
#  Residual plot vs. fitted values
ti.remove <- rstudent(wine.remove)
yhat.remove <- fitted(wine.remove)
plot(yhat.remove,ti.remove)

# Normal probabilty plot
qqnorm(rstudent(wine.remove))
qqline(rstudent(wine.remove)) #Relatively normal distribution, although not perfect (tails)


# use test data to see how the model wine.remove performs
yhat.lm <- predict(wine.remove, newdata = test[,-5])
yhat.lm
mean((yhat.lm - test$points)^2) # MSE: 6.898929

## Cross Validation 
library(boot)
# for model  wine.result
cv.err=cv.glm(train, wine.remove)
cv.err$delta # 

### Conclusion ### 
# The summary table shows that MSE is a little higher, the R square lower compared to the full model. 
# Therefore, we still go with our original model selected by forward, backward and stepwise selection. 
# Though the variable has large VIF, it still contributes to the model so we decided to keep it. 

wine.final.lm <- wine.result

