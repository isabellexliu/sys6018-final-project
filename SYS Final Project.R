# SYS 6018: Final Project
# Xinyang Liu: xl9qw
# Ni Zhuang: nz4gg
# Boh Young Suh: bs6ea

# Read in data
wine <- read.csv("wine.csv")


################################# Data Cleaning #################################

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

# Visualize wine score distribution
pts <- table(wine$points)
barplot(pts, main = "Distribution of Wine Scores", xlab = "Scores", ylab = "Counts", col = "Dark Red")

# Visualize wine price distribution
prc <- table(wine$price)
barplot(prc, main = "Distribution of Wine Prices", xlab = "Prices", ylab = "Counts", col = "Dark Red")

# Visualize wine score vs. price
plot(wine$points, wine$price, main = "Wine Points vs. Prices", xlab = "Points", ylab = "Prices")
cor(wine$points, wine$price)

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
