# SYS 6018: Final Project
# Xinyang Liu: xl9qw
# Ni Zhuang: nz4gg
# Boh Young Suh: bs6ea

# Read in data
wine <- read.csv("wine.csv")

# Convert region1 and region2 to character variables
wine$region_1 <- as.character(wine$region_1)
wine$region_2 <- as.character(wine$region_2)
# If NA in region2, fill in with values in region1
for (i in 1:nrow(wine)){
  if (wine$region_2[i] == ""){
    wine$region_2[i] = wine$region_1[i]
  }
}

# Remove all rows that still have missing values
wine[wine == ""] <- NA
wine <- wine[complete.cases(wine),]

# visualize wine rating distribution
pts <- table(wine$points)
barplot(pts, main = "Distribution of Wine Rating", xlab = "Rating", ylab = "Counts", col = "Dark Red")

# Factorize or characterize chategorical variables
wine$country <- factor(wine$country)
wine$description <- as.character(wine$description)
wine$designation <- factor(wine$designation)
wine$province <- factor(wine$province)
wine$region_1 <- as.factor(wine$region_1)
wine$region_2 <- as.factor(wine$region_2)
wine$variety <- factor(wine$variety)
wine$winery <- factor(wine$winery)

# Check for score distribution
max(wine$points)
min(wine$points)
unique(sort(wine$points))

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
