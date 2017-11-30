# SYS 6018: Final Project
# Xinyang Liu: xl9qw
# Ni Zhuang: nz4gg
# Boh Young Suh: bs6ea

# Read in data
wine <- read.csv("/Users/isabelleliu/Desktop/UVa/Fall/SYS 6018/Final Project/wine.csv")
# Convert description, region1, and region2 to characters
wine$region_1 <- as.character(wine$region_1)
wine$region_2 <- as.character(wine$region_2)
wine$description <- as.character(wine$description)
# If NA in region2, fill in with values in region1
for (i in 1:nrow(wine)){
  if (wine$region_2[i] == ""){
    wine$region_2[i] = wine$region_1[i]
  }
}
# Remove all rows that still have missing values
wine[wine == ""] <- NA
wine <- wine[complete.cases(wine),]

# Check for score distribution
max(wine$points)
min(wine$points)
unique(wine$points)
# Convert 100-based scores to 5-based ratings, and create a new column to store ratings 
wine$ratings <- 0
for (i in 1:nrow(wine)){
  if ((80 <= wine$points[i]) && (wine$points[i] < 84)){
    wine$ratings[i] <- 1
  } else if ((84 <= wine$points[i]) && (wine$points[i] < 88)){
    wine$ratings[i] <- 2
  } else if ((88 <= wine$points[i]) && (wine$points[i] < 92)){
    wine$ratings[i] <- 3
  } else if ((92 <= wine$points[i]) && (wine$points[i] < 96)){
    wine$ratings[i] <- 4
  } else{
    wine$ratings[i] <- 5
  }
}
