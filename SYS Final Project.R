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
