# SYS 6018: Final Project
# Xinyang Liu: xl9qw
# Ni Zhuang: nz4gg
# Boh Young Suh: bs6ea

# Read in original wine dataset
wine <- read.csv("wine.csv")

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
wine$ratings <- as.factor(wine$ratings)

# Only keep description, price, and ratings
wine <- wine[, c(3, 6, 12)]

# Remove all rows that still have missing values
wine <- wine[complete.cases(wine),]

# Split the data into different sets based on scores and prices
topscrwine <- wine[wine$ratings %in% 10,]
lowscrwine <- wine[wine$ratings %in% 1,]
highprcwine <- wine[wine$price > 150,]
lowprcwine <- wine[wine$price <= 8,]

# Text Mining
library(tm)
### The entire wine dataset ###
# Leave only the content
wine.dscpt <- as.data.frame(wine[, "description"], stringAsFactors = FALSE)
# Convert this part of the data frame to a corpus object.
wine.dscpt <- VCorpus(DataframeSource(wine.dscpt))

# There's a lot in the documents that we don't care about. clean up the corpus.
wine.clean = tm_map(wine.dscpt, stripWhitespace)                    # remove extra whitespace
wine.clean = tm_map(wine.clean, removeNumbers)                      # remove numbers
wine.clean = tm_map(wine.clean, removePunctuation)                  # remove punctuation
wine.clean = tm_map(wine.clean, content_transformer(tolower))       # ignore case
wine.clean = tm_map(wine.clean, removeWords, stopwords("english"))  # remove stop words
wine.clean = tm_map(wine.clean, stemDocument)                       # stem all words
wine.clean <- tm_map(wine.clean, PlainTextDocument)                 # treat preprocessed documents as text documents

# Word Cloud
library(SnowballC)
library(wordcloud)
wordcloud(wine.clean, max.words = 100, random.order = FALSE)

### The top-score wine dataset ###
# Leave only the content
topscrwine.dscpt <- as.data.frame(topscrwine[, "description"], stringAsFactors = FALSE)
# Convert this part of the data frame to a corpus object.
topscrwine.dscpt <- VCorpus(DataframeSource(topscrwine.dscpt))

# There's a lot in the documents that we don't care about. clean up the corpus.
topscrwine.clean = tm_map(topscrwine.dscpt, stripWhitespace)                    # remove extra whitespace
topscrwine.clean = tm_map(topscrwine.clean, removeNumbers)                      # remove numbers
topscrwine.clean = tm_map(topscrwine.clean, removePunctuation)                  # remove punctuation
topscrwine.clean = tm_map(topscrwine.clean, content_transformer(tolower))       # ignore case
topscrwine.clean = tm_map(topscrwine.clean, removeWords, stopwords("english"))  # remove stop words
topscrwine.clean = tm_map(topscrwine.clean, stemDocument)                       # stem all words
topscrwine.clean <- tm_map(topscrwine.clean, PlainTextDocument)                 # treat preprocessed documents as text documents

# Word Cloud
wordcloud(topscrwine.clean, max.words = 100, random.order = FALSE)

### The low-score wine dataset ###
# Leave only the content
lowscrwine.dscpt <- as.data.frame(lowscrwine[, "description"], stringAsFactors = FALSE)
# Convert this part of the data frame to a corpus object.
lowscrwine.dscpt <- VCorpus(DataframeSource(lowscrwine.dscpt))

# There's a lot in the documents that we don't care about. clean up the corpus.
lowscrwine.clean = tm_map(lowscrwine.dscpt, stripWhitespace)                    # remove extra whitespace
lowscrwine.clean = tm_map(lowscrwine.clean, removeNumbers)                      # remove numbers
lowscrwine.clean = tm_map(lowscrwine.clean, removePunctuation)                  # remove punctuation
lowscrwine.clean = tm_map(lowscrwine.clean, content_transformer(tolower))       # ignore case
lowscrwine.clean = tm_map(lowscrwine.clean, removeWords, stopwords("english"))  # remove stop words
lowscrwine.clean = tm_map(lowscrwine.clean, stemDocument)                       # stem all words
lowscrwine.clean <- tm_map(lowscrwine.clean, PlainTextDocument)                 # treat preprocessed documents as text documents

# Word Cloud
wordcloud(lowscrwine.clean, max.words = 100, random.order = FALSE)

### The high-price wine dataset ###
# Leave only the content
highprcwine.dscpt <- as.data.frame(highprcwine[, "description"], stringAsFactors = FALSE)
# Convert this part of the data frame to a corpus object.
highprcwine.dscpt <- VCorpus(DataframeSource(highprcwine.dscpt))

# There's a lot in the documents that we don't care about. clean up the corpus.
highprcwine.clean = tm_map(highprcwine.dscpt, stripWhitespace)                    # remove extra whitespace
highprcwine.clean = tm_map(highprcwine.clean, removeNumbers)                      # remove numbers
highprcwine.clean = tm_map(highprcwine.clean, removePunctuation)                  # remove punctuation
highprcwine.clean = tm_map(highprcwine.clean, content_transformer(tolower))       # ignore case
highprcwine.clean = tm_map(highprcwine.clean, removeWords, stopwords("english"))  # remove stop words
highprcwine.clean = tm_map(highprcwine.clean, stemDocument)                       # stem all words
highprcwine.clean <- tm_map(highprcwine.clean, PlainTextDocument)                 # treat preprocessed documents as text documents

# Word Cloud
wordcloud(highprcwine.clean, max.words = 100, random.order = FALSE)

### The low-price wine dataset ###
# Leave only the content
lowprcwine.dscpt <- as.data.frame(lowprcwine[, "description"], stringAsFactors = FALSE)
# Convert this part of the data frame to a corpus object.
lowprcwine.dscpt <- VCorpus(DataframeSource(lowprcwine.dscpt))

# There's a lot in the documents that we don't care about. clean up the corpus.
lowprcwine.clean = tm_map(lowprcwine.dscpt, stripWhitespace)                    # remove extra whitespace
lowprcwine.clean = tm_map(lowprcwine.clean, removeNumbers)                      # remove numbers
lowprcwine.clean = tm_map(lowprcwine.clean, removePunctuation)                  # remove punctuation
lowprcwine.clean = tm_map(lowprcwine.clean, content_transformer(tolower))       # ignore case
lowprcwine.clean = tm_map(lowprcwine.clean, removeWords, stopwords("english"))  # remove stop words
lowprcwine.clean = tm_map(lowprcwine.clean, stemDocument)                       # stem all words
lowprcwine.clean <- tm_map(lowprcwine.clean, PlainTextDocument)                 # treat preprocessed documents as text documents

# Word Cloud
wordcloud(lowprcwine.clean, max.words = 100, random.order = FALSE)


# Alternative way to do word cloud
dtm <- TermDocumentMatrix(wine.clean)
m <- as.matrix(dtm) # but for some reason, I cannot run this line. R crushes every time...
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Not sure how to use this...
# Compute TF-IDF matrix
wine.clean.tfidf = DocumentTermMatrix(wine.clean, control = list(weighting = weightTfIdf))
wine.clean.tfidf

# We've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
# Remove terms that are absent from at least 97% of documents (keep most terms)
tfidf.97 = removeSparseTerms(wine.clean.tfidf, 0.97)
tfidf.97

# Convert the document-term matrix to a dataframe
df.tfidf.97 = as.data.frame(as.matrix(tfidf.97))
df.tfidf.97 = cbind(wine$ratings, df.tfidf.97)
colnames(df.tfidf.97)[1] <- "ratings"
# Create a dataframe of distances between variables
df.dist.matrix = as.matrix(dist(df.tfidf.97))

