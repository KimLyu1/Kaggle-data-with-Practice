# read file
stock <- read.csv("D:/RFile/Kaggle/StockMarketSentiment/stock_data.csv")
str(stock)

# library
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


# Build corpus
corpus <- iconv(stock$Text, to = "UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean Text
# change text to lower case
corpus <- tm_map(corpus, tolower)

# remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# remove numbers
corpus <- tm_map(corpus, removeNumbers)

# taking some words which are has less impact
corpus <- tm_map(corpus, removeWords, stopwords("English"))

# remove url 
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
corpus <- tm_map(corpus, content_transformer(removeURL))


# remove space
cleanset <- tm_map(corpus, stripWhitespace)
inspect(cleanset[1:5])



# # Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Barplot
w <- rowSums(tdm)
w <- subset(w, w >= 10)
barplot(w,
        las = 2,
        col = rainbow(50))

# WordCloud
w <- data.frame(names(w), w)
colnames(w) <- c('Word', 'freq')
# head(w)
wordcloud2(w, 
           size = 0.7,
           shape = "circle",
           rotateRatio = 0.5,
           minSize = 1)

# sentiment Analysis
# read file
stock <- read.csv("D:/RFile/Kaggle/StockMarketSentiment/stock_data.csv")
tweets <- iconv(stock$Text, to = "UTF-8")

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
#tweets[4]
#get_nrc_sentiment("delay")

# Barplot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = "Count",
        main = "Sentiment Scores for Stock Tweets")



summary(factor(s$positive))
summary(factor(s$negative))
