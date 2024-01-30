rm(list = ls())

# Install the easypackages package 
install.packages("easypackages")
library(easypackages)

# Load multiple packages using easypackage function "packages"
packages("XML","wordcloud","RColorBrewer","readr","NLP","tm","quanteda", prompt = T)


# Read the comments from a CSV file
dataset <- read_csv("https://raw.githubusercontent.com/sharonjepkosgei/sharonjepkosgei.github.io/main/nbc_videos.csv")
head(dataset)

# Extract the 'description' column from the CSV as a character vector
vid_des <- dataset$description
head(vid_des)
#vid_toks <- tokens(vid_des)
# Create a corpus from the video description
corpus <- Corpus(VectorSource(vid_des))

# remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# remove numbers
corpus <- tm_map(corpus, removeNumbers)
# remove stopwords
corpus <- tm_map(corpus, function(x) removeWords(x,stopwords("english")))
# Create Term Document Matrix

words_to_remove <- c("said","from","what","told","over","more","other","have","last",
                     "with","this","that","such","when","been","says","will","also",
                     "where","why","would","today", "now", "two")
corpus <- tm_map(corpus, removeWords, words_to_remove)

tdm <- TermDocumentMatrix(corpus)
inspect(tdm)

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

# Create a bar plot for the top words
top_words <- head(wordCounts, 10)  
barplot(top_words, main="Top Words in Video Descriptions", 
        ylab="Frequency", col="darkslategrey", las=2)


# Create Wordcloud
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)

set.seed(1234)
wordcloud(cloudFrame$word,cloudFrame$freq)
wordcloud(names(wordCounts),wordCounts, min.freq=5,random.order=FALSE, max.words=500,scale=c(2.5,0.5), rot.per=0.35,colors=brewer.pal(8,"Dark2"))