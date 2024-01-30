# File: sentiment_tidytext01.R
# Theme: Running sentiment anlaysis using tidytext package
# Data: youtube data scrapped and stored on github
rm(list = ls())
install.packages(c("easypackages","rtweet","tidyverse","RColorBrewer","tidytext","syuzhet", "plotly"))
library(easypackages)
libraries("tidyverse","RColorBrewer","tidytext","data.table","tidyr", "plotly")
nbc_videos <- read_csv("https://raw.githubusercontent.com/sharonjepkosgei/sharonjepkosgei.github.io/main/nbc_videos.csv")
head(nbc_videos)
# Access the column with video description
vid_des <- nbc_videos$description
#create a tibble named textDF with a single column "txt" containing the text data.
textDF <- tibble(txt = vid_des)

#Preprocess text data
#tokenize the video titles and remove stopwords 
tidyyoutube = textDF %>% 
  unnest_tokens(word, txt)
tidyyoutube <- tidyyoutube %>% anti_join(stop_words)

# word frequency plot
tidyyoutube %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab("Keyword") + ylab("Count") +
  coord_flip() + theme_bw()

#Adding Line Numbers to words df
tidyyoutube <- tidyyoutube %>%
  mutate(linenumber = row_number())

# Joining bing lexicon using on average videos des of 10 words.
sentiment_des <- tidyyoutube %>%          
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Sentiment Score Plot
ggplot(sentiment_des, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) + theme_bw()

#Creating a "posneg" Column
sentiment_des$posneg = ifelse(sentiment_des$sentiment > 0, 1, ifelse(sentiment_des$sentiment < 0, -1, 0))

# Use Plotly library to plot density chart
ggplot(sentiment_des, aes(x = sentiment, fill = as.factor(posneg))) + 
  geom_density(alpha = 0.5) + 
  ggtitle("Stacked Sentiment Density Chart") + theme_bw()

# Word Sentiment Analysis

bing_word_counts <- tidyyoutube %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiments for nbc Youtude Videos", x = NULL) +
  coord_flip() + theme_bw() 