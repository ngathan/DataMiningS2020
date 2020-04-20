# Cleaning 
# Remove URls 

install.packages("tm")
library(tm)

# Remove URLs 

library(stringi)
removeURL <- function(x){gsub("http[[:alnum:][:punct:]]*", "", x)}
cuomotweets_df$text <- removeURL(cuomotweets_df$text)

# Remove Emojis 
library(tidyverse)
cuomotweets_df$text <- iconv(cuomotweets_df$text, "latin1", "ASCII", sub="")

# Remove usertag  using NLP 
install.packages("NLP")
library(NLP)
RemoveName <- function(x){gsub("@\\w+", "", x)}
cuomotweets_df$text<- RemoveName(cuomotweets_df$text)


### Exploratory

############################################################################################
#######################TOP 20 WORDS ########################################################
############################################################################################

install.packages("tm")
install.packages("tidytext")
library(tidytext)
library(dplyr)

tidy_Upwork<- cuomotweets_df %>%
  select(created, text) %>%
  unnest_tokens("word", text)

tidy_Upwork %>%
  count(word) %>%
  arrange(desc(n))

# remove stop words

data("stop_words")
tidy_Upwork<-tidy_Upwork %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

# Remove number 

tidy_Upwork<-tidy_Upwork[-grep("\\b\\d+\\b", tidy_Upwork$word),]

# Stemming 
library(SnowballC)
tidy_Upwork<-tidy_Upwork %>%
  mutate_at("word", funs(wordStem((.), language="en")))


cuomo_top_words<-
  tidy_Upwork[-grep("https|t.co|amp|rt",
                    tidy_Upwork$word),]

#create factor variable to sort by frequency
cuomo_top_words$word <- factor(cuomo_top_words$word, levels = cuomo_top_words$word[order(cuomo_top_words$n,decreasing=TRUE)])

#select only top words
cuomo_top_20<-cuomo_top_words[1:20,]

# Create a graph 

cuomo_top_20$word <- factor(cuomo_top_20$word , levels = unique(cuomo_top_20$word)[order(cuomo_top_20$n, decreasing = TRUE)])

library(ggplot2)

ggplot(cuomo_top_20, aes(x=word, y=n))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Times Word Appears in Cuomo Tweets")+
  xlab("")+
  guides(fill=FALSE)

save(cuomo_top_20, file = "cuomo_top_20.RData")


### 
## Sentiment Analysis 

install.packages("sentimentr")
library(dplyr)
library(sentimentr)
library(syuzhet)

#Sentiment is only a regular function 
# Traditional method="syuzhet"
cuomotweets_df$sentiment <- get_sentiment(cuomotweets_df$text)
#sentiment 2 is an average of all sentiment in the sentence 
cuomotweets_df$sentiment2 <- sentiment_by(cuomotweets_df$text)
save(cuomotweets_df, file = "cuomotweets_df.RData")

# Between the two values, sentiment has a wider range; sentiment 2 has smaller range 
# Let plot the sentiment over time
# Plot a CDF of sentiment with mean 

install.packages("psych")
library(psych)
library(ggplot2)



# CDF with mean

ggplot( data = cuomotweets_df, aes(x = cuomotweets_df$sentiment)) + 
  stat_ecdf(geom = "step", pad = FALSE) +
  geom_vline(xintercept = mean(cuomotweets_df$sentiment), color="red")+
  (labs(x="Upwork Sentiment", y="CDF"))

cuomotweets_df$sentiment2ave <- cuomotweets_df$sentiment2$ave_sentiment

ggplot( data = cuomotweets_df, aes(x = sentiment2ave)) + 
  stat_ecdf(geom = "step", pad = FALSE) +
  geom_vline(xintercept = mean(cuomotweets_df$sentiment2ave), color="red")+
  (labs(x="Upwork Sentiment 2 ", y="CDF"))

