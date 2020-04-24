# 03.26.2020  
# Scrape #Hatecrime vis-a-vis #Covid19
# #Chinesevirus looks like the most important hashtag. 
install.packages("stringr")
install.packages("twitteR")
install.packages("purrr")
install.packages("tidytext")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("scales")
install.packages("broom")
install.packages("ggplot2")
install_github("mkearney/rtweet")
install.packages("httpuv")

library(rtweet)
library(devtools)
library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)

app_name<-"YOURBOTNAME"
consumer_key<-"YOUR CONSUMER KEY"
consumer_secret<-"YOUR CONSUMER SECRET"
access_token<-"YOUR ACCESS TOKEN"
access_token_secret<-"YOUR ACCESS TOKEN SECRET"
create_token(app=app_name,
             consumer_key=consumer_key, consumer_secret=consumer_secret,
             access_token = access_token, access_secret = access_token_secret)

#ChineseVirus

ChineseVirus<- search_tweets("#ChineseVirus", n=10000000, include_rts = FALSE, 
                                  retryonratelimit = TRUE)

save(ChineseVirus, file = "ChineseVirus.RData")
write.csv(ChineseVirus, "ChineseVirus.csv")


# Plot Frequency 

library(ggplot2)
ts_plot(ChineseVirus200326, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about Chinese Virus - March 20-26",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## User's tweets 

cuomotweets<- userTimeline("NYGovCuomo", n = 3200)
cuomotweets_df <- tbl_df(map_df(cuomotweets, as.data.frame))
write.csv(cuomotweets_df, "cuomotweets_df.csv")

ts_plot(cuomotweets_df, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Andrew Cuomo - March 01- April 13",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")
