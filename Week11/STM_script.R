############################################################################################
###########################STM #############################################################
############################################################################################



install.packages("stm")
install.packages("streamR")
install.packages("RCurl")
install.packages("bitops")
install.packages("rjson")
install.packages("NLP")
library(bitops)
library(RCurl)
library(rjson)
library(ndjson)
library(streamR)
library(tidytext)
library(stminsights)
library(stringr)
library (Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(dplyr)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(stm)


set.seed(01238)

processed <- textProcessor(cuomotweets_df$text, metadata =cuomotweets_df)
plotRemoved(processed$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

#Removing 1028 of 1076 terms (2011 of 3133 tokens) due to frequency 
#Removing 23 Documents with No Words 
#Your corpus now has 319 documents, 48 terms and 1122 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)


cuomo_K5 <- stm(documents = out$documents, vocab = out$vocab, K= 5, 
                           data = out$meta, init.type = "Spectral")

save(cuomo_K5, file = "cuomo_K5.RData")


plot.STM(cuomo_K5,type="summary", xlim=c(0, .5), n=6)
#Look for best fit K here? (guess: between 5 and 15)

storage <- searchK(out$documents, out$vocab, K = c(3,6,9,12,15), data = meta) 
plot(storage)
knitr::kable(storage$results)


#lowest held out likelihood 
#lowest residual
#highest semantic coherence 
#highest lower bound

# Looks like the best fit is 9 


cuomo_K9 <- stm(documents = out$documents, vocab = out$vocab, K= 9, 
                data = out$meta, init.type = "Spectral")

save(cuomo_K9, file = "cuomo_K9.RData")
plot.STM(cuomo_K9 ,type="summary", xlim=c(0, .6), n=10)

labelTopics(cuomo_K9, topics = NULL, n = 7, frexweight = 0.5)


# Visualize the quotes 

install.packages("wordcloud")
library(wordcloud)
plot (cuomo_K9,  cex = 2.0)
names(meta)
thoughts5 <- findThoughts(cuomo_K9, texts=meta$text, topics=5, n=2)
plotQuote(thoughts5$docs[[1]], main = "Topic 5")
cloud(cuomo_K9, topic = 5, scale = c(2, .25))



thoughts9 <- findThoughts(cuomo_K9, texts=meta$text, topics=9, n=2)
plotQuote(thoughts9$docs[[1]], main = "Topic 9")
cloud(cuomo_K9, topic = 9, scale = c(2, .25))

