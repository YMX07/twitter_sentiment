library(syuzhet)
library(ggplot2)
library(twitteR)
library(ROAuth)
library(dplyr)
library(stringr)
library(httr)
library(RCurl)
library(plyr)
library(data.table)
library(base64enc)
library(httpuv)
library(httr)
library("openssl")

consumerKey <-"0r6QHjJEOLSl9IkU3k7py2cYn"
consumerSecret <-"gySrxbuX6LhCAogye95S2HFlE2YL2fZY0s1axFBEYD17yX0PG8"

access_token <- "1252120769259343872-v1NS9Qk7riTjzV87PT1Jjg56lPwSih"
access_secret<-"w07qTU363fmoErkLY4W8lFPJAHjDN78Ao5DcbUDVy9ajm"

oauth_endpoint(authorize = 'https://api.twitter.com/oauth',
               access = 'https://api.twitter.com/oauth/access_token')

download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
reqURL = 'https://api.twitter.com/oauth/request_token'
accessURL = 'https://api.twitter.com/oauth/access_token'
authURL = 'https://api.twitter.com/oauth/authorize'
red = OAuthFactory$new(consumerKey = consumerKey,
                       consumerSecret = consumerSecret,
                       requestURL = reqURL,
                       accessURL = accessURL,
                       authURL = authURL)


setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = access_token, access_secret = access_secret)

origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

################################################################################################################################################################"
################################################################################################################################################################"
people_crypto.tweets <- searchTwitter(searchString = "#crypto", n="25000")
people_crypto.tweets.df <- do.call("rbind", lapply(crypto.tweets , as.data.frame))

################################################################################################################################################################"
################################################################################################################################################################"

# Converting tweets to ASCII to trackle strange characters
people_tweets <- iconv(people_crypto.tweets.df, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
people_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

# removing mentions, in case needed
people_tweets <-gsub("@\\w+","",tweets)
people_ew_sentiment<-get_nrc_sentiment((people_tweets))
people_sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(people_sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(people_sentimentscores),people_sentimentscores)
rownames(people_sentimentscores) <- NULL


ggplot(data=people_sentimentscores,aes(x=sentimentscores$sentiment,y=people_sentimentscores$Score))+
  geom_bar(aes(fill=sentimentscores$sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Twitter sentiment based on scores")+
  theme_minimal()

combined_data <- cbind(people_sentimentscores$sentiment,sentimentscores$sentiment,people_sentimentscores$Score, sentimentscores$Score)

ggplot(data=combined_data,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Twitter sentiment based on scores")+
  theme_minimal()

