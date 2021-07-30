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

consumerKey <-"00"
consumerSecret <-"00"

access_token <- "00ih"
access_secret<-"w0000000m"

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
crypto.tweets <- searchTwitter(searchString = "#crypto", n="25000")
crypto.tweets.df <- do.call("rbind", lapply(crypto.tweets , as.data.frame))

################################################################################################################################################################"
################################################################################################################################################################"

# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(crypto.tweets.df, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Twitter sentiment based on scores")+
  theme_minimal()


