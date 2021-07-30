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

consumerKey <-"222"
consumerSecret <-"222"

access_token <- "222h"
access_secret<-"w22"

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
traders <- list("@CryptoWealthL","@GateCryptos","@eljaboom","@BusterTrades", "@crypto_thai", "@CryptoSpider1", "@AlgodTrading", 
                "@rickytheirish", "@Rager","@KILLSHOTCRYPTO","@damskotrades",
                "@CRYPT0HULK", "@Altcointraders_","@TheCryptoDog","crypto_birb","@cryptodude999", "@Cryptanzee", "@CryptoRevoltFR",
                "@jonathanbosh", "@cryptoquant_com","@TheCryptoLemon","@Cryptogenius19","@MuroCrypto","@DaanCrypto","@h_bitcoiner",
                "@CryptoTony__","@IamBitmannn","@TheEuroSniper", "@BTC_JackSparrow","@edwardmorra_btc","@eliz883","@rookiexbt",
                "@_Telekinesis","@Trader_XO","@CryptoKaleo","@trader1sz","@CryptoCred","@TraderWojak", "@CryptoTipsTrade", "@CT_WooZ", 
                '@kucoincoinpump','@George1Trader','@DYORCryptoBot')

traders <- unique(traders)

Coins <- list()
traders.tweets.DF <- data.frame()

for(trader in traders){
  Sys.sleep(5)
  trader.tweets <- {}
  trader.tweets <- userTimeline(user = trader, n = 20, includeRts = FALSE, excludeReplies = FALSE)
  print(trader)
  message(sprintf('Number of tweets: %d', length(trader.tweets)))
  Sys.sleep(5)
  traders.tweets.DF <- rbind(traders.tweets.DF,do.call("rbind",lapply(trader.tweets, as.data.frame)))
  Coins <- do.call("rbind",lapply(str_extract_all(string = traders.tweets.DF$text, pattern = "^\\$[a-zA-Z]*"),as.list))
}

################################################################################################################################################################"
################################################################################################################################################################"

# Converting tweets to ASCII to trackle strange characters
tweets <- iconv( traders.tweets.DF, from="UTF-8", to="ASCII", sub="")

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
  ggtitle("Traders sentiment based on scores")+
  theme_minimal()


