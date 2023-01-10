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

traders <- list("@WhaleTrades","@CryptoWealthL","@eljaboom","@BusterTrades", "@crypto_thai", "@AlgodTrading", 
                "@rickytheirish", "@Rager","@KILLSHOTCRYPTO","@damskotrades",
                "@CRYPT0HULK", "@Altcointraders_","@TheCryptoDog","crypto_birb","@cryptodude999", "@Cryptanzee", "@CryptoRevoltFR",
                "@jonathanbosh", "@cryptoquant_com","@TheCryptoLemon","@MuroCrypto","@DaanCrypto","@h_bitcoiner",
                "@CryptoTony__","@IamBitmannn","@TheEuroSniper","@edwardmorra_btc","@eliz883","@rookiexbt",
                "@_Telekinesis","@CryptoKaleo","@CryptoCred","@TraderWojak", "@CT_WooZ", 
                '@kucoincoinpump','@George1Trader','@DYORCryptoBot', "@Zeuss77249373")
Coins <- list()
traders.tweets.DF <- data.frame()

for(trader in traders){
  Sys.sleep(5)
  trader.tweets <- {}
  trader.tweets <- userTimeline(user = trader, n = 50, includeRts = FALSE, excludeReplies = FALSE)
  print(trader)
  message(sprintf('Number of tweets: %d', length(trader.tweets)))
  Sys.sleep(5)
  traders.tweets.DF <- rbind(traders.tweets.DF,do.call("rbind",lapply(trader.tweets, as.data.frame)))
}

tweets <- iconv(traders.tweets.DF, from="UTF-8", to="ASCII", sub="")

tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
tradersentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(tradersentimentscores) <- "Score"
tradersentimentscores <- cbind("sentiment"=rownames(tradersentimentscores),tradersentimentscores)
rownames(tradersentimentscores) <- NULL

tradersentimentscores <- data.frame(tradersentimentscores)
somme <- sum(tradersentimentscores$Score)
tradersentimentscores$Score <- round(tradersentimentscores$Score/somme * 100,1)

crypto.tweets1 <- searchTwitter(searchString = "#crypto", n="2000")
crypto.tweets.df1 <- do.call("rbind", lapply(crypto.tweets1 , as.data.frame))

crypto.tweets2 <- searchTwitter(searchString = "#bitcoin", n="2000")
crypto.tweets.df2 <- do.call("rbind", lapply(crypto.tweets2 , as.data.frame))

crypto.tweets3 <- searchTwitter(searchString = "#ethereum", n="2000")
crypto.tweets.df3 <- do.call("rbind", lapply(crypto.tweets3 , as.data.frame))

crypto.tweets.df <- rbind(rbind(crypto.tweets.df1,crypto.tweets.df2),crypto.tweets.df3)

crypto.tweets.df1 <- data.frame()
crypto.tweets.df2 <- data.frame()
crypto.tweets.df3 <- data.frame()
final <- data.frame()

crypto.tweets1 <- list()
crypto.tweets2 <- list()
crypto.tweets3 <- list()

tweets <- iconv( crypto.tweets.df, from="UTF-8", to="ASCII", sub="")
crypto.tweets.df <- data.frame()
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

sentimentscores <- data.frame(sentimentscores)
somme <- sum(sentimentscores$Score)
sentimentscores$Score <- round(sentimentscores$Score/somme * 100,1)

ZLON.tweets <- userTimeline(user = "@elonmusk", n = 199, includeRts = FALSE, excludeReplies = FALSE)
print(trader)
message(sprintf('Number of Elon tweets: %d', length(ZLON.tweets)))
ZLON.tweets.DF <- data.frame()
ZLON.tweets.DF <- rbind(ZLON.tweets.DF,do.call("rbind",lapply(ZLON.tweets, as.data.frame)))

tweets_elon <- iconv( ZLON.tweets.DF, from="UTF-8", to="ASCII", sub="")
tweets_elon <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets_elon)

tweets_elon <-gsub("@\\w+","",tweets_elon)
elon_sentiment<-get_nrc_sentiment((tweets_elon))
elon_sentimentscores<-data.frame(colSums(elon_sentiment[,]))
names(elon_sentimentscores) <- "Score"
elon_sentimentscores <- cbind("sentiment"=rownames(elon_sentimentscores),elon_sentimentscores)
rownames(elon_sentimentscores) <- NULL
elon_sentimentscores <- data.frame(elon_sentimentscores)
somme <- sum(elon_sentimentscores$Score)
elon_sentimentscores$Score <- round(elon_sentimentscores$Score/somme * 100,1)

final <- cbind(sentimentscores,tradersentimentscores$Score, elon_sentimentscores$Score)

now <- Sys.Date()
setwd("C:/Users/Youssef/Documents/Private/Business/business/mkt sentiment/")
jpeg(paste(now," MarketSentiment.jpg"))
ggplot(data=final, aes(x=sentiment, y=Score)) +
  geom_bar(aes(y=Score), stat="identity", position ="identity", alpha=.3, fill='firebrick2', color='red') +
  geom_bar(aes(y=tradersentimentscores$Score), stat="identity", position="identity", alpha=.8, fill='darkseagreen1', color='green')+
  geom_bar(aes(y=elon_sentimentscores$Score), stat="identity", position="identity", alpha=.8, fill='yellow', color='darkslateblue')+
  theme(legend.position="left")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Traders (green) versus twittos (red) % market sentiment versus Elon Musk (gold)")+
  theme_minimal()
dev.off()