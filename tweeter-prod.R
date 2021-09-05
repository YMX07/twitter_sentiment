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
  trader.tweets <- userTimeline(user = trader, n = 10, includeRts = FALSE, excludeReplies = FALSE)
  print(trader)
  message(sprintf('Number of tweets: %d', length(trader.tweets)))
  Sys.sleep(5)
  traders.tweets.DF <- rbind(traders.tweets.DF,do.call("rbind",lapply(trader.tweets, as.data.frame)))
  Coins <- do.call("rbind",lapply(str_extract_all(string = traders.tweets.DF$text, pattern = "^\\$[a-zA-Z]*"),as.list))
}

coin_list <- lapply(Coins,tolower)
comptageCoins <- as.data.frame(table(unlist(coin_list)))
Goldendf <- comptageCoins[order(-comptageCoins$Freq),]
GoldenList <- Goldendf$Var1


list_coins_to_tweet = ""
for(coin in GoldenList){

  if(coin != "$"){

    if(list_coins_to_tweet==""){
      list_coins_to_tweet <- paste(list_coins_to_tweet, coin, sep = "")
    }else{
      list_coins_to_tweet <- paste(list_coins_to_tweet, coin, sep = ";")
    }
  }
}


#TOP TEN ONLY
TOP10 = ""
Msg_to_Tweet = ""
i <- 0
for(coin in GoldenList){
  if(i<10){
    if(coin != "$"){
      if(TOP10==""){
        TOP10 <- paste(TOP10, coin, sep = "")
        i<-i-1
      }else{
        TOP10 <- paste(TOP10, coin, sep = " / ")
      }
    }
  }
  i<-i+1
}

Msg_to_Tweet<-paste(Msg_to_Tweet, "[crypto:list] ", sep = "")
Msg_to_Tweet<-paste(Msg_to_Tweet, TOP10, sep = "")
Msg_to_Tweet <- paste(Msg_to_Tweet, "", sep = " / ")
Msg_to_Tweet<-paste(Msg_to_Tweet,"#cryptolist ", sep = "")
Msg_to_Tweet<-paste(Msg_to_Tweet,"/ #crypto", sep = "")
tw <- updateStatus(Msg_to_Tweet)

for(coin in Goldendf$Var1){
  if(coin != "$"){
    print(coin)
  } 
}

