# This is an R script written to process Twitter tweets
# Include all necessary libraries
# Data Processing Libraries
library(tidyr)
library(dplyr)
library(skimr)
library(stringr)
library(stringi)
library(tibble)

# Textual Analysis Libraries
library(corpus)
library(tidytext)

# Plotting Libraries
library(ggplot2)
library(wordcloud2)
library(tidyquant)
library(plotly)
library(crosstalk)
library(syuzhet)
library(visNetwork)

# API libraries
library(rtweet)
library(jsonlite)

# Store api keys
api_key <- "BXGW5jlwDYbCyQKrOQofoRJ24"
api_secret_key <- "DD821BlKeEKI0puemwOoKkxeqiUYpBKujIuDKizYgzrvVAQrLu"
access_token <- "1435139671483314184-P5T7qjqBHqMXdzqlamX3suuidYt3tS"
access_token_secret <- "B5C4JF40lDJCRowUChNG5EzxxIIV43ADu0KvL6Pd3kAJe"

# Authenticate via web browser
token <- create_token(
  app = "cryptostatsresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# Get top-20 cryptocurrency coins
getTopCoins <- function() {
  url <- "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=market_cap_desc&per_page=20&page=1"
  data <- fromJSON(url)
  data <- as.data.frame(data)
  data <- data %>% select(id)
  return(data)
}

# Get top-20 cryptocurrency coins
getTopCoinsFull <- function() {
  url <- "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=market_cap_desc&per_page=20&page=1"
  data <- fromJSON(url)
  data <- as.data.frame(data)
  return(data)
}


# Pulls tweets from Twitter by topic, limited to 7-day period
pullTweets <- function(cryptoList, quantity) {
  # Get vector of dates from today to 7 days ago, due to Twitter API limitation
  dates <- seq.Date(Sys.Date() - 6 , Sys.Date(), by="days")
  
  # Create empty dataframe 
  tweets <- data.frame()
  
  # Search for tweets by topic, removing retweets to reduce duplicates impacting results
  for (coin in cryptoList) {
    for (i in seq_along(dates)) {
      search <- search_tweets(coin, n=100, include_rts = FALSE,  until= dates[i])
      search_text <- search %>% select(screen_name, created_at, text, )
      search_text$currency <- coin
      tweets <- rbind(tweets, search_text)
    }
  }
  
  # Sort by coin
  return(tweets)
}

# Cleans tweets
processTweets <- function(tweets) {
  # Perform pre-processing to ensure sentiment analysis is as accurate as possible
  # Adapted from https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
  # Clean text with above function
  tweets$text <- tweets$text %>%
      # Remove URLs
      str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
      # Remove mentions e.g. "@my_account"
      str_remove_all("@[[:alnum:]_]{4,}") %>%
      # Remove hashtags
      str_remove_all("#[[:alnum:]_]+") %>%
      # Replace "&" character reference with "and"
      str_replace_all("&amp;", "and") %>%
      # Remove puntuation
      str_remove_all("[[:punct:]]") %>%
      # Remove "RT: " from beginning of retweets
      str_remove_all("^RT:? ") %>%
      # Replace any newline characters with a space
      str_replace_all("\\\n", " ") %>%
      # Make everything lowercase
      str_to_lower() %>%
      # Remove any non alphabetical characters, including numbers
      str_replace_all("[^[:alpha:]]", " ") %>%
      # Remove non-english characters
      iconv(from="UTF-8", to="ASCII", sub="") %>% 
      # Remove Unnecessary whitespace in between words
      str_replace_all("\\s+"," ") %>%
      # Remove any trailing whitespace around the text
      str_trim("both") 

  # Extract date from datetime of created_at
  tweets$created_at <- as.Date(strftime(tweets$created_at, format="%Y-%m-%d"))
  
  # Remove any duplicate tweets
  tweets <- tweets[!duplicated(tweets$text), ]
  
  # Remove any NA rows
  tweets <- tweets %>% na.omit
  
  # Assign incrementing index column to keep track of quote_ids
  start <- 1
  tweets$tweet_id <- 0
  for (i in 1:nrow(tweets)) {
    tweets[i,"tweet_id"] <- start
    start <- start + 1
  }

  return(tweets)
}

unnestSentiments <- function(tweets) {
  # Split string by word into separate rows
  tweets_unnested <- tweets %>% unnest_tokens(word, text)
  
  # Remove any stop words
  tweets_unnested <- tweets_unnested %>% anti_join(stop_words) %>% rename(author = screen_name, text = word)
  
  # Remove any two characters and below words, since they will not be very meaningful
  tweets_unnested <- tweets_unnested[nchar(tweets_unnested$text) >= 3,]
  
  return(tweets_unnested)
}

mergeSentiments <- function(tweets, tweets_unnested) {
  # Get sentiment dictionary. Here, we are using the afinn dictionary, as it allows us to assign specific polarity scores.
  sentiment_dataset <- read.csv("./AFINN.csv", header = T)
  
  # Merge with the sentiment dataset to obtain our sentiment scores for each word
  afinn_sentiment <- tweets_unnested %>% rename(word = text) %>% merge(sentiment_dataset, by = 'word')
  
  # Sum up sentiment scores to get the sentiment polarity for entire tweet
  afinn_sentiment <- afinn_sentiment %>% arrange(tweet_id) %>% group_by(tweet_id) %>% summarize(sentiment_polarity = sum(value)) %>%  merge(tweets, by = 'tweet_id') %>% select(tweet_id, currency, sentiment_polarity, screen_name, created_at, text)
  
  return(afinn_sentiment)
}


# Plots the ggplotly plot by tweet id based on processed dataframe
plotSentimentByTweetId <- function(afinn_sentiment) {
  label_wrap <- label_wrap_gen(width = 60)
  
  
  afinn_sentiment_formatted <- afinn_sentiment %>% mutate(formatted_tweet = str_glue("Tweet ID: {tweet_id}
                                      Screen Name: {screen_name}
                                      Text: {label_wrap(text)}"))
  
  gplot <- afinn_sentiment_formatted %>%
    ggplot(aes(tweet_id, sentiment_polarity)) +
    geom_line(color = "#2c3e50", alpha = 0.5) +
    geom_point(aes(text = formatted_tweet), color = "#2c3e50") +
    geom_smooth(method = "loess", span=0.25, se=FALSE, color="blue") +
    geom_hline(aes(yintercept = mean(sentiment_polarity), linetype = "Mean Sentiment Polarity"), color="blue") +
    geom_hline(aes(yintercept = median(sentiment_polarity) + 1.96 * IQR(sentiment_polarity), linetype = "75th Percentile Sentiment Polarity"), color="red") +
    geom_hline(aes(yintercept = median(sentiment_polarity) - 1.96 * IQR(sentiment_polarity), linetype = "25th Percentile Sentiment Polarity"), color="red") +
    facet_wrap(~currency, ncol = 1, scales = "free") +
    theme_classic()+ 
    theme(legend.direction = "horizontal", legend.background = element_rect(fill = "white", colour = "gray30")) +
    labs(x = "Tweet ID", y="Sentiment Polarity")
  
  return(gplot)
}

# Plots the ggplotly plot by date based on processed dataframe
plotSentimentByTime <- function(afinn_sentiment) {
  mean_over_time <- afinn_sentiment %>% group_by(currency, created_at) %>% summarize(mean_sentiment = mean(sentiment_polarity)) %>% mutate(formatted_text = str_glue("Currency: {currency}
          Date: {created_at}
          Mean Sentiment Score: {round(mean_sentiment, 2)}"))
  
  gplottime <- mean_over_time %>% ggplot(aes(x=created_at, y=mean_sentiment)) +
    geom_line(aes(colour=currency), size=2, alpha=0.9) +
    geom_point(aes(text = formatted_text), color = "#2c3e50") +
    theme_minimal() +
    theme(plot.title = ggplot2::element_text(face = "bold")) +
    labs(
      x = NULL, y = NULL
    )
  
  ggplotly(gplottime, tooltip = "text") %>% layout(xaxis = list(rangeslider = list(type="date")))
}

# Gets the most recent sentiment values for display
getUpdatedSentimentValues <- function(afinn_sentiment) {
  mean_over_time <- afinn_sentiment %>% group_by(currency, created_at) %>% summarize(mean_sentiment = mean(sentiment_polarity)) %>% mutate(formatted_text = str_glue("Currency: {currency}
          Date: {created_at}
          Mean Sentiment Score: {round(mean_sentiment, 2)}"), yesterday_sentiment = dplyr::lag(mean_sentiment, n = 1, default = NA)) %>% arrange(desc(created_at)) %>% slice_head() %>% select(mean_sentiment, yesterday_sentiment) %>% mutate(mean_sentiment = round(mean_sentiment, 2), up = mean_sentiment > yesterday_sentiment, diff = round(((mean_sentiment - yesterday_sentiment)/yesterday_sentiment)*100, 2)) %>% arrange(currency)
}

# Plot the top occurring words
plotTopOccuringWords <- function(coinToAnalyze, tweets_unnested) {
  frequentdf <- tweets_unnested %>% filter(currency == coinToAnalyze)
  frequentdf %>%
    count(text, sort = TRUE) %>%
    top_n(15) %>%
    mutate(text = reorder(text, n)) %>%
    ggplot(aes(x = text, y = n)) +
    geom_col() +
    theme(axis.text.y = element_text(size=14), text = element_text(size=12)) + 
    coord_flip() +
    labs(x = "Words",
         y = "Frequency of unique words",
         title = "Most frequent words found in tweets")
}

# Plot exact sentiments
plotSentimentWords <- function(coinToAnalyze, tweets_unnested) {
  frequentdf <- tweets_unnested %>% filter(currency == coinToAnalyze)
  ew_sentiment<-get_nrc_sentiment((frequentdf$text))
  sentimentscores<-data.frame(colSums(ew_sentiment[,]))
  names(sentimentscores) <- "Score"
  sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
  rownames(sentimentscores) <- NULL
  ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
    geom_bar(aes(fill=sentiment),stat = "identity")+
    theme(legend.position="none")+
    xlab("Sentiments")+ylab("Scores")+
    ggtitle("Total sentiment based on scores")+
    theme_minimal()
}

# Generate wordcloud
wordCloud <- function(coinToAnalyze, tweets_unnested){
  set.seed(1234)
  wordclouddf <- tweets_unnested %>% filter(currency == coinToAnalyze) %>% group_by(text) %>% summarize(frequency = n()) %>% rename(words = text) %>% filter(frequency >= 10)
  wordcloud2(wordclouddf, size=3)
}

# Remove self-loops (currency maps back to itself)
mergecurrency <- function(matchingdf,currency, text) {
  df <- matchingdf %>% filter(id == currency)
  return(text %in% df$matching_name)
}

# Generate Network Cloud
network <- function(data, tweets_unnested) {
  # Expand on data of top 20 cryptocurrencies to get as many matching names as possible
  duplicate <- data
  duplicate <- duplicate %>% select(id) %>% mutate(matching_name = id)
  matchingdf <- data %>% select(id, symbol) %>% rename(matching_name = symbol) %>% bind_rows(duplicate)
  
  # Create nodelist for network visualisation
  nodes <- matchingdf %>% rename(name_of_coin = id) %>% rowid_to_column("id") %>% select(id, name_of_coin) %>% head(20)
  
  network <- tweets_unnested
  
  network <- network %>% select(text, currency) %>% filter(text %in% matchingdf$matching_name) %>% select(2, 1) %>% group_by(currency, text) %>% summarize(count = n())
  
  network$self_loop = FALSE
  
  for (i in 1:nrow(network)) {
    network[i, "self_loop"] = mergecurrency(matchingdf, network[i, "currency"]$currency, network[i, "text"]$text)
  }
  
  network <- network %>% filter(!self_loop & count > 1) %>% select(currency, text, count) %>% rename(from = currency, to = text, frequency = count)
  network <- merge(network, matchingdf, by.x='to', by.y='matching_name') %>% select(from, id, frequency) %>% rename(to = id) %>% group_by(from, to) %>% summarize(frequency = sum(frequency))
  network <- merge(network, nodes, by.x='from', by.y='name_of_coin')  %>% select(id, to, frequency) %>% rename(from = id)
  network <- merge(network, nodes, by.x='to', by.y='name_of_coin')  %>% select(from, id, frequency) %>% rename(to = id) %>% arrange(from)
  
  # Make a palette of 3 colors
  col  <- brewer.pal(3,"Blues")
  nodesize<- nodes
  for(id in c(1:20)){
    nodesize$size[id]<-network %>% filter(from == id | to==id) %>%  summarise(size=sum(frequency))
  }
  nodesize$size<- unlist(nodesize$size)
  # Create a vector of color
  my_color<- col[as.numeric(cut(nodesize$size, breaks=3))]
  sizingcat<- as.numeric(cut(nodesize$size, breaks=3))
  
  networkedges<- data.frame(from = network$from, to = network$to, 
                            label=as.character(network$frequency),
                            length = 300,
                            smooth = TRUE)
  
  visnodes<- data.frame(
    id= nodes$id,
    label = nodes$name_of_coin,                              
    shape = "circle", 
    font.size=sizingcat*9,
    color = my_color)
  visnodes <- visnodes %>% filter((id %in% c(network$from,network$to)))
  
  # 3D Network Map
  visNetwork(visnodes, networkedges, width = "100%", main="Connections between Crypto Coins") %>%
    #adding the option to choose coins from a drop down list and to also highlight the chosen coin and its relationships
    visOptions(highlightNearest =  list(enabled = TRUE, algorithm = "hierarchical", degree = list(from = 0, to = 1)), nodesIdSelection = list(enabled = TRUE)) %>%
    visEvents(type = "once", startStabilizing = "function() {
    this.moveTo({scale:0.5})}") %>%
    visPhysics(stabilization = FALSE)
}