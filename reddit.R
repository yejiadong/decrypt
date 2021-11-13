# This is an R script written to process Reddit comments
# Include all necessary libraries
# Data Processing Libraries
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)

# Textual Analysis Libraries
library(corpus)

# Plotting Libraries
library(ggplot2)
library(wordcloud)
library(tm)

# API libraries
library(RedditExtractoR)
library(jsonlite)

# Pulls comments from Reddit by topic, limited to a week
pullComments <- function(crypto) {
  r.urls <- find_thread_urls(
    keywords = crypto, sort_by = "top", period = "week"
  ) %>% arrange(desc(comments)) %>% head(5)
  r.urls
  r.content <- get_thread_content(r.urls$url) 
  r.content$comments <- r.content$comments %>% select(author, date, upvotes, comment)
  return(r.content$comments)
}

# Function for cleaning of Reddit data, adapted from tweet-cleaning
clean_reddit <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove punctuation
    str_remove_all("[[:punct:]]") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any non alphabetical characters, including numbers
    str_replace_all("[^[:alpha:]]", " ") %>%
    # Remove non-english characters
    iconv(from="UTF-8", to="ASCII", sub="") %>% 
    # Remove unnecessary whitespace in between words
    str_replace_all("\\s+"," ") %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

# Further cleaning of comments
cleanComments <- function(vec) {
  # Clean text with above function
  vec$comment <- vec$comment %>% clean_reddit
  
  # Remove duplicated comments
  vec <- vec[!duplicated(vec$comment), ]
  
  # Remove empty comments
  vec <- vec %>% na.omit
  
  # Assign incrementing index column to keep track of comments
  start <- 1
  vec$comment_id <- 0
  for (i in 1:nrow(vec)) {
    vec[i,"comment_id"] <- start
    start <- start + 1
  }
 return(vec) 
}

# Function for unnesting of Reddit data
unnest_reddit <- function(vec) {
  # Split string by word into separate rows
  reddit_unnested <- vec  %>% unnest_tokens(word, comment) %>% select(author,date, word, comment_id)
  
  # Remove any stop words
  reddit_unnested <- reddit_unnested %>% anti_join(stop_words)
  
  # Remove any two characters and below words, since they will not be very meaningful
  reddit_unnested <- reddit_unnested[nchar(reddit_unnested$word) >= 3,]
  return(reddit_unnested)
}

mergecommentSentiments <- function(vec, reddit_unnested) {
  # Get sentiment dictionary. Here, we are using the afinn dictionary, as it allows us to assign specific polarity scores.
  sentiment_dataset <- read.csv("./AFINN.csv", header = T)
  # Merge with the sentiment dataset to obtain our sentiment scores for each word
  afinn_sentiment <- reddit_unnested  %>% merge(sentiment_dataset, by = 'word')
  # Sum up sentiment scores to get the sentiment polarity for entire comment
  afinn_sentiment <- afinn_sentiment %>% arrange(comment_id) %>% group_by(comment_id) %>% summarize(sentiment_polarity = sum(value)) %>%  merge(vec, by = 'comment_id') %>% select(comment_id, sentiment_polarity, author, date, upvotes, comment)
  return(afinn_sentiment)
}


# Plots the ggplotly plot by comment id based on processed dataframe
plotSentimentByCommentId <- function(afinn_sentiment) {
  label_wrap <- label_wrap_gen(width = 60)
  
  afinn_sentiment_formatted <- afinn_sentiment %>% mutate(formatted_tweet = str_glue("Comment ID: {comment_id}
                                      Author: {author}
                                      Comment: {label_wrap(comment)}"))
  
  gplot <- afinn_sentiment_formatted %>%
    ggplot(aes(comment_id, sentiment_polarity)) +
    geom_line(color = "#2c3e50", alpha = 0.5) +
    geom_point(aes(text = formatted_tweet), color = "#2c3e50") +
    geom_smooth(method = "loess", span=0.25, se=FALSE, color="blue") +
    geom_hline(aes(yintercept = mean(sentiment_polarity), linetype = "Mean Sentiment Polarity"), color="blue") +
    geom_hline(aes(yintercept = median(sentiment_polarity) + 1.96 * IQR(sentiment_polarity), linetype = "75th Percentile Sentiment Polarity"), color="red") +
    geom_hline(aes(yintercept = median(sentiment_polarity) - 1.96 * IQR(sentiment_polarity), linetype = "25th Percentile Sentiment Polarity"), color="red") +
    theme_minimal() + 
    theme(legend.direction = "horizontal", legend.background = element_rect(fill = "white", colour = "gray30")) +
    labs(title = "Sentiment Polarity by Comment ID [Reddit]", x = "Comment ID", y="Sentiment Polarity")
  
  ggplotly(gplot, tooltip = "text") %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y= -1), xaxis = list(rangeslider = list(type="date")))
}

# Plots the ggplotly plot by date based on processed dataframe
plotCommentSentimentByTime <- function(afinn_sentiment, coinid) {
  coin_history_df <- coin_history(coin_id = coinid, vs_currency = "usd", days = 10)
  coin_history_df$date<- substr(coin_history_df$timestamp,1,10)
  coin_history_average <- coin_history_df%>% group_by(date) %>% summarise(average_price = mean(price)) %>% arrange(date)
  
  mean_over_time <- afinn_sentiment %>% group_by(date) %>% summarize(mean_sentiment = mean(sentiment_polarity)) %>% mutate(formatted_text = str_glue("Date: {date}
          Mean Sentiment Score: {round(mean_sentiment, 2)}"))
  
  merged_sentiment <- merge(mean_over_time, coin_history_average, by = "date")
  merged_sentiment <- merged_sentiment %>% select(c(1,3,2,4)) %>% gather("type", "value", 3:4)
  
  gplottime <- merged_sentiment %>% ggplot(aes(x=date, y=value)) +
    geom_line(size=2, alpha=0.9, color="blue", group = 1) +
    geom_point(aes(text = formatted_text), color = "#2c3e50") +
    theme(text = element_text(size=13)) +
    labs(
      x = NULL, y = NULL,
      title = "Avg Price of Coin vs Mean Sentiment Score of Comments",
      subtitle = paste("Comments collected from", (Sys.Date() - 6) , "to" , Sys.Date())
    ) + facet_wrap(~factor(type), scales="free") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    theme(text = element_text(size=13))
  
  return(gplottime)
}

# Gets the most recent sentiment values for display
getUpdatedCommentSentimentValues <- function(afinn_sentiment) {
  mean_over_time <- afinn_sentiment %>% group_by(date) %>% summarize(mean_sentiment = mean(sentiment_polarity)) %>% mutate(formatted_text = str_glue("Date: {date}
          Mean Sentiment Score: {round(mean_sentiment, 2)}"), yesterday_sentiment = dplyr::lag(mean_sentiment, n = 1, default = NA)) %>% arrange(desc(date)) %>% slice_head() %>% select(mean_sentiment, yesterday_sentiment) %>% mutate(mean_sentiment = round(mean_sentiment, 2), up = mean_sentiment > yesterday_sentiment, diff = round(((mean_sentiment - yesterday_sentiment)/yesterday_sentiment)*100, 2))
  return(mean_over_time)
}

# Plot the top occurring words
plotTopOccuringWordsComments <- function(reddit_unnested) {
  # Plotting bar graph of top 15 words, and word cloud
  reddit_unnested %>%
    count(word, sort = TRUE) %>%
    head(15) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    theme(text = element_text(size=13)) +
    coord_flip() +
    labs(x = "Count",
         y = "Unique words",
         title = "Most frequent words found in comments") 
}

# Generate wordcloud
wordCloudComments <- function(reddit_unnested){
  set.seed(1234)
  wordcloud(reddit_unnested$word, min.freq=10, scale=c(3.5, .5), random.order=FALSE, max.words=100, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
}
