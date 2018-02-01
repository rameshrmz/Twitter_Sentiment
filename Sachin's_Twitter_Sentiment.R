# Text analysis of Sachin's tweets confirms he writes More Positive Messages from his iPhone, Android Twitter App 

# First we'll retrieve the content of Sachin's timeline using the userTimeline function in the twitteR package:
    library(dplyr)
    library(purrr)
    library(twitteR)

# You'd need to set global options with an authenticated app.

    consumerKey <- "[twitter_consumer_key]"
    consumerSecret <- "[twitter_consumer_secret]"
    accessToken <- "[twitter_access_token]"
    accessTokenSecret <- "[twitter_access_token_secret]"
    setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret, access_token = accessToken, access_secret = accessTokenSecret)

# We can request only 3200 tweets at a time; it will return fewer depending on the API. Here I am requesting last 1000 tweets 
    sachin_tweets <- userTimeline("sachin_rt", n=1000)
    sachin_tweets_df <- tbl_df(map_df(sachin_tweets, as.data.frame))

# We're looking only at the iPhone and Android tweets- a much smaller number are from the web client or iPad
# We will get count of tweets by sachin tweeted from iPhone and Andriod using Twitter App
    library(tidyr)
    tweets <- sachin_tweets_df %>% select(id, statusSource, text, created) %>% extract(statusSource, "source", "Twitter for (.*?)<") %>% filter(source %in% c("Android","iPhone"))
    View(tweets)

# Spot a difference between Number of Percent Tweets vs Time of the day (EST Time)
    library(lubridate)
    library(scales)
    tweets %>% count(source, hour = hour(with_tz(created, "EST"))) %>% mutate(percent = n/sum(n)) %>% ggplot(aes(hour, percent, color = source)) + geom_line() + scale_y_continuous(labels = percent_format()) + labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

# Tweets Count between "Picture/Link" tweets vs "No Picture/Link" tweets.
    library(stringr)
    library()
    tweet_picture_counts <- tweets %>% filter(!str_detect(text, '^"')) %>% count(source, picture = ifelse(str_detect(text, "t.co"), "Picture/link", "No picture/link"))
    ggplot(tweet_picture_counts, aes(source, n, fill = picture)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "", y = "Number of tweets", fill = "")


# Sentimentr Analysis Result gives more accuracy result compare to other sentiment analysis method

# Sentiment Analysis: Sachin's conveying more Positive Thoughts in his Twitter

# Installing the Sentimentr Package

    # install.packages('sentimentr') or
    # install.packages('devtools'), install_github('trinker/sentimentr')

    library(devtools)

# Check Aggregated Sentiment Score:
    Sentiment_Scores <- tweets$text %>% sentimentr::sentiment_by(by=NULL)
    View(Sentiment_Scores)

# Check Positive & Negative words from Sentiment Analysis:
    Positive_Negative_words <- tweets$text %>% sentimentr::extract_sentiment_terms()
    View(Positive_Negative_words)

# Check List of sentences having Positive Content (Highlighted in Green Color) and Negative Content (Highlighted in Red Color).
    tweets$text %>% sentimentr::sentiment_by(by = NULL) %>% sentimentr::highlight()