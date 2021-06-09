library(twitteR)
library(openssl)
library(httpuv)
library(httr)
library(dplyr)
library(purrr)
library(tidyr)
library(tidytext)
library(quanteda)
library(stringr)
library(ggplot)
library(textdata)

apikey <- "Phdkc5Irp6JMnbjrC3iTEdMWF"
apisecret <- "ovKi4IYUwrTdD8YtVRNSOmDuUCBYOynzhN697rYGa1afKAj3DE"
token <- "1391410623167143938-xBN1pL4cECnvNv8wauc1ENrO4PDlHO"
tokensecret <- "smY1K5Oz5ZCwteg2OqddJfx383uE2bI1KKilqd5yU9xmm"
setup_twitter_oauth(apikey, apisecret, token, tokensecret)


biden_tweets <- userTimeline("POTUS", n = 3200)
biden_tweets_df <- tbl_df(map_df(biden_tweets, as.data.frame))
save(x = biden_tweets_df,file = "C:/Users/user/Desktop/程式設計與資料科學導論/final project/biden_tweets_df.rda")

JoeBiden_tweets <- userTimeline("JoeBiden", n = 3200)
JoeBiden_tweets_df <- tbl_df(map_df(JoeBiden_tweets, as.data.frame))
save(x = JoeBiden_tweets_df,file = "C:/Users/user/Desktop/程式設計與資料科學導論/final project/JoeBiden_tweets_df.rda")

load("C:/Users/user/Desktop/程式設計與資料科學導論/final project/biden_tweets_df.rda")
biden_tweets_df <- biden_tweets_df %>%
    extract(statusSource, "source", "([a-zA-Z]+ ?[a-zA-Z]+ [a-zA-Z]+|Sprinklr|Periscope)") %>%
    select(id, source, text, created, favoriteCount, retweetCount) %>%
    arrange(source, desc())

#關鍵字與喜歡數、轉推數的關係
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- biden_tweets_df %>%
    filter(!str_detect(text, '^"')) %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = reg) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "[a-z]")) %>%
    arrange(desc(favoriteCount))

tweet_words %>%
    head(20) %>%
    group_by(favoriteCount) %>%


#詞頻圖
tweet_words %>%
    count(word) %>%
    mutate(word = reorder(word, n)) %>%
    top_n(20, n) %>%
    ggplot() +
    geom_bar(aes(x = word, y = n), stat = "identity") +
    ylab("frequency of occurrence") +
    coord_flip()

#情緒字眼
sentiment_overview <- tweet_words %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(!is.na(sentiment)) %>%
    count(sentiment, sort = TRUE)

ggplot(sentiment_overview) +
    geom_bar(aes(x = sentiment, y = n), stat = "identity")

sources <- tweet_words %>%
    group_by(source) %>%
    mutate(total_words = n()) %>%
    ungroup() %>%
    distinct(id, source, total_words)

by_source_sentiment <- tweet_words %>%
    inner_join(get_sentiments("nrc"), by = "word") %>%
    count(sentiment, id) %>%
    ungroup() %>%
    complete(sentiment, id, fill = list(n = 0)) %>%
    inner_join(sources) %>%
    group_by(source, sentiment, total_words) %>%
    summarize(words = sum(n)) %>%
    arrange(desc(total_words)) %>%
    ungroup()

