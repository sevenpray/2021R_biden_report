library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)
load("C:/Users/USER/Downloads/biden_tweets_df.rda")

# mutate source
df = biden_tweets_df %>% 
    mutate(source = sapply(biden_tweets_df$statusSource , function(x) {
        if (str_detect(x, pattern = "Twitter Web App")) return("Twitter Web App")
        else if (str_detect(x, pattern = "Periscope")) return("Periscope")
        else if (str_detect(x, pattern = "Twitter Media Studio")) return("Twitter Media Studio")
        else if (str_detect(x, pattern = "Sprinklr")) return("Sprinklr")
        else return("The White House")
    })) %>% select(id, source, text, created, retweetCount, favoriteCount)



# 各 source 數量比較
df %>% 
    group_by(source) %>% 
    summarise(count = n()) %>%
    view()

df %>% 
    group_by(source) %>% 
    summarise(count = n()) %>% 
    ggplot() + 
    geom_bar(aes(x = source, y = count), stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = rel(0.75))) + 
    labs(x = "")



# retweetCount 和 favoriteCount 的 timeline
ggplot(data = df) + 
    geom_col(mapping = aes(x = created, y = retweetCount, color = source))


ggplot(data = df) + 
    geom_col(mapping = aes(x = created, y = favoriteCount, color = source))




# picture/link for each source
df %>% 
    filter(!str_detect(text, '^"')) %>% 
    count(source, picture = ifelse(str_detect(text, "t.co"), 
                                   "Picture/link", "No picture/link")) %>% 
    view()



df %>% 
    filter(!str_detect(text, '^"')) %>% 
    count(picture = ifelse(str_detect(text, "t.co"), 
                                   "Picture/link", "No picture/link")) %>% 
    ggplot() + 
    geom_bar(aes(picture, n), stat = "identity") + 
    labs(x = "", y = "count")



# timeline for whole day
df %>% 
    count(hour = hour(with_tz(created, "EST"))) %>% 
    mutate(percent = n / sum(n)) %>%
    ggplot(mapping = aes(hour, percent)) + 
    geom_line() + 
    scale_y_continuous(labels = percent_format()) + 
    labs(x = "Hour of day", y = "% of tweets")






