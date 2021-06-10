library(twitteR)
library(openssl)
library(httpuv)
library(httr)
library(dplyr)
library(purrr)
library(tidyr)
library(tidytext)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)

library(readr)
#some

library(jiebaRD)
library(jiebaR)
library(plyr)
load("C:/software/github/2021R_biden_report/biden_tweets_df.rda")

# 檔案載入
df = biden_tweets_df  


#提取推特內容
text <-df %>%
  select(text)
#儲存在SCV中
write.csv(text,file="text.csv",row.names = F)
#推文處理(合成向量,轉小寫)
twitext = read_csv("text.csv")
r1 = as.character(twitext$text)
r1<-tolower(r1)

#分詞處理
cutter=worker() #設置分詞引擎
segWords <- segment(r1,cutter)  #對文本分詞處理
#詞頻清洗以及篩選計算   
f <- readLines("stopword.txt")
stopwords <- c(NULL)
for (i in 1:length(f))
{
  stopwords[i]<- f[i]
}
segWords<- filter_segment(segWords,stopwords) #过滤单词，filter_segment(源文本,过滤的词)
segWords<-gsub("[0-9[:punct:]]+?","",segWords) #去除数字  0-9 表示数字，[:punct:]表示特殊字符 “! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~”
tableWord <- count(segWords) #统计词频
view(tableWord)
write.csv(tableWord,"tableWord.csv",fileEncoding = "UTF-8")
topwords <- tableWord %>%
  select(x,freq)%>%
  mutate(words=x)%>%
  arrange(desc(freq))
topwords<-select(topwords,words,freq)
write.csv(topWord,"topWord.csv",fileEncoding = "UTF-8")
topwords_30<-top_n(topwords,30,freq)
topwords_30
#繪製圖表
ggplot(data = topwords_30) + 
  geom_col(mapping = aes(x = words, y = freq))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
