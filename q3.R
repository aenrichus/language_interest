library(twitteR)
library(ROAuth)
library(httr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)

# setting Twitter API keys
api_key <- "xxx"
api_secret <- "xxx"
access_token <- "xxx"
access_token_secret <- "xxx"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# scrape Twitter for data on interests in learning
tweets_learn <-  searchTwitter('/"I want to learn/"', n=10000)  # currently returns 8675 responses

# converts to data frames
tweets_learn_data <- do.call("rbind", lapply(tweets_learn, as.data.frame))

# write to csv
write.csv(tweets_learn_data,file="tweets_learn.csv")

# focus on language learning
tweets_want_English <- searchTwitter('/"I want to learn English/"', n=1000)
tweets_want_Japanese <- searchTwitter('/"I want to learn Japanese/"', n=1000)
tweets_want_Chinese <- searchTwitter('/"I want to learn Chinese/"', n=1000)

tweets_need_English <- searchTwitter('/"I need to learn English/"', n=1000)
tweets_need_Japanese <- searchTwitter('/"I need to learn Japanese/"', n=1000)
tweets_need_Chinese <- searchTwitter('/"I need to learn Chinese/"', n=1000)

tweets_teach_English <- searchTwitter('/"teach me English/"', n=1000)
tweets_teach_Japanese <- searchTwitter('/"teach me Japanese/"', n=1000)
tweets_teach_Chinese <- searchTwitter('/"teach me Chinese/"', n=1000)

tweets_help_English <- searchTwitter('/"help me with English/"', n=1000)
tweets_help_Japanese <- searchTwitter('/"help me with Japanese/"', n=1000)
tweets_help_Chinese <- searchTwitter('/"help me with Chinese/"', n=1000)

# to data frames
want_English_data <- do.call("rbind", lapply(tweets_want_English, as.data.frame))
want_Japanese_data <- do.call("rbind", lapply(tweets_want_Japanese, as.data.frame))
want_Chinese_data <- do.call("rbind", lapply(tweets_want_Chinese, as.data.frame))

need_English_data <- do.call("rbind", lapply(tweets_need_English, as.data.frame))
need_Japanese_data <- do.call("rbind", lapply(tweets_need_Japanese, as.data.frame))
need_Chinese_data <- do.call("rbind", lapply(tweets_need_Chinese, as.data.frame))

teach_English_data <- do.call("rbind", lapply(tweets_teach_English, as.data.frame))
teach_Japanese_data <- do.call("rbind", lapply(tweets_teach_Japanese, as.data.frame))
teach_Chinese_data <- do.call("rbind", lapply(tweets_teach_Chinese, as.data.frame))

help_English_data <- do.call("rbind", lapply(tweets_help_English, as.data.frame))
help_Japanese_data <- do.call("rbind", lapply(tweets_help_Japanese, as.data.frame))
help_Chinese_data <- do.call("rbind", lapply(tweets_help_Chinese, as.data.frame))

# join data frames by language
English_1 <- full_join(want_English_data, need_English_data)
English_2 <- full_join(teach_English_data, help_English_data)
English <- full_join(English_1, English_2)

Japanese_1 <- full_join(want_Japanese_data, need_Japanese_data)
Japanese_2 <- full_join(teach_Japanese_data, help_Japanese_data)
Japanese <- full_join(Japanese_1, Japanese_2)

Chinese_1 <- full_join(want_Chinese_data, need_Chinese_data)
Chinese_2 <- full_join(teach_Chinese_data, help_Chinese_data)
Chinese <- full_join(Chinese_1, Chinese_2)

# add a language column
English$language <- "en"
Japanese$language <- "jp"
Chinese$language <- "cn"

# join all languages
Language <- full_join(English, Japanese)
Language <- full_join(Language, Chinese)

# remove retweets
Language_not_RT <- Language %>% filter(grepl("RT ", text) == FALSE)
Language_not_RT %>% group_by(language) %>% summarise(learners = n())  # en > jp > cn

# identify influencers
influencers <- Language_not_RT %>% filter(retweetCount > 0)
influencers %>% group_by(language) %>% summarise(learners = n())  # en = jp > cn

# find users willing to pay
Language_pay <- Language_not_RT %>% filter(grepl("pay", text))  # 2 users, en only

# check date range
range(Language$created)  # ~9.5 days worth of Tweets
365/9.5  # ~38 search timeframes in a year

# potential follower/userbase annually
Language_not_RT %>% group_by(language) %>% summarise(learners = n()*38)  # en > jp > cn

# plot language popularity
ggplot(Language_not_RT, aes(language)) + geom_bar()  # shows the relative popularity

# create corpus to develop word cloud
Japanese_corpus <- Corpus(VectorSource(Japanese$text))
Jc_temp <- sapply(Japanese_corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
Japanese_corpus <- Corpus(VectorSource(Jc_temp))
Japanese_corpus <- tm_map(Japanese_corpus, PlainTextDocument)
Japanese_corpus <- tm_map(Japanese_corpus, removePunctuation)
Japanese_corpus <- tm_map(Japanese_corpus, removeWords, stopwords('english'))

# create word cloud for Japanese
wordcloud(Japanese_corpus, min.freq = 5, scale = c(5, 2), rot.per = 0.25, random.color = T, max.word = 30, 
          random.order = F, colors = brewer.pal(6, "Dark2"))

# %% Future directions: (1) Set up Twitter and Youtube accounts. (2) Generate language content (scrape for best
# %% materials to modify). (3) Target potential users on Twitter, focusing on influencers (scrape follower 
# %% counts). (4) Collect additional data (location, etc.) via newsletter. (5) Expand search to include different
# %% combinations or words.




