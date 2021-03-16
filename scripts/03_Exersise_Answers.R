

### Exercise 3.1

# 1. What are the most frequently expressed positive words in Treasure Island?
library(gutenbergr)
library(tidytext)
library(tidyverse)

TreasureIsland <- gutenberg_download(120)

tidyTreasureIsland <- TreasureIsland %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

sentiment <- get_sentiments(lexicon = "bing")

tidyTreasureIsland %>% 
  inner_join(sentiment, by="word") %>%
  arrange(desc(sentiment), -n) %>% 
  head()


# 2. Is the Treasure Island text mostly positive or negative?
tidyTreasureIsland %>% 
  inner_join(sentiment, by="word") %>% 
  group_by(sentiment) %>% 
  summarise(total = sum(n))

tidyTreasureIsland %>% 
  inner_join(get_sentiments(lexicon = "afinn"), by="word") %>%
  mutate(total_score = n * value) %>%
  summarise(final_total = sum(total_score))

# 3. How does sentiment change throughout the treasure Island text?
TreasureIsland <- gutenberg_download(120)

TreasureIsland_sectioned <- TreasureIsland %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments(lexicon = "afinn"), by="word") %>%
  mutate(booksection = linenumber %/% 100)

head(TreasureIsland_sectioned)

TreasureIsland_sectioned %>% 
  group_by(booksection) %>% 
  summarise(sentiment = mean(value)) %>% 
  ggplot(aes(booksection, sentiment)) +
  geom_line() + geom_point() + geom_smooth()




### Exercise 3.2

# 1. Make two word clouds, one using positive and the other using negative sentiment 
#      from the Treasure Island text
library(wordcloud)

TreasureIsland <- gutenberg_download(120)

TreasureIsland_sentiment <- TreasureIsland %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments(lexicon = "bing"), by="word")

head(TreasureIsland_sentiment)


TreasureIsland_negative <- TreasureIsland_sentiment %>%
  filter(sentiment == "negative")

wordcloud(TreasureIsland_negative$word,  
          TreasureIsland_negative$n, max.words = 50, colors ="darkred")


TreasureIsland_positive <- TreasureIsland_sentiment %>%
  filter(sentiment == "positive")

wordcloud(TreasureIsland_positive$word,  
          TreasureIsland_positive$n, max.words = 50, colors ="darkgreen")


# 2. Combine/compare the two clouds

compCloud <- TreasureIsland_sentiment %>%
  spread(sentiment, n, fill = 0) %>% 
  data.frame() # tibble rownames are deprecated

rownames(compCloud) <- compCloud$word
compCloud <- select(compCloud, - word)

head(compCloud)

comparison.cloud(compCloud, 
                 colors = c("darkred", "darkgreen"),
                 max.words = 50)

