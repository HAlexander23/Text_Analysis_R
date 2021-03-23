
### Exercise 2.1

# 1. Extract the “Treasure Island” text from the **gutenbergr** public library
#     and tidy the book into the tidy one-token-per-row structure
library(gutenbergr)
library(tidytext)
library(tidyverse)

gutenberg_works(title == "Treasure Island")
TreasureIsland <- gutenberg_download(120)
head(TreasureIsland)

tidyTreasureIsland <- TreasureIsland %>% unnest_tokens(word, text)
head(tidyTreasureIsland)


 
# 2. Find the most common words in the Treasure Island text
tidyTreasureIsland %>% 
  count(word, sort = TRUE) %>% 
  head()

# 3. What are the most common words after removing stop words
tidyTreasureIsland %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head()


# 4. Stem the Treasure Island text. Does this change the order of the most common words within the text?
library(SnowballC)

tidyTreasureIsland %>% 
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>% 
  count(word, sort = TRUE) %>% 
  head()




### Exercise 2.2

# 1. Tokenize the Treasure Island data into word triplets (trigrams)
TreasureIsland <- gutenberg_download(120)

tidyTreasureTriplets <- TreasureIsland %>% 
  unnest_tokens(output = trigram,
                input = text,
                token = "ngrams",
                n = 3)

head(tidyTreasureTriplets)

# 2. Which words co-occur most frequently?
tidyTreasureTriplets %>% 
  count(trigram, sort = TRUE) %>% 
  head()
  
