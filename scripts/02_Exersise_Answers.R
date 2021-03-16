

### Exercise 2.1

# 1. Using the fruits vector above, select fruits that start with 'a' or 'b'
library(stringr)
fruit <- c("apple", "orange", "banana", "kiwi")
str_subset(fruit, pattern = "^[ab]")

# 2. How would you select a fruit with six letters?
str_subset(fruit, pattern = "[a-z]{6}")
  
# 3. Clean the string "abcd2ef7gh9ij2klmn98op" to keep only the numeric characters
str_replace_all("abcd2ef7gh9ij2klmn98op", pattern = "[a-z]", replacement = "")

# 4. Think about how you could apply this to search a text for phone numbers, 
      # emails or similarly coded text? 
text <- c("example.email@gmail.com", "07719 377 390", "User123", "SN14 0GB")

str_subset(text, pattern = "\U0040")
str_subset(text, pattern = "@")

str_subset(text, pattern = "[0-9]{5} [0-9]{3} [0-9]{3}")
str_subset(text, pattern = "[A-Za-z]=[0-9]{3}")
str_subset(text, pattern = "[A-Z]\\d+ \\d+[A-Z]+")





### Exercise 2.2

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




### Exercise 2.3

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
  