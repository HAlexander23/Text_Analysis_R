

# 02 Simple Text Manipulation ---------------------------------------------


# Basic String Manipulation

library(stringr)

fruit <- c("apple", "orange", "banana", "kiwi")

str_c("Fruit", fruit, sep=":")
str_c(fruit, collapse = ", ")
str_sub(fruit, start = 1, end = 3)
str_sub(fruit, start = -1)


# Regular Expressions

fruit <- c("apple", "orange", "banana", "kiwi")

str_replace(fruit, pattern = "a", replacement = "A")
str_replace(fruit, pattern = ".a.", replacement = "A")
str_replace(fruit, pattern = "^a", replacement = "A")
str_replace(fruit, pattern = "a$", replacement = "A")


# escape characters
fruit <- c("apple.orange", "banana.kiwi")

unlist(str_split(fruit, pattern = "."))
unlist(str_split(fruit, pattern = "\\."))

user_ids <- c("User123", "User234", "hello world", "banana", "User345")

str_subset(user_ids, pattern = "\\d")
str_extract(user_ids, pattern = "\\d{3}")
str_extract(user_ids, pattern = "[A-Z a-z]+")
str_subset(user_ids, pattern = "bann?ana")
str_replace_all("abcd2ef7gh9ij2klmn98op", 
                pattern = "[1-9]+", 
                replacement = "")

# --------
# Tidying and Summarising Text Data

library(gutenbergr)

# view The Wizard of OZ
gutenberg_works(title == "The Wonderful Wizard of Oz")

# download text
wizardOfOz <- gutenberg_download(55)
head(wizardOfOz)


# tokenise data

library(tidytext)
library(dplyr)

# convert text into tokenised format
tidyWizard <- wizardOfOz %>% unnest_tokens(word, text)
head(tidyWizard)

tidyWizard %>% count(word, sort = TRUE) %>% head()


# stop words
head(stop_words)

tidyWizard  %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  head()

# remove other terms
characters <- data.frame(word = c("dorothy", "scarecrow", "woodman",
                                  "lion", "tin", "witch", "toto"))

tidyWizardNoCharacters <-  tidyWizard %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(characters, by = "word") %>%
  count(word, sort = TRUE)

head(tidyWizardNoCharacters)

# stemming 

library(SnowballC)

wordStem(c("fear", "fearing", "fearful", "play", "played", "playing"))


tidyWizardStemmed <-  tidyWizard %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>% 
  count(word, sort = TRUE)

head(tidyWizardStemmed)


# tokenise into bigrams
tidyWizardNgram <- wizardOfOz %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)

tidyWizardNgram  %>%
  count(word, sort = TRUE) %>%
  head()

# make tidy ngram data
tidyWizardNgram  <- wizardOfOz %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE) %>%
  separate(word, c("firstWord", "secondWord"), sep = " ")

head(tidyWizardNgram)

# remove stop words
tidyWizardNgram <- tidyWizardNgram %>%
  anti_join(stop_words, by = c("firstWord" = "word")) %>%
  anti_join(stop_words, by = c("secondWord" = "word"))

head(tidyWizardNgram)


# Visualising related words

library(ggraph)
library(igraph)
library(tidyr)

# Make tidy ngram data
tidyWizardNgram  <- wizardOfOz %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE) %>%
  separate(word, c("firstWord", "secondWord"), sep = " ")

# Remove stop words
tidyWizardNgram <- tidyWizardNgram %>%
  anti_join(stop_words, by = c("firstWord" = "word")) %>%
  anti_join(stop_words, by = c("secondWord" = "word"))

# Filter so only common pairs about Dorothy or the scarecrow remain
tidyWizardNgram <- tidyWizardNgram %>%
  filter((firstWord  %in% c("dorothy", "scarecrow")) |
           secondWord %in% c("dorothy", "scarecrow"),
         n > 1)


head(tidyWizardNgram)

# Create the igraph object
igraph_wizard <-  graph_from_data_frame(tidyWizardNgram)       

# Plot the ggraph  
ggraph(igraph_wizard, layout = 'stress') +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "coral", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



# 03 Sentiment Analysis ---------------------------------------------------


library(gutenbergr)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(wordcloud)



# get tidy data
wizardOfOz <- gutenberg_download(55)

tidyWizard <- wizardOfOz %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

head(tidyWizard)


# get sentiments

sentiment <- get_sentiments(lexicon = "bing")

head(sentiment)

tidyWizard %>% 
  inner_join(sentiment, by = "word") %>% 
  head()

tidyWizard %>% 
  inner_join(sentiment, by = "word") %>% 
  group_by(sentiment) %>% 
  summarise(total = sum(n))

# quantitative results - AFINN

tidyWizard %>% 
  inner_join(get_sentiments(lexicon = "afinn"), by = "word") %>% 
  head()

afinnWizard <- wizardOfOz %>% 
  mutate(lineNumber = row_number()) %>%   # add row number to df
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments(lexicon = "afinn"), by = "word")
  

afinnWizard <- afinnWizard %>% 
  mutate(booksection = lineNumber %/% 100) %>%    # floor division
  group_by(booksection) %>% 
  summarise(score = mean(value))

afinnWizard %>% ggplot(aes(booksection, score)) +
  geom_point() +
  geom_line() + 
  labs(title = "Sentiment throughout the Wizard of Oz",
       y = "Average Afinn Sentiment Score")


### Word clouds


library(wordcloud)


cloudWizard <- wizardOfOz %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)


wordcloud(words = cloudWizard$word, 
          freq = cloudWizard$n, 
          max.words = 50, 
          colors = "cornflowerblue")


# comparison cloud

# term-frequency matrix
compCloud <- wizardOfOz %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments(lexicon = "bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>%
  pivot_wider(names_from = "sentiment", 
              values_from = "n", 
              values_fill = list(n = 0)) %>% 
  data.frame()

# row names
rownames(compCloud) <- compCloud$word
compCloud <- select(compCloud, -word)  

head(compCloud)


comparison.cloud(compCloud, 
                 colors = c("darkred", "darkgreen"), 
                 max.words = 50)


# 04 Word Document Frequency ----------------------------------------------


# import the text
wizardOfOz <- gutenberg_download(55)

# make into tidy form and add line numbers
tidyWizard <- wizardOfOz %>%
  mutate(lineNumber = row_number()) %>%
  unnest_tokens(word, text)

# label into sections by line number
tidySplitWizard <- tidyWizard %>%
  mutate(booksection = lineNumber %/% 1000) # split every 1000 lines

# add document-term count
tidySplitWizard <- tidySplitWizard %>%
  group_by(word, booksection) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(-count)

head(tidySplitWizard, 8)


# apply TF-IDF
tidySplitWizard <- tidySplitWizard %>%
  bind_tf_idf(term = word, document = booksection, n = count)

head(tidySplitWizard, 8)


# Visualise top word by section
tidySplitWizard %>%
  group_by(booksection) %>%
  top_n(6, tf_idf) %>% 
  ggplot(aes(reorder_within(x = word, 
                            by = tf_idf, 
                            within = booksection),
             y = tf_idf)) +
    geom_col(show.legend = FALSE, fill = "azure4") +
    facet_wrap(vars(booksection), ncol = 2, scales = "free_y") +
    coord_flip() +
    scale_x_reordered("words")


# 05 Topic Modelling ------------------------------------------------------


library(topicmodels)


BBC_Articles <- read_csv("../data/BBC_Articles.csv")

BBC_Articles <- BBC_Articles %>% 
  anti_join(stop_words, by = "word") %>% 
  count(document, word, sort = TRUE)

head(BBC_Articles)

# cast into document-term matrix
dtm_BBC_Articles <- cast_dtm(data = BBC_Articles,
                             document = document,
                             term = word,
                             value = n)

# log ratio
BBC_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .005 | topic2 > .005) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  top_n(10, abs(log_ratio)) %>% 
  arrange(log_ratio)

# visualising lda
top_words <- BBC_topics %>%
  group_by(topic) %>%
  top_n(12, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_words %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_col(fill = "azure4", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered("Words")



### Document-Topic Probabilities

BBC_documents <- tidy(LDA_model, matrix = "gamma")

head(BBC_documents)

BBC_documents %>% filter(document == "Business_20")


### predict 


# read in and add counts
test <- read_csv("../data/BBC_Politics_123.csv") %>%
  count(document, word, sort = TRUE)

head(test)


# cast into document-term matrix
dtm_test <- cast_dtm(data = test,
                     document = document,
                     term = word,
                     value = n)
dtm_test

# get new predictions
test_output <- posterior(LDA_model, dtm_test)

test_output$topics


# 06 Web Scraping ---------------------------------------------------------

library(rvest)


read_html("https://en.wikipedia.org/wiki/World_population")

# Read from local copy
# Read_html("data/WorldPopulationWikipedia.html")



WikiPage <- read_html("https://en.wikipedia.org/wiki/World_population")

# or read from local file
# WikiPage <- read_html("../data/WorldPopulationWikipedia.html")

WikiNodes <- html_nodes(WikiPage, "h2 .mw-headline")
WikiNodes

html_text(WikiNodes)

html_attr(WikiNodes, "id")
html_attr(WikiNodes, "class")



## tables

WikiPage <- read_html("https://en.wikipedia.org/wiki/World_population")

# or locally;
# WikiPage <- read_html("../data/WorldPopulationWikipedia.html")

# read all html tables
WikiDfList <- html_table(WikiPage, header = TRUE, fill = TRUE)

# print just the 6th
WikiDfList[6]



WikiPage <- read_html("https://en.wikipedia.org/wiki/World_population")

# gather all the html table nodes
WikiTables <- html_nodes(WikiPage, "table")

# convert just the 6th into a data frame
html_table(WikiTables[6], header = TRUE)
