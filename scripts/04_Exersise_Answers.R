
### Exercise 4.1

# 1. Split the Treasure Island text into several sections, similar to above, and then 
#      manipulate it into the form required for the TF-IDF analysis

library(gutenbergr)
library(tidyverse)
library(tidytext)

treasureIsland <- gutenberg_download(120)


tidyIsland <- treasureIsland %>%
  mutate(booksection = row_number() %/% 1000) %>% 
  unnest_tokens(word, text) %>% 
  count(word, booksection) %>%
  arrange(-n)

head(tidyIsland)


# 2. Find the TF-IDF for the words in Treasure Island, by book section

tidyIsland <- tidyIsland %>%
  bind_tf_idf(term = word, document = booksection, n = n)

head(tidyIsland)


# 3. Visualise your results

tidyIsland %>%
  group_by(booksection) %>%
  top_n(6, tf_idf) %>% 
  ggplot(aes(reorder_within(x = word, 
                            by = tf_idf, 
                            within = booksection),
             y = tf_idf)) +
  geom_col(show.legend = FALSE, fill = "cornflowerblue") +
  facet_wrap(vars(booksection), ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered("words")
