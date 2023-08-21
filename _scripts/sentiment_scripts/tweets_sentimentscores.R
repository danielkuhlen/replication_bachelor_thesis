# sentiment scores
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)
library(grid)
library(tm)
library(quanteda)

# data import ------------------------------------------------------------------

# import tweets
tweets <- import("../02_data/02_output_data/tweets_master.rds")

# import dictionary
sent.dictionary <- import("../02_data/01_raw_data/dictionary/1_Dictionaries/sent.dictionary.complete.rds")

# 1) sentiment scores for each tweet -------------------------------------------
################################################################################

tweets <- tweets %>%
  # remove superfluous white spaces
  mutate(text_clean = str_trim(text_clean, side = "both")) %>% 
  mutate(text_clean = str_replace_all(text_clean, "  ", " ")) %>%
  # Create a temporary column "help" for computation, remove punctuations
  mutate(help = str_remove_all(text_clean, "[,\\.\\?:!-]")) %>%
  # Compute the number of terms in each text
  mutate(terms = str_count(help, " ") + 1) %>%
  # Remove the temporary "help" column
  select(-help)

# Plot a histogram of the number of terms in each text
ggplot(tweets, aes(x = terms)) +
  geom_histogram(bins = 200) +
  labs(title = "Length of sentences in sample", x = "Number of terms", y = "Frequency")

# add exactly one whitespace left and right (ensure identification of full terms)
tweets <- tweets %>%
  mutate(text_clean = str_pad(text_clean, width = str_length(text_clean) + 2, side = "both"))

# Scoring
#-------------------------------------------------------------------------------

# get positive and negative terms from the sentiment dictionary
positive_terms <- str_trim(sent.dictionary$feature[sent.dictionary$sentiment > 0])
negative_terms <- str_trim(sent.dictionary$feature[sent.dictionary$sentiment < 0])

# create a quanteda dictionary
sentiment_dictionary <- dictionary(list(positive = positive_terms, negative = negative_terms))

# tokenize the text and create a document-feature matrix
dfm_obj <- dfm(tweets$text_clean, dictionary = sentiment_dictionary)

# compute the occurrences of positive and negative terms
tweets$posterms <- rowSums(dfm_match(dfm_obj, "positive"))
tweets$negterms <- rowSums(dfm_match(dfm_obj, "negative"))

# calculate the raw sentiment score
tweets <- tweets %>%
  mutate(sentiment = posterms - negterms,
         sentiment.norm = sentiment / terms)

# remove unessecary files
rm(dfm_obj, negative_terms, positive_terms)


# Aggregate Scores
# ------------------------------------------------------------------------------

# mean sentiment scores by account and party
tweets <- tweets %>%
  group_by(twitter_handle) %>%
  mutate(account_sentiment = mean(sentiment.norm, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(party) %>%
  mutate(party_sentiment = mean(sentiment.norm, na.rm = TRUE)) %>%
  ungroup()

# Test if worked
# ------------------------------------------------------------------------------

# for all
test <- tweets %>%
  head(100) %>%
  select(text_clean, posterms, negterms, sentiment, sentiment.norm)

# test for negation
test <- tweets %>%
  filter(grepl("NOT_", text_clean)) %>%
  select(text, text_clean, posterms, negterms, sentiment, sentiment.norm, terms)


# export data ------------------------------------------------------------------
export(tweets, "../02_data/02_output_data/tweets_sentiment.rds")
