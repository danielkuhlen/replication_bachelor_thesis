# sentiment scores
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)
library(grid)
library(lubridate)
library(tm)
library(quanteda)

# 1) dataimport ----------------------------------------------------------------
################################################################################

# import tweets
tweets <- import("../02_data/02_output_data/tweets_sentiment.rds")

# import dictionary
sent.dictionary <- import("../02_data/01_raw_data/dictionary/1_Dictionaries/sent.dictionary.complete.rds")

# 2) datawrangling -------------------------------------------------------------
################################################################################

# time specific variables ------------------------------------------------------

# add week variable
tweets <- tweets %>%
  mutate(tweet_week = paste0(substr(year(tweet_date), 3, 4), "_", week(tweet_date))) %>%
  mutate(tweet_week_date = floor_date(tweet_date, "week"))

# text on party and and date aggregate level -----------------------------------

tweets <- tweets %>%
  group_by(party, tweet_week_date) %>%
  summarise(text_clean = paste(text_clean, collapse = "")) %>% 
  mutate(help = str_remove_all(text_clean, "[,\\.\\?:!-]")) %>%
  mutate(terms = str_count(help, " ") + 1) %>%
  select(-help)

# 3) sentiment scores ----------------------------------------------------------
################################################################################

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
         sentiment.norm = sentiment / terms) %>%
  select(-text_clean)

# remove unessecary files
rm(dfm_obj, negative_terms, positive_terms)

# 3) export dataframes ---------------------------------------------------------
################################################################################
export(tweets, "../02_data/02_output_data/time_sentiment/weekly_sentiment.rds")
