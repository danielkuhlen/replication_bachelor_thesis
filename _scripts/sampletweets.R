# sentiment scores
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)

# data import ------------------------------------------------------------------
tweetstidy <- readRDS("../02_data/02_output_data/tweets_tidy.rds") %>% 
  select(tweet_id, sourcetweet_type)

tweets <- readRDS("../02_data/02_output_data/tweets_sentiment.rds") %>%
  left_join(tweetstidy, by = "tweet_id") %>%
  mutate(sourcetweet_type = ifelse(is.na(sourcetweet_type), "normal", sourcetweet_type)) %>%
  filter(sourcetweet_type != "retweeted") %>%
  filter(!is.na(gender))

# sample 200 tweets
sample <- tweets %>%
  sample_n(200) %>%
  select(tweet_id, text)

# export for respondents -------------------------------------------------------
export(sample, "../02_data/02_output_data/sample/survey/scoring_daniel.xlsx")
export(sample, "../02_data/02_output_data/sample/survey/scoring_linda.xlsx")
export(sample, "../02_data/02_output_data/sample/survey/scoring_johanna.xlsx")
export(sample, "../02_data/02_output_data/sample/survey/scoring_philipp.xlsx")
export(sample, "../02_data/02_output_data/sample/survey/scoring_luna.xlsx")
export(sample, "../02_data/02_output_data/sample/survey/scoring_vandy.xlsx")
export(sample, "../02_data/02_output_data/sample/survey/scoring_thony.xlsx")


# import and append surveys ----------------------------------------------------