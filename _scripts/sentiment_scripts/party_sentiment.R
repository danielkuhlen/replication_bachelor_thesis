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
sent.dictionary <- import("../02_data/01_raw_data/dictionary/1_Dictionaries/Rauh_SentDictionaryGerman.Rdata")

# 2) datawrangling -------------------------------------------------------------
################################################################################

# subset dataset for different time periods ------------------------------------

# pre and post election datasets
election <- as.POSIXct("2021-09-26", format = "%Y-%m-%d")

pre_election <- tweets %>%
  filter(tweet_date < election)

post_election <- tweets %>%
  filter(tweet_date > election)

# hot electioneering phase
three_months_before_election <- election %m-% months(3)

electioneering_phase <- tweets %>%
  filter(tweet_date >= three_months_before_election & tweet_date < election)

# after new goverment was formed
newgoverment <- as.POSIXct("2021-12-08", format = "%Y-%m-%d")

new_goverment <- tweets %>%
  filter(tweet_date > newgoverment)

# remove process files
rm(election, newgoverment, three_months_before_election)

# text on account aggregate level ----------------------------------------------

# create list with dataframes
dfs <- list(tweets, pre_election, post_election, electioneering_phase, new_goverment)

# append the text for tweets by one person
df_list_grouped <- lapply(dfs, function(df) {
  df %>%
    # group on account level
    group_by(party) %>%
    # append all text by one account
    summarise(text_clean = paste(text_clean, collapse = "")) %>% 
    # help column
    mutate(help = str_remove_all(text_clean, "[,\\.\\?:!-]")) %>%
    # compute the number of terms in each text
    mutate(terms = str_count(help, " ") + 1) %>%
    # Remove the temporary "help" column
    select(-help)
})

names <- c("tweets_agg", "pre_election_agg", "post_election_agg",
           "electioneering_phase_agg", "new_goverment_agg")

for(i in seq_along(df_list_grouped)) {
  assign(names[i], df_list_grouped[[i]])
}

# remove process files
rm(df_list_grouped, dfs, i, names)

# 3) sentiment scores ----------------------------------------------------------
################################################################################

# get positive and negative terms from the sentiment dictionary
positive_terms <- str_trim(sent.dictionary$feature[sent.dictionary$sentiment > 0])
negative_terms <- str_trim(sent.dictionary$feature[sent.dictionary$sentiment < 0])

# create a quanteda dictionary
sentiment_dictionary <- dictionary(list(positive = positive_terms, negative = negative_terms))

# define function
compute_sentiment_scores <- function(df, sentiment_dictionary) {
  # tokenize the text and create a document-feature matrix
  dfm_obj <- dfm(df$text_clean, dictionary = sentiment_dictionary)
  
  # compute the occurrences of positive and negative terms
  df$posterms <- rowSums(dfm_match(dfm_obj, "positive"))
  df$negterms <- rowSums(dfm_match(dfm_obj, "negative"))
  
  # calculate the raw sentiment score
  df <- df %>%
    mutate(sentiment = posterms - negterms,
           sentiment.norm = sentiment / terms)
  
  return(df)
}

# apply the function
df_names <- c("tweets_agg", "pre_election_agg", "post_election_agg",
              "electioneering_phase_agg", "new_goverment_agg")

for (df_name in df_names) {
  # Get the dataframe from the global environment
  df <- get(df_name, envir = .GlobalEnv)
  
  # Compute sentiment scores, formatt dataframe
  df <- compute_sentiment_scores(df, sentiment_dictionary) %>%
    select(-text_clean)
  # Assign the updated dataframe back to the global environment
  assign(df_name, df, envir = .GlobalEnv)
}

# remove process files
rm(df, df_name, df_names, negative_terms, positive_terms, compute_sentiment_scores)

# 3) export dataframes ---------------------------------------------------------
################################################################################

# dataframes for export
df_names <- list("tweets_agg_party" = tweets_agg, 
                 "pre_election_agg_party" = pre_election_agg, 
                 "post_election_agg_party" = post_election_agg,
                 "electioneering_phase_agg_party" = electioneering_phase_agg, 
                 "new_goverment_agg_party" = new_goverment_agg)

# loop over the list and export each dataframe
for(i in names(df_names)){
  rio::export(df_names[[i]], paste0("../02_data/02_output_data/party_sentiment/", i, ".rds"))
}

