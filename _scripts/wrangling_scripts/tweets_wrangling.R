# clean text and datawrangling
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(quanteda)
library(rio)
library(tm)
library(scales)

# data import ------------------------------------------------------------------
tweets <- import("/Users/danielkuhlen/Desktop/uni/ba/02_data/02_output_data/tweets_tidy.rds")

# import & datawrangling metadata
candidates2021_raw <- import("/Users/danielkuhlen/Desktop/uni/ba/02_data/01_raw_data/twitter2021.rds")
institutions2021_raw <- import("/Users/danielkuhlen/Desktop/uni/ba/02_data/01_raw_data/epin2021.RDs")

# negation dictionary
negation.dictionary <- import("../02_data/01_raw_data/dictionary/1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata")
# datawrangling ----------------------------------------------------------------

# datawrangling tweets
tweets <- tweets %>%
  select(tweet_id, user_username, text, lang, created_at,
         user_verified, user_location, user_created_at, user_url,
         retweet_count, like_count, quote_count,
         user_tweet_count, user_list_count, user_followers_count,
         user_following_count) %>%
  rename(language = lang,
         tweet_date = created_at,
         twitter_handle = user_username) %>%
  mutate(twitter_handle = tolower(twitter_handle)) %>%
  filter(language == "de")

# datawrangling candidates
candidates2021 <- candidates2021_raw %>%
  mutate(name = paste(firstname, lastname),
         gender = recode(gender,
                         "w" = "female",
                         "m" = "male",
                         "d" = "diverse"),
         party = recode(party,
                        "DIE LINKE" = "LINKE")) %>%
  rename(twitter_handle = screen_name1,
         listed_candidate = isListed,
         direct_candidate = isDC,
         region = state) %>%
  filter(!is.na(twitter_handle)) %>% 
  select(name, twitter_handle, gender, party, district_name, district_number,
         region, incumbent, listed_candidate, direct_candidate)

# datawrangling institutions
institutions2021 <- institutions2021_raw %>%
  ungroup() %>% 
  rename(name = official_name) %>%
  mutate(district_name = NA,
         district_number = NA,
         listed_candidate = NA,
         direct_candidate = NA,
         region = case_when(
           region == "Brandenburg" ~ "BB",
           region == "Schleswig-Holstein" ~ "SH",
           region == "Saarland" ~ "SL",
           region == "Hamburg" ~ "HH",
           region == "Baden-Württemberg" ~ "BW",
           region == "Bavaria" ~ "BY",
           region == "Berlin" ~ "BE",
           region == "Bremen" ~ "HB",
           region == "Hesse" ~ "HE",
           region == "Mecklenburg-West Pomerania" ~ "MV",
           region == "Lower Saxony" ~ "NI",
           region == "North Rhine-Westphalia" ~ "NW",
           region == "Rhineland-Palatinate" ~ "RP",
           region == "Saxony" ~ "SN",
           region == "Saxony-Anhalt" ~ "ST",
           region == "Thuringia" ~ "TH",
           TRUE ~ region),
         party = recode(party,
                        "DIE LINKE" = "LINKE",
                        "Bündnis 90/Die Grünen" = "GRÜNE"),
         incumbent = case_when(
           party == "CDU" ~ 1,
           party == "CSU" ~ 1,
           party == "SPD" ~ 1,
           TRUE ~ 0),
         twitter_handle = tolower(twitter_handle)) %>%
  filter(office == "Parliamentary Party Group") %>%
  select(name, twitter_handle, gender, party, district_name, district_number,
         region, incumbent, listed_candidate, direct_candidate) %>% 
  ungroup()

# rbind into one dataframe
pol_twitter_accounts <- rbind(candidates2021, institutions2021) %>%
  distinct(twitter_handle, .keep_all = TRUE)


# join information which institution and office were candidates part of?
institution_join <- institutions2021_raw %>%
  ungroup() %>% 
  filter((is.na(until) | until >= "2017-10-24") &
           !(office == "Speaker" |
               office == "Parliamentary Party Group" |
               office == "Ministry")) %>%
  mutate(level = case_when(
    institution %in% c("Federal Parliament", "Federal Government") ~ "federal",
    institution %in% c("State Parliament", "State Government") ~ "state",
    institution == "European Parliament" ~ "european", TRUE ~ NA_character_)) %>%
  mutate(binary_federal_parliamentarian = if_else(level == "federal" & office == "Parliamentarian", 1, 0),
         binary_state_parliamentarian = if_else(level == "state" & office == "Parliamentarian", 1, 0),
         binary_european_parliamentarian = if_else(level == "european" & office == "Parliamentarian", 1, 0),
         binary_federal_state_secretary = if_else(level == "federal" & office == "State Secretary", 1, 0),
         binary_state_state_secretary = if_else(level == "state" & office == "State Secretary", 1, 0),
         binary_federal_minister = if_else(level == "federal" & office == "Minister", 1, 0),
         binary_state_minister = if_else(level == "state" & office == "Minister", 1, 0)) %>%
  group_by(twitter_handle) %>%
  summarise(across(starts_with("binary_"), max, na.rm = TRUE)) %>% 
  ungroup()

# left join
pol_twitter_accounts <- pol_twitter_accounts %>%
  left_join(institution_join, by = "twitter_handle")

# when binary variables NA -> 0
pol_twitter_accounts <- pol_twitter_accounts %>%
  mutate(across(starts_with("binary_"), ~if_else(is.na(.), 0, .), .names = "{col}"))

# remove process files
rm(candidates2021, candidates2021_raw, institutions2021, institutions2021_raw, institution_join)

# leftjoin metadata to tweets 

tweets_master <- left_join(tweets, pol_twitter_accounts, by = "twitter_handle")

# textual formatting -----------------------------------------------------------

# text cleaning
tweets_master <- tweets_master %>%
  mutate(text_clean = str_replace_all(text, "#", "")) %>% # remove hashtags
  mutate(text_clean = gsub("\\bRT\\b", "", text_clean, ignore.case = TRUE)) %>% # remove "rt"
  mutate(text_clean = gsub("@\\w+", "", text_clean)) %>% # remove mentions
  mutate(text_clean = gsub("https?://.+", "", text_clean)) %>% # remove urls
  mutate(text_clean = gsub("\\d+\\w*\\d*", "", text_clean)) %>% # remove numbers
  mutate(text_clean = gsub("[^[:alpha:][:space:][:punct:]]*", "", text_clean)) %>% # remove emojis
  mutate(text_clean = gsub("[[:punct:]]", " ", text_clean)) %>% # replace punctuation with spaces
  mutate(text_clean = gsub("\n", " ", text_clean)) %>% # remove newlines
  mutate(text_clean = gsub("^\\s+|\\s+$", "", text_clean)) %>% # remove leading and trailing spaces
  mutate(text_clean = gsub("[ |\t]+", " ", text_clean)) %>% # replace multiple spaces or tabs with a single space
  mutate(text_clean = tolower(text_clean))

# # replace negations correctly so they are catched by the dictionary later
# start_time <- Sys.time()
# for(i in 1:nrow(negation.dictionary)) {
#   tweets_master$text_clean <- str_replace_all(tweets_master$text_clean,
#                                      negation.dictionary$pattern[i],
#                                      negation.dictionary$replacement[i])
# }
# end_time <- Sys.time()
# end_time-start_time

start_time <- Sys.time()
# Convert dictionary columns to vectors
patterns <- negation.dictionary$pattern
replacements <- negation.dictionary$replacement

# Vectorized string replace
tweets_master$text_clean <- stringi::stri_replace_all_regex(tweets_master$text_clean, 
                                                            patterns, 
                                                            replacements, 
                                                            vectorize_all = FALSE)
end_time <- Sys.time()
end_time-start_time

# date formatting --------------------------------------------------------------

# format date correctly
tweets_master <- tweets_master %>%
  mutate(tweet_date = ymd_hms(tweet_date))

# data export ------------------------------------------------------------------

# Columns in correct order
tweets_master <- tweets_master %>%  
  select(
    tweet_id,
    twitter_handle,
    text,
    text_clean,
    tweet_date,
    retweet_count,
    like_count,
    quote_count,
    name,
    gender,
    party,
    district_name,
    district_number,
    region,
    incumbent,
    listed_candidate,
    direct_candidate,
    binary_federal_parliamentarian,
    binary_state_parliamentarian,
    binary_european_parliamentarian,
    binary_federal_state_secretary,
    binary_state_state_secretary,
    binary_federal_minister,
    binary_state_minister,
    user_verified,
    user_location,
    user_created_at,
    user_url,
    user_tweet_count,
    user_list_count,
    user_followers_count,
    user_following_count)

# remove process files
rm(tweets)

# export files
export(tweets_master, "../02_data/02_output_data/tweets_master.rds")
export(tweets_master, "../02_data/02_output_data/tweets_master.xlsx")
export(pol_twitter_accounts, "../02_data/02_output_data/pol_twitter_accounts.rds")