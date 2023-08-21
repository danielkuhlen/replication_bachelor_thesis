# combine dictionaries
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)

# import dictionaries
sent.dictionary <- import("../02_data/01_raw_data/dictionary/1_Dictionaries/Rauh_SentDictionaryGerman.Rdata")
negation.dictionary <- import("../02_data/01_raw_data/dictionary/1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata")

# rbind dictionaries
sent.dictionary.complete <- rbind(sent.dictionary, 
                         negation.dictionary[c('feature','sentiment')])

# export -----------------------------------------------------------------------

export(sent.dictionary.complete, "../02_data/01_raw_data/dictionary/1_Dictionaries/sent.dictionary.complete.rds")
