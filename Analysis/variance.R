
## Messi H.J. Lee
# Project Milestone

## Script date: 21 Nov 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
library(reshape2)
library(car)

# Import text ------------------------------------------------------------------

text = read.csv('../Data/collected_text.csv') %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub('[[:punct:] ]+', ' ', text)) %>% 
  mutate(text = str_trim(text))

# Remove text completions that are longer than five words 
# We remove 28 completions total
filtered.text = text %>% 
  rowwise() %>% 
  mutate(length = str_count(text, '\\w+')) %>%
  filter(length < 5) %>%
  ungroup() %>%
  mutate(text = as.factor(text))

# Compare the distributions ----------------------------------------------------

all.possible.levels = unique(filtered.text$text)
filtered.text$text = factor(filtered.text$text, levels = all.possible.levels)

complete_table <- table(filtered.text$race, filtered.text$text)
prop_table <- prop.table(complete_table, margin = 1)

white_props <- prop_table['white',]
asian_props <- prop_table['asian',]
black_props <- prop_table['black',]
hispanic_props <- prop_table['hispanic',]

var.test(white_props, black_props, alternative = 'greater')
var.test(white_props, asian_props, alternative = 'greater')
var.test(white_props, hispanic_props, alternative = 'greater')

