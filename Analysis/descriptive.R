
## Messi H.J. Lee, Patrick Farley, Sangwook Suh
# Project Milestone

## Script date: 19 Nov 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}

# Import text ------------------------------------------------------------------

text = read.csv('../Data/collected_text.csv') %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_trim(text)) %>%
  mutate(text = gsub('[[:punct:] ]+', ' ', text))

# Remove text completions that are longer than five words 
# We remove 28 completions total
filtered.text = text %>% 
  rowwise() %>% 
  mutate(length = str_count(text, '\\w+')) %>%
  filter(length < 5) %>%
  ungroup() %>%
  mutate(text = as.factor(text))

# Subset the text by group -----------------------------------------------------

black.text <- filtered.text %>% filter(race == 'black')
asian.text <- filtered.text %>% filter(race == 'asian')
hispanic.text <- filtered.text %>% filter(race == 'hispanic')
white.text <- filtered.text %>% filter(race == 'white')

male.text <- filtered.text %>% filter(gender == 'M')
female.text <- filtered.text %>% filter(gender == 'F')

# Compare number of unique jobs ------------------------------------------------

unique.boot <- function(R, df, alpha = 0.05){
  set.seed(1048596)
  results = NULL
  for (i in 1:R) {
    sample_indices <- sample(1:nrow(df), replace = TRUE)
    sampled_data <- df[sample_indices, ]
    results <- c(results, sampled_data %>% pull(text) %>% unique() %>% length())
  }
  lower_bounds <- quantile(results, probs = alpha/2)
  upper_bounds <- quantile(results, probs = 1 - alpha/2)
  median <- quantile(results, probs = 0.50)
  return(c(lower_bounds, median, upper_bounds))
}

# Calculate the percentile confidence intervals by racial/ethnic group
(black.unique <- unique.boot(1000, black.text))
(asian.unique <- unique.boot(1000, asian.text))
(hispanic.unique <- unique.boot(1000, hispanic.text))
(white.unique <- unique.boot(1000, white.text))

# Calculate the percentile confidence intervals by gender group
(male.unique <- unique.boot(1000, male.text))
(female.unique <- unique.boot(1000, female.text))

# Top proportions by race ------------------------------------------------------

frequency.by.race <- filtered.text %>% group_by(race, text) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(race) %>%
  mutate(probability = count / sum(count))

n = 5  # Replace 5 with the number of top topics you want

top.frequency.by.race <- frequency.by.race %>%
  group_by(race) %>%
  arrange(desc(probability)) %>%
  slice_head(n = n)

top.frequency.by.race %>%
  group_by(race) %>%
  summarise(sum_probability = sum(probability))

# Top proportions by gender ----------------------------------------------------

frequency.by.gender <- filtered.text %>% group_by(gender, text) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(gender) %>%
  mutate(probability = count / sum(count))

n = 5  # Replace 5 with the number of top topics you want

top.frequency.by.gender <- frequency.by.gender %>%
  group_by(gender) %>%
  arrange(desc(probability)) %>%
  slice_head(n = n)

top.frequency.by.gender %>%
  group_by(gender) %>%
  summarise(sum_probability = sum(probability))
