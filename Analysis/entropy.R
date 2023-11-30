
## Messi H.J. Lee
# Project Milestone

## Script date: 27 Nov 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}


# Import classified text -------------------------------------------------------

jobs = read.csv('../Zero-Shot Classification/classified_jobs_21.csv') %>%
  mutate(text = factor(text, levels = unique(text))) %>%
  mutate(industry = factor(industry, levels = unique(industry)))

male = jobs %>% filter(gender == 'M')
female = jobs %>% filter(gender == 'F')

# Define function to estimate 95% CI of entropy measure (text column) ----------

boot_text <- function(data, R, conf.level = 0.05){
  
  entropy_list = numeric(R)
  
  for (i in c(1:R)){
    sampled_data <- sample_n(data, nrow(data), replace = TRUE)
    text_table <- table(sampled_data$text)
    text_prop <- prop.table(text_table)
    
    # Calculate entropy measure using the proportions
    small_value = .Machine$double.eps # Use some epsilon value to address categories with 0 value 
    entropy = -sum((text_prop + small_value) * log(text_prop + small_value))
    entropy_list[i] = entropy
  }
  
  lower.bound = quantile(entropy_list, conf.level/2)
  upper.bound = quantile(entropy_list, 1 - conf.level/2)
  estimate = quantile(entropy_list, 0.50)
  
  print(c(lower.bound, estimate, upper.bound))
  return(c(lower.bound, estimate, upper.bound))
  
}

# Calculate 95% CI of the four groups (text) -----------------------------------

male.text.ci = boot_text(male, 1000)
female.text.ci = boot_text(female, 1000)

# Compare distributions of the industry column ---------------------------------

boot_industry <- function(data, R, conf.level = 0.05){
  
  entropy_list = numeric(R)
  
  for (i in c(1:R)){
    sampled_data <- sample_n(data, nrow(data), replace = TRUE)
    industry_table <- table(sampled_data$industry)
    industry_prop <- prop.table(industry_table)
    
    # Calculate entropy measure using the proportions
    small_value = .Machine$double.eps # Use some epsilon value to address categories with 0 value 
    entropy = -sum((industry_prop + small_value) * log(industry_prop + small_value))
    entropy_list[i] = entropy
  }
  
  lower.bound = quantile(entropy_list, conf.level/2)
  upper.bound = quantile(entropy_list, 1 - conf.level/2)
  estimate = quantile(entropy_list, 0.50)
  
  print(c(lower.bound, estimate, upper.bound))
  return(c(lower.bound, estimate, upper.bound))
  
}

# Calculate 95% CI of the four groups (industry) -------------------------------

male.industry.ci = boot_industry(male, 1000)
female.industry.ci = boot_industry(female, 1000)

# Visualize the distributions --------------------------------------------------

proportions_df <- jobs %>%
  group_by(gender, industry) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count), proportion = count / total)%>%
  complete(industry, gender, fill = list(proportion = 0))

ggplot(proportions_df, aes(x = industry, y = proportion, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Proportions Across Industries by Gender",
       x = "Industry",
       y = "Proportion",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_aaas()

ggsave("../Figures/industry_proportions_by_gender.png", 
       width = 8, height = 8, dpi = "retina", bg = "white")

