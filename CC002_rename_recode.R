library(tidyverse)

github <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/comma-survey/comma-survey.csv")

github
colnames(github)

github %>%
  rename(respondent="RespondentID",
         oxford_or_not="In your opinion, which sentence is more gramatically correct?",
         known_or_unknown="Prior to reading about it above, had you heard of the serial (or Oxford) comma?",
         care_levels="How much, if at all, do you care about the use (or lack thereof) of the serial (or Oxford) comma in grammar?",
         writing_style="How would you write the following sentence?",
         consider_or_not="When faced with using the word \"data\", have you ever spent time considering if the word was a singular or plural noun?",
         debatecare_levels="How much, if at all, do you care about the debate over the use of the word \"data\" as a singluar or plural noun?",
         important_or_not="In your opinion, how important or unimportant is proper use of grammar?",
         gender="Gender",age="Age",household_income="Household Income",
         education="Education",location="Location (Census Region)") %>%
  #colnames() %>%
  #select(important_or_not) %>%
  mutate(oxford_or_not=recode(oxford_or_not,"It's important for a person to be honest, kind and loyal."="non_oxford",
                              "It's important for a person to be honest, kind, and loyal."="oxford"),
         writing_style=recode(writing_style,"Some experts say it's important to drink milk, but the data is inconclusive."="singular",
                              "Some experts say it's important to drink milk, but the data are inconclusive."="plural")) %>%
  count(writing_style) %>%
  mutate(percentage=100*n/sum(n))

