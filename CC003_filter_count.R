library(tidyverse)

github <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/comma-survey/comma-survey.csv")

github
colnames(github)

github <- github %>%
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
                              "Some experts say it's important to drink milk, but the data are inconclusive."="plural")) 
github %>% count(oxford_or_not)
github %>% count(writing_style)
github %>% count(writing_style,gender)
github %>% count(education)
github %>% filter(oxford_or_not=="oxford")
github %>% filter(oxford_or_not=="oxford" & writing_style=="plural") %>%
  count(oxford_or_not,writing_style)
github %>% filter(oxford_or_not=="oxford" | writing_style=="plural") %>%
  count(oxford_or_not,writing_style)
github %>% filter(gender == "Female" & oxford_or_not=="oxford" | writing_style== "plural") %>%
  count(oxford_or_not,writing_style,gender)
github %>% filter(oxford_or_not=="oxford" & (care_levels=="Some" | care_levels=="A lot")) %>%
  count(oxford_or_not,care_levels)
github %>% filter(known_or_unknown == "Yes") %>%
  count(care_levels)
github %>% count(location) %>% arrange(desc(n))
github %>% count(important_or_not)github %>% filter(important_or_not == "Somewhat important" | important_or_not == "Very important") 
github %>% count(important_or_not)
github %>% filter(important_or_not == "Somewhat important" | important_or_not == "Very important") %>%
  count(oxford_or_not)
github %>% count(age)
github %>% filter(age == "45-60" | age == "> 60") %>% count(oxford_or_not)

