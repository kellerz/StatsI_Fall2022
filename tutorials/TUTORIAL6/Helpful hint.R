library(tidyverse)

dat <- read.csv("movies.csv") # You need to edit this for your system

## How to select Warner films
# There is more than one Warner Bros. studio
sort(unique(dat$studio))

# The grepl() function can be used to filter character strings based on
# a word within the string.
dat %>%
  filter(grepl("Warner|WARNER", studio)) %>%
  summarise(c_score = mean(critics_score))

# We can see that none of the Warner films have won best picture
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_win == "yes") %>%
  select(title)

# But they did get two nominations...
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_nom == "yes") %>%
  select(title, thtr_rel_year)
