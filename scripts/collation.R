#-------------------------------------------------------------------------------#
#       Preparing Social Media Data for Flood Mapping                           #
#-------------------------------------------------------------------------------#

#----   Loading necessary packages----
library(readxl)
library(here)
library(tidyverse)
library(dplyr)


#read_data collated from social media----

kk_data <- read_excel("inputs/collated/kk_data.xlsx", 
                      col_types = c("numeric", "text", "text", 
                                    "text", "text", "text", "date", "text", 
                                    "text"))

#Fix time column

# Unique Social Media Posts----


unique_kk_data <- kk_data %>%
  group_by(Post_Text) %>%
  summarize(
    Term = toString(Term),
    Post_Link = first(Post_Link),
    Village = first(Village),
    TA = first(TA),
    Date = first(Date),
    Time_Hrs = first(Time_Hrs),
    `Other Information` = first(`Other Information`),
    .groups = "drop"
  )


#There are 167 unique posts 
# Some posts, we lost url's. Make the posts with URL#s to be included in the final list

#check posts with no URLs

sum(is.na(unique_kk_data$Post_Link))
#86 posts without URL's

# Of the search terms, which ones were helpful----
summary(as.factor(unique_kk_data$Term))
summary(as.factor(kk_data$Term))

# Write a CSV for the datasets

write.csv(unique_kk_data,'inputs/collated/final_full.csv')











