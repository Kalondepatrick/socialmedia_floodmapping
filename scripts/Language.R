#  Search

disaster = read.csv('inputs/collated/final_full.csv')

# How many of these were in Chichewa
library(tidyr)

# Separate rows based on commas in the `Term` column
disasterfull <- disaster %>%
  separate_rows(Term, sep = ",")

#
library(dplyr)
library(dplyr)
library(stringr)


disasterfull <- disasterfull %>%
  mutate(
    Term_normalized = str_to_lower(str_squish(Term)),  # Normalize text
    Language = case_when(
      str_detect(Term_normalized, "kusefukira.*madzi.*(dwangwa|nkhotakota)") ~ "Chichewa",
      str_detect(Term_normalized, "madzi.*osefukira.*(dwangwa|nkhotakota)") ~ "Chichewa",
      str_detect(Term_normalized, "(flooding|floods).*in.*(dwangwa|nkhotakota)|nkhotakota.*floods|dwangwa.*floods") ~ "English",
      TRUE ~ NA_character_
    )
  )



# Count the number of posts in each language
language_counts <- disasterfull %>%
  count(Language)

print(language_counts)


term_counts <- table(disaster$Term)



# Print the results
print(term_counts)



# Range of the posts


# Load necessary library
library(dplyr)

# Remove rows with NA in the Date column
disasterfull_clean <- disasterfull %>%
  filter(!is.na(Date))

# Convert the Date column to Date type
disasterfull_clean <- disasterfull_clean %>%
  mutate(Date = as.Date(Date))

# Remove X 166 as it is from 2022

disasterfull_clean <- disasterfull_clean[disasterfull_clean$X != 166, ]


# Get the minimum and maximum dates
date_range <- range(disasterfull_clean$Date, na.rm = TRUE)

# Print the date range
print(paste("Date range:", date_range[1], "to", date_range[2]))


# Convert fractional hours to hours and minutes
convert_time <- function(time_fraction) {
  hours <- floor(time_fraction * 24)
  minutes <- floor((time_fraction * 24 - hours) * 60)
  sprintf("%02d:%02d", hours, minutes)
}

# Apply the conversion to the Time_Hrs column
head(sapply(disasterfull_clean$Time_Hrs, convert_time))
