#-------------------------------------------------------------------------------#
#       Preparing Social Media Data for Flood Mapping                           #
#-------------------------------------------------------------------------------#

#----   Loading necessary packages----
library(readxl)
library(here)
library(tidyverse)
#read_data collated from social media----
kk_data <- read_excel("inputs/collated/kk_data.xlsx")



# Get unique non-missing values of Post_Link

length(unique(kk_data$Post_Text))

unique_Post_Text <- unique(kk_data$Post_Text)
subset_data <- subset(kk_data, Post_Text %in% unique_Post_Text)




unique_Post_Link <- unique(kk_data$Post_Link[!is.na(kk_data$Post_Link)])

# Subset the data frame kk_data based on unique Post_Link values
unique_url <- kk_data[kk_data$Post_Link %in% unique_Post_Link, ]

unique_url <- subset(kk_data, Post_Link %in% unique_Post_Link)



length(duplicated(kk_data$Post_Link[!is.na(kk_data$Post_Link)]))



#check posts with no URLs

sum(is.na(kk_data$Post_Link))













































































########################
drr_stages <- c("warning", "disaster", "response", "relief", "recovery")
random_stages <- sample(drr_stages, nrow(unique_data), replace = TRUE)

# Now, add this vector as a new column to your dataframe
unique_data$drr_stage <- random_stages

#-----------------------------------------------------------------------------------------------#

# First, let's create a vector of random drr_stage values
drr_stages <- c("warning", "disaster", "response", "relief", "recovery")
random_stages <- sample(drr_stages, nrow(unique_data), replace = TRUE)

# Now, add this vector as a new column to your dataframe
unique_data$drr_stage <- random_stages

# Print the first few rows to verify
head(unique_data)

# Plot number of Tweets with time for each drr_stage

# Summarize number of rows based on date and drr_stage
summary_data <- aggregate(SN ~ Date + drr_stage, data = unique_data, FUN = length)

# Rename the columns
colnames(summary_data) <- c("Date", "DRR_Stage", "Posts")

# Display the summary dataframe
print(summary_data)

# Plot using ggplot with line plot
library(ggplot2)

# Set theme options for publication quality plot
theme_set(theme_minimal(base_size = 14) +
            theme(plot.title = element_text(size = 16, face = "bold"),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12)))

# Plot highlighting Temporal trends of social media posts related to the floods for each DRR stage
ggplot(summary_data, aes(x = Date, y = Posts, color = DRR_Stage, group = DRR_Stage)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(x = "Date", y = "Number of Posts", color = "DRR Stage") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position = "right")


#-----------------------------------------------------------------------------------------------#




#-----------------------------------------------------------------------------------------------#


# Print the first few rows to verify
head(unique_data)
########################


#Plot number of Tweets with time

# Summarize number of rows based on date
summary_data <- aggregate(SN ~ Date, data = unique_data, FUN = length)

# Rename the columns
colnames(summary_data) <- c("Date", "Posts")

# Display the summary dataframe
print(summary_data)

# Plot using ggplot with line plot
library(ggplot2)

####

# Set theme options for publication quality plot
theme_set(theme_minimal(base_size = 14) +
            theme(plot.title = element_text(size = 16, face = "bold"),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12)))

# Plot highlighting Temporal trends of social media posts related to the floods
ggplot(summary_data, aes(x = Date, y = Posts, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(x = "Date", y = "Number of Posts") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position = "none")


# Across this time-space, these posts can be disgregated based on the Disaster Management Cycle (https://un-spider.org/risks-and-disasters ) 
# We need to highlight number of posts on preparedness, disaster as it happens, response, rehabilitation and recovery, potentially up to prevention and mitigation.
#Some are about the actual flooding as it happened. Highlight number of posts related to each of these


#Few posts that remain, go on social media to download the graphics ()

library(rvest)

# Function to download images from URL
download_images <- function(url) {
  # Read HTML content from URL
  html <- read_html(url)
  
  # Extract image URLs from HTML content
  image_urls <- html %>%
    html_nodes("img") %>%
    html_attr("src")
  
  # Download images
  for (img_url in image_urls) {
    # Construct absolute URL if necessary
    img_url <- ifelse(startsWith(img_url, "http"), img_url, paste0(url, img_url))
    
    # Download image
    file_name <- basename(img_url)
    download.file(img_url, destfile = file_name, mode = "wb")
    cat("Downloaded:", file_name, "\n")
  }
}

# Loop through each URL and download images
for (url in urls) {
  download_images(url)
}


#Group images of the same location and report number of those with no location

#Group posts based on the circle of disaster cycle
#Identify the ebuildings using OSM data
#Highlight exposed flood extent using such images
#Discuss questions that should be targeted in future studies