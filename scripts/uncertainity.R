library(sf)
library(dplyr)
library(stringr)

# Read the file names
image_folder <- "inputs/mappingpictures/33Pictures/"
image_files <- list.files(image_folder)
image_names <- str_replace(image_files, "\\.[^.]+$", "")

# Read the GeoJSON files and assign the person's name
geojson_files <- list.files("inputs/mappingpictures/GeoJSON/", pattern = "\\.geojson$", full.names = TRUE)

# Extract names from file names (removing path and extension)
person_names <- str_replace(basename(geojson_files), "\\.geojson$", "")

# Read each file into a list and add "Person" column
geojson_list <- lapply(seq_along(geojson_files), function(i) {
  st_read(geojson_files[i]) %>%
    mutate(Person = person_names[i])  # Add Person column
})

# Combine all spatial points into one dataset
all_points <- bind_rows(geojson_list)

# Create a new dataset without geometry, keeping only "Name" and "Person"
all_points_nogeom <- all_points %>%
  st_drop_geometry() %>%
  select(Name, Person)

# Count how many of the image names appear in "Name"
mapped_images_count <- sum(image_names %in% all_points_nogeom$Name)
total_images <- length(image_names)
mapped_percentage <- round((mapped_images_count / total_images) * 100, 2)

# Identify images that were NOT mapped
unmapped_images <- image_names[!(image_names %in% all_points_nogeom$Name)]

# Print the summary
cat(sprintf("We mapped %d of %d images (%.2f%%)\n", mapped_images_count, total_images, mapped_percentage))

# Print the unmapped image names
if (length(unmapped_images) > 0) {
  cat("Images not mapped:\n")
  print(unmapped_images)
} else {
  cat("All images were mapped!\n")
}

# ---- New Additions ----

# Find pictures mapped by at least two people
mapped_counts <- all_points_nogeom %>%
  group_by(Name) %>%
  summarise(mappers = n()) %>%
  filter(mappers >= 2) %>%
  pull(Name)

cat("\nPictures that at least 2 people mapped:\n")
if (length(mapped_counts) > 0) {
  print(mapped_counts)
} else {
  cat("No pictures were mapped by at least two people.\n")
}

# Identify pictures that each person did NOT map but others did
missed_by_each <- list()
for (person in person_names) {
  person_mapped <- all_points_nogeom %>%
    filter(Person == person) %>%
    pull(Name)
  
  others_mapped <- all_points_nogeom %>%
    filter(Person != person) %>%
    pull(Name)
  
  missed_by_person <- setdiff(others_mapped, person_mapped)
  missed_by_each[[person]] <- missed_by_person
}

# Print the email content
cat("\nHi all,\n\n")
cat(sprintf("For the pictures shared, I noticed the following. Of the 33 images, still %d (%d%%) were not mapped. Actually, %d (%d%%) by all of us, and %d by Chifu, %d by Precious, and %d by Mastala.\n", 
            length(unmapped_images), mapped_percentage, mapped_images_count, mapped_percentage, 
            length(image_names) - length(unmapped_images) - mapped_images_count, 
            length(mapped_counts[ mapped_counts %in% missed_by_each$Chifu ]), 
            length(mapped_counts[ mapped_counts %in% missed_by_each$Precious]), 
            length(mapped_counts[ mapped_counts %in% missed_by_each$Mastala])))

cat("\nI am assuming that all the photos that you did not map, you did not have confidence to assign them location simply based on looking at visual cues from satellite imagery. Is this assumption correct? If yes, I would like to provide a statement to describe our inability to map this. I am thinking of the following:\n\n")
cat("‘Despite being seen initially as relevant for mapping, xx of the 33 image (xx %) files could not be located on the map. Figure xx (a) presents the suggested locations where the photos were captured from. Regarding which of these 21 images could be mapped varied based on the annotator, with some images being mapped by all the three annotators while some just mapped by only one. Figure xx (b) presents uncertainty of the mapped location based on inter-operator agreements. In locations where the annotators disagreed, the standard deviation was large and the circle is big compared to those where the annotators relatively pointed on a certain location. It is worth noting that images where only one person mapped, despite being mapped level of certainity regarding the mapped location could not be reasonably established.’\n\n")
cat("Again if yes above, this has implication on flood mapping based on social media data. It means that we can confidently use the centroid of those we have ‘a measure of certainty’ for location where the picture was taken for mapping flood water. While that this quantification of certainty can be extended to mapping of flood water or identification of exposed infrastructure from the images (we will not do that for now). But we have just demonstrated that it is valuable to do so.\n\n")
cat("For your reference, below this email is the breakdown of the images you mapped and those you did not map.\n\n")
cat("Best, Patrick\n\n")

cat("Descriptions, pictures that at least 2 people mapped\n")
print(mapped_counts)

cat("\nPictures that each one of us did not map but others mapped:\n")
for (person in person_names) {
  cat(sprintf("\nPerson: %s\n", person))
  print(missed_by_each[[person]])
}

#### CHIBWANA
# Count how often all persons wrote the same name (i.e., same Name for each annotator)
same_name_count <- all_points_nogeom %>%
  group_by(Name) %>%
  summarise(unique_persons = n_distinct(Person)) %>%
  filter(unique_persons == length(unique(all_points_nogeom$Person))) %>%
  nrow()

# Print the result
cat(sprintf("The number of times all persons wrote the same name: %d\n", same_name_count))



at_least_two_count <- all_points_nogeom %>%
  group_by(Name) %>%
  summarise(unique_persons = n_distinct(Person)) %>%
  filter(unique_persons >= 2) %>%
  nrow()

# Print the result
cat(sprintf("The number of times at least two persons wrote the same name: %d\n", at_least_two_count))
