# Load required libraries
library(sf)
library(tmap)
library(dplyr)

# Read the GeoJSON files
patrick <- st_read("inputs/mappingpictures/GeoJSON/patrickkalonde.geojson")
precious <- st_read("inputs/mappingpictures/GeoJSON/preciousmastala.geojson")
chifuniro <- st_read("inputs/mappingpictures/GeoJSON/chifunirobaluwa.geojson")

# Combine all spatial points into one dataset
all_points <- bind_rows(patrick, precious, chifuniro)

# Ensure "Name" is a factor or character
all_points$Name <- as.character(all_points$Name)

# Compute centroids for each Name
centroids <- all_points %>%
  group_by(Name) %>%
  summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop") %>%
  st_as_sf()

# Compute standard deviation and count contributors
sd_table <- all_points %>%
  group_by(Name) %>%
  summarise(
    num_contributors = n(),  # Count number of people who mapped the picture
    sd_x = sd(st_coordinates(geometry)[,1]),
    sd_y = sd(st_coordinates(geometry)[,2]),
    sd_total = sqrt(sd_x^2 + sd_y^2),  # Combined SD
    .groups = "drop"
  )

# Join SD values and contributor count to centroids
centroids <- centroids %>%
  left_join(st_drop_geometry(sd_table), by = "Name")

# Interactive Map: Points & Centroids (Bubbles)
tmap_mode("view")  # Enable interactive mode
tm_shape(all_points) + 
  tm_dots(col = "blue", size = 0.1) +  # Original points
  tm_shape(centroids) + 
  tm_bubbles(size = "sd_total", col = "red", 
             popup.vars = c("Name", "num_contributors", "sd_x", "sd_y", "sd_total"))  # Centroids as bubbles
