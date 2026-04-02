# Freiburg Spatial Analysis using OpenStreetMap
# Author: Sagarika Vishwanath
# Objective: Analyze spatial distribution of urban amenities
#--------------------------
#1. Load Libraries
#--------------------------
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
#--------------------------
#2. Get Freiburg Data
#--------------------------
freiburg_bb <- getbb("Freiburg im Breisgau")

osm_freiburg <- opq(bbox = freiburg_bb) %>%
  add_osm_feature(key = "amenity") %>%
  osmdata_sf()
osm_points <- osm_freiburg$osm_points
# --------------------------
# 3. Categorize amenities
# --------------------------

osm_points <- osm_points %>%
  mutate(category = case_when(
    amenity %in% c("restaurant", "cafe", "fast_food") ~ "Food",
    amenity %in% c("hospital", "clinic", "pharmacy") ~ "Health",
    amenity %in% c("school", "university", "kindergarten") ~ "Education",
    TRUE ~ "Other"
  ))
#---------------------------
#4. Summary Statistics
#---------------------------
    table(osm_points$category)
#---------------------------
#5. Map visualization
#---------------------------
    ggplot() +
      geom_sf(data = osm_points, aes(color = category), size = 1) +
      theme_minimal() +
      ggtitle("Amenities in Freiburg by Category")
ggsave("amenities_map.png")
#---------------------------
#6. Distance to City Centre
#---------------------------
    city_center <- st_sfc(st_point(c(7.85, 47.99)), crs = 4326)
    
    osm_points$distance_to_center <- st_distance(osm_points, city_center)
    
    ggplot(osm_points, aes(x = as.numeric(distance_to_center)/1000)) +
      geom_histogram(bins = 30, fill = "steelblue") +
      theme_minimal() +
      xlab("Distance (km)") +
      ggtitle("Distance to City Center")
    ggsave("distance_histogram.png")
#---------------------------
#7. Identifying underserved areas (Spatial Grid Analysis)
#---------------------------
    # Create grid over Freiburg
    grid <- st_make_grid(osm_points, cellsize = 0.01)
    
    # Convert to spatial dataframe
    grid_sf <- st_sf(geometry = grid)
    library(dplyr)
    
    grid_counts <- st_join(grid_sf, osm_points) %>%
      group_by(geometry) %>%
      summarise(count = n())
    grid_counts$count[is.na(grid_counts$count)] <- 0
    # Define underserved threshold (you can change this)
    underserved <- grid_counts %>%
      filter(count < 5)
    library(ggplot2)
    
    ggplot() +
      geom_sf(data = grid_counts, aes(fill = count), color = NA) +
      scale_fill_viridis_c(option = "plasma") +
      theme_minimal() +
      ggtitle("Amenity Density in Freiburg")
    ggplot() +
      geom_sf(data = grid_counts, fill = "grey90") +
      geom_sf(data = underserved, fill = "red", alpha = 0.6) +
      theme_minimal() +
      ggtitle("Underserved Areas in Freiburg (Low Amenity Access)")
    ggsave("freiburg_amenity_heatmap.png")
    ggsave("underserved_areas.png")