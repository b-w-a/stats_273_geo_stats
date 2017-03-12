

# create a dataset with voters that have no duplicates. 
library(tidyverse)
library(data.table)
library(sp)
df <- fread("/Users/bryanwilcox/Dropbox/projects/professors/collingwood/la_county_FINAL.csv", header = T, stringsAsFactors = F)
glimpse(df)

df$voted_general <- ifelse(grepl("A", df$general_2012) == T | grepl("V", df$general_2012) == T,1,0)

df$voted_primary <- ifelse(grepl("A", df$primary_2012) == T | grepl("V", df$primary_2012) == T,1,0)

df <- df %>%  dplyr::select(x = lon, y = lat, z = voted_general) 


dat <- data.frame(Longitude = df$x, Latitude = df$y, z = df$z)
head(dat)
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(shapefile)

clean_spatial <- remove.duplicates(dat)
head(clean_spatial)

final <- data.frame(clean_spatial$coords, z = clean_spatial$data)

saveRDS(clean_spatial, "/Users/bryanwilcox/Dropbox/Courses/UCLA/2017_winter/stats_273_geostats/stats_273_geo_stats/clean_data.RDS")

