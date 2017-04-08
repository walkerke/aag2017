library(tidyverse)
library(sf)
library(viridis)
library(tmap)
library(extrafont)

# Any functions up here

lq <- function(group, total) {
  num <- group / total
  denom <- sum(group) / sum(total)
  quotient <- num / denom
  x <- ifelse(total == 0, 0, quotient)
  return(x)
}


lq_map <- function(dataset, hall) {
  
  ring <- st_buffer(hall, 20000) %>%
    st_intersection(dataset) %>%
    st_union() %>%
    st_sf()
  
  dataset %>%
    tm_shape() + 
    tm_fill(col = c("lqba", "lqnoba"), palette = viridis(7), n = 7, 
            style = "quantile", title = "Location quotient") + 
    tm_facets(free.scales = FALSE) + 
    tm_layout(panel.labels = c("With degree", "Without degree")) + 
    tm_shape(ring) + 
    tm_borders(col = "black")
  
}


lq_plot <- function(dataset) {
  
  dataset %>%
    select(dist, lqba, lqnoba) %>%
    gather(key = level, value = lq, -dist, -geometry) %>%
    ggplot(aes(x = dist / 1000, y = lq, color = level)) + 
    geom_smooth(span = 0.3, method = "loess") + 
    geom_hline(yintercept = 1, color = 'black', linetype = 'dashed') + 
    theme_minimal() + 
    scale_color_brewer(palette = "Dark2", 
                       labels = c("College degree", "No degree")) + 
    labs(x = "Distance from downtown (km)", 
         y = "Concentration relative to metropolitan area", 
         caption = "Data source: NHGIS. Refers to individuals aged 25-34. The dashed line represents the concentration for the overall metropolitan area.") +
    theme(#axis.ticks.y = element_blank(), 
      legend.title = element_blank(), 
      #axis.text.y = element_blank(), 
      plot.caption = element_text(size = 6))
  
  
}

# Prepare the demographic data

total <- read_csv("data-raw/nhgis0084_csv/nhgis0084_ds215_20155_2015_tract.csv") %>%
  select(GISJOIN, total = ADKWE001)

df <- read_csv("data-raw/nhgis0083_csv/nhgis0083_ds216_20155_2015_tract.csv") %>%
  mutate(milba = AD0PE017 + AD0PE018 + AD0PE058 + AD0PE059, 
         milnoba = AD0PE012 + AD0PE013 + AD0PE014 + AD0PE015 + AD0PE016 + 
           AD0PE053 + AD0PE054 + AD0PE055 + AD0PE056 + AD0PE057) %>%
  left_join(total, by = "GISJOIN")


# Read and process the tract data

tracts <- st_read("data-raw/nhgis0077_shape/us_tract_2015.shp", 
                  stringsAsFactors = FALSE) %>%
  mutate(countyid = paste0(STATEFP, COUNTYFP))

metros <- read_rds("data/metros_only.rds")

chi_counties <- metros$st_cty_code[metros$cbsa_code == 16980]

sea_counties <- metros$st_cty_code[metros$cbsa_code == 42660]

phi_counties <- metros$st_cty_code[metros$cbsa_code == 37980]

atl_counties <- metros$st_cty_code[metros$cbsa_code == 12060]

# City hall sf objects

chi_hall <- c(-87.631969, 41.883835) %>%
  st_point() %>%
  st_sfc(crs = 4269) %>%
  st_transform(26916)


sea_hall <- c(-122.3298441, 47.6038962) %>%
  st_point() %>%
  st_sfc(crs = 4269) %>%
  st_transform(26910)

phi_hall <- c(-75.1635996, 39.953168) %>%
  st_point() %>%
  st_sfc(crs = 4269) %>%
  st_transform(26918)

atl_hall <- c(-84.3925818, 33.7488977) %>%
  st_point() %>%
  st_sfc(crs = 4269) %>%
  st_transform(26916)


# Tracts for each metro

chi_tracts <- tracts %>%
  filter(countyid %in% chi_counties) %>%
  st_transform(26916) %>%
  mutate(dist = as.numeric(
    st_distance(
      st_centroid(.), chi_hall
    )
  )) %>%
  left_join(df, by = "GISJOIN") %>%
  mutate(lqba = lq(milba, total), 
         lqnoba = lq(milnoba, total))

sea_tracts <- tracts %>% 
  filter(countyid %in% sea_counties) %>%
  st_transform(26910) %>%
  mutate(dist = as.numeric(
    st_distance(
      st_centroid(.), sea_hall
    )
  )) %>%
  left_join(df, by = "GISJOIN") %>%
  mutate(lqba = lq(milba, total), 
         lqnoba = lq(milnoba, total))

phi_tracts <- tracts %>% 
  filter(countyid %in% phi_counties) %>%
  st_transform(26918) %>%
  mutate(dist = as.numeric(
    st_distance(
      st_centroid(.), phi_hall
    )
  )) %>%
  left_join(df, by = "GISJOIN") %>%
  mutate(lqba = lq(milba, total), 
         lqnoba = lq(milnoba, total))


atl_tracts <- tracts %>% 
  filter(countyid %in% atl_counties) %>%
  st_transform(26916) %>%
  mutate(dist = as.numeric(
    st_distance(
      st_centroid(.), atl_hall
    )
  )) %>%
  left_join(df, by = "GISJOIN") %>%
  mutate(lqba = lq(milba, total), 
         lqnoba = lq(milnoba, total))

cities <- c(chi_tracts, sea_tracts, phi_tracts, atl_tracts)

ggsave("img/chicago_profile.png", lq_plot(chi_tracts), 
       dpi = 300, width = 8, height = 5.5)

ggsave("img/seattle_profile.png", lq_plot(sea_tracts), 
       dpi = 300, width = 8, height = 5.5)

ggsave("img/philly_profile.png", lq_plot(phi_tracts), 
       dpi = 300, width = 8, height = 5.5)

ggsave("img/atlanta_profile.png", lq_plot(atl_tracts), 
       dpi = 300, width = 8, height = 5.5)


save_tmap(lq_map(chi_tracts, chi_hall), "img/chicago_map.png", dpi = 300, 
          width = 8.5, height = 4.5)

save_tmap(lq_map(sea_tracts, sea_hall), "img/seattle_map.png", dpi = 300, 
          width = 8.5, height = 4.5)

save_tmap(lq_map(phi_tracts, phi_hall), "img/philly_map.png", dpi = 300, 
          width = 8.5, height = 4.5)

save_tmap(lq_map(atl_tracts, atl_hall), "img/atlanta_map.png", dpi = 300, 
          width = 8.5, height = 4.5)


# Old code

# chi_tracts %>%
#   tm_shape() + 
#   tm_fill(col = c("lqba", "lqnoba"), palette = viridis(7), n = 7, 
#           style = "quantile") + 
#   tm_facets(free.scales = FALSE)
# 
# ggplot(chi_tracts, aes(x = dist, y = lqnoba)) + 
#   geom_smooth()
# 
# ggplot(chi_tracts, aes(fill = lqba, color = lqba)) + 
#   geom_sf() + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# ggplot(chi_tracts, aes(fill = lqnoba, color = lqnoba)) + 
#   geom_sf(size = 0) + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# 
#   
# 
# 
# 
# lq_map(sea_tracts)
# 
# tm_shape(sea_tracts) + 
#   tm_fill("lqba", palette = viridis(7), n = 7, 
#           style = "quantile") + 
#   tm_legend(outside = TRUE)
# 
# ggplot(sea_tracts, aes(fill = lqba, color = lqba)) + 
#   geom_sf() + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# ggplot(sea_tracts, aes(fill = lqnoba, color = lqnoba)) + 
#   geom_sf(size = 0) + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# 
# 
# 
# ggplot(phi_tracts, aes(fill = lqba, color = lqba)) + 
#   geom_sf() + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# ggplot(phi_tracts, aes(fill = lqnoba, color = lqnoba)) + 
#   geom_sf(size = 0) + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# 
# ggplot(phi_tracts) + 
#   geom_smooth(aes(x = dist, y = lqba), color = "red") + 
#   geom_smooth(aes(x = dist, y = lqnoba), color = "blue")
# 
# 
# 
# 
# 
# 
# lq_map(atl_tracts)
# 
# lq_plot(atl_tracts)
# 
# ggplot(atl_tracts, aes(fill = lqba, color = lqba)) + 
#   geom_sf() + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# ggplot(atl_tracts, aes(fill = lqnoba, color = lqnoba)) + 
#   geom_sf(size = 0) + 
#   scale_fill_viridis() + 
#   scale_color_viridis()
# 
# 
# ggplot(atl_tracts) + 
#   geom_smooth(aes(x = dist, y = lqba), color = "red") + 
#   geom_smooth(aes(x = dist, y = lqnoba), color = "blue")
# 
# 
# 
# 
# 
# 
# 
