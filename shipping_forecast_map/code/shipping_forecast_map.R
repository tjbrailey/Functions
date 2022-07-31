######################################################################################################################################################
######

rm(list = ls())
options(scipen = 999)
gc()

library(ggplot2)
library(magrittr)

fp_main <- paste0(here::here(), "/shipping_forecast_map")
fp_data <- paste0(fp_main, "/data")
fp_exhibits <- paste0(fp_main, "/exhibits")

googlesheets4::gs4_auth(email = "thomasjbrailey@gmail.com")

######################################################################################################################################################
######

df_shp_gbr        <- sf::st_read(paste0(fp_data, "/gadm41_GBR_shp/gadm41_GBR_0.shp"))
df_shp_mar_bndry  <- sf::st_read(paste0(fp_data, "/World_EEZ_v11_20191118/eez_boundaries_v11.shp"))
df_shp_ocean      <- sf::st_read(paste0(fp_data, "/World_EEZ_v11_20191118/eez_v11.shp"))
df_shp_world      <- sf::st_read(paste0(fp_data, "/gadm_410-levels/gadm_410-levels.gpkg"))
df_forecast_bndry <- readxl::read_xlsx(paste0(fp_data, "/shipping_forecast_boundaries.xlsx"))

######################################################################################################################################################
######

sf::sf_use_s2(FALSE)

df_forecast_bndry_shp <-
  df_forecast_bndry %>%
  dplyr::mutate(
    lon = ifelse(!is.na(lon_vis), lon_vis, lon),
    lat = ifelse(!is.na(lat_vis), lat_vis, lat),
    ) %>% 
  dplyr::mutate(lon = as.numeric(lon), 
                lat = as.numeric(lat)) %>% 
  dplyr::filter(!is.na(lon)) %>% 
  dplyr::mutate(boundary = stringr::str_to_title(boundary)) %>%
  dplyr::filter(., north_atlantic_areas == "no") %>%
  sf::st_as_sf(., coords = c("lon", "lat"))

# create concave hull
split_dat <- split(df_forecast_bndry_shp, df_forecast_bndry_shp$boundary)
hulls <- lapply(split_dat, concaveman::concaveman)

df_forecast_bndry_sub <-
  dplyr::bind_rows(hulls) %>%
  dplyr::mutate(boundary = names(hulls)) %>%
  dplyr::group_by(boundary) %>% 
  dplyr::summarise() %>% 
  sf::st_cast("POLYGON") %>% 
  sf::st_buffer(., dist = 0)

df_shp_world_sub <- 
  df_shp_world %>% 
  dplyr::filter(
    COUNTRY %in% c(
      "United Kingdom", 
      "Ireland", 
      "France", 
      "Spain", 
      "Portugal", 
      "Andorra", 
      "Belgium", 
      "Netherlands", 
      "Norway", 
      "Luxembourg",
      "Denmark",
      "Iceland",
      "Germany"
      )
    )

sf::st_crs(df_forecast_bndry_sub) <- 4326
sf::st_transform(df_forecast_bndry_sub, crs = "EPSG:27700")
sf::st_transform(df_shp_world_sub, crs = "EPSG:27700")

#fr_sp <- dplyr::filter(df_shp_world_sub, COUNTRY %in% c("France", "Spain"))
#test <- df_forecast_bndry_sub 
#test <- test[]

#ggplot() +
#  geom_sf(data = test) + 
#  geom_sf(data = fr_sp)

#test2 <- sf::st_intersection(fr_sp, test) %>% 
#  dplyr::summarise()

#ggplot() + 
#  geom_sf(data = test2)

#test3 <- sf::st_difference(test, test2) %>% 
#  dplyr::summarise()

#ggplot() + 
  #geom_sf(data = test, fill = "blue", alpha = 0.5) + 
#  geom_sf(data = test3, fill = "green", alpha = 0.5) + 
#  theme_minimal()

###

######################################################################################################################################################
######

plot_map_color <- ggplot() +
  geom_sf(
    data = df_forecast_bndry_sub,
    mapping = aes(color = boundary),
    alpha = 1,
    fill = NA) +
  geom_sf(data = df_shp_world_sub, fill = "#bdd588", color = "#a0b66e") +
  geom_sf_text(
    data = dplyr::mutate(
      df_forecast_bndry_sub, 
      boundary_plot = stringr::str_replace_all(boundary, " ", "\n")),
    mapping = aes(label = unique(boundary_plot)), 
    color = "black",
    family = "LM Roman 10", 
    fontface = "bold",
    size = 3.5, 
    hjust = 0.5,
    vjust = 0, angle = 45) +
  coord_sf(
    xlim = c(-18, 6), 
    ylim = c(40, 65), 
    label_axes = list(
      top = "E", 
      left = "N")) +
  theme_minimal() + 
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        text = element_text(size = 18, family = "LM Roman 10"),
        plot.background = element_rect(fill = "#b8d5ee"))
plot_map_color

# 16.5 23.375
ggsave(plot = plot_map_color, file = paste0(fp_exhibits, "/shipping_forecast_map_color.pdf"), device = cairo_pdf,
       width = 8.3, height = 11.7)

plot_map <- ggplot() +
  geom_sf(
    data = df_forecast_bndry_sub,
    mapping = aes(color = boundary),
    alpha = 1,
    fill = NA) +
  geom_sf(data = df_shp_world_sub, fill = "grey80", color = "grey75") +
  geom_sf_text(
    data = dplyr::mutate(
      df_forecast_bndry_sub, 
      boundary_plot = stringr::str_replace_all(boundary, " ", "\n")),
    mapping = aes(label = unique(boundary_plot)), 
    color = "black",
    family = "LM Roman 10", 
    fontface = "bold",
    size = 3.5, 
    hjust = 0.5,
    vjust = 0, angle = 45) +
  coord_sf(
    xlim = c(-18, 6), 
    ylim = c(40, 65), 
    label_axes = list(
      top = "E", 
      left = "N")) +
  theme_minimal() + 
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        text = element_text(size = 18, family = "LM Roman 10"))
plot_map

# 16.5 23.375
ggsave(plot = plot_map, file = paste0(fp_exhibits, "/shipping_forecast_map.pdf"), device = cairo_pdf,
       width = 8.3, height = 11.7)

