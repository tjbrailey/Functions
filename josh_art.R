
library(magrittr)
library(ggplot2)

rm(list = ls())

seq(-1, 1, by = .01) %>%
  expand.grid(x = ., y = .) %>% 
  dplyr::mutate(
    x = 1 - x^2 - sin(x),
    y = cos(y)^2 - tan(y)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = x, y = y, color = cos(y))) +
  scale_color_viridis_c() +
  theme_minimal() + 
  theme(legend.position = "none")
