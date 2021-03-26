library(magrittr)
library(ggplot2)

# Plot 1
plot1 <- 
  seq(-3, 3, by = .01) %>%
  expand.grid(x = ., y = .) %>%
  ggplot(aes(x = (1 - x - sin(y^3)), 
             y = (1 + y - cos(x^3)))) +
  geom_point(alpha=.05, shape=19, size=0)+
  theme_void()+
  coord_polar()
plot1

# Plot 2
data <- 
  seq(from = -10, to = 10, by = 0.02) %>% 
  expand.grid(x = ., y = .)


ggplot(
  data,
  aes(
    x = (x + pi * sin(y)), 
    y = (y + pi * cos(x))
  )
) + 
  geom_point(
    inherit.aes = TRUE, 
    alpha = .08, 
    shape = 20, 
    size = 0, 
    color = factor(abs(data$y))
  ) + 
  theme_void() +
  coord_polar() 
 
# Plot 3
dat <- 
  seq(from = -10, to = 10, by = 0.01) %>% 
  expand.grid(x = ., y = .)

ggplot(
  dat,
  aes(
    x = (x + abs(pi * sin(y))), 
    y = (y + pi * sin(x))
  )
) + 
  geom_point(
    alpha = .08, 
    shape = 20, 
    size = 0, 
    color = factor(abs(dat$y))) + 
  theme_void()

# Plot 4 
dat <- 
  seq(from = -10, to = 10, by = 0.025) %>% 
  expand.grid(x = ., y = .)

ggplot(dat, 
       aes(x = (x + sinh(y)), 
           y = (y + cosh(x)))) + 
  geom_point(inherit.aes = TRUE, 
             alpha = .08, 
             shape = 20, 
             size = 0, 
             color = factor(abs(dat$y))) + 
  theme_void()


### Plot 5
library(tidyverse)                                                              
library(sf)                                                                     
library(maps)  

test <-
  st_graticule(ndiscr = 10000) %>% 
  st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40.1 +ellps=WGS84 +no_defs") %>% 
  st_geometry %>% 
  plot

                                                                 

world1 <- st_as_sf(map('world', plot = FALSE, fill = TRUE)) %>%                 
  st_transform(., "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs")

ggplot() + 
  geom_point(
    data = data, 
    aes(
    x = (x + pi * sin(y)), 
    y = (y + pi * cos(x))),
    inherit.aes = TRUE, 
    alpha = .08, 
    shape = 20, 
    size = 0, 
    color = factor(abs(data$y))
  ) + geom_sf(data = world1)
  

######
my_cols <- c("tan4", "orangered4", "sienna2", "indianred4")

#extrafont::font_import(paths = c("C:/Users/tbrai/Downloads/Spectral_SC"), prompt = FALSE)

windowsFonts(sans = "Spectral SC")
extrafont::loadfonts(device = "win")
extrafont::loadfonts(device="postscript")

#extrafont::choose_font("Spectral SC")

library(extrafont) 
#font_import(prompt = FALSE)
#loadfonts(device = "win")

library(showtext)
font_add("Spectral SC", "C:/Users/tbrai/Downloads/Spectral_SC/SpectralSC-Regular.ttf")
showtext_auto()

library(Cairo)
library(magrittr)
library(ggplot2)

t <- seq(1,200, by = .001)
p1 <- 
  tibble::tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
       y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(aes(x, sinh(y), color = t), alpha = 0.1, shape = 20, size = 1) +
  #scale_color_manual(values = wesanderson::wes_palette("GrandBudapest1")) +
  scale_color_gradientn(colors = my_cols) +
  ggtitle(label = "Stedham Group Limited") + 
  coord_polar() + 
  theme_void() +
  theme(legend.position = "none",
        text = element_text(size = 60, family = "Spectral SC", color = "#D67236"),
        plot.title = element_text(vjust = -150, hjust = 0.5),
        plot.background = element_rect(fill = "transparent", color = NA))
p1
ggsave(p1, filename = paste0(here::here(), "/logo_with_text_final_medium.png"), bg = "transparent", type = "cairo")

p1_2 <- 
  tibble::tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
                 y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(aes(x, sinh(y), color = t), alpha = 0.1, shape = 20, size = 1) +
  scale_color_gradientn(colors = my_cols) +
  ggtitle(label = "Stedham Group Limited") + 
  coord_polar() + 
  theme_void() +
  theme(legend.position = "none",
        text = element_text(size = 120, family = "Spectral SC", color = "#D67236"),
        plot.title = element_text(vjust = -150, hjust = 0.5),
        plot.background = element_rect(fill = "transparent", color = NA))
ggsave(p1_2, filename = paste0(here::here(), "/logo_with_text_final_large.png"), width = 24, height = 15.42, units = "in", bg = "transparent", type = "cairo")

p1_3 <- 
  tibble::tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
                 y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(aes(x, sinh(y), color = t), alpha = 0.1, shape = 20, size = 1) +
  scale_color_gradientn(colors = my_cols) +
  ggtitle(label = "Stedham Group Limited") + 
  coord_polar() + 
  theme_void() +
  theme(legend.position = "none",
        text = element_text(size = 30, family = "Spectral SC", color = "#D67236"),
        plot.title = element_text(vjust = -150, hjust = 0.5),
        plot.background = element_rect(fill = "transparent", color = NA))
ggsave(p1_3, filename = paste0(here::here(), "/logo_with_text_final_small.png"), width = 6, height = 3.855, units = "in", bg = "transparent", type = "cairo")

p1_4 <- 
  tibble::tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
                 y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(aes(x, sinh(y), color = t), alpha = 0.1, shape = 20, size = 1) +
  scale_color_gradientn(colors = my_cols) +
  #ggtitle(label = "Stedham Group Limited") + 
  coord_polar() + 
  theme_void() +
  theme(legend.position = "none",
        text = element_text(size = 30, family = "Spectral SC", color = "#D67236"),
        plot.title = element_text(vjust = -150, hjust = 0.5),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave(p1_4, filename = paste0(here::here(), "/logo_final_medium_notext.png"), units = "in", bg = "transparent", type = "cairo")
ggsave(p1_4, filename = paste0(here::here(), "/logo_final_large_notext.png"), width = 12, height = 15.42, units = "in", bg = "transparent", type = "cairo")
ggsave(p1_4, filename = paste0(here::here(), "/logo_final_small_notext.png"), width = 6, height = 3.855, units = "in", bg = "transparent", type = "cairo")

















p2 <-
tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
       y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(aes(x, sinh(y), color = t), alpha = 0.05, shape = 20, size = 1) +
  scale_color_viridis_c() +
  coord_fixed() + 
  theme_void()
ggsave(p2, file = paste0(here::here(), "/logo2.png"))

p3 <- 
  tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
       y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x,y,color = abs(t))) +
  scale_color_viridis_c() +
  coord_polar() + 
  theme_void()
ggsave(p3, file = paste0(here::here(), "/logo3.png"))

###### 

p4 <-
  seq(-2, 2, by = .01) %>% 
  expand.grid(x=.,y=.) %>%
  ggplot(dat, 
         mapping = aes(x=(x^3-sin(y)),
             y = (y^3-cos(x)),
             color = abs(dat$y))) +
    geom_point(alpha =0.05, shape = 20, size = 0) +
  scale_color_viridis_c() +
  theme_void() +
  coord_polar()
ggsave(p4, file = paste0(here::here(), "/logo4.png"))
