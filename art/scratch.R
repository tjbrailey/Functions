library(magrittr)
library(ggplot2)

# Plot 1
seq(-3, 3, by = .01) %>%
  expand.grid(x = ., y = .) %>%
  ggplot(aes(x = (1 - x - sin(y^3)), 
             y = (1 + y - cos(x^3)))) +
  geom_point(alpha=.05, shape=19, size=0)+
  theme_void()+
  coord_polar()

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

t <- seq(1,200, by = .001)
tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
       y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(aes(x, sinh(y)), alpha = 0.05, shape = 20, size = 1, color = factor(t)) + 
  coord_cartesian() + 
  theme_void()


tibble(x = exp(-.01*t) * sin(t*3+2.4) + exp(-.01*t)*sin(t*3+2.1),
       y = exp(-.01*t) * sin(t*2.975+.9) + exp(-.01*t) * sin(t*3+.9)) %>% 
  ggplot() + 
  geom_point(aes(x,y),
             color = factor(abs(t))) + 
  coord_polar() + 
  theme_void()


###### 

seq(-2, 2, by = .01) %>% 
  expand.grid(x=.,y=.)%>% 
  ggplot(aes(x=(x^3-sin(y)),
             y = (y^3-cos(x)))) +
  geom_point(alpha =0.05, shape = 20, size = 0) +
  theme_void() +
  coord_polar()
