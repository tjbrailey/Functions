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
    color = factor(abs(dat$y))
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
