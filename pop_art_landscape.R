library(artpack)
library(tidyverse)

x_lim = c(0,150)
y_lim = c(0,175)

set.seed(23)

df_river <-
  wave_data(
    start = 0,
    end = 100,
    size = 20,
    freq = 2,
    orientation = "vertical",
    amplify = 10,
    type = "sin"
  ) |>
  mutate(
    curve_intensity = (y / max(y))^1.5 * (1 - (y / max(y))^3),
    # Very gentle random walk
    random_walk = cumsum(rnorm(n(), mean = 0, sd = .9)),  # Much smaller steps
    random_walk = random_walk - mean(random_walk),
    # Apply smoothing
    random_walk = zoo::rollmean(random_walk, k = 20, fill = "extend"),
    x = (x - mean(x)) * (1 + .5 * curve_intensity) + mean(x) + random_walk,
    x = x + 20
  )

df_lake <-
  tibble(
    x = c(
      seq(27, 96, l = 30),
      seq(96, 0, l = 30),
      seq(0, 27, l = 30)
    ),
    y = c(
      seq(97, 115, l = 30),
      seq(115, 115, l = 30),
      seq(115, 90, l = 30)
    )
  ) |>
  mutate(
    # Add gentle random walk similar to the river
    random_walk_x = cumsum(rnorm(n(), mean = 0, sd = 0.9)),
    random_walk_x = random_walk_x - mean(random_walk_x),
    random_walk_x = zoo::rollmean(random_walk_x, k = 5, fill = "extend"),
    
    random_walk_y = cumsum(rnorm(n(), mean = 0, sd = 0.9)),
    random_walk_y = random_walk_y - mean(random_walk_y),
    random_walk_y = zoo::rollmean(random_walk_y, k = 5, fill = "extend"),
    
    # Apply the randomness
    x = x + random_walk_x,
    y = y + random_walk_y
  )

df_grass <-

set.seed(23)

ggplot() +
  geom_polygon(data = df_river, aes(x + 20,y), fill = "#000000", color = "#000000", linewidth = 2) +
  geom_polygon(data = df_lake, aes(x + 20, y), fill = "#000000", color = "#000000", linewidth = 2) +
  geom_polygon(data = df_lake, aes(x/2 + 30, y = y - 3), fill = "lightblue") +
  geom_polygon(data = df_river, aes((x / 2) + 33, y), fill = "lightblue") +
  coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE)