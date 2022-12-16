# Library Load-In====
library(tidyverse) # For everything data

# Setting parameters to prep ggplot to plot data in a "circular" fashion on the Cartesian coordinate system====
## Number of divisions/rows in the data wanted====
n <- 10000

## Angle "slices"/ Sine/Cosine Frequency====
theta <- seq(sample(-100:100,1), sample(-100:100,1)*pi, length = n) 



## "Radial" setting of the "circle" to create "n" different marks====
r = 1:n

## Setting up the custom color palette====
colors <- c("#af3918", "#a21152", "#822b75","#612884","#154baf",
            "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a")

colors <- rev(rep(colors, each = n/10))

border_colors <- rev(map_chr(colors, ~colorRampPalette(c(.x, "#000000"))(10)[7]))

## Placing everything into a dataset====
data <- tibble(x = cos(theta)*r,
               y = sin(theta)*r)

# Pulling it all together====
Rainbow_Rose <- data %>%
  ggplot(aes(x = x, y = y, color = color))+
  geom_point(shape = 21, color = "#000000", size = sample(seq(.1,15, length.out = 20), n, replace = TRUE),
             stroke = .1,
             fill = colors,
             position = position_jitter(width = 5, height = 6)
             )+
  theme_void()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))+
  coord_equal()

# View it #
Rainbow_Rose
