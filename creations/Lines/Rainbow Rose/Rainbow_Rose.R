# Library Load-In====
library(tidyverse) # For everything data

# Setting parameters to prep ggplot to plot data in a "circular" fashion on the Cartesian coordinate system====

## Angle "slices"/ Sine/Cosine Frequency====
theta <- seq(0, 40*pi, length = 100) 

## Number of divisions/rows in the data wanted====
n <- 500

## "Radial" setting of the "circle" to create "n" different marks====
r = 1:500

## Setting up the custom color palette====
colors <- c("#af3918", "#a21152", "#822b75","#612884","#154baf",
            "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a")

## Placing everything into a dataset====
data <- tibble(x = cos(theta)*r,
               y = sin(theta)*r)

# Pulling it all together====
Rainbow_Rose <- data %>%
  ggplot(aes(x = x, y = y, color = color))+
  geom_path(color = rep(colors, each = n/10), size = 1)+
  theme_void()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))

# View it #
Rainbow_Rose

