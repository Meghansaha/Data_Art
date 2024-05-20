#==============================================================================#
# Library Load-In---------------------------------------------------------------
#==============================================================================#
library(tidyverse) # For everything data

# Setting parameters to prep ggplot to plot data in a "circular" fashion...
# on the Cartesian coordinate system====
## Number of divisions/rows in the data wanted----------------------------------
n <- 500

## Angle "slices"/ Sine/Cosine Frequency----------------------------------------
theta <- seq(0, 40*pi, length = n/5) 

## "Radial" setting of the "circle" to create "n" different marks---------------
r <- 1:n

## Setting up the custom color palette------------------------------------------
vec_colors <- 
  c(
    "#af3918", "#a21152", "#822b75","#612884","#154baf",
    "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a"
  )

vec_colors <- rep(vec_colors, each = n/10)

## Placing everything into a dataset====
df_data <-
  tibble(
    x = cos(theta) * r,
    y = sin(theta) * r
    )

# Pulling it all together====
df_data |>
  ggplot(aes(x = x, y = y))+
  geom_path(color = vec_colors, linewidth = 1)+
  theme_void()+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "black")
  ) +
  coord_equal()

