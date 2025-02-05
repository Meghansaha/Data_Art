#===============================================================================
# Library Load-in---------------------------------------------------------------
#===============================================================================
library(tidyverse)
library(artpack)
library(sp)

# Reproducible!#
set.seed(0205)

#===============================================================================
# Some Arbitrary Options--------------------------------------------------------
#===============================================================================
## Sizing-----------------------------------------------------------------------
x_lim <- c(0,200)
y_lim <- c(0,300)
creek_width <- 125
creek_side_points <- 100

## Colors-----------------------------------------------------------------------
sky_pal <-
  c(art_pals("sunnyside", n = 5), art_pals("cosmos", n = 5)) |>
  sort(decreasing = TRUE)

sky_underlay_pal <-
  colorRampPalette(c("#FE9C7F", "#FFC1A0", "#F09F9C", "#C86B98", "#632B6C", "#280F36") |> sort())(100)

water_pal <- colorRampPalette(c("#427D9D", "#227B94", "#1679AB", "#16325B", "#0F2167"))(20)

grass_pal <- art_pals("plants", n = 100)

texture_pal <- colorRampPalette(c("#666666", "#ffffff"))(20) |> sample()

#===============================================================================
# Data Creation-----------------------------------------------------------------
#===============================================================================
## Creek Outline Data-----------------------------------------------------------
theta <- seq(0,pi/2, l = creek_side_points)

### Left Side-------------------------------------------------------------------
df_creek_left <-
  tibble(
    x = seq(x_lim[2]/2, 0, l = creek_side_points),
    y = seq(y_lim[1], y_lim[2]/2, l = creek_side_points)
  ) |>
  mutate(
    x = ((cos(theta) * x_lim[2]/2) + sin(theta) / 2) - cos(x/3),
    y =  (y * sin(theta) + cos(theta)/.3)
  )

vec_creek_left_last <-
  df_creek_left |>
  tail(1)

vec_creek_left_first <-
  df_creek_left |>
  head(1)

### Top Side--------------------------------------------------------------------
df_creek_top <-
  tibble(
    x = seq(vec_creek_left_last$x, creek_width, l = creek_side_points),
    y = rep(vec_creek_left_last$y, creek_side_points) |> jitter(factor = .2)
  )

### Right Side------------------------------------------------------------------
df_creek_right <-
  df_creek_left |>
  arrange(desc(y)) |>
  mutate(x = x + creek_width)

vec_creek_right_last <-
  df_creek_right |>
  tail(1)

### Bottom Side-----------------------------------------------------------------
df_creek_bottom <-
  tibble(
    x = seq(vec_creek_right_last$x, vec_creek_left_first$x, l = creek_side_points),
    y = seq(vec_creek_right_last$y, vec_creek_left_first$y, l = creek_side_points)
  )

## Final Outline Data-----------------------------------------------------------
df_creek <-
  bind_rows(
    df_creek_left,
    df_creek_top,
    df_creek_right,
    df_creek_bottom
  ) |>
  mutate(
    group = "creek",
    y = y - 5
  )

## Water Pattern Data-----------------------------------------------------------
# Number of assets to start with initially#
water_n <- 50

# Grid to cover the area we want the pattern applied to#
df_water_grid <-
  tibble(
    expand_grid(
      x = seq(0,200, l = water_n),
      y = seq(0, 150, l = water_n)
    )
  )

# Aesthetic/Translation Options#
vec_water_sizes <- sample(seq(5, 15, l = water_n^2), replace = TRUE)
vec_water_fills <- sample(water_pal, water_n^2, replace = TRUE) |> sort()
vec_water_colors <- map(vec_water_fills, ~colorRampPalette(c(.x, "#ffffff"))(10)[2]) |> list_c()
vec_water_rotations <- sample(0:270, water_n^2, replace = TRUE)
vec_water_groups <- 1:water_n^2 |> group_numbers() |> sample()

# Store options in a list for the watter patterns#
lst_water_pattern_opts <-
  list(
    df_water_grid$x,
    df_water_grid$y,
    vec_water_sizes,
    vec_water_fills,
    vec_water_colors,
    vec_water_groups,
    vec_water_rotations
  )

# Water pattern compilation#
df_water_patterns <-
  pmap(
    lst_water_pattern_opts,
    ~square_data(
      x = ..1,
      y = ..2,
      size = ..3,
      fill = ..4,
      color = ..5,
      group_var = TRUE
    ) |>
      mutate(group = paste0(group, ..6)) |>
      rotator(x = x, y = y, angle = ..7)
  ) |>
  list_rbind() |>
  mutate(
    # This let's us know if the data is inside the bounds of df_creek#
    keep = point.in.polygon(x,y, df_creek$x, df_creek$y)
  ) |>
  # We only want those that are inside the bounds#
  filter(keep == 1)

creek_top_border <- df_creek$y |> max()

## Grass Underlay Data----------------------------------------------------------
grass_underlay_n <- 100
vec_grass_underlay_colors <- rep_along(1:grass_underlay_n, grass_pal) |> sort()
vec_grass_underlay_y_trans <- seq(0, 150, l = grass_underlay_n)
vec_grass_underlay_groups <- 1:grass_underlay_n |> group_numbers()

lst_grass_underlay <-
  list(
    vec_grass_underlay_y_trans,
    vec_grass_underlay_colors,
    vec_grass_underlay_groups
  )

# Grass underlay compilation#
df_grass_underlay <-
  pmap(lst_grass_underlay, ~tibble(
    x = 0,
    xend = 200,
    y = ..1,
    yend = ..1,
    color = ..2,
    group = paste0("grass_", ..3)
  )
  ) |>
  list_rbind() |>
  filter(y <= creek_top_border)

## Grass Blade Data-------------------------------------------------------------
#Amount of grass blades desired#
n = 4000

#Grass height/size#
grass_height = sample(seq(10,35, length.out = 100), n, replace = TRUE)
grass_level = 0

#Grass "waviness"#
waviness = sample(seq(5,100, length.out = 100), n, replace = TRUE)

#Grass wave direction#
direction = c("left", "right")

#X scale transformations#
x_trans <- sample(seq(0,200, length.out = 100), n, replace = TRUE)
y_trans <- sample(seq(0,150, length.out = 100), n, replace = TRUE)

#List options for iterations#
list_opts <- list(
  grass_height,
  grass_level,
  waviness,
  x_trans,
  colorRampPalette(grass_pal)(n),
  colorRampPalette(grass_pal)(n),
  1:n,
  y_trans
)

#Grass Blade data compilation#
df_grass_blades <-
  pmap(
    list_opts, ~tibble(y = c(seq(..2,..1,length.out = 100),
                             seq(..1,..2, length.out = 100),
                             ..2),
                       x = cos(y)/..3) |>
      mutate(x = x + c(rep(0,100),
                       seq(0,1.5, length.out = 100),
                       0),
             group = paste0("grass_",..7),
             fill = ..5,
             color = ..6,
             x = x + ..4 |> jitter(factor = .4),
             y = y + ..8 |> jitter(factor = .4))) |>
  list_rbind() |>
  mutate(
    keep = point.in.polygon(x,y, df_creek$x, df_creek$y)
  ) |>
  filter(keep == 0)

# Special blades for the left creek border#
df_grass_blade_edge_left <-
  df_creek_left |>
  mutate(
    y = y - 8,
    yend = y + sample(grass_height, df_creek_left |> nrow()),
    xend = x,
    color = sample(grass_pal,  df_creek_left |> nrow())
  )

# Special blades for the right creek border#
df_grass_blade_edge_right <-
  df_creek_right |>
  mutate(
    y = y - 8,
    yend = y + sample(grass_height, df_creek_right |> nrow()),
    xend = x,
    color = sample(grass_pal,  df_creek_right |> nrow())
  )

# Special blades for the top creek border#
df_grass_blade_edge_top <-
  df_creek_top |>
  mutate(
    y = y - 8,
    yend = y + sample(grass_height, df_creek_top |> nrow()),
    xend = x,
    color = sample(grass_pal,  df_creek_top |> nrow())
  )

#==============================================================================#
## Sky Data---------------------------------------------------------------------
#==============================================================================#
# Make packed circles for the "sky"#
df_sky_outline <-
  packer(1000,
         big_r = 70,
         min_x = 0,
         max_x = 200,
         min_y = creek_top_border,
         max_y = 300,
         circle_type = "whole",
         color_pal = sky_pal
  ) |>
  mutate(
    # Set Transparencies#
    alpha = case_when(
      # If its a CHONK Boi, let is show fully
      str_detect(group, "big_") ~ 1,
      # Otherwise, small bois are transparent
      .default = .1
    )
  )

## Sky Underlay Data------------------------------------------------------------
# Aesthetic/Translation Options#
sky_underlay_n <- 1000
vec_sky_underlay_colors <- rep_along(1:sky_underlay_n, sky_underlay_pal) |> sort() |> rev()
vec_sky_underlay_y_trans <- seq(creek_top_border, 300, l = sky_underlay_n)
vec_sky_underlay_groups <- 1:sky_underlay_n |> group_numbers()

# Options for the sky underlay#
lst_sky_underlay <-
  list(
    vec_sky_underlay_y_trans,
    vec_sky_underlay_colors,
    vec_sky_underlay_groups
  )

# Final compilation of Sky underlay#
df_sky_underlay <-
  pmap(lst_sky_underlay, ~tibble(
    x = 0,
    xend = 200,
    y = ..1,
    yend = ..1,
    color = ..2,
    group = paste0("sky_", ..3)
  )
  )|>
  list_rbind()

## Texture----------------------------------------------------------------------
#Set the "size" of your "pixels"#
# The smaller the value, the longer it'll take#
# Never said this code would be efficient lol
texture_size <- 1

#Set the x points of the canvas#
vec_x_canvas <- seq(x_lim[1], x_lim[2], by = texture_size)

#Set the y points of the canvas#
vec_y_canvas <- seq(y_lim[1], y_lim[2], by = texture_size)

#Making the full grid#
# This may take a little bit#
df_canvas <-
  expand_grid(
    "x" = vec_x_canvas,
    "y" = vec_y_canvas
  ) |>
  mutate(group_num = row_number() |> group_numbers())

# Storing the options#
lst_grid_opts <-
  lst(
    df_canvas$x,
    df_canvas$y,
    df_canvas$group_num
  )

# Creating the base square#
df_base_square <-
  tibble(
    x = c(0, texture_size, texture_size, 0, 0),
    y = c(0, 0, texture_size, texture_size, 0)
  )

# Compile the texture data#
# This may take a little bit#
df_canvas_texture <-
  pmap(
    lst_grid_opts,
    ~df_base_square |>
      mutate(x = x + ..1,
             y = y + ..2,
             fill = sample(texture_pal, 1),
             group = paste0("square_",..3),
             alpha = sample(seq(.01,.1, length = 100), 1)
      )
  ) |>
  list_rbind()

#==============================================================================#
# Final Image-------------------------------------------------------------------
#==============================================================================#
df_creek |>
  ggplot(aes(x, y, group = group)) +
  theme_void()+
  geom_segment(
    data = df_sky_underlay,
    aes(x, y, xend = xend, yend = yend, group = group),
    color = df_sky_underlay$color,
    linewidth = 1.2
  ) +
  geom_polygon(
    data = df_sky_outline,
    fill = df_sky_outline$fill,
    linewidth = .05,
    alpha = df_sky_outline$alpha
  ) +
  geom_path(
    data = df_sky_outline,
    color = "#000000",
    linewidth = .5,
    alpha = .1,
    position = position_jitter(width = .1, height = .2)
  ) +
  geom_segment(
    data = df_grass_underlay,
    aes(x, y, xend = xend, yend = yend, group = group),
    color = df_grass_underlay$color,
    linewidth = 1.2) +
  geom_polygon(
    data = df_water_patterns,
    fill = df_water_patterns$fill,
  ) +
  geom_path(
    data = df_water_patterns,
    color = df_water_patterns$color,
    alpha = .03
  ) +
  geom_path(
    data = df_creek,
    color = "#083109",
    linewidth = 5
  ) +
  geom_segment(
    data = df_grass_blade_edge_top ,
    aes(x, y, xend = xend, yend = yend),
    color = df_grass_blade_edge_top$color,
    linewidth = 0.8, inherit.aes = FALSE,
    position = position_jitter(width = .8, height = 1)
    ) +
  geom_segment(
    data = df_grass_blade_edge_left ,
    aes(x, y, xend = xend, yend = yend),
    color = df_grass_blade_edge_left$color,
    linewidth = 0.8, inherit.aes = FALSE
  ) +
  geom_polygon(
    data = df_grass_blades,
    fill = df_grass_blades$fill
  ) +
  geom_segment(
    data = df_grass_blade_edge_right ,
    aes(x, y, xend = xend, yend = yend),
    color = df_grass_blade_edge_right$color,
    linewidth = 0.8, inherit.aes = FALSE,
    position = position_jitter(width = .8, height = 1)
  ) +
  geom_segment(
    data = df_grass_blade_edge_left ,
    aes(x, y, xend = xend, yend = yend),
    color = df_grass_blade_edge_left$color,
    linewidth = 0.8, inherit.aes = FALSE
  ) +
  geom_polygon(
    data = df_canvas_texture,
    fill = df_canvas_texture$fill,
    alpha = df_canvas_texture$alpha
  )+
  coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE)

# # Save it out
# ggsave(
#   filename = here::here(
#     "creations",
#     "Public_Work",
#     "Statics",
#     "Rivercreek",
#     "rivercreek.png"
#   ),
#   dpi = 500,
#   bg = "transparent",
#   units = "px",
#   height = 3000,
#   width = 3000/1.5
# )