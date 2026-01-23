# Libraries
library(tidyverse)
library(artpack)
library(ambient)

# Image Specs----
canvas_size <- list("x_lim" = c(0, 100), "y_lim" = c(0, 70))
sky_color <- "#201B22"

# Custom Functions----
## Make the mountain outline and texture----
make_mountain <-
  function(
    width,
    height,
    base_center_x,
    base_center_y,
    n_points_side,
    pattern_density
  ) {
    bottom_left_point <-
      c(
        "x" = base_center_x - (width / 2),
        "y" = base_center_y
      )

    bottom_right_point <-
      c(
        "x" = base_center_x + (width / 2),
        "y" = base_center_y
      )

    top_point <-
      c(
        "x" = base_center_x,
        "y" = base_center_y + height
      )

    make_mountain_side <-
      function(
        start_x,
        start_y,
        end_x,
        end_y,
        n_points,
        amplitude
      ) {
        side_x <- seq(start_x, end_x, l = n_points)
        side_y_default <- seq(start_y, end_y, l = n_points)
        side_y_noise <- gen_perlin(
          side_y_default,
          interpolator = "linear",
          frequency = sample(seq(.3, .7, l = 20), 1)
        ) *
          amplitude
        side_y <- side_y_default + side_y_noise

        tibble(x = side_x, y = side_y)
      }

    left_side <-
      make_mountain_side(
        bottom_left_point["x"],
        bottom_left_point["y"],
        top_point["x"],
        top_point["y"],
        n_points_side,
        1.3
      )

    right_side <-
      make_mountain_side(
        top_point["x"],
        top_point["y"],
        bottom_right_point["x"],
        bottom_right_point["y"],
        n_points_side,
        .4
      )

    base_x <- seq(
      bottom_right_point["x"],
      bottom_left_point["x"],
      l = floor(n_points_side / 2)
    )
    base_y <- seq(
      bottom_right_point["y"],
      bottom_left_point["y"],
      l = floor(n_points_side / 2)
    )

    df_mountain <-
      bind_rows(left_side, right_side) |>
      add_row(
        x = base_x,
        y = base_y + rnorm(floor(n_points_side / 2), .1, .01)
      ) |>
      mutate(group = "outline")

    pattern_x_points <- seq(
      min(base_x),
      max(base_x),
      l = ceiling(n_points_side * pattern_density)
    )

    # helpers for shifting left_side copies
    left_side_min_x <- min(left_side$x)
    jitter_amount <- width * 0.01

    df_mountain_texture <-
      map2(
        pattern_x_points,
        group_numbers(
          seq_along(pattern_x_points),
          suffix = "texture",
          sep = "_"
        ),
        ~ {
          # anchor x for this texture column with small width-scaled jitter
          x_jittered <- .x + runif(1, -jitter_amount, jitter_amount)

          # shift the left_side shape so its left-most x aligns with the anchor
          shifted_left <- left_side |>
            mutate(
              x = x - left_side_min_x + x_jittered,
              group = .y
            )

          shifted_left |>
            mutate(
              inside = point_in_polygon(
                x,
                y,
                poly_x = c(df_mountain$x, df_mountain$x[1]),
                poly_y = c(df_mountain$y, df_mountain$y[1])
              )
            ) |>
            filter(inside == 1) |>
            select(-inside)
        }
      ) |>
      list_rbind()

    df_mountain |>
      bind_rows(df_mountain_texture)
  }

## Make a row of mountains----
make_mountain_row <-
  function(x_min, x_max, y_min, y_max, n) {
    mountain_width_range <- (x_max - x_min)
    mountain_width_max <- mountain_width_range * .50
    mountain_width_min <- mountain_width_range * .20

    mountain_height_range <- (y_max - y_min)
    mountain_height_max <- mountain_height_range * .40
    mountain_height_min <- mountain_height_range * .10

    df_specs <-
      tibble(
        width = seq(mountain_width_min, mountain_width_max, l = 100) |>
          sample(n, replace = TRUE),
        height = seq(mountain_height_min, mountain_height_max, l = 100) |>
          sample(n, replace = TRUE),
        base_x = seq(x_min, x_max, l = n) |> sample(),
        pattern_density = sample(seq(.9, 1, l = 25), n, replace = TRUE),
        groups = group_numbers(
          1:n,
          suffix = "mountain",
          sep = "_"
        )
      )

    df_mountain_row <-
      pmap(df_specs, \(width, height, base_x, pattern_density, groups) {
        make_mountain(width, height, base_x, y_min, 100, pattern_density) |>
          mutate(
            mountain_id = groups
          )
      }) |>
      list_rbind()
  }

## Make a ggplot layer of the row----
# This ensures the outlines and texture display on the ggplot properly
make_mountain_layer <-
  function(
    df,
    id,
    mountain_fill,
    mountain_color,
    texture_color,
    mountain_linewidth,
    texture_linewidth,
    overlay_linewidth
  ) {
    map(
      unique(id),
      \(id) {
        df_mountain <- df |> filter(mountain_id == id)

        list(
          geom_polygon(
            data = df_mountain |> filter(group == "outline"),
            aes(x, y, group = group),
            fill = mountain_fill,
            linewidth = mountain_linewidth
          ),
          geom_path(
            data = df_mountain |> filter(str_detect(group, "texture")),
            aes(x, y, group = group),
            linewidth = texture_linewidth,
            alpha = sample(
              seq(0, .5, l = 100),
              nrow(df_mountain |> filter(str_detect(group, "texture"))),
              replace = TRUE
            )
          ),
          geom_path(
            data = df_mountain |> filter(group == "outline"),
            aes(x, y, group = group),
            color = mountain_color,
            linewidth = overlay_linewidth,
            alpha = sample(
              seq(.3, 1, l = 100),
              nrow(df_mountain |> filter(str_detect(group, "outline"))),
              replace = TRUE
            )
          )
        )
      }
    ) |>
      list_flatten()
  }

# Build the mountains----
df_mountains_back <-
  make_mountain_row(
    canvas_size$x_lim[1],
    canvas_size$x_lim[2],
    3,
    canvas_size$y_lim[2],
    20
  )

df_mountains_middle <-
  make_mountain_row(
    canvas_size$x_lim[1],
    canvas_size$x_lim[2],
    2,
    canvas_size$y_lim[2],
    10
  )

df_mountains_bottom <-
  make_mountain_row(
    canvas_size$x_lim[1],
    canvas_size$x_lim[2],
    0,
    canvas_size$y_lim[2],
    6
  )

# Build the ggplot layers----
back_mountain_layer <-
  make_mountain_layer(
    df_mountains_back,
    df_mountains_back$mountain_id,
    "#ffffff",
    sky_color,
    sky_color,
    .4,
    .6,
    .8
  )

middle_mountain_layer <-
  make_mountain_layer(
    df_mountains_middle,
    df_mountains_middle$mountain_id,
    "#ffffff",
    sky_color,
    sky_color,
    .6,
    .7,
    1
  )

bottom_mountain_layer <-
  make_mountain_layer(
    df_mountains_bottom,
    df_mountains_bottom$mountain_id,
    "#ffffff",
    sky_color,
    sky_color,
    .8,
    .8,
    1.5
  )


# Make the sky----
df_sky_grid <-
  expand_grid(
    x = seq(canvas_size$x_lim[1], canvas_size$x_lim[2], l = 100),
    y = seq(canvas_size$y_lim[1], canvas_size$y_lim[2], l = 100)
  ) |>
  mutate(
    color_cat = rnorm(10000, mean = 5, sd = 5),
    mid_low = mean(color_cat) - 5,
    mid_high = mean(color_cat) + 5,
    sky_color = case_when(
      color_cat < mid_low ~ set_brightness(sky_color, .01),
      between(color_cat, mid_low, mid_high) ~ sky_color,
      .default = set_brightness(sky_color, .19)
    )
  )

# Make the sky texture----
df_sky_texture_left_specs <-
  tibble(
    x = canvas_size$x_lim[2] / 6,
    y = canvas_size$y_lim[2] / 10,
    radius = seq(50, 1, l = 35)
  ) |>
  mutate(
    n = row_number(),
    group_val = paste0(str_pad(n, width = 2, pad = "0"), "_left_ring")
  )

df_sky_texture_right_specs <-
  tibble(
    x = canvas_size$x_lim[2] / 1.2,
    y = canvas_size$y_lim[2] / 1.2,
    radius = seq(50, 1, l = 35)
  ) |>
  mutate(
    n = row_number(),
    group_val = paste0(str_pad(n, width = 2, pad = "0"), "_right_ring")
  )

df_sky_texture_left <-
  pmap(df_sky_texture_left_specs, \(x, y, radius, n, group_val) {
    circle_data(x, y, radius, "#ffffff") |>
      mutate(group = group_val)
  }) |>
  list_rbind()

df_sky_texture_right <-
  pmap(df_sky_texture_right_specs, \(x, y, radius, n, group_val) {
    circle_data(x, y, radius, "#ffffff") |>
      mutate(group = group_val)
  }) |>
  list_rbind()

# texture overlay----
df_overlay <-
  grid_maker(
    canvas_size$x_lim,
    canvas_size$x_lim,
    1000,
    fill_pal = c("#ffffff", "#000000"),
    fill_style = "random"
  )


# Make the final image----
ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = sky_color)) +
  geom_point(
    data = df_sky_grid,
    aes(x, y),
    color = df_sky_grid$sky_color,
    alpha = sample(seq(.01, .04, l = 30), nrow(df_sky_grid), replace = TRUE),
    size = sample(seq(10, 60, l = 50), nrow(df_sky_grid), replace = TRUE),
    position = position_jitter(.4, .01)
  ) +
  geom_path(
    data = df_sky_texture_left,
    aes(x, y, group = group),
    color = df_sky_texture_left$color,
    alpha = sample(
      seq(0, .2, l = 50),
      nrow(df_sky_texture_left),
      replace = TRUE
    ),
    position = position_jitter(width = .01, height = .001)
  ) +
  geom_path(
    data = df_sky_texture_right,
    aes(x, y, group = group),
    color = df_sky_texture_right$color,
    alpha = sample(
      seq(0, .2, l = 50),
      nrow(df_sky_texture_right),
      replace = TRUE
    ),
    position = position_jitter(width = .01, height = .001)
  ) +
  back_mountain_layer +
  middle_mountain_layer +
  bottom_mountain_layer +
  geom_point(
    data = df_sky_grid,
    aes(x, y),
    color = sample(c("#ffffff", "#000000"), nrow(df_sky_grid), replace = TRUE),
    alpha = sample(seq(.001, .01, l = 30), nrow(df_sky_grid), replace = TRUE),
    size = sample(seq(1, 10, l = 50), nrow(df_sky_grid), replace = TRUE),
    position = position_jitter(.4, .01)
  ) +
  coord_equal(
    xlim = canvas_size$x_lim,
    ylim = canvas_size$y_lim,
    expand = FALSE
  )

ggsave(
  "test.png",
  width = 11,
  height = 8.5,
  units = "in",
  device = "png",
  bg = "transparent"
)
