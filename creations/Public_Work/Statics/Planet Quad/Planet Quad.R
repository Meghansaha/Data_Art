# Library Load-In====
library(tidyverse) # For everything data
library(cowplot) # For placing plots on top of each other easily with ggdraw and draw_image
library(patchwork) # For packaging up the plots in the final image

# Star Backgrounds====

## Making four different stars "backgrounds" with four different colors====
Space_colors <- c("Topleft"="#000000",
                  "Topright"="#130321",
                  "Bottomleft"="#330500",
                  "Bottomright"="#8a4500")

## Making a data set that will be used to create all star backgrounds====
stars <- tibble(crossing(x = seq(1,2000,100),
                         y = seq(1,2000,100)))

## Making the list to hold the "space" plots====
All_stars <- list()

## Iterating through the "space_colors" to make four different plots====
for(i in seq_along(Space_colors)){
  
  All_stars[[i]] <- stars %>%
    ggplot(aes(x = x, y = y))+
    geom_jitter(size = sample(c(.02,.04,.06,.8),nrow(stars), replace = TRUE), color = "white")+
    theme_void()+
    theme(plot.background = element_rect(fill = Space_colors[i], color = "#ffffff", size = 6))
  
}

## Naming the plots just to help keep track of what's going where====
names(All_stars) <- names(Space_colors)

# Planet Creations====
## Setting the color palettes for each planet quad====
Planet_colors <- list("Topleft" = c("#194157","#008dd7","#085b88","#26925e","#095c88"),
                      "Topright" = c("#480463","#392242","#1e0329","#bf9232","#120b17"),
                      "Bottomleft" = c("#47322d","#6b1c09","#a30000","#6b1d09","#851205"),
                      "Bottomright" = c("#c7a602","#998523","#ba690d","#755b3d","#dbab39"))

## Setting the colors of each planet's borders====
Planet_borders <- c("Topright" = "#333333",
                    "Topright" = "#120b17",
                    "Bottomleft" = "#260f09",
                    "Bottomright" = "#5c523a")

## Making a dataset that will be used to create all the planets====
planet <- tibble(crossing(x = sample(1:1000,100, replace = TRUE),
               y = sample(1:2000, 100, replace = TRUE)))

## Making the list to hold the "planet" plots====
All_planets <- list()

## Iterating through the "planet_colors" to make four different planet plots====
for(i in seq_along(Planet_colors)){
  
  All_planets[[i]] <- planet %>%
    ggplot(aes(x = x, y = y))+
    scale_fill_manual(values = sample(Planet_colors[[i]],100, replace = TRUE))+ #100 is just a arbitrary "safe" number I picked. geom_density does background calcs to create levels that varies based on data.
    geom_density2d_filled(color = Planet_borders[i], size = 2)+
    coord_polar(clip = "on")+
    theme_void()+
    theme(legend.position = "none")
}

## Naming the plots just to help keep track of what's going where====
names(All_planets) <- names(Planet_colors)


# Saving Planets into the directory====
for(i in seq_along(All_planets)){
  
ggsave(paste0("planets/",
              names(All_planets)[i],
              "_planet.png"),
       All_planets[[i]],
       bg = "transparent", 
       device = "png")

}

# Loading Planets back into the environment as PNGs====
Planet_PNGs <- list()

for(i in seq_along(All_planets)){
  
Planet_PNGs[[i]] <- png::readPNG(paste0("planets/",
                                        names(All_planets)[i],
                                        "_planet.png"))

}

## Setting names to keep track of the planets====
names(Planet_PNGs) <- names(All_planets)

# Combining both the stars and planets to create four plots in total====
Combined_plots <- list()

for(i in seq_along(All_planets)){
  
  Combined_plots[[i]] <- ggdraw(All_stars[[i]]) +
                                  draw_image(Planet_PNGs[[i]])
  
}

## Setting names to keep track of the plots===
names(Combined_plots) <- names(Planet_PNGs)


## Plucking out all the individual plots===
Topleft <- Combined_plots %>% pluck("Topleft")
Topright <- Combined_plots %>% pluck("Topright")
Bottomright <- Combined_plots %>% pluck("Bottomright")
Bottomleft <- Combined_plots %>% pluck("Bottomleft")

# Final output construction with patchwork functions====
Planet_Quad <- (Topleft + Topright) / (Bottomleft + Bottomright)


# View the piece#
Planet_Quad

