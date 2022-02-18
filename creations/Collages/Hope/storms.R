# For M & N

# Library load-in====
library(tidyverse)
library(sysfonts)
library(showtext)

# Backgrounds====
storm_background <- tibble(x = c(0,50,50,0),
                           y = c(0,0,75,75))

forever_background <- tibble(x = c(50,100,100,50),
                             y = c(0,0,75,75))

# Cloud space====
clouds <- tibble(crossing(x = seq(0,50, length.out = 50),
                 y = seq(57,75, length.out = 100)))

cloud_size <- sample(seq(1,15, length.out = nrow(clouds)))

# Rain space====
rain <- tibble(crossing(x = seq(0,50, length.out = 50),
                        y = seq(0,51, length.out = 50),
                        rain = "|")) 

rain_size <- sample(seq(.1,3, length.out = nrow(rain)))

# Dark ground====
dark_ground_base <- tibble(x = c(0,50,50,0),
                              y = c(0,0,10,10))

dark_ground_texture <- tibble(crossing(x = seq(0,50, length.out = 20),
                               y = seq(0,10, length.out = 20)))

ground_size <- sample(seq(.1,1, length.out = 25),nrow(dark_ground_texture),replace = TRUE)

ground_color <- sample(c("#333333","#666666"), nrow(dark_ground_texture), replace = TRUE)

# Rainbow fx====
rainbow_maker <- function(n){
  
  theta <- seq(0,pi, length.out = 1000)
  r <- seq(75,100,  length.out = n)
  color_pal <- rev(c("#EC0F47","#EE6B3B","#FBBF54","#087353","#022C7A","#262949","#1A1333"))
  rb_pal <- colorRampPalette(color_pal)(99) 
  rainbow_list <- list()
  
  for(i in seq_along(1:(n-1))){
    
    rainbow_list[[i]] <- tibble(x = c(cos(theta)*r[i],
                                      r[i],r[i+1],
                                      rev(cos(theta)*r[i+1]),
                                      -r[i+1], -r[i]),
                                y = c(sin(theta)*r[i],
                                      rep(0,2),
                                      rev(sin(theta)*r[i+1]),
                                      rep(0,2)),
                                group = i,
                                color = rb_pal[i])
  }
  
  return(bind_rows(rainbow_list))
}

# Rainbow ====
rainbow <- rainbow_maker(100) %>%
  mutate(logic = sp::point.in.polygon(x,y,forever_background$x, forever_background$y)) %>%
  filter(logic == 1)

# Forever sky ====
sky <- tibble(crossing(x = 50,
                xend = 100,
                y = sample(seq(0,75, length.out = 100)),
                yend = sample(seq(0,75, length.out = 100))))

sky_pal <- sample(colorRampPalette(c("#0A81AB","#005A8D","#78C4D4"))(nrow(sky)))

sky_size <- sample(seq(.1,5, length.out = 50),nrow(sky), replace = TRUE)

# Grass ====
grass_base <- tibble(x = c(49.5,100,100,49.5),
                     y = c(0,0,10,10))

grass <- tibble(crossing(x = seq(49,100, length.out = 100),
                         y = seq(0,8, length.out = 100),
                         label = ";"))

grass_pal <- sample(colorRampPalette(c("#0B4619","#519259","#A6CF98","#A3DA8D","#B4C6A6"))(50), nrow(grass), replace = TRUE)

grass_size <- sample(seq(.1,2, length.out = 30),nrow(grass), replace = TRUE) 

# Hope====
first_half <- tibble(x = c(25,49.5,49.5,25),
                     y = c(35,35,45,45))

second_half <- tibble(x = c(49.5,75,75,49.5),
                     y = c(35,35,45,45))

hope <- tibble(x = c(37.5,62.5),
               y = 40,
               label = c("Storms Don't", "Last Forever"),
               color = c("#ffffff","#000000"))

font_add_google("Indie Flower")
showtext_auto()

# Final Piece
ggplot(storm_background,aes(x,y))+
  theme_void()+
  coord_cartesian(expand = FALSE, 
                  clip = "on", 
                  xlim = c(0,100), 
                  ylim = c(0,75))+
  geom_polygon(fill = "black")+
  geom_text(data = rain, aes(label = rain), 
            color = "#ffffff",  
            size = rain_size,
            position = position_jitter(width = 2, height = 1), 
            alpha = .4)+
  geom_polygon(data = dark_ground_base, 
               fill = "#000000")+
  geom_point(data = dark_ground_texture, 
             position = position_jitter(width = 10, height = 2),
             size = ground_size, 
             alpha = .1, 
             color = ground_color)+
  geom_polygon(data = forever_background, 
               fill = "#398AB9")+ 
  geom_segment(data = sky, aes(xend = xend, yend = yend), 
               size = sky_size, 
               alpha = .1, 
               color = sky_pal,
               position = position_jitter(height = 5))+
  geom_path(data = rainbow, aes(x = x+1, y = y), 
            color = rainbow$color, 
            size = 2, 
            position = position_jitter(width = 2, height = 2))+
  geom_point(data = clouds, 
           fill = NA,
           color = "#D1D1D1",
           alpha = .01, 
           size = cloud_size, 
           position = position_jitter(width = 2, height = 5))+
  geom_polygon(data = grass_base, 
               fill = "#519259")+ 
  geom_text(data = grass, aes(label = label),
               color = grass_pal,
               size = grass_size,
               alpha = .9,
               position = position_jitter(width = 1, height = 2))+
  geom_polygon(data = first_half, 
               fill = "#000000")+
  geom_polygon(data = second_half,
               fill = "#ffffff")+
  geom_text(data = hope, aes(label = label), 
            color = hope$color, 
            size = 12,
            family = "Indie Flower",
            fontface = "bold")+
  theme(plot.background = element_rect(size = 20, color = "#666666"))
  

