library(tidyverse)

# Circle Parameters====
theta <- seq(0, 2*pi, length = 1000) #Theta represents the angles that make up the circle

n <- 50 #This can be any number more than zero but less than the length of "theta"
#old radius is 5
radius <- seq(0,4.9,.12) #Radi that's set for the circle. This corresponds to how wide a "regular circle is" but this will affect how many "swirls" we see.

# Circle Data Set to be Mapped====
circle_data <- data.frame(x = radius*cos(theta),
                          y = radius*sin(theta), # x and why will give a warning because depending on "n", we will be recycling the radius values through the length of theta# 
                          t = sample(runif(100,-.08,.08), length(theta), replace = TRUE)) #This recycling is what allows for different designs of the swirl. 

cloud <-  data.frame(x = radius*cos(theta),
                     y = radius*sin(theta), # x and why will give a warning because depending on "n", we will be recycling the radius values through the length of theta# 
                     t = sample(runif(100,-.01,.01), length(theta), replace = TRUE)) #T

background_diag <- tibble(x = c(-8,0,-8,-8,
                                -8,0,-8,-8,
                                -8,0,8,-8,
                                0,8,8,0,
                                0,8,8,0),
                          y = c(0,0,5,0,
                                5,0,10,5,
                                10,0,10,10,
                                0,5,10,0,
                                0,0,5,0),
                          color = c(rep("#01084f",4),
                                    rep("#391954",4),
                                    rep("#631e50",4),
                                    rep("#a73c5a",4),
                                    rep("#ff7954",4)))

ocean <- tibble(x = c(-8,8,8,-8),
                y = c(0,0,1.5,1.5),
                fill = "#01084f")

stars <- tibble(x = sample(runif(1000, -8,8),500),
                y = sample(runif(1000, 0,10),500))

gradient1 <- colorRampPalette(c("#01084f","#391954"))(5)
gradient2 <- colorRampPalette(c("#391954","631e50"))(5)
gradient3 <- colorRampPalette(c("631e50","#a73c5a"))(5)
gradient4 <- colorRampPalette(c("#a73c5a","#ff7954"))(5)

gradient1x <- rep(0,5)
gradient1y <- seq(2.5,7.5, length.out = 5)

gradient2x <- rep(0,5)
gradient2y <- c(7.5,10,10,10,10)


circle_data %>%
  ggplot(aes(x=x+t,y=y+t))+
  layer(inherit.aes = FALSE,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        data = background_diag,
        mapping = aes(x=x,y=y, group = color),
        params = list(fill = background_diag$color))+
  annotate(geom = "line", 
           x = c(0,-8),
           y = c(0,5),
           color ="#01084f",
           size = 4,
           alpha = .7)+
  annotate(geom = "line", 
           x = c(0,-8),
           y = c(0,10),
           color ="#391954",
           size = 4,
           alpha = .7)+
  annotate(geom = "line", 
           x = c(0,8),
           y = c(0,5),
           color ="#a73c5a",
           size = 4,
           alpha = .7)+
  annotate(geom = "line", 
           x = c(0,8),
           y = c(0,10),
           color ="#a73c5a",
           size = 4,
           alpha = .7)+
  layer(inherit.aes = FALSE,
        geom = "point",
        stat = "identity",
        position = "identity",
        data = stars,
        mapping = aes(x=x,y=y),
        params = list(color = "white", size = sample(runif(100, min = .1, max = .3),500, replace = TRUE), alpha = sample(runif(100, min = .2, max = .9), 500, replace = TRUE)))+
  geom_polygon(fill = "#F8CF9B", color = "black", size = .01)+
  coord_cartesian(xlim=c(-7,7),
                  ylim=c(1,9),
                  expand = TRUE,
                  clip = "on")+
  layer(inherit.aes = FALSE,
        geom = "point",
        stat = "identity",
        data = cloud,
        mapping = aes(x=-x,y=-y),
        position = "identity",
        params = list(color = "#a8792d", size = sample(runif(100, min = .1, max = 3),1000, replace = TRUE), alpha = .3 ))+
layer(inherit.aes = FALSE,
      geom = "polygon",
      stat = "identity",
      data = ocean,
      mapping = aes(x=x,y=y),
      position = "identity",
      params = list(fill = "#000000"))+
  layer(inherit.aes = FALSE,
        geom = "line",
        stat = "identity",
        data = cloud,
        mapping = aes(x=-x-7,y=y-1),
        position = "identity",
        params = list(color = "#000000", size = 3 ))+
  layer(inherit.aes = FALSE,
        geom = "line",
        stat = "identity",
        data = cloud,
        mapping = aes(x=-x+7,y=-y-1),
        position = "identity",
        params = list(color = "#000000", size = 3 ))+
  theme(plot.background = element_rect(color = "#333333", fill = "#000000", size = 10),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank())+
  

  xlim(-7.5,7.5)+
  ylim(0,10)