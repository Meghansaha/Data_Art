# Library Load-in====
library(tidyverse)


#We set parameters that help construct a circle in ggplot#

# Circle Parameters====
theta <- seq(0, 30*pi, length = 1000) #Theta represents the angles that make up the circle

n <- 50 #This can be any number more than zero but less than the length of "theta"

radius <- 1:n #Radi that's set for the circle. This corresponds to how wide a "regular circle is" but this will affect how many "swirls" we see.

# Circle Data Set to be Mapped====
circle_data <- data.frame(x = radius*cos(theta),
               y = radius*sin(theta), # x and why will give a warning because depending on "n", we will be recycling the radius values through the length of theta# 
               theta = theta) #This recycling is what allows for different designs of the swirl. 
                              #To remove the warning set "n" to length(theta) and see how that affects the visual#

# Final Plotting of the swirl/manipulated circle
circle_data %>%
ggplot(aes(x = x, y = y))+
  geom_polygon(color = "#d9a045", size = .1, fill ="#200538")+ #Using the polygon geom forces ggplot to create spaced solid figures#
  theme_void()+
  theme(plot.background = element_rect(fill = "#0e011a"),
        panel.background = element_rect(fill = "#0e011a"),
        legend.position = "none")+
  coord_equal()




