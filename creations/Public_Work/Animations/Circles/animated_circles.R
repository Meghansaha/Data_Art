# Library Load-in====
library(tidyverse)
library(gganimate)
library(transformr)

# Really like my color palette for the tidy Tuesday (years - billboard) - Let's see if we can make anything...#

# Want to make random functions to generate data====
squaresx <- function(x1,size){

firstx <- x1
secondx <- x1 + size
thirdx <- secondx
fourthx <- firstx


return(c(firstx,secondx,thirdx,fourthx))

}

squaresy <- function(y1,size){
  
  firsty <- y1
  secondy <- firsty
  thirdy <- secondy + size
  fourthy <- thirdy
  

  return(c(firsty,secondy,thirdy,fourthy))
  
}

# Creating random data with the functions====
data <- tibble(x = unlist(lapply(sort(sample(1:150,50, replace = FALSE)), function(i) squaresx(-sin(i)*cos(i),1))),
                 y = unlist(lapply(sort(sample(1:150,50, replace = FALSE)), function(i) squaresy(-cos(i)*sin(i),1))),
                 color = as.character(sample(c("1990":"1999"),200, replace = TRUE)))


# Just copying over the color palette from the tidy tuesday====
Year_colors <- c("1990" = "#af3918", 
                 "1991" = "#a21152", 
                 "1992" = "#822b75",
                 "1993" = "#612884",
                 "1994" = "#154baf",
                 "1995" = "#0b82b9",
                 "1996" = "#277e9d",
                 "1997" = "#488e35",
                 "1998" = "#e3a934",
                 "1999" = "#b2336a")

#Making a random cluster of settings to see what results====
circles <- data %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(color = color, group = 1L), size = sample(1:5,200, replace = TRUE), alpha = sample(seq(.1,1,.1),200, replace = TRUE))+
  scale_color_manual(values = Year_colors)+
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000"),
        legend.position = "none")+
  coord_polar("y")+
  transition_states(
    color,
    transition_length = 10,
    state_length = .1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('circular-in-out')

anim_save("creations/Animations/Circles/circles.gif", circles)


