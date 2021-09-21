# Library Load-in====
library(tidyverse)
library(gganimate)
library(showtext)

#Loading in Google Fonts
font_add_google("Amatic SC")
showtext_auto()

# Creating a dataset that will "control" the breathing rate through aesthetic changes====
data <- tibble(x = rep(0,100),
               y = rep(0,100)) %>%
  mutate(size = c(rep(20,10), #Hold
                  seq(20,178,4),#Inhale
                  rep(178,10), #Hold
                  seq(178,20,-4)),#Exhale
         stage = row_number(), # This acts as a frame of reference for the animation
         text = c(rep("H O L D",10),
                  rep("I N H A L E",40),
                  rep("H O L D",10),
                  rep("E X H A L E",40)),
         text_size = c(rep(5,10), #Hold
                       seq(5,28.8,.6), # Inhale
                       rep(28.8,10), #Hold
                       seq(28.8,5,-.6) #Exhale
                       ),
         alpha = c(rep(0,10),
                   seq(0,.975,.025),
                   rep(.975,10),
                   seq(.975,0,-.025))) # These control the transparency of the outer-ring to let people know when the pause is coming


# Loading in the space background image
space <- png::readPNG("creations/Animations/Breathe/space.png")


# Putting it all together====
breathe <- data %>%
ggplot(aes(x = x, y = y, label = text)) +
  ggpubr::background_image(space)+
  geom_point(size = data$size)+
  geom_point(aes(x = 0, y = 0),size = 178, color = "white", shape = 1, alpha = data$alpha)+
  geom_text(size = data$text_size - 2, color = "#ffffff", family = "Amatic SC")+
  xlim(-200,200)+
  ylim(-200,200)+
  theme_void()+
  theme(legend.position = "none")+
  transition_reveal(
    stage) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear') # Linear allows the animation to place through the whole data set once. 
#The second half of the data set decreases the size of everything to create the illusion of a continuous cycle

anim_save("creations/Animations/Breathe/breathe.gif", breathe)
