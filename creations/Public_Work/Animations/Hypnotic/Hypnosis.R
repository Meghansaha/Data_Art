# Hypnotic Spirals fueled by insomnia....

# Library Load-in====
library(tidyverse)
library(gganimate)

# Random data creation====
bars <- tibble(x = 1:100,
               y = 50,
               z = sample(c("first","second","third",'fourth',"five"), 100, replace = TRUE)) # This sets different animation stages which "creates the illusion"

hypnotic <- bars %>%
  ggplot(aes(x = x, y = y)) +
  geom_bar(stat = "identity", color = "#ffffff", fill = "#ffffff") +
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000"),
        legend.position = "none")+
  coord_polar("y", clip = "on")+
  ylim(0,50)+
  transition_states(
    z,
    transition_length = 10,
    state_length = .1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('circular-in-out')

anim_save("creations/Animations/Hypnotic/hypnotic.gif", hypnotic)