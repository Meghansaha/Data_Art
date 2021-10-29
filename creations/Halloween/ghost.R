#Let's try to animate a cute ghost for Halloween! :)

#Library Load-in====
library(tidyverse) # For everything data
library(ggimage) # For loading in ghost image pieces
library(showtext) # For loading in Google Fonts
library(gganimate) # For animating the plot
require(transformr) # needed for gganimate transition states


# Font Import and Load-in====
font_add_google("Cute Font")
showtext_auto()

# Ghost Bottom====
# First we make the "bottom" of the ghost since it doesn't involve curves#
ghost_bottom <- tibble(x = c(-10,seq(-10,10, length.out = 5),10),
                y = c(20,0,1,0,1,0,20))

# Ghost Head====
#We need to set the range for our ghost head's curve#
#This will give a sequence of numbers from -10 to 10 which matches up with the ghost's bottom#
theta <- seq(-10,10, length.out = 2000)

# This sets how big (tall) we want the head to be from the top of the ghost's "bottom" aka radius of our half-circle#
r = 10

#tibble for the head#
ghost_head <- tibble(x = sin(theta)*r,
                     y = cos(theta)*r+20) %>%
  filter(y >= 20)

# Whole Ghost Body====
#Binding both datasets to make the whole ghost#
ghost <- rbind(ghost_bottom,ghost_head) %>%
  mutate(right_eye_x = 5,
         left_eye_x = -5,
         right_mark_x = 5,
         left_mark_x = -5,
         marks_y = 17,
         eyes_y = 20,
         mouth_x = 0,
         mouth_y = 10)

# Ghost Image====
#Building the ghost in ggplot with emojis#
ghost_img <- ghost %>%
  ggplot(aes(x=x,y=y))+
  geom_polygon(fill = "#ffffff", color = "#242322", size = 4)+
  geom_segment(aes(x=-9.65, xend = 9.65, y=20,yend = 20), color = "#ffffff", size = 5, inherit.aes = FALSE)+
  geom_text(aes(right_eye_x,eyes_y),label = "<", size = 40, fontface ="bold")+
  geom_text(aes(left_eye_x,eyes_y),label = ">", size = 40, fontface = "bold")+
  geom_image(aes(right_mark_x,marks_y),image = here::here("creations/Halloween/ghost_blush_right.png"), size = .2)+
  geom_image(aes(left_mark_x,marks_y),image = here::here("creations/Halloween/ghost_blush_left.png"), size = .2)+
  geom_image(aes(mouth_x,mouth_y),image = here::here("creations/Halloween/ghost_mouth.png"), size = .3)+
  coord_equal()+
  theme_void()



#Saving the ghost image into the directory
ggsave(here::here("creations/Halloween/ghost.png"),
  ghost_img,
  bg = "transparent")

# Animations Data Set====
animations <- tibble(x = 0,
                     y = c(seq(-.7,.3, length.out = 100), #Going up#
                           seq(.3,-.7, length.out = 100)), #Going down#
                     ghost = here::here("creations/Halloween/ghost.png"),
                     font_color = c(colorRampPalette(c("#a86232","#c99e4f","#3d3931"))(100),
                                    rev(colorRampPalette(c("#a86232","#c99e4f","#3d3931"))(100))),
                     font_alpha = rep(c(seq(1,.2, length.out = 20), #flashing lights 
                                          rev(seq(1,.2, length.out = 20))),5)) %>%
  mutate(stage = row_number())


# Slap everything on that bad boy in a final animation====
happy_halloween <- animations %>%
ggplot(aes(x=x,y=y, image=ghost))+
  geom_image(size = 1)+
  coord_cartesian(ylim = c(-5,4), expand = FALSE)+
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))+
  geom_text(aes(x = 0, y = -4),
            label = "Happy Halloween!", color = animations$font_color, size = 20,
            family = "Cute Font", alpha = animations$font_alpha)+
  transition_reveal(
    stage) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear')

# Save it out to the directory====
anim_save("creations/Halloween/Happy Halloween.gif", happy_halloween)