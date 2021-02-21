# Happy Valentine's day 2/14/21 #

library(tidyverse)
library(scales)
library(ggtext)
library(extrafont)


loadfonts()

heart<- data.frame(z=seq(0, 2*pi, by=0.1) )
topheart <- function(z) 13*cos(z)-5*cos(2*z)-2*cos(3*z)-cos(4*z)
bottomheart <- function(z) 16*sin(z)^3

heart$y=topheart(heart$z)
heart$x=bottomheart(heart$z)
with(heart, plot(x,y, type="l"))

heart %>%
ggplot(aes(x = x, y = y))+
  geom_polygon(fill = "#ed66eb") +
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#3d343d"),
    plot.title = element_textbox_simple(
      size = 30,
      color = "#ffffff",
      padding = margin(5,5,5,5),
      margin = margin(19,0,0,0),
      halign = .5,
      family = "AvantGarde Bk BT",
      face = "bold")) +
  labs(title = "Happy Valentine's Day")
  

