#Circles

library(tidyverse)

set.seed(08052016)
random_data <- tibble(y = sample(1:500,100),
                      x = rep(500,100),
                      color = sample(RColorBrewer::brewer.pal(name ="BuPu", 9),100, replace = TRUE),
                      alpha = sample(seq(.6:1, by = .1), 100, replace = TRUE),
                      size = sample(seq(.5:1, by = .1), 100, replace = TRUE))



Rings <- ggplot(random_data, aes(xend = 0, yend = y, x = x, y = y,color = color, alpha = alpha, size = size))+
  geom_segment(color = random_data$color)+
  coord_polar(theta = "x", clip = "off", start = -3.14)+
  scale_x_continuous(lim=c(0, 1000)) +
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000"))+
  annotate(geom = "point", x =0, y =-1000, color = "#24036b", size = 84)+
  annotate(geom = "point", x =0, y =-1000, color = "#ffffff", size = 90, shape = 1)+
  annotate(geom = "point", x =0, y =-1000, color = "#552ab0", size = 70, alpha = .6)+
  annotate(geom = "point", x =0, y =-1000, color = "#9579d1", size = 50, alpha = .6)+
  annotate(geom = "point", x =0, y =-1000, color = "#efdeff", size = 30, alpha = .6)
  


ggsave(file="creations/Circles/Rings.svg", plot=test, width=12.5, height=7.03125)


  
  
