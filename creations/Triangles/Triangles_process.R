#art practice?
library(tidyverse)

t = tibble(x = 1:10000,
           xend = x*20,
      y = 1:10000,
      yend = y*20)

x <- (sin(13*pi*t$x))
xend <-t$xend * pi
y <- (tan(pi*t$y))
yend <-t$yend * pi

numbers <- tibble(x = x,
                  xend = xend,
       y = y,
       yend = yend,
       color = sample(RColorBrewer::brewer.pal(9,'Reds'),nrow(t), replace = TRUE),
       alpha = sample(seq(0.2,1,.2), nrow(t), replace = TRUE),
       size = sample(.1:2, nrow(t), replace = TRUE))

numbers %>%
  ggplot(aes(x = x,
             y = y))+
  geom_path(
             color = numbers$color,
            size = numbers$size,
            alpha = numbers$alpha) + 
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))+
  geom_jitter(color = "#262323", size = .01)
