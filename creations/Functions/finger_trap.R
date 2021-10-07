#Plotting functions

library(tidyverse) # As always

#I want to practice plotting functions!

#Setting some numbers for the function
theta <- 1
start <- 1
end <- 4000

#Variables to assist with the calculations
x <- start:end
pal.iter <- end/500 #(500 = palette length * each option in the rep fx below)

#Creating the dataframe to be plotted
data <- tibble(x = x,
              y = cos((x)*theta))

#Adding a color palette. Numbers have to change b
data <- data %>%
  mutate(color = rep(rep(RColorBrewer::brewer.pal(10,"PRGn"), each = 50),pal.iter)) %>%
  mutate(alpha = sample(runif(10,.2,1.0),nrow(data), replace = TRUE))

#Creating the function to be plotted
f <- function(x) sin((x)*theta)

#Creating a separate data frame for the polygon in the back
bg <- tibble(x = c(0,4000,4000,0),
             y = c(-1,-1,1,1))

#Final Plot
plot <- data %>%
ggplot(aes(x = x, y = y))+
  coord_cartesian(ylim = c(-2,2), xlim = c(400,3600), clip = "on", expand = TRUE)+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = bg,
        mapping= aes(x = x,
                     y = y),
        params = list(fill = "#0e021c"),
        inherit.aes = FALSE)+
  geom_point(color = data$color, size = 1, alpha = data$alpha)+
  stat_function(fun = f, color = NA)+ #This is the magic that plot's functions#
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))

plot


