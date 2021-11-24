#========# Wave Tutorial on The Tidy Trekker Site #========#

# Library Load-In====
library(tidyverse) #For everything data#

#Let’s Start with a Trig Refresher#========================#
theta <- seq(from = 0,
             to = 2*pi, 
             length.out = 100)

sine <- tibble(x = theta,
               y = sin(theta),
               label = 1:length(theta))

# A basic sine curve======
sine %>%
  ggplot(aes(x=x,y=y))+
  geom_line(color= "red", size = 3)

# A basic sine curve with more detail pointed out======
sine %>%
  ggplot(aes(x=x,y=y, label = label))+
  geom_vline(xintercept = 0, size = 2)+
  geom_vline(xintercept = 2*pi, size = 2)+
  geom_line(color= "red", size = 3)+
  geom_point(color = "blue")+
  ggrepel::geom_text_repel(max.overlaps = 20, size = 3)+
  geom_text(aes(x = 0,y = -1),
           label =paste(sprintf('\u2190'),"theta's '0'"),
           nudge_x = .3,
           inherit.aes = FALSE)+
  geom_text(aes(x = 2*pi,y = 1),
           label = paste("theta's '2*pi'",sprintf('\u2192')),
           nudge_x = -.3)



# Starting the transition to a pretty wave====
##Setting up our "range" on the x axis for horizontal waves=====
wave_theta <- seq(from = -.4,
             to = -1.5*pi, 
             by = -.1) 

##Creating the "top" of our wave polygon====
curve_top <- tibble(x = wave_theta,
                    y = (sin(x)*cos(wave_theta))+exp(x*2)) %>%
  arrange(x)

##See what the "top" of the wave looks like====
curve_top %>%
  ggplot(aes(x=x, y=y))+
  geom_path()

##Grab the max X value in the wave from the top wave====
max_x_point <- curve_top[which(curve_top$x == max(curve_top$x)),]

##Create a subset of curve_top to create a "side" of the wave====
curve_side_right <- max_x_point %>%
  add_row(max_x_point - c(0,0.5))%>%
  arrange(desc(y))

##Viewing the top curve and curve_side_right together====
curve_top %>%
  ggplot(aes(x=x, y=y))+
  geom_path()+
  geom_path(data = curve_side_right, aes(x=x, y=y))

##Create a copy of the curve_top dataset with the y values decreased by .5====
curve_bottom <- curve_top %>%
  mutate(y = y - .5) %>%
  arrange(desc(x))

##Viewing the top curve, curve_side_right, and curve_bottom together====
curve_top %>%
  ggplot(aes(x=x, y=y))+
  geom_path()+
  geom_path(data = curve_side_right, aes(x=x, y=y), inherit.aes = FALSE)+
  geom_path(data = curve_bottom, aes(x=x, y=y),inherit.aes = FALSE)

##Grab the min X value in the wave from the top wave====
min_x_point <- curve_top[which(curve_top$x == min(curve_top$x)),]

##Create a side that will connect curve_top and curve_bottom on the left side====
curve_side_left <- min_x_point %>%
  add_row(min_x_point - c(0,.5)) %>%
  arrange(y)

##Slap all of them together into one dataframe IN ORDER (top,right,bottom,left)
wave <- bind_rows(curve_top,curve_side_right,curve_bottom,curve_side_left)


##View the entire thing====
wave %>%
  ggplot(aes(x=x, y=y))+
  geom_polygon()

# Creating a function for iterations====

wave_maker <- function(n, wave_df){

  #Creating an empty list to store our multiple dataframes(waves)#
  wave_list<- list()
  
  #Creating a for loop to iteratively make "n" amount of waves#
  for(i in seq_along(1:n)){
    
    wave_list[[i]] <- wave_df %>%
      mutate(y = y - (.5*i),
             group = i)
    
  }
  
 #returning the completed data frame to the environment#
  return(bind_rows(wave_list))
  
}

# Creating the final data frame used for plotting====
wave_layers <- wave_maker(5, wave)

# Picking a random color palette from RColorBrewer for the waves
#We made 5 waves, so let's pick 5 colors#
colors_pal <- sample(RColorBrewer::brewer.pal(10,"Spectral"),nrow(wave_layers)/nrow(wave), replace = TRUE)

# Final Plotting====
wave_layers %>%
  ggplot(aes(x=x,y=y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#ffffff"))+
  geom_polygon(color = "black", 
               size = 2, 
               fill = rep(colors_pal, each = nrow(wave)))

# You've got the foundation you need now! Go forth and make some awesome waves! Manipulating these aren't too bad#

#An Example: - Simply changing the y value in the aes call creates a different set of waves :)#
wave_layers %>%
  ggplot(aes(x=x,y=y*x, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#ffffff"))+
  geom_polygon(color = "black", 
               size = 2, 
               fill = rep(colors_pal, each = nrow(wave)))

