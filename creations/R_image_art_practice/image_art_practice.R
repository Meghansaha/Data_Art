#================== R Data Art Practice - Space Collage ==================#

#Following Michael Freeman's "R Image Art" Tutorial on his site (mfviz.com - http://mfviz.com/r-image-art/ ).#

# Library Load-in====
pacman::p_load("imager","tidyverse","ggvoronoi")

# Image Load-in (function from imager package)====
space <- load.image(here::here("creations","R_image_art_practice","space collage.jpg"))

# You can print and plot the image. I'll plot to make sure it looks as intended===
plot(space)

#Looks beautiful.#

# Apparently, we can coerce this image into a data frame===
space_df <- as.data.frame(space)

# Let's look at what was created===
str(space_df)

#THATS SO MANY OBS.#

# Michael explains that the data frame will generate four columns===
names(space_df)

# X is the horizontal position of a pixel in the image.#
# Y is the vertical position of a pixel in the image.#
# cc is the color channel that is being presented (based off of the RGB color model).#
# value is the value of the actual color channel. (Scale of 0 to 1).#

#So frame manipulation is needed to get ggplot to accurately plot this. Each observation (row) needs to be a single pixel. Michael uses the "spread" function from the tidyr package. But I think this would be the equivalent to using pivot_wider for the cc column. Each value in the CC column corresponds to either Red, Blue, or Green (The RGB color channels).#

# Data Frame Manipulation====
#Recoding for the values in the CC column===
space_df_mod <- space_df %>%
  mutate(color_channel = ifelse(cc == 1,"Red",
                                ifelse(cc == 2, "Green",
                                       ifelse(cc == 3, "Blue",NA))))

#Adding a catch for any errors===
if(TRUE %in% unique(is.na(space_df_mod$cc))){
  stop("Something isn't right in the \"cc\" column. Expecting a numeric value of 1, 2, or 3. Anomaly detected.")
} 

#Removing the original cc column and transposing the frame===
space_df_mod <- space_df_mod %>%
  select(-cc) %>%
  pivot_wider(id_cols = c("x","y"),
              names_from = "color_channel",
              values_from = "value")

#We also need to add the hex color codes that corresponds to each (RGB) channel. We do this by adding a color that will generate this from the "red", "green" and "blue" columns in the set. the "rgb" function is within the "grDevices" package===
space_df_mod <- space_df_mod %>%
  mutate(color = rgb(Red, Green, Blue))

#We can remove the red, green, and blue columns now===
space_df_mod <- space_df_mod %>%
  select(-c(Red,Green,Blue))

# ggplot Rendering====

#With this modded dataset, we can now simply plot the image.#

#ggplot image. Using scale_color_identity actually let's ggplot use the actual values in that column! Did not know that mind is blown===
ggplotspace <- ggplot(space_df_mod)+
  geom_point(aes(x = x, y = y, color = color))+
  scale_color_identity()+
  theme_void()+
  scale_y_reverse() #Apparently Michael's image was upside-down. I can't tell with mine, but adding this just in case!#

#Saving the image to the file===
ggsave("ggplotspace.png",
       ggplotspace, 
       path = here::here("creations","R_image_art_practice"), 
       bg = "transparent",
       height = 3.5,
       width = 5,
       units = "in",
       dpi = 100)

#Ahh So cool! it works! To wrap this practice session up. I'd like to practice the Voronoi Diagrams presented in Michael's tutorial.#

#First, we need to get a random sample of the data points, Choosing a random 5,000 rows===
space_samp <- space_df_mod[sample(nrow(space_df_mod),5000),]

# Voroni ggplot Rendering====
voronoispace <- ggplot(space_samp)+
  geom_voronoi(aes(x = x, y = y, fill = color))+
  scale_fill_identity()+ #Mind is still blown about this#
  theme_void()+
  scale_y_reverse()

#Saving the image to the file===
ggsave("voronoispace.png",
       voronoispace, 
       path = here::here("creations","R_image_art_practice"), 
       bg = "transparent",
       height = 3.5,
       width = 5,
       units = "in",
       dpi = 100)
