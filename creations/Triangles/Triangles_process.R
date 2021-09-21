#Library Load-in 
library(tidyverse)

# We need to learn how to make a triangle?? IDK MAN I suck at geometry and trig====
n <- 20
xlimit <- 200


# Upright triangle=====
x1 <- seq(0,xlimit-n,n)
x2 <- seq(n,xlimit,n)
x3 <- (x1 + x2) / 2
y1 <- rep(n - n, length(x1))
y2 <- y1
y3 <- rep(n/2,length(x1))

x <- c(x1,x2,x3)
y <- c(y1,y2,y3)

# Color Randomizer===
Blues <- RColorBrewer::brewer.pal(5,"Blues")
Purples <- RColorBrewer::brewer.pal(5,"Purples")
Greens <- RColorBrewer::brewer.pal(5,"Greens")
Colors <- c(Blues,Purples,Greens)
Colors_ref_upright <- sample(Colors,length(x1))

# Upright traingle dataframe===
upright_triangles <- list()

for(i in seq_along(x1)){
  upright_triangles[[i]] <- tibble(
    x = c(x1[i],x2[i],x3[i]),
    y = c(y1[i],y2[i],y3[i]),
    group = Colors_ref_upright[i]
  )
}

# Reversed triangle=====
x1r <- seq(-n/2,xlimit-n/2,n)
x2r <- seq(n-n,xlimit,n)
x3r <- (x1r + n)
y1r <- rep(n/2, length(x1r))
y2r <- rep(n-n,length(x1r))
y3r <- y1r

xr <- c(x1r,x2r,x3r)
yr <- c(y1r,y2r,y3r)

Colors_ref_rev <- sample(Colors,length(x1r))

# reversed traingle dataframe===
reversed_triangles <- list()

for(i in seq_along(x1r)){
  reversed_triangles[[i]] <- tibble(
    x = c(x1r[i],x2r[i],x3r[i]),
    y = c(y1r[i],y2[i],y3r[i]),
    group = Colors_ref_rev[i]
  )
}



bottom_triangles <- bind_rows(upright_triangles)
top_triangles <- bind_rows(reversed_triangles)

bottom_triangles_2 <- bottom_triangles %>%
  mutate(y = y + 11)

bottom_triangles_3 <- bottom_triangles_2 %>%
  mutate(y = y + 11)

bottom_triangles_4 <- bottom_triangles_3 %>%
  mutate(y = y + 11)

top_triangles_2 <- top_triangles %>%
  mutate(y = y + 11)

top_triangles_3 <- top_triangles_2 %>%
  mutate(y = y + 11)

top_triangles_4 <- top_triangles_3 %>%
  mutate(y = y + 11)


# Testing out our basic triangle====
bottom_triangles %>%
  ggplot() +
  geom_hline(yintercept = 10.5, color = "#ffffff", linetype ="dotted")+
  geom_hline(yintercept = 10.5 + 11, color = "#ffffff", linetype ="dotted")+
  geom_hline(yintercept = 10.5 + 22, color = "#ffffff", linetype ="dotted")+
  geom_polygon(aes(x = x,
                   y = y,
                   group = group), 
               fill = bottom_triangles$group,
               color = NA,
               linetype = "dashed",
               size = .5) +
  coord_cartesian(ylim = c(4,40), xlim= c(10,180), clip = "off")+
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = top_triangles,
        mapping = aes(x = x,
                      y= y,
                      group = group),
        
        params = list(fill = top_triangles$group,
                      linetype = "dashed",
                      color = NA,
                      size = .5))+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = top_triangles_2,
        mapping = aes(x = x,
                      y= y,
                      group = group),
        
        params = list(fill = top_triangles_2$group,
                      linetype = "dashed",
                      color = NA,
                      size = .5))+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = top_triangles_3,
        mapping = aes(x = x,
                      y= y,
                      group = group),
        
        params = list(fill = top_triangles_3$group,
                      linetype = "dashed",
                      color = NA,
                      size = .5))+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = top_triangles_4,
        mapping = aes(x = x,
                      y= y,
                      group = group),
        
        params = list(fill = top_triangles_4$group,
                      linetype = "dashed",
                      color = NA,
                      size = .5))+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = bottom_triangles_2,
        mapping = aes(x = x,
                      y= y,
                      group = group),
        
        params = list(fill = bottom_triangles_2$group,
                      linetype = "dashed",
                      color = NA,
                      size = .5))+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = bottom_triangles_3,
        mapping = aes(x = x,
                      y= y,
                      group = group),
        
        params = list(fill = bottom_triangles_3$group,
                      linetype = "dashed",
                      color = NA,
                      size = .5))+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = bottom_triangles_4,
        mapping = aes(x = x,
                      y= y,
                      group = group),
        
        params = list(fill = bottom_triangles_4$group,
                      linetype = "dashed",
                      color = NA,
                      size = .5))
  