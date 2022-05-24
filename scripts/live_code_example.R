#========== Live Code Example =========#

# Library Load In====
library(tidyverse) #Because I'm lazy and reckless.


#=== Chaos to commence below ===#
  
circle_randomizer <- function(color_pal = c("rainbow","nature","galaxy","void","random"),
                              border_color = c("white", "black", "random"),
                              crowding = c("light", "middle", "heavy", "random"),
                              coordinates = c("cartesian", "polar", "equal", "random")){
  
#=============== Preset Color Palettes==============#  
  rainbow_pal = c("#af3918", "#a21152", "#822b75", "#612884",
                  "#154baf", "#0b82b9", "#277e9d", "#488e35",
                  "#e3a934","#b2336a")
  
  nature_pal = c("#ba3834","#731b18","#6a6a2a","#3d3d12","#634d41",
                 "#9e7057","#EEC373")
  
  galaxy_pal = c("#000839","#2C003E", "#3B064D", "#0F4C81", "#015668", "#003F5C", "#E497CD",
                 "#505BDA")
  #==================================================#
  
  # Parameter Lists==========#
  
  pal_list = list("rainbow" = rainbow_pal,
                  "nature" = nature_pal,
                  "galaxy" = galaxy_pal,
                  "void" = "#ffffff")
  
  border_list = list("white" = "#ffffff",
                     "black" = "#000000")
  
  crowds_list = list("light" = 10,
                     "medium" = 50,
                     "heavy" = 100)
  
  coords_list = list("cartesian" = coord_cartesian(),
                     "polar" = coord_polar(),
                     "equal" = coord_equal())
  
  #==============================================#
  
  #===Input Variable Lists===#
  
  #Color Palette
  input_pal = switch(color_pal,
                     "rainbow" = pal_list$rainbow,
                     "nature" = pal_list$nature,
                     "galaxy" = pal_list$galaxy,
                     "void" = pal_list$void,
                     "random" = unlist(sample(pal_list, 1)))
  
  #Crowding/Spacing of points
  input_crowding = switch(crowding,
                          "light" = crowds_list$light,
                          "medium" = crowds_list$medium,
                          "heavy" = crowds_list$heavy,
                          "random" = unlist(sample(crowds_list,1)))
  
  #Border color of circles/points
  input_border = switch(border_color,
                        "black" = border_list$black,
                        "white" = border_list$white,
                        "random" = unlist(sample(border_list,1)))
  
  #Ggplot plot coordinate systems
  input_coord = switch(coordinates,
                       "cartesian" = coords_list$cartesian,
                       "polar" = coords_list$polar,
                       "equal" = coords_list$equal,
                       "random" = unlist(sample(coords_list,1))
                       )
  
  #Randomized and calculated background of the ggplot
  background_color = sample(colorRampPalette(c(input_pal,"#000000","#ffffff"))(100),1)
  
  
# Data set up=======================#

# In Geometry used to create/set angles, here, used as a random transformer for the data points#
theta = seq(sample(1:100, 1), sample(1:100, 1)*pi, length.out = 100)

# Base data sets to be transformed#
circles <- tibble(crossing(x = seq(0,10, length.out = input_crowding),
                           y = x))

# Modified data set that adds varied transformations to the x and y variables as well as store the chosen color palette#
circles_mod <- circles %>%
  mutate(x = sin(theta)*x,
         y = cos(theta)*y,
         color = colorRampPalette(input_pal)(nrow(circles)))

#==============================================#

# Final work for the ggplot ===================#

circles_mod %>%
  ggplot(aes(x,y))+
  theme_void()+
  geom_point(shape = 21,
             size = seq(.001, 15, length.out = nrow(circles_mod)),
             stroke = sample(seq(.01, 1.5, length.out = nrow(circles_mod))),
             alpha = .5,
             fill = sample(circles_mod$color),
             color = input_border,
             position = "jitter")+
  theme(plot.background = element_rect(fill = background_color))+
  input_coord

}

#=================================================#

#Function ready to go and use!#

circle_randomizer(color_pal = "random",
                  border_color = "random",
                  crowding = "random",
                  coordinates = "random")

