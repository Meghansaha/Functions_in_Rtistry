# Load in your libraries====
library(ggplot2)
library(dplyr)
library(purrr)

#Setting seed for reproducibility====
set.seed(1129)

# Set up some options====
x_transformer <- c(0,15,30)
rectange_groups <- c("right","middle","left")
rectangle_colors <- c("#1C658C", "#83BD75", "#CDBE78")
back_color <- "#363062"


# Just wrap everything in a custom function for better readability and ease====
rectangle_maker <- function(transformer, groups, colors){
  
# Calculate border colors====
  borders <- map_chr(colors, ~colorRampPalette(c(.x,"#000000"))(10)[5])
# Create a "base" rectangle====

  rectangle <- tibble(x = c(seq(0,10, length.out = 100),
                                 rep(10,100),
                                 seq(10,0, length.out = 100),
                                 rep(0,100)),
                           y = c(rep(0,100),
                                 seq(0,20, length.out = 100), 
                                 rep(20,100),
                                 seq(20,0, length.out = 100)))

# Use Purrr functions to apply options to the base rectangle and create the data needed====
   pmap_df(list(transformer,
               groups,
               colors,
               borders), ~rectangle %>%
                                    mutate(x = x + ..1,
                                           group = ..2,
                                           fill = ..3,
                                           color = ..4))
}

# Use the custom function to make the data====
rectangles_df <- rectangle_maker(x_transformer, rectange_groups, rectangle_colors)

# Plot the piece====
rectangles_df  %>%
  ggplot(aes(x = x, y = y, group = group)) +
  theme_void()+
  geom_polygon(fill = rectangles_df$fill , 
               color = rectangles_df$color, 
               size = 1.5, 
               position = position_jitter(width = .05, height = .02)) +
  theme(plot.background = element_rect(fill = back_color))


# Using base R
plot(rectangles_df$x,
     rectangles_df$y,
     ylab="",yaxt="n",
     xlab="",xaxt="n",
     bty="n")

polygon(rectangles_df$x,
        rectangles_df$y)














