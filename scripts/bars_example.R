#Library Load-in====
library(purrr)
library(dplyr)
library(ggplot2)

# Custom function to make vertical bars====
vertical_bars <- function(n_bars, color_pal, background_color){
  
  #Setting iterations for "n" amount of bars===
  iterations = 1:n_bars
  
  #Creating a color gradient based on supplied colors values and desired "n"===
  bar_colors = colorRampPalette(color_pal)(n_bars)
  
  #Creating a "base" bar to copy and repeat over===
  base_bar = tibble(x = c(0,1,1,0),
                    y = c(0,0,10,10),
                    group = 0)
  
  #Creating all the data needed to repeat the base bar, shift copies down the x-axis, and apply the generated color palette===
  bars <- map2_df(iterations,
                  bar_colors, ~base_bar %>%
                    mutate(x = x + 1.5*.x,
                           group = group + .x,
                           color = .y))
  
  #Creating the actual ggplot===
  bars %>%
    ggplot(aes(x,y, group = group))+
    theme_void()+
    geom_polygon(fill = bars$color, color = "#000000", size = 1)+
    theme(plot.background = element_rect(fill = background_color))}

#Options for the Piece===
color_pal_1 <- c("#363062", "#4D4C7D", "#827397", "#E9D5DA") # A purple-ish color palette
back_color_1 <- "#383838" # Some type of dark gray

color_pal_2 <- c("#006E7F", "#0093AB", "#00AFC1", "#B6FFCE", "#6BCB77", "#139487", "#064635") # A blue-ish - greenish color palette
back_color_2 <- "#F5EEDC" # A light pale tan-ish/yellow

color_pal_3 <- c("#000000", "#D8B6A4", "#EEEBDD" ) # A black-ish to Cream-ish palette
back_color_3 <- "#630000" # A dark red


# Using the function====
#Example 1
vertical_bars(10, color_pal_1, back_color_1) 

#Example 2
vertical_bars(100, color_pal_2, back_color_2) 

#Example 3
vertical_bars(15, color_pal_3, back_color_3) 