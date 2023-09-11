
# Reproducing Playfair's plot of wheat prices, wages and English rulers
# Homework 2 - Introduction to R Programming 

# Set-up ------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)

options(device = "RStudioGD")
data <- read.csv("./data/raw/02_playfair-wheat-wages.csv",
                 header = T,
                 sep = ",")

# This will create a new window where the plot is 9 in width and 6 in tall
#   Useful to make sure you are adjusting for the right dimensions that
#   you want your graph to be 
dev.new(width = 9,height = 6,
        noRStudioGD = TRUE)


# Create necessary objects ------------------------------------------------

# Ruler labels
ruler_labels <- 
  data %>%
  group_by(ruler_label1) %>%
  # Fix William & Mary label so it's in two lines 
  mutate(ruler_label1 =
           if_else(
             ruler_label1 == "William and Mary",
             "William\n & Mary",
             ruler_label1
           )) %>%
  # Keep only one obs per ruler, where x is the median year of the ruler reign
  summarise(ruler_lab_x = median(year),
            ruler_lab_y = mean(ruler_y))



# Century labels 
century_labels <- 
  tibble(c_x = c(1574, 1650, 1740, 1818),
         c_y = rep(102, 4), 
         labels = c("16th Century",
                    "17th Century",
                    "18th Century",
                    "19th Century"))

# Grids (we are artificially creating grids, so we can specify exactly at which
#        layer they go)

grid_hM <-  # Major horizontal grid 
  tibble(x = rep(1565, 9),
         y = seq(10, 90, 10),
         xend = rep(1830, 9),
         yend = seq(10,90, 10))
grid_hm <-  # Minor horizontal grid 
  tibble(x = rep(1565, 17),
         y = seq(10, 90, 5),
         xend = rep(1830, 17),
         yend = seq(10,90, 5))
grid_vM <-  # Major vertical grid
  tibble(x = seq(1565, 1830, 50),
         y = rep(0, 6),
         xend = seq(1565, 1830, 50),
         yend = rep(100, 6))
grid_vm <-  # Minor vertical grid
  tibble(x = seq(1565, 1830, 5),
         y = rep(0, 54),
         xend = seq(1565, 1830, 5),
         yend = rep(100, 54))



# Plot --------------------------------------------------------------------

data %>% 
    mutate(n = row_number(),
           # Creating variables to change the wheat bar colors to a gradient
           # (to be used in section "Bars: wheat")
           rnoise = runif(1, 0, 100),
           wheat_gradient = wheat + rnoise 
           ) %>% 
  ggplot(aes(x = year, 
             y = wheat)) +
  ## Minor grid -----------------------
geom_segment(aes( x= x, y = y, xend = xend, yend = yend ),
             data = grid_hm,
             linewidth = 0.2) +
  geom_segment(aes( x= x, y = y, xend = xend, yend = yend ),
               data = grid_vm,
               linewidth = 0.2) + 
  ## Major grid ----------------------
geom_segment(aes( x= x, y = y, xend = xend, yend = yend ),
             data = grid_hM) +
  geom_segment(aes( x= x, y = y, xend = xend, yend = yend ),
               data = grid_vM)  +
  ## Ellipse: "Chart showing ..." ---- 
geom_ellipse(aes(x0 = 1650, y0 = 65, a =40, b = 13, angle = 0),
             linewidth = .2,
             fill = "white") +
  annotate("text", 
           x = 1650, y = 66, 
           label = "CHART 
              \n Showing at One View
              \n The Price of The Quarter of Wheat
              \n & Wages of Labour by the Week,
              \n The Year 1565 to 1821,
              \n WILLIAM PLAYFAIR",
           lineheight = .5,
           size = 3.5,
           fontface = "bold.italic",
           family = "serif") +
  ## Bars: wheat -------
  geom_bar(stat = "identity", # Very important! 
           width = 1.1,
           alpha = 1,
           aes(y = wheat, 
               fill = wheat), # We are going to make the bars change slightly
                              # to give the appearance of a gradient  
           show.legend = F) + 
  scale_fill_gradient2(position = "bottom", 
                       low = "white",
                       mid = "white",
                       midpoint = 25,
                       high = "black") +
  ## Area/Line: wages -----------
  geom_area(aes(year, wages),
            fill = "lightblue",
            alpha = 0.8) +
  geom_line(aes(year, wages + .5),
            color = "darkred",
            linewidth = 1,
            alpha = .7) +
  geom_line(aes(year, wages),
            color = "black") +
  geom_text(x = 1630, y = 8, 
            label = "Weekly wages of a good mechanic",
            size = 2.5,
            angle = 2,
            family = "serif") +
  geom_text(x = 1750, y = 18, 
            label = "Weekly wages of a good mechanic",
            size = 2.5,
            angle = 10,
            alpha = .5,
            family = "serif") +
  ## Timeline of English rulers ---------
  geom_line(aes(year, ruler_y),
            linewidth = .4) +
  geom_line(aes(year, ruler_y, 
                group = ruler_label1),
            linewidth = 2) +
  geom_text(aes(x = ruler_lab_x, y = ruler_lab_y - 2,
                label = ruler_label1),
            data = ruler_labels,
            lineheight = .8,
            size = 2,
            fontface = "bold",
            family = "serif") +
 ## Centuries -------------------
  # Labels
  geom_text(data = century_labels,
            aes(x = c_x, y = c_y,
                label = labels),
            lineheight = .8,
            size = 2,
            fontface = "bold",
            family = "serif") +
  # Arches
  geom_curve(x = 1600, y = 100, xend = 1565, yend= 104, curvature = 0.2) +
  geom_curve(x = 1700, y = 100, xend = 1600, yend= 100, curvature = 0.2) +
  geom_curve(x = 1800, y = 100, xend = 1700, yend= 100, curvature = 0.2) +
  geom_curve(x = 1830, y = 104, xend = 1800, yend= 100, curvature = 0.3)  + 
  # Nº1
  geom_text(aes(x = 1700, y = 103),
            label = "Nº1",
            family = "serif") + 
## Overall appearance -------------------
  # Graph border
  geom_rect(xmin = 1565, xmax = 1830,
              ymin = 0, ymax = 100,
              fill = NA,
              color = "black") +
  # Axes 
  scale_y_continuous(
      sec.axis = dup_axis( name="Price ofd the quarter of wheat in Schillings"),
      breaks = seq(10,100, 10),
      limits = c(0, 105),
      name = NULL
    ) +
    scale_x_continuous(
      breaks = seq(1565, 1830, 35),
      limits = c(1565, 1830),
      name = "5 years each division"
    ) +
  
  theme_minimal() +
    theme(axis.title.y.right = element_text(angle =90),
          panel.grid = element_blank(),
          panel.ontop = F,
          legend.position = NULL,
          text = element_text(family = "serif"),
          axis.text.y = element_text(size = 10,
                                     family = "serif",
                                     face = "bold",
                                     hjust = 2.3),
          axis.text.x = element_text(vjust = 10,
                                     size = 8,
                                     face = "bold"),
          axis.title.x = element_text(size = 7,
                                      face = "italic"),
          axis.title.y = element_text(face = "italic") )   +
  # Major grid ----------------------
geom_segment(aes( x= x, y = y, xend = xend, yend = yend ),
             data = grid_hM %>% filter(y < 60)) +
  geom_segment(aes( x= x, y = y, xend = xend, yend = yend ),
               data = grid_vM %>% 
                 mutate(yend = if_else(x < 1670, 50,
                                       yend)))  

# Saving ------------------------------------------------------------------

ggsave("./slides/inputs/playfair_homework_amazing.png",
       width = 9,
       height = 6)


  