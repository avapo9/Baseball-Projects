# Dartmouth Baseball 
# Rapsodo Pitch Locations

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(ggpubr)

pitcher_rapsodo <- read_csv("Dartmouth Baseball 24-25\\albertoct27_rapsodo.csv")

strike_zone <- data.frame(
  x = c(-8.0, 0.0, 8.0, -8.0, 0.0, 8.0, -8.0, 0.0, 8.0),
  y = c(16, 16, 16, 29, 29, 29, 42, 42, 42),
  zone = c(7, 8, 9, 4, 5, 6, 1, 2, 3))
strike_zone
#creating the strike zone with 9 different zones and turning it into the data frame

zone_graph<- ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 4, xmax = x + 4, ymin = y - 6.5, ymax = y + 6.5, fill = as.factor(zone))) +
  geom_point(aes(x = - 4, y = 40), size = 2.5) +
  scale_fill_manual(values = rainbow(9)) +
  coord_fixed() +
  theme_void() +
  labs(title = "Baseball Strike Zone Check", fill = "Zone")
zone_graph

background_colors <-c("grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95")

pitch_type_colors <- sample(colors(), length(unique(pitcher_rapsodo$Pitch_Type)))

#### ALL SWING ####
all_swings <- pitcher_rapsodo %>%
  ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 4, xmax = x + 4, ymin = y - 6.5, ymax = y + 6.5, fill = as.factor(zone))) +
  geom_rect(data = strike_zone, aes(xmin = x - 4, xmax = x + 4, ymin = y - 6.5, ymax = y + 6.5), color = "black", fill = NA) +
  geom_point(aes(x = Strike_Zone_Side, y = Strike_Zone_Height, color = Pitch_Type), size = 2.5) +
  scale_color_manual(values = pitch_type_colors) +
  scale_fill_manual(values = background_colors) +
  coord_fixed() +
  theme_void() +
  labs(title = "All Swings", color = "Pitch Type", fill = "Zone") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")
all_swings

#### MERGING ####

pitcher_swings <- read_csv("Dartmouth Baseball 24-25\\albertoct27.csv")

pitcher_swings %>%
  count(result)

full_pitcher <- full_join(pitcher_rapsodo, pitcher_swings, by = "Pitch_num")

#### WHIFFS ####

swings_and_misses <- full_pitcher %>%
  filter(result == "swing and miss") %>%
  ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 4, xmax = x + 4, ymin = y - 6.5, ymax = y + 6.5, fill = as.factor(zone))) +
  geom_rect(data = strike_zone, aes(xmin = x - 4, xmax = x + 4, ymin = y - 6.5, ymax = y + 6.5), color = "black", fill = NA) +
  geom_point(aes(x = Strike_Zone_Side, y = Strike_Zone_Height, color = Pitch_Type), size = 2.5) +
  scale_color_manual(values = pitch_type_colors) +
  scale_fill_manual(values = background_colors) +
  coord_fixed() +
  theme_void() +
  labs(title = "WHIFFS", color = "Pitch Type", fill = "Zone") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")
swings_and_misses


#### SPIN ####

# Define radii for the concentric circles
radii <- c(2600, 2500, 2400, 2300, 2200, 2100, 2000, 1900, 1800)
theta <- seq(pi / 2, 2 * pi + pi / 2, length.out = 100)

# Create data frames for each radius and store them in a list
concentric_circles <- lapply(radii, function(r) {
  data.frame(x = r * cos(theta), y = r * sin(theta))
})

# Main radius of the clock
radius <- 2700
circle_data <- data.frame(x = radius * cos(theta), y = radius * sin(theta))

# Create data for the hour markers (12 points around the clock face)
hour_angles <- seq(pi / 2, 2 * pi + pi / 2, length.out = 13)
hour_markers <- data.frame(x = radius * cos(hour_angles), y = radius * sin(hour_angles))

# Positions for clock labels (12, 3, 6, and 9)
label_positions <- data.frame(
  x = c(0, radius * 1.15, 0, -radius * 1.15),
  y = c(radius * 1, 0, -radius * 1.25, 0),
  label = c("12", "3", "6", "9")
)

# Convert Spin_Direction to radians and calculate point coordinates
full_pitcher$Spin_Direction_Rad <- with(full_pitcher, {
  sapply(Spin_Direction, function(time) {
    time_parts <- strsplit(time, ":")[[1]]
    hour <- as.numeric(time_parts[1])
    minute <- as.numeric(time_parts[2])
    (pi / 2) - (2 * pi * (hour + minute / 60) / 12)
  })
})
full_pitcher$point_x <- full_pitcher$Total_Spin * cos(full_pitcher$Spin_Direction_Rad)
full_pitcher$point_y <- full_pitcher$Total_Spin * sin(full_pitcher$Spin_Direction_Rad)

spin <- ggplot() +
  # Draw each concentric circle
  lapply(concentric_circles, function(circle_data) {
    geom_path(data = circle_data, aes(x = x, y = y), color = "lightgray", size = 0.5)
  }) +
  # Draw the main outline of the clock
  geom_path(data = circle_data, aes(x = x, y = y), color = "black", size = 1) +
  # Draw the hour markers
  geom_point(data = hour_markers, aes(x = x, y = y), color = "black", size = 2) +
  # Add labels for 12, 3, 6, and 9
  geom_text(data = label_positions, aes(x = x, y = y, label = label), 
            size = 5, vjust = -0.5, fontface = "bold") +
  # Plot points based on Total_Spin and Spin_Direction
  geom_point(data = full_pitcher, aes(x = point_x, y = point_y, color = Pitch_Type), size = 2.5) +
  scale_color_manual(values = pitch_type_colors) +
  coord_fixed() +
  theme_void() +
  labs(title = "Spin Direction", color = "Pitch Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")
spin


bullpens <- read_csv("Dartmouth Baseball 24-25\\albertbullpen.csv")

bullpens$Spin_Direction_Rad <- with(bullpens, {
  sapply(Spin_Direction, function(time) {
    time_parts <- strsplit(time, ":")[[1]]
    hour <- as.numeric(time_parts[1])
    minute <- as.numeric(time_parts[2])
    (pi / 2) - (2 * pi * (hour + minute / 60) / 12)
  })
})

bullpens$point_x <- bullpens$Total_Spin * cos(bullpens$Spin_Direction_Rad)
bullpens$point_y <- bullpens$Total_Spin * sin(bullpens$Spin_Direction_Rad)

pitch_type_colors_bullpen <- sample(colors(), length(unique(bullpens$Pitch_Type)))

spin_bullpens <- ggplot() +
  # Draw each concentric circle
  lapply(concentric_circles, function(circle_data) {
    geom_path(data = circle_data, aes(x = x, y = y), color = "lightgray", size = 0.5)
  }) +
  # Draw the main outline of the clock
  geom_path(data = circle_data, aes(x = x, y = y), color = "black", size = 1) +
  # Draw the hour markers
  geom_point(data = hour_markers, aes(x = x, y = y), color = "black", size = 2) +
  # Add labels for 12, 3, 6, and 9
  geom_text(data = label_positions, aes(x = x, y = y, label = label), 
            size = 5, vjust = -0.5, fontface = "bold") +
  # Plot points based on Total_Spin and Spin_Direction
  geom_point(data = full_pitcher, aes(x = point_x, y = point_y, color = Pitch_Type), size = 2.5) +
  scale_color_manual(values = pitch_type_colors_bullpen) +
  coord_fixed() +
  theme_void() +
  labs(title = "Spin Direction", color = "Pitch Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")
spin_bullpens
