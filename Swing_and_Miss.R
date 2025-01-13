## Data Visualization (GOVT16-QSS17) Winter 2024
## Project 2, The final project
##
## Name: Ava Politis
## Date: March 8-15, 2024

#loading all libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(ggpubr)

pitcher <- read_csv("data/albert021824.csv")
#getting data

strike_zone <- data.frame(
  x = c(-1.0, 0.0, 1.0, -1.0, 0.0, 1.0, -1.0, 0.0, 1.0),
  y = c(-0.75, -0.75, -0.75, 0.75, 0.75, 0.75, 2.25, 2.25, 2.25),
  zone = c(7, 8, 9, 4, 5, 6, 1, 2, 3))
strike_zone
#creating the strike zone with 9 different zones and turning it into the data frame

zone_graph<- ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75, fill = as.factor(zone))) +
  scale_fill_manual(values = rainbow(9)) +
  coord_fixed() +
  theme_void() +
  labs(title = "Baseball Strike Zone Check", fill = "Zone")
zone_graph

swing_miss_counter <- pitcher %>%
  count(Swing)
swing_miss_counter
# no swing 85, swing and foul 30, swing and hit 16, swing and miss 15

pitch_counter <- pitcher %>%
  count(Pitch_Type)
pitch_counter
# fastball = 1, curveball = 2, slider = 3, changeup = 4

zone_one_fastball <- pitcher %>%
  filter(Zone == 1 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_one_fastball
# 0 %

zone_two_fastball <- pitcher %>%
  filter(Zone == 2 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_two_fastball
# 0 %

zone_three_fastball <- pitcher %>%
  filter(Zone == 3 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_three_fastball
# 0 %

zone_four_fastball <- pitcher %>%
  filter(Zone == 4 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_four_fastball
# 0 %

zone_five_fastball <- pitcher %>%
  filter(Zone == 5 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_five_fastball
# 50 %

zone_six_fastball <- pitcher %>%
  filter(Zone == 6 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_six_fastball
# 0 %

zone_seven_fastball <- pitcher %>%
  filter(Zone == 7 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_seven_fastball
# 0 %

zone_eight_fastball <- pitcher %>%
  filter(Zone == 8 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_eight_fastball
# 50 %

zone_nine_fastball <- pitcher %>%
  filter(Zone == 9 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_nine_fastball
# 25 %

zone_zero_fastball <- pitcher %>%
  filter(Zone == 0 & Pitch_Type == 1) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_zero_fastball
# 4.55%

fastball_colors <-c("grey95", "grey95", "grey95", "grey95", "#EE8083", "grey95", "grey95", "#EE8083", "#F3CCCD")

fastball <- pitcher %>%
  ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75, fill = as.factor(zone))) +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75), color = "black", fill = NA) +
  geom_text(data = NULL, aes(x = -1, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -0.75, label = "50%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = -0.75, label = "25%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 0.75, label = "50%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -1.75, label = "Outside the Zone: \n 4.55%"),
            color = "black", size = 3) +
  scale_fill_manual(values = fastball_colors) +
  coord_fixed() +
  theme_void() +
  labs(title = "Fastball", fill = "Zone") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
fastball
#fastball data

zone_one_curveball <- pitcher %>%
  filter(Zone == 1 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_one_curveball
# 0 %

zone_two_curveball <- pitcher %>%
  filter(Zone == 2 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_two_curveball
# 25 %

zone_three_curveball <- pitcher %>%
  filter(Zone == 3 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_three_curveball
# 66.7 %

zone_four_curveball <- pitcher %>%
  filter(Zone == 4 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_four_curveball
# 0 %

zone_five_curveball <- pitcher %>%
  filter(Zone == 5 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_five_curveball
# 0 %

zone_six_curveball <- pitcher %>%
  filter(Zone == 6 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_six_curveball
# 0 %

zone_seven_curveball <- pitcher %>%
  filter(Zone == 7 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_seven_curveball
# 0 %

zone_eight_curveball <- pitcher %>%
  filter(Zone == 8 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_eight_curveball
# 0 %

zone_nine_curveball <- pitcher %>%
  filter(Zone == 9 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_nine_curveball
# 0 %

zone_zero_curveball <- pitcher %>%
  filter(Zone == 0 & Pitch_Type == 2) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_zero_curveball
# 25%

curveball_colors <-c("grey95", "#F3CCCD", "#EE8089", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95")

curveball <- pitcher %>%
  ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75, fill = as.factor(zone))) +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75), color = "black", fill = NA) +
  geom_text(data = NULL, aes(x = -1, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 2.25, label = "25%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 2.25, label = "66.7%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -1.75, label = "Outside the Zone: \n 25%"),
            color = "black", size = 3) +
  scale_fill_manual(values = curveball_colors) +
  coord_fixed() +
  theme_void() +
  labs(title = "Curveball", fill = "Zone") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
curveball
#curveball data

zone_one_slider <- pitcher %>%
  filter(Zone == 1 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_one_slider
# 0 %

zone_two_slider <- pitcher %>%
  filter(Zone == 2 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_two_slider
# 0 %

zone_three_slider <- pitcher %>%
  filter(Zone == 3 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_three_slider
# 0 %

zone_four_slider <- pitcher %>%
  filter(Zone == 4 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_four_slider
# 0 %

zone_five_slider <- pitcher %>%
  filter(Zone == 5 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_five_slider
# 0 %

zone_six_slider <- pitcher %>%
  filter(Zone == 6 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_six_slider
# 0 %

zone_seven_slider <- pitcher %>%
  filter(Zone == 7 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_seven_slider
# 0 %

zone_eight_slider <- pitcher %>%
  filter(Zone == 8 & Pitch_Type == 3) %>%
  count(Swing) %>%
  mutate(perc = n/sum(n)*100)
zone_eight_slider
# 0 %

zone_nine_slider <- pitcher %>%
  filter(Zone == 9 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_nine_slider
# 0 %

zone_zero_slider <- pitcher %>%
  filter(Zone == 0 & Pitch_Type == 3) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_zero_slider
# 12.5 %

slider_colors <-c("grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95")

slider <- pitcher %>%
  ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75, fill = as.factor(zone))) +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75), color = "black", fill = NA) +
  geom_text(data = NULL, aes(x = -1, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -1.75, label = "Outside the Zone: \n 12.5%"),
            color = "black", size = 3) +
  scale_fill_manual(values = slider_colors) +
  coord_fixed() +
  theme_void() +
  labs(title = "Slider", fill = "Zone") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
slider
#slider data

zone_one_changeup <- pitcher %>%
  filter(Zone == 1 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_one_changeup
# 0 %

zone_two_changeup <- pitcher %>%
  filter(Zone == 2 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_two_changeup
# 0 %

zone_three_changeup <- pitcher %>%
  filter(Zone == 3 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_three_changeup
# 0 %

zone_four_changeup <- pitcher %>%
  filter(Zone == 4 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_four_changeup
# 0 %

zone_five_changeup <- pitcher %>%
  filter(Zone == 5 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_five_changeup
# 0 %

zone_six_changeup <- pitcher %>%
  filter(Zone == 6 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_six_changeup
# 0 %

zone_seven_changeup <- pitcher %>%
  filter(Zone == 7 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_seven_changeup
# 0 %

zone_eight_changeup <- pitcher %>%
  filter(Zone == 8 & Pitch_Type == 4) %>%
  count(Swing) %>%
  mutate(perc = n/sum(n)*100)
zone_eight_changeup
# 0 %

zone_nine_changeup <- pitcher %>%
  filter(Zone == 9 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_nine_changeup
# 0 %

zone_zero_changeup <- pitcher %>%
  filter(Zone == 0 & Pitch_Type == 4) %>%
  count(Swing)%>%
  mutate(perc = n/sum(n)*100)
zone_zero_changeup
# 0 %

changeup_colors <-c("grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95", "grey95")

changeup <- pitcher %>%
  ggplot() +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75, fill = as.factor(zone))) +
  geom_rect(data = strike_zone, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.75, ymax = y + 0.75), color = "black", fill = NA) +
  geom_text(data = NULL, aes(x = -1, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = -0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 0.75, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = -1, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 1, y = 2.25, label = "0%"),
            color = "black", size = 4) +
  geom_text(data = NULL, aes(x = 0, y = -1.75, label = "Outside the Zone: \n 0%"),
            color = "black", size = 3) +
  scale_fill_manual(values = changeup_colors) +
  coord_fixed() +
  theme_void() +
  labs(title = "Changeup", fill = "Zone") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
changeup
#changeup data

all_pitches <- grid.arrange(fastball, curveball, slider, changeup, ncol = 2)
#combining all plots

title <- textGrob("Swing and Miss Percentages in each Zone per Pitch", gp = gpar(fontsize = 16))
subtitle <- textGrob("Data is from Dartmouth Baseball Pitchers pitching an entire 9 innings", gp = gpar(fontsize = 12))
caption <- textGrob("Zone 1: Top left, Zone 2: Top middle, Zone 3: Top right \n Zone 4: Middle left, Zone 5: Middle middle, Zone 6: Middle right \n Zone 7: Bottom left, Zone 8: Bottom middle, Zone 9: Bottom right \n The redder the zone, the higher the swing and miss percentage.", 
                    x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = c("center", "bottom"), gp = gpar(fontsize = 10))
#adding text to the graph to explain information shown

final_plot <- arrangeGrob(grobs = list(title, subtitle, all_pitches, caption), heights = c(0.5,0.52, 14, 1))
grid.draw(final_plot)
#final graphing