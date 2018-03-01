# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(ggplot2)
library(tidygraph)
library(ggraph)
library(forcats)
library(envreportutils)
library(dplyr)

dir.create("roadless_figs", showWarnings=FALSE)

# Make some example data ------------------------------------------------

Ecoregion <- "CCRM"
Distance <- c("<500", "500-5000", ">5000")
Category <- c("Land with\nRoads", "Roadless\nLand", "Roadless\nLand")
Percent <- c(26.46, 54.32, 19.22)
Area <- c(985885, 2023710, 716251)

CCRM <- data.frame(Ecoregion,Distance,Category,Percent,Area)

# Bar charts -----------------------------------------------------------

# plot colours and labels
colrs <- c("<500" = "gray61",
           "500-5000" = "lightgreen",
           ">5000" = "forestgreen")

nmes <- c("<500" = "Less than 500 metres",
          "500-5000" = "More than 500 metres",
          ">5000" = "More than 5 kilometers")

# plot order
distance.order <- c(">5000","500-5000","<500")
CCRM$Distance <- factor(CCRM$Distance, levels = distance.order)

# stacked bar chart
bar_plot_stacked <- CCRM %>% 
  ggplot(aes(x=Category, y=Percent)) +
  geom_col(aes(fill = Distance), alpha = 0.7) +
  scale_fill_manual(values = colrs, name = "Distance to\nNearest Road", labels = nmes) +
  theme_soe() +
  # coord_flip() +
  labs(x = "", y = "Area (%)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(5, 5, 5, 5), "mm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        #    legend.position = c(.27,.67),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"))
plot(bar_plot_stacked)

png_retina(filename = "roadless_figs/roadless_land_barchart_stacked.png", width = 500, height = 500, units = "px", type = "cairo-png")
plot(bar_plot_stacked)
dev.off()

# 3 bar bar chart

bar_plot <- CCRM %>% 
  ggplot(aes(x=fct_rev(Distance), y=Percent)) +
  geom_col(aes(fill = Distance), alpha = 0.7, show.legend=FALSE) +
  scale_fill_manual(values = colrs) +
  theme_soe() +
  # coord_flip() +
  labs(x = "Distance to Roads (Meters)", y = "Area (%)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(5, 5, 5, 5), "mm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        #    legend.position = c(.27,.67),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"))
plot(bar_plot)

png_retina(filename = "roadless_figs/roadless_land_barchart.png", width = 500, height = 500, units = "px", type = "cairo-png")
plot(bar_plot)
dev.off()


# Sunburst plots -----------------------------------------------------------------

# General example from blog post:
# From https://medium.com/optima-blog/create-basic-sunburst-graphs-with-ggplot2-7d7484d92c61


# plot colours and labels
colrs_sunburst <- c("<500" = "gray61",
                    ">500" = "lightgreen",
                    ">5000" = "forestgreen")

nmes_sunburst <- c("<500" = "Less than 500 metres",
                   ">500" = "More than 500 metres",
                   ">5000" = "More than 5 kilometers")


# nested sunburst plot
df1 <- CCRM %>% summarise(total_area=sum(Area))

layer1 <- df1 %>% 
  ggplot() + # foundation
  geom_bar(aes(x=1, y=total_area), fill='white', stat='identity')

layer1 + coord_polar("y") 

df2 <- CCRM %>% 
  group_by(Category) %>% 
  mutate(total_area=sum(Area)) %>% 
  mutate(total_area = replace(total_area, Distance == ">5000", NA)) %>% 
  # filter(Distance != ">5000") %>% 
  mutate(Distance = recode(Distance, "500-5000" = ">500")) %>% 
  mutate(Distance = fct_rev(Distance))

layer2 <- layer1 +
  geom_bar(data = df2, aes(x=2, y=total_area, fill=Distance),
           stat='identity', position='stack', size=0.6) +
  scale_fill_manual(values = colrs_sunburst) 
# geom_text(data=df2, aes(label=Distance, x=2, y=total_area/2),
#          position='stack', colour = "black")

layer2 + coord_polar("y") +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        line = element_blank())

df3 <- CCRM %>% 
  mutate(Distance = recode(Distance, "500-5000" = ">500")) %>% 
  mutate(Area = replace(Area, Distance == "<500", NA),
         Area = replace(Area, Distance == ">500", NA))

layer3 <- layer2 +
  geom_bar(data = df3, aes(x=3, y=Area, fill=Distance),
           stat='identity', position='stack', size=0.6) 
# geom_text(data=df3, aes(label=Category, x=3, y=Area/2),
#           position='stack', colour = "white")

sunburst_nested <- layer3 + coord_polar("y") +
  scale_fill_manual(values = colrs_sunburst, "Distance to\nNearest Road", labels = nmes_sunburst) +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        line = element_blank())
sunburst_nested

png_retina(filename = "roadless_figs/roadless_land_sunburst_nested.png", width = 500, height = 500, units = "px", type = "cairo-png")
plot(sunburst_nested)
dev.off()

# sunbirst plot

colrs_sunburst <- c("<500" = "gray61",
                    ">500" = "lightgreen",
                    ">5000" = "forestgreen")

nmes_sunburst <- c("<500" = "Less than 500 metres",
                   ">500" = "More than 500 metres",
                   ">5000" = "More than 5 kilometers")


df4 <- CCRM %>% 
  mutate(Distance = recode(Distance, "500-5000" = ">500")) %>% 
  mutate(Distance = fct_rev(Distance))

layer4 <- layer1 +
  geom_bar(data = df4, aes(x=2, y=Area, fill=Distance),
           stat='identity', position='stack', size=0.6) +
  scale_fill_manual(values = colrs_sunburst)
# geom_text(data=df2, aes(label=Distance, x=2, y=total_area/2),
#          position='stack', colour = "black")

sunburst <- layer4 + coord_polar("y") +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        line = element_blank())
sunburst

png_retina(filename = "roadless_figs/roadless_land_sunburst.png", width = 500, height = 500, units = "px", type = "cairo-png")
plot(sunburst)
dev.off()
