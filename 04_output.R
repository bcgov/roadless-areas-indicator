# Copyright 2017 Province of British Columbia
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

source("header.R")

library(ggplot2)
library(purrr)
library(envreportutils)
library(rasterVis)

#Read in intermediate data sets and set variables if required
# EcoRegRastS <- raster(file.path(dataOutDir,"EcoRegRast.tif"), format="GTiff")
# ProvRastS <- raster(file.path(dataOutDir,"ProvRast.tif"), format="GTiff")
# areaIN<-res(ProvRastS)[1]*res(ProvRastS)[2]/10000 #e.g. for 200m grid 4 ha

#define some categorical variables and plotting labels based on distance breaks
DistanceCls<-c(0,1,2,3)
DistLbls<-c('0-500','500-5000','>5000')
CumLbls<-gsub(".*-","<",DistLbls)

#Set up a standard set of colours for graphs and maps
nclr<-length(DistLbls)
# col_vec<-c(brewer.pal(nclr,"Greens"))
col_vec<-c('gray61','lightgreen','forestgreen')

# Generate a list of rasters, one for each strata
# Prepare ecoregions by removing marine then intersecting with bc boundary 
Strata <- bcmaps::ecoregions() %>% # from bcmaps
  filter(!ECOREGION_CODE %in% c("HCS", "IPS", "OPS", "SBC", "TPC")) %>% 
  st_intersection(bc_bound_hres())

SrataName <- "ECOREGION_NAME"

## raster_by_poly with parallelization from Andy Teucher:
# Generate a list of rasters, one for each strata, put Province at front of list
rbyp_par <- raster_by_poly(EcoRegRastS, Strata, SrataName, parallel = TRUE)
## Add the province to the list and name it
rbyp_par<-c(ProvRastS,rbyp_par)
names(rbyp_par)[1] <- 'Province'

# Generate summary of the province and each ecoregion
rbyp_par_summary <- summarize_raster_list(rbyp_par)

# Check if there is data in strata, if none then drop strata from list
rbyp_par<-rbyp_par[lapply(rbyp_par_summary,length)>0]
rbyp_par_summary<-rbyp_par_summary[lapply(rbyp_par_summary,length)>0] 

#write out summaries for output routine
saveRDS(rbyp_par, file = "tmp/rbyp_par")
saveRDS(rbyp_par_summary, file = "tmp/rbyp_par_summary")
rbyp_par<-readRDS(file = "tmp/rbyp_par")
rbyp_par_summary<-readRDS(file = "tmp/rbyp_par_summary")

## Make a data frame of ecoregion metrics of distance to roads
## purrr::map_df is like purrr::map in that it loops over a list/vector,
## but assumes that each iteration is either a list of the same length,
## or a data.frame, and at the end combines it all into one data.frame
ecoreg_summary <- map_df(rbyp_par_summary, ~ {
  xDF <- data.frame(Distance = .x,
                    distance_class = cut(.x, breaks = DistanceCls, 
                                         labels = DistLbls),
                                         area_ha = areaIN)
  #Group by Distance Class 
  xDF %>%
    dplyr::select(distance_class, area_ha) %>%
    group_by(distance_class)  %>%
    summarise(area_ha=sum(area_ha)) %>% 
    mutate(percent_in_distance_class = area_ha/sum(area_ha)*100, 
           roaded_class = factor(ifelse(distance_class == "0-500", "Roaded", "Not Roaded")))
    }, .id = "name")

#### FUNCTIONS
#A set of functions that will be called for displaying table, map and graphs

ggmap_strata <- function(strata) {
  e <- extent(strata)
  loc <- c(e[1] - 2, e[3] - 2, e[2] + 2, e[4] + 2)
  get_map(loc, maptype = "satellite")
}

#Mapping function - removed tile and legend
RdClsMap<-function(dat, Lbl, MCol, title="", plot_gmap = FALSE, legend = FALSE, 
                   n_classes = 3, max_px = 1000000) {
  if (n_classes == 2) {
    dat[dat == 3] <- 2
    Lbl <- c(Lbl[1], ">500m")
    MCol <- MCol[c(1,3)]
  }
  
  if (plot_gmap) {
    dat <- projectRaster(dat, crs = CRS("+proj=longlat +datum=WGS84"))
    gmap <- ggmap_strata(dat)
    gg_start <- ggmap(gmap) + rasterVis::gplot(dat, maxpixels = max_px)
    ext <- extent(dat)
    coords <- coord_cartesian(xlim = c(ext@xmin, ext@xmax), 
                              ylim = c(ext@ymin, ext@ymax), 
                              expand = TRUE)
  } else {
    coords <- coord_fixed()
    gg_start <- rasterVis::gplot(dat, maxpixels = max_px)
  }
  gg_start + 
    geom_raster(aes(fill=factor(value)), alpha=0.8) +
    coords + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(labels = Lbl, values = MCol) +
    labs(fill = "Distance to Roads") + 
    theme_minimal() + 
    theme(
      axis.text=element_blank(),
      axis.title=element_blank(),
      legend.position=ifelse(legend, "bottom", "none"),
      panel.grid = element_blank()
    )
}

#Graphing function - from the summarized data
# plotCummulativeFn = function(data, Yvar, ScaleLabels, title){
#   ggplot(data, aes(x = distance_class, y = Yvar, fill=distance_class)) +
#     scale_fill_manual(values=col_vec) +
#     geom_bar(stat="identity") +
#     geom_text(label=paste(round(Yvar,2),'%',sep=''),  vjust = -0.25, size=3, alpha=0.8) +
#     scale_x_discrete(label=ScaleLabels) +
#     theme(legend.position="none") +
#     theme(axis.text.x = element_text(face="bold", size=6),
#           axis.text.y = element_text(face="bold", size=10)) +
#   ylab("% Area") +
#   xlab(title) 
# }

# Similar to plotCumulativeFn, but allows to specify if using two or three classes 
# i.e., roaded/not roaded vs <500, 500-5000, >5000
strata_barchart <- function(data, labels, colours, n_classes = 3) {
  if (n_classes == 2) {
    data <- data %>% 
      mutate(distance_class = factor(ifelse(distance_class == "0-500", 
                                             "Roaded", "Not Roaded"))) %>% 
      group_by(distance_class) %>% 
      summarize(percent_in_distance_class = sum(percent_in_distance_class), 
                area_ha = sum(area_ha))
    
    colours <- colours[c(3,1)]
    x_lab <- ""
  } else {
    x_lab <- "Distance to roads (m)"
  }
  
  ggplot(data, aes(x = distance_class, y = percent_in_distance_class, fill=distance_class)) +
    geom_bar(stat="identity", alpha = 0.8) +
    scale_fill_manual(values=colours) +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() + 
    geom_text(label = paste(format(round(data$area_ha), big.mark = ","),"ha"),  
              hjust = ifelse(data$area_ha < max(data$area_ha) * 0.8, -0.1, 1.1), 
              size = 4) +
    theme_soe() + 
    theme(legend.position="none", 
          panel.grid.major.y = element_blank(), 
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)) +
    labs(y = "% Area", x = x_lab)
}

###### END of FUNCTIONS

# Read in patch table and print table and a simple plot  
PatchGroup<-read_csv(file.path(dataOutDir,"PatchGroup.csv"))
print(PatchGroup)
plot(PatchGroup$Npatch, type='l')

#Loop through each strata and generate a map and a bar chart
plot_list <- imap(rbyp_par, ~ {
  ## .x is the object itself (the raster), .y is the name
  print(.y)
#Call graph function for distance and cummulative distance
  # plotCumm<-plotCummulativeFn(xDFGroup2, xDFGroup2$distCumCls, CumLbls, 'Cumulative Distance Class')
  xDFGroup2 <- filter(ecoreg_summary, name == .y)
  # plotDist<-plotCummulativeFn(xDFGroup2, xDFGroup2$percent_in_distance_class, DistLbls, 'Distance Class')
  strata_plot <- strata_barchart(xDFGroup2, colours = col_vec, n_classes = 2)

  plotMap<-RdClsMap(.x, DistLbls, col_vec, title=.y, 
                    plot_gmap = FALSE, legend = FALSE, n_classes = 2)
  
  # Save in a list
  list(map = plotMap, 
       barchart = strata_plot)

})

# walk loops over a list and executes functions but doesn't return anything to the 
# environment. Good for plotting
walk(plot_list, ~ {
  plot(.x$map)
  plot(.x$barchart)
})

# Save the list of plots and the ecoregion summary 
saveRDS(plot_list, file = "tmp/plotlist.rds")
write_csv(ecoreg_summary, "out/data/ecoreg_summary.csv")

#save pngs of plots:
for (n in names(plot_list)) {
  print(n)
  barchart <- plot_list[[n]]$barchart
  barchart_fname <- file.path(figsOutDir, paste0(n, "_barchart.svg"))
  barchart_pv_name <- file.path(figsOutDir, paste0(n, "_barchart.png"))
  map <- plot_list[[n]]$map
  map_fname <- file.path(figsOutDir, paste0(n, "_map.png"))
  svg_px(file = barchart_fname, width = 500, height = 500)
  plot(barchart)
  dev.off()
  png_retina(file = barchart_pv_name, width = 500, height = 500, units = "px")
  plot(barchart)
  dev.off()
  png_retina(filename = map_fname, width = 500, height = 500, units = "px")
  plot(map)
  dev.off()
}

#summary of results

bc_area_summary <- ecoreg_summary %>% 
  filter(name == "Province") %>% 
  group_by(name, roaded_class) %>% 
  mutate(total_area=sum(area_ha),
         total_perc=sum(percent_in_distance_class)) %>% 
  filter(distance_class != ">5000") %>% 
  mutate(distance_class = recode(distance_class, "500-5000" = ">500")) %>% 
  ungroup() %>% 
  dplyr::select(-area_ha, -percent_in_distance_class) 
bc_area_summary


ecoregion_area_summary <- ecoreg_summary %>% 
  filter(name != "Province") %>% 
  group_by(name, roaded_class) %>% 
  mutate(total_area=sum(area_ha),
         total_perc=sum(percent_in_distance_class)) %>% 
  filter(distance_class != ">5000") %>% 
  mutate(distance_class = recode(distance_class, "500-5000" = ">500")) %>% 
  ungroup() %>% 
  dplyr::select(-area_ha, -percent_in_distance_class) %>% 
  filter(roaded_class == "Not Roaded")
ecoregion_area_summary

more50 <- ecoregion_area_summary %>% count(total_perc > 50)
less25 <- ecoregion_area_summary %>% count(total_perc < 25)
