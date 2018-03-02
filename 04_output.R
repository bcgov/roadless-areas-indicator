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
library(RColorBrewer)
library(gridExtra)
library(ggplot2)
library(purrr)
library(envreportutils)
library(readr)

#library(dplyr)
#library(devtools)
#library(tidyverse)
#library(rgdal)
#library(gapminder)
#library(pryr)
#library(grid)
#library(gridGraphics)
#library(rasterVis)
#library(igraph)

#Set/Read in provincial map
roadsSC <- raster(file.path(dataOutDir,"roadsSC.tif"), format="GTiff")
#Get ha of each grid cell based on cell size
areaIN<-res(roadsSC)[1]*res(roadsSC)[2]/10000 #e.g. for 200m grid 4 ha

#Read in ice and water for evaluating using them as part of the mapping
#IceWaterIn <- mask(raster(file.path(DataDir,"IceWater.tif"), format="GTiff"),BCr)
#IceWater<-aggregate(IceWaterIn, fact=16, fun=mean)
#Ocean<-(IceWater==1)*6
#Ocean[Ocean==0]<- NA
#Ice<-(IceWater==3)*5
#Ice[Ice==0]<-NA
#Water<-(IceWater==2)*4
#Water[Water==0]<-NA
#Rd[Rd==0]<-NA
#roads_sf<-readRDS(file = "data/DRA_roads_sf_clean.rds")

#define some categorical variables and plotting labels based on distance breaks
DistanceCls<-c(0,1,2,3)
DistLbls<-c('0-500','500-5000','>5000')
CumLbls<-gsub(".*-","<",DistLbls)

#Set up a standard set of colours for graphs and maps
nclr<-length(DistLbls)
# col_vec<-c(brewer.pal(nclr,"Greens"))
col_vec<-c('gray61','lightgreen','forestgreen')

## raster_by_poly with parallelization from Andy Teucher:
#Generate a list of rasters, one for each strata - Slow for entire Province
#strata to evaluate
#Strata <- bcmaps::ecosections(class = "sp") # from bcmaps
#SrataName <-"ECOSECTION_NAME"
Strata <- bcmaps::ecoregions() %>% 
  st_intersection(bc_bound_hres()) %>% 
  as("Spatial")# from bcmaps

SrataName <- "ECOREGION_NAME"
#save strata as a shape for checking
#writeOGR(obj=Strata, dsn=dataOutDir, layer="Strata", driver="ESRI Shapefile") # this is in geographical projection

## raster_by_poly with parallelization from Andy Teucher:
#Generate a list of rasters, one for each strata - Slow for entire Province
rbyp_par <- raster_by_poly(roadsSC, Strata, SrataName, parallel = TRUE)
rbyp_par<-c(roadsSC,rbyp_par)
rbyp_par_summary <- summarize_raster_list(rbyp_par)
names(rbyp_par)[1] <- names(rbyp_par_summary)[1] <- 'Province'


#Check if there is data in strata, if none then drop strata from list
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
                                         labels = DistLbls),#, right=FALSE, include.lowest=TRUE),
                    area_ha = areaIN) 
  
  #Group by Distance Class 
  xDFGroup<-xDF %>%
    dplyr::select(distance_class, area_ha) %>%
    group_by(distance_class)  %>%
    summarise(area_ha=sum(area_ha)) %>% 
    mutate(percent_in_distance_class = area_ha/sum(area_ha)*100, 
           roaded_class = factor(ifelse(distance_class == "0-500", 
                                        "Roaded", "Not Roaded")))
}, .id = "name")

#clean up the workspace
gc()

#### FUNCTIONS
#A set of functions that will be called for displaying table, map and graphs

ggmap_strata <- function(strata) {
  e <- extent(strata)
  loc <- c(e[1] - 2, e[3] - 2, e[2] + 2, e[4] + 2)
  
  get_map(loc, maptype = "satellite")
}

#Mapping function - removed tile and legend
RdClsMap<-function(dat, Lbl, MCol, title="", plot_gmap = FALSE, legend = FALSE, n_classes = 3){

  # dat_poly <- spTransform(dat_poly, "+init=epsg:4326")
  # dat_poly@data$id <- 1:nrow(dat_poly@data)
  # dat_df <- fortify(dat_poly)
  # dat_df <- merge(dat_df, dat_poly@data, by.x = "id", by.y = "id")
  
  if (plot_gmap) {
    dat <- projectRaster(dat, crs = CRS("+proj=longlat +datum=WGS84"))
    dat_pts <- data.frame(rasterToPoints(dat))
    dat_pts$roadsSC <- round(dat_pts$roadsSC)
    gmap <- ggmap_strata(dat)
    gg_start <- ggmap(gmap)
    coords <- coord_cartesian(xlim = range(dat_pts$x), ylim = range(dat_pts$y), expand = TRUE)
  } else {
    dat_pts <- data.frame(rasterToPoints(dat))
    gg_start <- ggplot()
    coords <- coord_fixed()
  }
  
  if (n_classes == 2) {
    dat_pts$roadsSC[dat_pts$roadsSC == 3] <- 2
    Lbl <- c(Lbl[1], ">500m")
    MCol <- MCol[c(1,3)]
  }
  
  gg_start +
    geom_raster(data = dat_pts, aes(x = x, y = y, fill=factor(roadsSC, labels=Lbl),
                                    colour = NULL), alpha=0.8) +
    coords + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values= MCol) +
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
plotCummulativeFn = function(data, Yvar, ScaleLabels, title){
  ggplot(data, aes(x = distance_class, y = Yvar, fill=distance_class)) +
    scale_fill_manual(values=col_vec) +
    geom_bar(stat="identity") +
    geom_text(label=paste(round(Yvar,2),'%',sep=''),  vjust = -0.25, size=3, alpha=0.8) +
    scale_x_discrete(label=ScaleLabels) +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(face="bold", size=6),
          axis.text.y = element_text(face="bold", size=10)) +
  ylab("% Area") +
  xlab(title) 
}

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
    scale_fill_manual(values=colours) +
    geom_bar(stat="identity") +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() + 
    geom_text(label=paste(format(round(data$area_ha), big.mark = ","),"Ha"),  hjust = 1.1, size=3.5) +
    theme_soe() + 
    theme(legend.position="none", 
          panel.grid.major.y = element_blank()) +
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
  
# #Calculate cummulative percent and area  
#   nCases<-length(unique(xDF$DistCls))
#   totArea<-sum(xDFGroup$AreaHa)
#   distCumCls<-NULL
#   areaCumCls<-NULL
#   for (i in 1:nCases) {
#     distCumCls<-c(distCumCls,(sum(xDF$Distance>DistanceCls[i]))/nrow(xDF)*100)
#     areaCumCls<-c(areaCumCls,distCumCls[i]*totArea/100)
#   }
# #Merge all the data into a single data frame.  
#   xDFGroup2<-cbind(xDFGroup,distCumCls,areaCumCls)
  
#Create a table object of the data frame
  # tblIN<-data.frame(Distance=xDFGroup2$DistCls, pcDistance=round(xDFGroup2$pcDistCls,2), AreaDistance=round(xDFGroup2$AreaHa,2), pcCumDistance=round(xDFGroup2$distCumCls,2), AreaCumDistance=round(xDFGroup2$areaCumCls,2) )
  # tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)), padding=unit(c(1, 1), "mm"))
  # tbl <- tableGrob(format(tblIN,big.mark=","), rows=NULL, theme=tt)
   
#Call graph function for distance and cummulative distance
  # plotCumm<-plotCummulativeFn(xDFGroup2, xDFGroup2$distCumCls, CumLbls, 'Cumulative Distance Class')
  xDFGroup2 <- filter(ecoreg_summary, name == .y)
  # plotDist<-plotCummulativeFn(xDFGroup2, xDFGroup2$percent_in_distance_class, DistLbls, 'Distance Class')
  strata_plot <- strata_barchart(xDFGroup2, colours = col_vec, n_classes = 2)

#Map of distances

 #Test plot with ice, water, roads
 #col_vec<-c('gray61','lightgreen','forestgreen','darkblue','gray32')
 #plot(.x)
 #plot(Water, add=TRUE,col='blue')
 #plot(Ice,add=TRUE,col='gray32')
 #lines(roads_sf,col='red')
 
  plotMap<-RdClsMap(.x, DistLbls, col_vec, title=.y, 
                    plot_gmap = FALSE, legend = FALSE, n_classes = 2)
  
  # Save in a list
  list(map = plotMap, 
       barchart = strata_plot)

}, .id = "name")

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
  map <- plot_list[[n]]$map
  map_fname <- file.path(figsOutDir, paste0(n, "_map.png"))
  barchart_fname <- file.path(figsOutDir, paste0(n, "_barchart.svg"))
  p <- barchart + theme(axis.text = element_text(size = 14),
                   axis.title = element_text(size = 16))
  svg_px(file = barchart_fname, width = 500, height = 500)
  plot(p)
  dev.off()
  png_retina(filename = map_fname, width = 500, height = 500, units = "px")
  plot(map)
  dev.off()
}

