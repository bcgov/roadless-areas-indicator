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

library(dplyr)
library(ggplot2)
library(devtools)
library(tidyverse)
library(rgdal)
library(RColorBrewer)
library(gapminder)
library(gridExtra)
#library(pryr)
library(grid)
library(gridGraphics)
library(rasterVis)
library(igraph)

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
col_vec<-c(brewer.pal(nclr,"Greens"))
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
names(rbyp_par_summary)[1]<-'Province'

#Check if there is data in strata, if none then drop strata from list
rbyp_par<-rbyp_par[lapply(rbyp_par_summary,length)>0]
rbyp_par_summary<-rbyp_par_summary[lapply(rbyp_par_summary,length)>0] 
#write out summaries for output routine

saveRDS(rbyp_par, file = "tmp/rbyp_par")
saveRDS(rbyp_par_summary, file = "tmp/rbyp_par_summary")

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
RdClsMap<-function(dat, Lbl, MCol, title="", plot_gmap = FALSE, legend = FALSE){
  
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
  
  gg_start +
    geom_raster(data = dat_pts, aes(x = x, y = y, fill=factor(roadsSC, labels=Lbl),
                                    colour = NULL), alpha=0.5) +
    #ggtitle(title)+
    coords + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values= MCol
    #                  name= "Distance Class"
                      # ,
                      # guide = guide_legend(
                      #   keyheight = unit(2, units = "mm"),
                      #   keywidth = unit(70/length(labels), units = "mm"),
                      #   title.position = 'top',
                      #   title.hjust = 0.5,
                      #   label.hjust = 1,
                      #   nrow = 1,
                      #   byrow = T,
                      #   reverse = T,
                      #   label.position = "bottom"
                      # )
                      ) +
    labs(fill = "Distance to Roads") + 
    theme_minimal() + 
    theme(
      #plot.title = element_text(size = 24, colour = "black"),
      axis.text=element_blank(),
      axis.title=element_blank(),
      legend.position=ifelse(legend, "bottom", "none"),
      panel.grid = element_blank()
      #legend.key.height=unit(2,"line"),
      #legend.key=element_blank(),
      #legend.text=element_text(size = 24, colour = "black"),
      #legend.title=element_text(size = 24, colour = "black")
    )
}

#Graphing function - from the summarized data
plotCummulativeFn = function(data, Yvar, ScaleLabels, title){
  ggplot(data, aes(x = DistCls, y = Yvar, fill=DistCls)) +
    scale_fill_manual(values=col_vec) +
    geom_bar(stat="identity") +
    geom_text(label=paste(round(Yvar,2),'%',sep=''),  vjust = -0.25, size=3, alpha=0.8) +
    scale_x_discrete(label=ScaleLabels) +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(face="bold", size=6),
          axis.text.y = element_text(face="bold", size=10)) +
  ylab("% Area") +
  xlab(title) }

###### END of FUNCTIONS

# Read in patch table and print table and a simple plot  
PatchGroup<-read_csv(file.path(dataOutDir,"PatchGroup.csv"))
print(PatchGroup)
plot(PatchGroup$Npatch, type='l')

#Loop through each strata and generate a png of summary table, map and graphs
j<-25 #issues with this map will check high res to see if persists
plot_list <- lapply(seq_along(1:3), function(j) {
    
  #map of strata - clip raster to strata extent and colour consistent with graphs
  Strata1<-rbyp_par[[j]]
  
  #get the name of the strata for plotting
  StrataName<-names(rbyp_par_summary[j])
  
  #Subset Provincial preprocessed distance map for plotting
  RdClsdf<-Strata1
  #Quick check on percent that is un-roaded
  #tt<-freq(RdClsdf)*areaIN
  #pSum<-sum(tt[,2])-tt[5,2]
  #round(tt[3,2]/pSum*100,2)
  
#Make a data frame of the strata info
  xDF<-data.frame(Distance=rbyp_par_summary[[j]],
            DistCls=cut(rbyp_par_summary[[j]], breaks = DistanceCls, labels=DistLbls),#, right=FALSE, include.lowest=TRUE),
             AreaHa=areaIN
            ) 

#Group by Distance Class 
  xDFGroup<-xDF %>%
    dplyr::select(DistCls, Distance, AreaHa) %>%
    group_by(DistCls)  %>%
    summarise(AreaHa=sum(AreaHa), Distance=sum(Distance))
#Calculate percent of area in each class 
  xDFGroup<-mutate(xDFGroup, pcDistCls=AreaHa/sum(AreaHa)*100)
#Calculate cummulative percent and area  
  nCases<-length(unique(xDF$DistCls))
  totArea<-sum(xDFGroup$AreaHa)
  distCumCls<-NULL
  areaCumCls<-NULL
  for (i in 1:nCases) {
    distCumCls<-c(distCumCls,(sum(xDF$Distance>DistanceCls[i]))/nrow(xDF)*100)
    areaCumCls<-c(areaCumCls,distCumCls[i]*totArea/100)
  }
#Merge all the data into a single data frame.  
  xDFGroup2<-cbind(xDFGroup,distCumCls,areaCumCls)
  
#Create a table object of the data frame
  tblIN<-data.frame(Distance=xDFGroup2$DistCls, pcDistance=round(xDFGroup2$pcDistCls,2), AreaDistance=round(xDFGroup2$AreaHa,2), pcCumDistance=round(xDFGroup2$distCumCls,2), AreaCumDistance=round(xDFGroup2$areaCumCls,2) )
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)), padding=unit(c(1, 1), "mm"))
  tbl <- tableGrob(format(tblIN,big.mark=","), rows=NULL, theme=tt)
   
#Call graph function for distance and cummulative distance
  plotCumm<-plotCummulativeFn(xDFGroup2, xDFGroup2$distCumCls, CumLbls, 'Cumulative Distance Class')
  plotDist<-plotCummulativeFn(xDFGroup2, xDFGroup2$pcDistCls, DistLbls, 'Distance Class')

#Map of distances
  #Set variables for passing to the mapping function
  nUnique<-length(unique(RdClsdf))
  Lbl<-DistLbls[1:nUnique]
  MapCol<-col_vec[1:nUnique]
 
 #Test plot with ice, water, roads
 #col_vec<-c('gray61','lightgreen','forestgreen','darkblue','gray32')
 #plot(RdClsdf)
 #plot(Water, add=TRUE,col='blue')
 #plot(Ice,add=TRUE,col='gray32')
 #lines(roads_sf,col='red')
 
  if (StrataName == "Province") {
    plotMap<-RdClsMap(RdClsdf,Lbl,MapCol, title=StrataName, 
                      plot_gmap = FALSE, legend = FALSE)
  } else {
    plotMap<-RdClsMap(RdClsdf,Lbl,MapCol, title=StrataName, 
                      plot_gmap = TRUE, legend = TRUE)
  }
  
#write Strata to a pdf: table, map, distance and cummulative graphs
  # png(file=file.path(figsOutDir,paste0(StrataName,"_Graphs.png")))
  #    lay <- rbind(c(1,1,2,2), c(1,1,2,2),c(3,3,3,3))
  #    #Alternatives to grid.arrange patchwork and cowplot
  #    grid.arrange(plotDist, plotCumm, tbl, layout_matrix=lay,top=names(rbyp_par_summary[j]))
  #    #+theme(plot.margin=unit(c(1,1,1,1), "cm"))
  #    dev.off()
  #  
    # envreportutils::png_retina(file=file.path(figsOutDir,paste0(StrataName,".png")))
    plotMap
    # dev.off()
})
