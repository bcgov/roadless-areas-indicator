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
install.packages('bcmaps.rdata', repos='https://bcgov.github.io/drat/')
#Link to Provincial maps from bcgov/bcmaps - to do area summaries
#library(devtools)
#install_github("bcgov/bcmaps", build_vignettes = TRUE)
#install.packages('bcmaps.rdata', repos='https://bcgov.github.io/drat/')
library(bcmaps)

#define some categorical variables and plotting labels based on distance breaks
reclCls<-c(0,100,1, 100,250,2 ,250,500,3 ,500,1000,4 ,1000,2000,5 ,2000,5000,6 ,5000,10000,7)
DistanceCls<-c(0,100,250,500,1000,2000,5000,1000000)
DistLbls<-c('0-100','100-250','250-500','500-1000','1000-2000','2000-5000','5000-10000')
CumLbls<-gsub(".*-","<",DistLbls)
#CumLbls<-c('>0','>100','>250','>500','>1000','>2000','>5000')

#Set up a standard set of colours for graphs and maps
nclr<-7
col_vec<-c(brewer.pal(nclr,"RdYlGn"))
areaIN<-0.25 #for 50m grid 1/4 ha

#input raster, add 50 to the value since each each road cell is 
#100m (50 each side of road), 0 buffer is actually 50m
roads <- distRdsR+50
#strata to evaluate
Strata <- bcmaps::ecosections(class = "sp") # from bcmaps
SrataName <-"ECOSECTION_NAME"

#Crop strata to raster extent for testing
ClipS<-crop(Strata,roads)

## raster_by_poly with parallelization from Andy Teucher:
#Generate a list of rasters, one for each strata
#Slow for entire Province
rbyp_par <- raster_by_poly(roads, ClipS, SrataName, parallel = TRUE)
rbyp_par_summary <- summarize_raster_list(rbyp_par)

#### FUNCTIONS
#Create a set of functions that will be called for displaying table, map and graphs

#Mapping function
RdClsMap<-function(dat, Lbl, MCol, title=""){
  gplot(dat)+geom_tile(aes(fill=factor(value, labels=Lbl )), alpha=0.8) +
    ggtitle(title)+
    coord_equal()+ 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values= MCol, name= "Distance Class") +
    theme(
      axis.text=element_blank(),
      axis.title=element_blank()
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

#Loop through each strata and generate a pdf of summary table, map and graphs
j<-1
for (j in 1:length(rbyp_par_summary)) {
  
  #map of strata - clip raster to strata extent and colour consistent with graphs
  Strata1<-rbyp_par[[j]]
  #get the name of the strata for plotting
  StrataName<-names(rbyp_par_summary[j])
  #Reclass the roads for plotting
  recl<-matrix(reclCls,ncol=3,byrow=TRUE)
  Rdcls<-reclassify(Strata1, rcl=recl)
  
#Make a data frame of the strata info
  xDF<-data.frame(Distance=rbyp_par_summary[[j]],
            DistCls=cut(rbyp_par_summary[[j]], breaks = DistanceCls, labels=DistLbls),
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
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(tblIN, rows=NULL, theme=tt)
  
#Call graph function for distance and cummulative distance
  plotCumm<-plotCummulativeFn(xDFGroup2, xDFGroup2$distCumCls, CumLbls, 'Cumulative Distance Class')
  plotDist<-plotCummulativeFn(xDFGroup2, xDFGroup2$pcDistCls, DistLbls, 'Distance Class')

#Map of distances
  #Set variables for passing to the mapping function
  nUnique<-length(unique(Rdcls))
  Lbl<-DistLbls[1:nUnique]
  MapCol<-col_vec[1:nUnique]
  plotMap<-RdClsMap(Rdcls,Lbl,MapCol, title=StrataName)
  
#write Strata to a pdf: table, map, distance and cummulative graphs
  pdf(file=paste(figsOutDir,names(rbyp_par_summary[j]),".pdf",sep=""))
    t <- textGrob(names(rbyp_par_summary[j]))
    lay <- rbind(c(1,1,1,1), c(1,1,1,1), c(2,2,3,3), c(4,4,4,4))
    grid.arrange(plotMap+theme(plot.margin=unit(c(1,1,1,1), "cm")), plotDist, plotCumm, tbl, layout_matrix=lay)
  dev.off()
}

pl = replicate(3, ggplot(), FALSE)
grid.arrange(grobs = pl) 

marg = theme(plot.margin = unit(c(2,2,2,2), "cm"))
grid.arrange(grobs = lapply(pl, "+", marg))

+theme(plot.margin=unit(c(1,1,1,1), "cm"))
