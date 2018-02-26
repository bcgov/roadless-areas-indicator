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

RoadDens<-raster(file.path(DataDir,"RoadDensR.tif"))

#Rasterize the Province for subsequent masking
ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5, ymn=173787.5,ymx=1748187.5,crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",res=c(100,100),vals=0)
#BCr<-rasterize((bcmaps.rdata::bc_bound_hres),ProvRast,mask=TRUE)
BCr<-raster(file.path(dataOutDir,"BCr.tif"))

#crop RoadDens to ProvRast, will mask later
RoadDensP100<-crop(RoadDens,ProvRast)

#write out processed rasters
#writeRaster(BCr, filename=file.path(dataOutDir,"BCr.tif"), format="GTiff", overwrite=TRUE)
#writeRaster(RoadDensP100, filename=file.path(dataOutDir,"RoadDensP100.tif"), format="GTiff", overwrite=TRUE)
