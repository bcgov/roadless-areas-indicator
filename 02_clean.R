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
require(SpaDES)
library(parallel)
library(doMC)

#Common parameters
nTiles<-100
Tilebuf<-100

#set RoadDensP as a binary road/no-road
RoadDensP100<-raster(file.path(dataOutDir,"RoadDensP100.tif"))
Rd <- RoadDensP100 > 0

ptm <- proc.time()
#To remove edge effects a 100 (5000/5km) cell buffer is used
RdTiles=splitRaster(Rd, nx=sqrt(nTiles), ny=sqrt(nTiles), buffer=c(Tilebuf,Tilebuf), path=tileOutDir)

#Use mapply to apply gridDistance over RdTiles
dT<-mclapply(RdTiles, gridDistance, origin=1, mc.cores = 3)
dT_merge<-mergeRaster(dT)

# need to use raster SetValues to work properly - add 50m since 100m road already has a 50m buffer
roadsS<-setValues(dT_merge,values(dT_merge)) + 50

proc.time() - ptm 

gc()

#write out raster for further inspection
writeRaster(roadsS, filename=file.path(dataOutDir,"roadsS.tif"), format="GTiff", overwrite=TRUE)

