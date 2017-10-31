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

require(raster)
require(bcmaps)

#A couple of methdos for testing buffering
#Common parameters
bufferRad<-5000

#Method 1 - using raster pkg buffer

ptm <- proc.time()

rd<-RoadDensP
#Set 0 to NA
rd[rd[]==0]=NA
rdB1<-buffer(rd,(bufferRad-50))

writeRaster(rdB1, file=paste(dataOutDir,"RastBuf.tif",sep=''), format="GTiff",overwrite=TRUE)
proc.time() - ptm 

#Method 2 - using focal function
#Adapted from: https://gis.stackexchange.com/questions/188413/how-to-generate-a-buffer-outside-polygons-having-a-given-pixel-value

ptm <- proc.time()

rd<-RoadDensP
radius <- ceiling((bufferRad-50) / min(res(rd))) #Reduce buffer by 50m since road raster is 100m wide with 1ha cells
diameter <- 2*radius + 1
i <- outer(seq(-radius,radius), seq(-radius,radius), function(a,b) a^2+b^2) > radius^2
w <- matrix(1, diameter, diameter)
w[i] <- 0
w <- w / sum(w)
#With this in hand, the raster operations are fast:

#Apply focal function using neighbourhood 'w'
r.focal <- focal(rd>0, w)
#Overlay r.forcal and original raster rd using function where
#if rd>0 then use rd, else if r.focal>0 then set to 1 otherwise 0
rdB2 <- overlay(r.focal, rd, fun=function(x, y) ifelse(y > 0, 1, ifelse(x > 0, 2, 0)))
writeRaster(rdB2, file=paste(dataOutDir,"FocalBuf",".tif",sep=''), format="GTiff",overwrite=TRUE)

proc.time() - ptm 

#Method 3 using distance function
ptm <- proc.time()

rd<-RoadDensP
#set rd to NA if 0
rd1 <- rd
rd1[rd1 == 0] <- NA
rdB3 <- overlay(distance(rd1) <= (bufferRad-50), rd1, fun=function(x,y) ifelse(y > 0, y,2*x))

writeRaster(rdB3, file=paste(dataOutDir,"EquildBuf",".tif",sep=''), format="GTiff",overwrite=TRUE)

proc.time() - ptm 

#e<-extract(RoadDensP,ecoregions,df=TRUE)

