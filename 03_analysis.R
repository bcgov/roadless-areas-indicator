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

#Common parameters
bufferRad<-5000

#Adapted from: https://gis.stackexchange.com/questions/188413/how-to-generate-a-buffer-outside-polygons-having-a-given-pixel-value

ptm <- proc.time()

rd<-RoadDensP
radius <- ceiling((bufferRad-50) / min(res(rd))) #Reduce buffer by 50m since road raster is 100m wide with 1ha cells
#radius <- ((bufferRad) / min(res(rd))) #Reduce buffer by 50m since road raster is 100m wide with 1ha cells

diameter <- 2*radius+1 #needs to be an odd diameter for focal to work

i <- outer(seq(-radius,radius), seq(-radius,radius), function(a,b) a^2+b^2) > radius^2

w <- matrix(1, diameter, diameter)
w[i] <- 0
w <- w / sum(w)
#With this in hand, the raster operations are fast:

#Apply focal function using neighbourhood 'w'
r.focal <- focal(rd>0, w)
rdB2<-r.focal
#Overlay r.forcal and original raster rd using function where
#if rd>0 then use rd, else if r.focal>0 then set to 1 otherwise 0
#rdB2 <- overlay(r.focal, rd, fun=function(x, y) ifelse(y > 0, 1, ifelse(x > 0, 2, 0)))

#Resample to 100m using nearest neighbourhood approach
rdB2100ngb<-resample(rdB2,ProvRast,method='ngb') #Better method
writeRaster(rdB2100ngb, file=paste(dataOutDir,"rdB2100ngbM",".tif",sep=''), format="GTiff",overwrite=TRUE)
rdB2100bil<-resample(rdB2,ProvRast,method='bilinear')
writeRaster(rdB2100bil, file=paste(dataOutDir,"rdB2100bilM",".tif",sep=''), format="GTiff",overwrite=TRUE)
#writeRaster(rdB2100ngb, file=paste(dataOutDir,"FocalBuf50",".tif",sep=''), format="GTiff",overwrite=TRUE)

#Resample to 100m


proc.time() - ptm 
#user  system elapsed 
#2.094   0.097   2.274 
#Entire province at 50m
#user    system   elapsed 
#52363.731   616.444 53286.580 ~ 14hours

#e<-extract(RoadDensP,ecoregions,df=TRUE)

