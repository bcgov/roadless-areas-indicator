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

require(gdistance)
require(raster)
require(bcmaps)

#Smaller raser for testing
#ProvRast<-raster(nrows=5899, ncols=4619, xmn=634387.5, xmx=1096287.5, ymn=777687.5,ymx=1367587.5,crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5, ymn=173787.5,ymx=1748187.5,crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
                 ,res=c(100,100),vals=0)
UrbanP<-crop(Urban,ProvRast)
RoadDensP<-crop(RoadDens,ProvRast)

#Create a distance from roads layer
ptm <- proc.time()
rp<-RoadDensP>0
rp2 = rp ; rp2[rp2[]!=3]=NA
plot(rp2)
#Now we can buffer that to 100m:
  
rp2b = buffer(rp2, 100)
plot(rp2b)
#Now the ring of the buffer is where rp2 is NA and rp2b is not NA:
  
rbuff = is.na(rp2) & !is.na(rp2b)
plot(rbuff)
writeRaster(rbuff, file=paste(dataOutDir,"UrbBuf100.tif",sep=''), format="GTiff",overwrite=TRUE)
proc.time() - ptm 

#From this post: https://gis.stackexchange.com/questions/188413/how-to-generate-a-buffer-outside-polygons-having-a-given-pixel-value
bufferRad<-5000
r<-RoadDensP>0
r[is.na(r)] <- 0
ptm <- proc.time()

radius <- ceiling((bufferRad-50) / min(res(r))) #Reduce buffer by 50m since road raster is 100m wide with 1ha cells
diameter <- 2*radius + 1
i <- outer(seq(-radius,radius), seq(-radius,radius), function(a,b) a^2+b^2) > radius^2
w <- matrix(1, diameter, diameter)
w[i] <- 0
w <- w / sum(w)
#With this in hand, the raster operations are fast:
  
r.focal <- focal(r>0, w)
result <- overlay(r.focal, r, fun=function(x, y) ifelse(y > 0, y, ifelse(x > 0, 3, 0)))

r.0 <- RoadDensP
r.0[r.0 != 3] <- NA
result <- overlay(distance(r.0) <= 20, rp, fun=function(x,y) ifelse(y > 0, y, 3*x))

writeRaster(r.focal, file=paste(dataOutDir,'r.focal',(bufferRad-50),'.tif',sep=''), format="GTiff",overwrite=TRUE)
writeRaster(result, file=paste(dataOutDir,"result",(bufferRad-50),".tif",sep=''), format="GTiff",overwrite=TRUE)

proc.time() - ptm 

e<-extract(RoadDensP,ecoregions,df=TRUE)