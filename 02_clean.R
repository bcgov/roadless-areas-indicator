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

require(SpaDES)

#Common parameters
nTiles<-100
Tilebuf<-100

tileOutDir<-file.path(dataOutDir,"Ptiles")

#set RoadDensP as a binary road/no-road
#RoadDensP100<-raster(file.path(dataOutDir,"RoadDensP100.tif"))
Rd<-RoadDensP100>0

ptm <- proc.time()
#To remove edge effects a 100 (5000/5km) cell buffer is used
RdTiles=splitRaster(Rd, nx=sqrt(nTiles), ny=sqrt(nTiles), buffer=c(Tilebuf,Tilebuf), path=paste(tileOutDir,'/',sep=''))

#Use mapply to apply gridDistance over RdTiles
dT<-mapply(gridDistance, RdTiles, origin=1)
dTmerge<-mergeRaster(dT)

#Set all non-terrestiral area to NA
distRdsR<-mask(dTmerge, BCr)

proc.time() - ptm 
gc()

#write out raster for further inspection
writeRaster(distRdsR, filename=file.path(tileOutDir,"distRdsR.tif"), format="GTiff", overwrite=TRUE)
