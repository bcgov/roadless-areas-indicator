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
require(SpaDES)

#Common parameters
nTiles<-10
Tilebuf<-100

#set RoadDensP as a binary road/no-road
Rd<-RoadDensM50>0

#Set the timer
ptm <- proc.time()

#R was crashing with large data sets, use tiling to resolve and to increase speed
#tiles are written to the tileOutDir and held in memory.
#To remove edge effects a 100 (5000/5km) cell buffer is used since 
#all areas greater than 5K are considered intact.
tileOutDir<-paste(dataOutDir,"Ptiles/",sep='')
RdTiles=splitRaster(Rd, nx=sqrt(nTiles), ny=sqrt(nTiles), buffer=c(Tilebuf,Tilebuf), path=tileOutDir)

#Use mapply to apply gridDistance over RdTiles then merge them back together
dT<-mapply(gridDistance, RdTiles, origin=1)
distRdsR<-mergeRaster(dT)

#write out raster for further inspection
writeRaster(distRdsR, filename=paste(tileOutDir,"PdistRdsR_",Tilebuf,".tif",sep=''), format="GTiff", overwrite=TRUE)

proc.time() - ptm 

####TEST Loop - Loop was used but was slower than using mapply
#for (i in 1:nTiles) {
#  distRdsR<-gridDistance(RdTiles[[i]], origin=1)
#  writeRaster(distRdsR, filename=paste(tileOutDir,"PdistRdsR_",i,"_",buf,".tif",sep=''), format="GTiff", overwrite=TRUE)
#  gc()
#  }
####END of TEST

####TEST small data set - do gridDistance on test data
Rd<-RoadDensM50>0
distRdsR<-gridDistance(Rd, origin=1)

writeRaster(distRdsR, filename=paste(tileOutDir,"MdistRdsR_",Tilebuf,".tif",sep=''), format="GTiff", overwrite=TRUE)
