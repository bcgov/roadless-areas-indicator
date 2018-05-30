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

#Read in road surface and Provincial mask - if required
roadsS<-raster(file.path(dataOutDir,"roadsS.tif"), format="GTiff")
BCr <- raster(file.path(dataOutDir,"BCr.tif"), format="GTiff")

#define the distance class breaks 
reclCls<-c(0,500,1, 500,5000,2 ,5000,1000000,3)
#then for patch analysis - assess those in class 2 and 3
reclPCls<-c(0,2,0,2,99,2)

### TESTING - aggregate to coarser resolution to increase speed
# roadsS<-aggregate(roadsS, fact=16, fun=mean) #For testing
# BCr<-aggregate(BCr, fact=16, fun=mean)
###

#Set the timer
ptm <- proc.time()

# Get ha of each grid cell based on cell size
areaIN<-res(roadsS)[1]*res(roadsS)[2]/10000 #e.g. for 200m grid 4 ha

# Reclass the Provincial surface to the desired distance class - 
# do not mask since the strata clip in 04_output.R will do
recl<-matrix(reclCls,ncol=3,byrow=TRUE)
EcoRegRastS<-reclassify(roadsS, rcl=recl, right=TRUE, include.lowest=TRUE)
# mask the surface for provincial reporting in 04_output.R
ProvRastS<-mask(EcoRegRastS, BCr)

#Save files in tmp directory
writeRaster(EcoRegRastS, filename=file.path(dataOutDir,"EcoRegRast.tif"), format="GTiff", overwrite=TRUE)
writeRaster(ProvRastS, filename=file.path(dataOutDir,"ProvRast.tif"), format="GTiff", overwrite=TRUE)


