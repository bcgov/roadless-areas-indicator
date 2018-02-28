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

#Read in road surface - add 50m since 100m road already has a 50m buffer
distRdsR<-raster(file.path(dataOutDir,"distRdsR.tif"), format="GTiff")+50
roadsS<-distRdsR

#define the distance class breaks 
reclCls<-c(0,500,1, 500,5000,2 ,5000,1000000,3)

# define the patch classes
patchCls<-c(0,1000,2000,5000,10000,50000,100000,500000,1000000,10000000,1000000000)
patchLbls<-c('0-1000','1000-2000','2000-5,000','5,000-10,000','10,000-50,000','50,000-100,000','100,000-500,000','500,000-1,000,000','1M-10M','>10M')
reclPCls<-c(0,500,1,5000,1000000,2)

### TESTING - aggregate to coarser resolution to increase speed
#roadsS<-aggregate(distRdsR, fact=16, fun=mean) #For testing
###

#Set the timer
ptm <- proc.time()

# Get ha of each grid cell based on cell size
areaIN<-res(roadsS)[1]*res(roadsS)[2]/10000 #e.g. for 200m grid 4 ha

# Reclass the Provincial surface to the desired distance class - 
# - to be used in 04_output.R for maps and graphs
recl<-matrix(reclCls,ncol=3,byrow=TRUE)
roadsSC<-reclassify(roadsS, rcl=recl, right=FALSE, include.lowest=TRUE)
#Save files in tmp directory
writeRaster(roadsSC, filename=file.path(dataOutDir,"roadsSC.tif"), format="GTiff", overwrite=TRUE)

# Calculate the patch classes for areas >500m from a road - 
# - generate a table to be sourced by the text on the frequency of small patches
# reclassify the Provincial surface to a binary of 0-500 and >500
recl<-matrix(reclPCls,ncol=3,byrow=TRUE)
roadsSC<-reclassify(roadsS, rcl=recl, right=FALSE, include.lowest=TRUE)
writeRaster(roadsSC, filename=file.path(dataOutDir,"roadsSC.tif"), format="GTiff", overwrite=TRUE)

#Calculate the patch size distribution
#Code adapted from https://stackoverflow.com/questions/24465627/clump-raster-values-depending-on-class-attribute
r1<-PRdclsP
# extend raster, otherwise left and right edges are 'touching'
r <- extend(r1, c(1,1))
# get all unique class values in the raster
clVal <- unique(r)
nclVal <- length(clVal)
# remove '0' (background)
clVal <- clVal[!clVal==0]
# create a 1-value raster, to be filled in with NA's
r.NA <- setValues(raster(r), 1)
# set background values to NA
r.NA[r==0]<- NA
# create & fill in class raster
  r.class <- setValues(raster(r), NA)
  r.class[r == 2]<- 1
  # clump class raster
  clp <- clump(r.class)
  # calculate frequency of each patch
  cl.freq <- as.data.frame(freq(clp))
  patch.freq <- data.frame(idn=cl.freq$value, areaHa=cl.freq$count*areaIN, patch=cut(cl.freq$count*areaIN,breaks=patchCls,labels=patchLbls))
 # generate summary table 
   PatchGroup<-patch.freq %>%
    dplyr::select(patch, idn, areaHa) %>%
    group_by(patch)  %>%
    summarise(AreaHa=sum(areaHa),Npatch=(n()))
# write out table as a csv
   write_csv(PatchGroup, file.path(dataOutDir,"PatchGroup.csv"))
   
proc.time() - ptm 

gc()