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


## This script produces a distribution of roadless area patchsizes for B.C. 
## Both 01_load.R and 02_clean.R need to be run for this script to work.

source("header.R")

#define the patch class breaks - only evaluate areas >500m from roads
reclPCls<-c(0,500,0, 500,1000000,1)
# define the patch classes for the output summar
patchCls<-c(0,1000,2000,5000,10000,50000,100000,500000,1000000,10000000,1000000000)
patchLbls<-c('0-1000','1000-2000','2000-5,000','5,000-10,000','10,000-50,000','50,000-100,000','100,000-500,000','500,000-1,000,000','1M-10M','>10M')

#Set the timer
ptm <- proc.time()

#aggregate to 4ha, system crashes with 1ha - other option was tiles, however  
#could be issues with patch metrics if using tiles
roadsS_4<-aggregate(roadsS, fact=2, fun=max) 
BCr_4<-aggregate(BCr, fact=2, fun=mean)

# reclassify the Provincial surface to a binary of 0-500 and >500 and mask with province
recl<-matrix(reclPCls,ncol=3,byrow=TRUE)
PatchRast<-mask(reclassify(roadsS_4, rcl=recl, right=FALSE, include.lowest=TRUE), BCr_4)

#Calculate the patch size distribution
#Code adapted from https://stackoverflow.com/questions/24465627/clump-raster-values-depending-on-class-attribute
r1<-PatchRast
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
r.class[r == 1]<- 1
  
  ###Testing code using tiel
  #nTiles<-100
  # set a large tile buffer - issue of large patches lost on tile boundaries
  #Tilebuf<-10000
  #ClumpTiles=splitRaster(r.class, nx=sqrt(nTiles), ny=sqrt(nTiles), buffer=c(Tilebuf,Tilebuf), path=tileOutDir)
  #clp_1<-mclapply(ClumpTiles, clump, mc.cores = 3)
  #clp<-do.call(merge,clp_1)

# Raster clump to identify 'patches'
clp<-clump(r.class)
# calculate frequency of each patch
cl.freq <- as.data.frame(freq(clp))
patch.freq <- data.frame(idn=cl.freq$value, areaHa=cl.freq$count*areaIN, patch=cut(cl.freq$count*areaIN,breaks=patchCls,labels=patchLbls))
# generate a table on the frequency of different patch classes
   PatchGroup<-patch.freq %>%
    dplyr::select(patch, idn, areaHa) %>%
    group_by(patch)  %>%
    summarise(AreaHa=sum(areaHa),Npatch=(n()))
# write out table as a csv
   write_csv(PatchGroup, file.path(dataOutDir,"PatchGroup.csv"))
   
proc.time() - ptm 

gc()