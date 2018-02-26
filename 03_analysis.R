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
#
distRdsR<-raster(paste(tileOutDir,"/distRdsR.tif",sep=''), format="GTiff")+50
roadsS<-distRdsR

#define the distance class breaks 
#reclCls<-c(0,500,1, 500,1000,2 ,1000,2000,3 ,2000,5000,4 ,5000,1000000,5)
reclCls<-c(0,500,1, 500,5000,2 ,5000,1000000,3)
#minimum intact clump size
minClump<-2000

### TESTING - aggregate to coarser resolution to increase speed
roadsAgg<-aggregate(distRdsR, fact=16, fun=mean) 
roadsS <- roadsAgg #for testing
###

#Set the timer
ptm <- proc.time()

#Get ha of each grid cell based on cell size
areaIN<-res(roadsS)[1]*res(roadsS)[2]/10000 #e.g. for 200m grid 4 ha

#Reclass the Provincial surface to the desired distance class, the Province is 1 in the rbyp_par raster list
recl<-matrix(reclCls,ncol=3,byrow=TRUE)
#PRdcls<-reclassify(rbyp_par[[1]], rcl=recl, right=FALSE, include.lowest=TRUE)
PRdcls<-reclassify(roadsS, rcl=recl, right=FALSE, include.lowest=TRUE)

#Reclassify areas less than 2000ha 
#Code adapted from https://stackoverflow.com/questions/24465627/clump-raster-values-depending-on-class-attribute
r1<-PRdcls
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

# loop over all unique class values
for (i in clVal) {
  print(i)
  # create & fill in class raster
  r.class <- setValues(raster(r), NA)
  r.class[r == i]<- 1
  
  # clump class raster
  clp <- clump(r.class)
  
  # calculate frequency of each clump/patch
  cl.freq <- as.data.frame(freq(clp))
  
  # store clump ID's with frequency of minClump/areaIN - using 2000
  rmID <- cl.freq$value[which(cl.freq$count <= round(minClump/areaIN))]

  # assign NA to all clumps whose ID's have frequency 2000/areaIN
  #r.NA[clp %in% rmID] <- NA
  #set the small areas to a distance class unique identifier
  r.NA[clp %in% rmID] <- (i + nclVal) # 1  2  3  4  5  6 14 24 36 50?
  #writeRaster(r.NA, filename=paste(dataOutDir,"r.NA",i,".tif",sep=''), format="GTiff", overwrite=TRUE)
  gc()
  } 

# multiply original raster by the NA raster
r2 <- r * r.NA

# crop the originally extended raster
roadsSC <- crop(r2, r1)

# assign all clumps to a single group
roadsSC[roadsSC > nclVal] <- (nclVal+1)

writeRaster(roadsSC, filename=paste(dataOutDir,"/roadsSC.tif",sep=''), format="GTiff", overwrite=TRUE)

proc.time() - ptm 

gc()