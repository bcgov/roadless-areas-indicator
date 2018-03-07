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

RoadDens<-raster(file.path(DataDir,"RoadDensR.tif"))

#Rasterize the Province for subsequent masking
ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5, ymn=173787.5,ymx=1748187.5,crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",res=c(100,100),vals=0)

BCr_file <- file.path(dataOutDir,"BCr.tif")
if (!file.exists(BCr_file)) {
  BCr <- rasterize(bcmaps::bc_bound_hres(),ProvRast,mask=TRUE, filename = BCr_file)
  writeRaster(BCr, filename=file.path(dataOutDir,"BCr.tif"), format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
}

#crop RoadDens to ProvRast, will mask later
RoadDensP100<-crop(RoadDens,ProvRast)

#write out processed rasters
#writeRaster(RoadDensP100, filename=file.path(dataOutDir,"RoadDensP100.tif"), format="GTiff", overwrite=TRUE)

# Read in or create Lakes raster
LakesR_file <- file.path(dataOutDir,"LakesR.tif")

if (!file.exists(LakesR_file)) {
  # Link to BTM file download from BCDC:
  # https://catalogue.data.gov.bc.ca/dataset/baseline-thematic-mapping-present-land-use-version-1-spatial-layer
  #Dowload file manually and put *.zip in this script and place file in the data directory
  BTMZip <- 'BCGW_78757263_1520272242999_7572.zip'
  unzip(file.path(DataDir, BTMZip), exdir = file.path(DataDir, "Lakes"))
  
  # List feature classes in the geodatabase
  BTM_gdb <- list.files(file.path(DataDir, "Lakes"), pattern = ".gdb", full.names = TRUE)[1]
  fc_list <- st_layers(BTM_gdb)
  BTM <- as(read_sf(BTM_gdb, layer = "WHSE_BASEMAPPING_BTM_PRESENT_LAND_USE_V1_SVW"),'Spatial')

  # Subset out lakes
  Lakes<-subset(BTM,BTM@data$PRESENT_LAND_USE_LABEL=='Fresh Water')
  
  # Using gdal to rasterize - raster::rasterize does not handle islands in lakes 
  library(rgdal)
  library(gdalUtils)
  
  # write out a Lakes shapefile for gdal_rasterize - requires a shape file and 
  # attribute field to populate the raster
  Lakes@data$water<-1
  writeOGR(Lakes, file.path(dataOutDir,"Lakes.shp"), "Lakes",driver="ESRI Shapefile", overwrite=TRUE) 
  
  # Make and write out a blank (0) lakes raster to be populated by gdal_rasterize
  LakesR<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5, ymn=173787.5,ymx=1748187.5,
                    crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
                    res=c(100,100),vals=0)
  writeRaster(LakesR, filename=file.path(dataOutDir,"LakesR.tif"), format="GTiff", overwrite=TRUE)
  
  # gdal_rasterize requires the raster extents
  extLR<-extent(LakesR)

  # gdal_rasterize has some perculiar requirments - requires a shapefile and raster on disk
  LakesR <- (gdal_rasterize(file.path(dataOutDir,"Lakes.shp"), file.path(dataOutDir,"LakesR.tif"),
                             # extents for output raster
                             te = c(extLR[1], extLR[3], extLR[2], extLR[4]), 
                             # resolution for x and y
                             tr = res(LakesR),   
                             # target aligned pixels - align coords of extent of output to values of tr
                             tap = TRUE,
                             # attribute field for raster 'burn' and value for nodata
                             a="water", a_nodata = NA, 
                             # layer for burn (shapefile)
                             l="Lakes",
                             # return the raster to the R enviornment
                             output_Raster=TRUE,
                             # verbose and a 'raster brick' is returned but only has 1 layer which is selected
                             verbose=TRUE))[[1]]
   } else {
  LakesR <- raster(LakesR_file)
}
