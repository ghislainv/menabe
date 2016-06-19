##=====================================================
## Deforestation in central Menabe, Madagascar
## Ghislain Vieilledent <ghislain.vieilledent@cirad.fr>
## June 2016
##=====================================================

##= Libraries
library(sp)
library(raster)
library(rgdal)
library(rgrass7)

## GRASS GIS 7.x.x is needed to run this script
## https://grass.osgeo.org/

## Create some directories
dir.create("gisdata") ## To save GIS data

## Download forest cover
f <- c("for1990","for2000","for2010","for2014","forest2050")
for (i in 1:length(f)) {
  d <- paste0("http://bioscenemada.net/FileTransfer/",f[i],".tif")
  download.file(url=d,destfile=paste0("gisdata/",f[i],".tif"),method="wget",quiet=TRUE)
}

## Create new grass location in UTM 38S
dir.create("grassdata")
system("grass70 -c epsg:32738 grassdata/menabe")
## Connect R to grass location
initGRASS(gisBase="/usr/local/grass-7.0.1",home=tempdir(), 
          gisDbase="grassdata",
          location="menabe",mapset="PERMANENT",
          override=TRUE)
## Import rasters in grass
for (i in 1:length(f)) {
   system(paste0("r.in.gdal input=gisdata/",f[i],".tif output=",f[i]))
}

## Set region for Kirindy-Mitea National Park and Menabe-Antimena NAP
Extent.KMNP <- c(365000,7640000,430000,7730000)
Extent.MENAP <- c(419600,7750744,478890,7834872) # A voir
## system("g.region rast= -ap")

