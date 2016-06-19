##=====================================================
## Deforestation in central Menabe, Madagascar
## Ghislain Vieilledent <ghislain.vieilledent@cirad.fr>
## June 2016
##=====================================================

##= Libraries
library(sp)
library(rgdal)
library(raster)

## Create some directories
dir.create("gisdata/rast",recursive=TRUE) ## To save new raster data

## Download forest cover
f <- c("for1990","for2000","for2010","for2014","forest2050")
for (i in 1:length(f)) {
  d <- paste0("http://bioscenemada.net/FileTransfer/",f[i],".tif")
  download.file(url=d,destfile=paste0("gisdata/rast_raw",f[i],".tif"),method="wget",quiet=TRUE)
}

##========================================
## Prepare rasters for the two study areas

## Set region for Kirindy-Mitea National Park
Extent.KMNP <- "365000 7640000 430010 7730000"

## gdalwrap
for (i in 1:length(f)) {
  system(paste0("gdalwarp -overwrite -ot Byte \\
          -r near -tr 30 30 -te ",Extent.KMNP," -of GTiff \\
          -co 'compress=lzw' -co 'predictor=2' \\
          gisdata/rast_raw/",f[i],".tif \\
          gisdata/rast/",f[i],"_KMNP.tif"))
}

# Import into R
for1990 <- raster("gisdata/rast/for1990_KMNP.tif")
plot(for1990)

## Set region for Menabe-Antimena NAP
Extent.MENAP <- c(419600,7750744,478890,7834872) # A voir
## system("g.region rast= -ap")

