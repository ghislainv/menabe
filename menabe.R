##=====================================================
## Deforestation in central Menabe, Madagascar
## Ghislain Vieilledent <ghislain.vieilledent@cirad.fr>
## June 2016
##=====================================================

##= Libraries
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(rasterVis)

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

## Import into R
for2000 <- raster("gisdata/rast/for2000_KMNP.tif")
for2010 <- raster("gisdata/rast/for2010_KMNP.tif")
for2014 <- raster("gisdata/rast/for2014_KMNP.tif")

## One raster for deforestation
defor_KMNP <- for2000
defor_KMNP[defor_KMNP==1 & is.na(for2010)] <- 2
defor_KMNP[defor_KMNP==1 & is.na(for2014)] <- 3

## Plot raster with plot.raster
plot(defor_KMNP,colNA="transparent")


## Plot raster with ggplot
df <- as.data.frame(defor_KMNP,xy=TRUE)
names(df) <- c("Longitude","Latitude","cat")
df$cat <- as.factor(df$cat)
levels(df$cat)
p <- ggplot(data=df,aes(x=Longitude,y=Latitude)) + 
  geom_tile(aes(fill=cat)) +
  scale_fill_manual(values = c("forestgreen","orange","red")) +
  theme_bw() +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  coord_equal()
plot(p)

## Set region for Menabe-Antimena NAP
Extent.MENAP <- c(419600,7750744,478890,7834872) # A voir
## system("g.region rast= -ap")

