##=====================================================
## Deforestation in central Menabe, Madagascar
## Ghislain Vieilledent <ghislain.vieilledent@cirad.fr>
## June 2016
##=====================================================

##= Libraries
library(sp)
library(rgdal)
library(raster)
## library(plyr)
library(ggplot2)
library(broom) ## to convert map into data-frame with tiny()
## library(cowplot) ## for theme_nothing()
library(gridExtra)
## library(maptools) ## to plot vectors with ggplot2
library(rasterVis) ## for gplot()

## Create some directories
dir.create("gisdata/rast",recursive=TRUE) ## To save new raster data

## Download forest cover
f <- c("for1990","for2000","for2010","for2014","forest2050")
for (i in 1:length(f)) {
  d <- paste0("http://bioscenemada.net/FileTransfer/",f[i],".tif")
  download.file(url=d,destfile=paste0("gisdata/rast_raw/",f[i],".tif"),method="wget",quiet=TRUE)
}

##========================================
## Prepare rasters for the two study areas

## Set region for Kirindy-Mitea National Park (KMNP)
xmin.KMNP <- 365000; xmax.KMNP <- 430010
ymin.KMNP <- 7640000; ymax.KMNP <- 7730000
Extent.KMNP <- paste(xmin.KMNP,ymin.KMNP,xmax.KMNP,ymax.KMNP)

## Set region for Menabe Antimena New Protected Area (MANAP)
xmin.MANAP <- 419600; xmax.MANAP <- 478890
ymin.MANAP <- 7750744; ymax.MANAP <- 7834872
Extent.MANAP <- paste(xmin.MANAP,ymin.MANAP,xmax.MANAP,ymax.MANAP)

## gdalwrap
for (i in 1:length(f)) {
  system(paste0("gdalwarp -overwrite -ot Byte \\
          -r near -tr 30 30 -te ",Extent.KMNP," -of GTiff \\
          -co 'compress=lzw' -co 'predictor=2' \\
          gisdata/rast_raw/",f[i],".tif \\
          gisdata/rast/",f[i],"_KMNP.tif"))
}
for (i in 1:length(f)) {
  system(paste0("gdalwarp -overwrite -ot Byte \\
          -r near -tr 30 30 -te ",Extent.MANAP," -of GTiff \\
          -co 'compress=lzw' -co 'predictor=2' \\
          gisdata/rast_raw/",f[i],".tif \\
          gisdata/rast/",f[i],"_MANAP.tif"))
}

##=======================
## Import into R for KMNP
for2000 <- raster("gisdata/rast/for2000_KMNP.tif")
for2010 <- raster("gisdata/rast/for2010_KMNP.tif")
for2014 <- raster("gisdata/rast/for2014_KMNP.tif")
for2050 <- raster("gisdata/rast/forest2050_KMNP.tif")
## One raster for deforestation
defor_KMNP <- for2000
defor_KMNP[defor_KMNP==1 & is.na(for2010)] <- 2
defor_KMNP[defor_KMNP==1 & is.na(for2014)] <- 3
## One raster for projections
proj_KMNP <- for2010
proj_KMNP[proj_KMNP==1 & is.na(for2050)] <- 2

##========================
## Import into R for MANAP
for2000 <- raster("gisdata/rast/for2000_MANAP.tif")
for2010 <- raster("gisdata/rast/for2010_MANAP.tif")
for2014 <- raster("gisdata/rast/for2014_MANAP.tif")
for2050 <- raster("gisdata/rast/forest2050_MANAP.tif")
## One raster for deforestation
defor_MANAP <- for2000
defor_MANAP[defor_MANAP==1 & is.na(for2010)] <- 2
defor_MANAP[defor_MANAP==1 & is.na(for2014)] <- 3
## One raster for projections
proj_MANAP <- for2010
proj_MANAP[proj_MANAP==1 & is.na(for2050)] <- 2

##=================================================================================
## Import maps (shapefiles) and convert to data-frame for ggplot with bloom::tiny()

## SAPM ("système d'aires protégées à Madagascar")
sapm <- readOGR(dsn="gisdata/vectors/sapm",layer="AP-NAP_38s")
sapm.df <- tidy(sapm)

## Madagascar boundaries
mada.latlong <- readOGR(dsn="gisdata/vectors/mada",layer="MAD_outline")
proj4string(mada.latlong) <- "+init=epsg:4326"
mada <- spTransform(mada.latlong,CRSobj=CRS("+init=epsg:32738"))
mada.df <- tidy(mada)

## Roads
roads.latlong <- readOGR(dsn="gisdata/vectors/roads",layer="tr_route_polyline")
roads <- spTransform(roads.latlong,CRSobj=CRS("+init=epsg:32738"))
roads.df <- tidy(roads)

## Localities and field observations
Belo <- readOGR(dsn="gisdata/vectors/additional_points",layer="Belo")
Lambokely_Kirindy <- readOGR(dsn="gisdata/vectors/additional_points",layer="Lambokely_Kirindy_Village")
Morondava_BeloTsi <- readOGR(dsn="gisdata/vectors/additional_points",layer="Morondava_BeloTsi")
Obs <- readOGR(dsn="gisdata/vectors/additional_points",layer="Obs")
## df for localities and field observations
Belo.df <- as.data.frame(Belo)
Obs.df <- as.data.frame(Obs)
Lambokely_Kirindy.df <- as.data.frame(Lambokely_Kirindy)
Morondava_BeloTsi.df <- as.data.frame(Morondava_BeloTsi)

##========================================
## Plot raster with gplot() from rasterVis

## New theme for ggplot
theme_defor <- function(plot.margin=unit(c(0,0,-0.5,-0.5),"line")) {
  theme_bw() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.margin=plot.margin)
}

## KMNP
# Build deforestation plot
plot.defor.KMNP <- gplot(defor_KMNP,maxpixels=10e5) + 
  annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="(a)",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen","orange","red")) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_line(data=roads.df, aes(x=long, y=lat, group=id), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3500) +
  geom_point(data=Obs.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Obs.df, aes(label=Obs), size=3, hjust=1, nudge_x=-1000) +
  coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_defor(plot.margin=unit(c(0,0.2,0,0),"cm"))
# Build projection plot
plot.proj.KMNP <- gplot(proj_KMNP,maxpixels=10e5) + 
  annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="(b)",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen",grey(0.5))) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_line(data=roads.df, aes(x=long, y=lat, group=id), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3500) +
  coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_defor(plot.margin=unit(c(0,0,0,0.2),"cm"))
# Grid plot
plot.KMNP <- grid.arrange(plot.defor.KMNP, plot.proj.KMNP, ncol=2)
ggsave(filename="figs/KMNP.png",plot=plot.KMNP,width=14,height=10,unit=c("cm"))

## MANAP
# Build deforestation plot
plot.defor.MANAP <- gplot(defor_MANAP,maxpixels=5e4) + 
  annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="(a)",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen","orange","red")) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_line(data=roads.df, aes(x=long, y=lat, group=id), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
  geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
  coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_defor(plot.margin=unit(c(0,0.2,0,0),"cm"))
# Build projection plot
plot.proj.MANAP <- gplot(proj_MANAP,maxpixels=5e4) + 
  annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="(b)",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen",grey(0.5))) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_line(data=roads.df, aes(x=long, y=lat, group=id), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
  geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
  coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_defor(plot.margin=unit(c(0,0,0,0.2),"cm"))
# Grid plot
plot.MANAP <- grid.arrange(plot.defor.MANAP, plot.proj.MANAP, ncol=2)
ggsave(filename="figs/MANAP.png",plot=plot.MANAP,width=14,height=10,unit=c("cm"))
