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
library(broom) ## to convert map into data-frame with tiny()
library(gridExtra) ## to combine several ggplots
library(rasterVis) ## for gplot()
library(knitr)
library(rmarkdown) 
library(rgeos) ## for crop()

##================================================
## Household income in MGA on the period 2006-2012
## Sources: The Gallup Organization and The World Bank

household.income.usd <- 1013 ## The Gallup Organization
rate <- c(2142.30,1873.88,1708.37,1956.21,2089.95,2025.12,2194.97)
year <- c(2006,2007,2008,2009,2010,2011,2012)
m.rate <- round(mean(rate))
household.income.mga <- m.rate*household.income.usd

##========================
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
e.KMNP <- extent(c(xmin.KMNP,xmax.KMNP,ymin.KMNP,ymax.KMNP))
r.KMNP <- raster(ext=e.KMNP,crs="+init=epsg:32738")
r.KMNP.latlong <- projectRaster(r.KMNP,crs="+init=epsg:4326")
e.KMNP.latlong <- extent(r.KMNP.latlong)

## Set region for Menabe Antimena New Protected Area (MANAP)
xmin.MANAP <- 419600; xmax.MANAP <- 478890
ymin.MANAP <- 7750744; ymax.MANAP <- 7834872
Extent.MANAP <- paste(xmin.MANAP,ymin.MANAP,xmax.MANAP,ymax.MANAP)
e.MANAP <- extent(c(xmin.MANAP,xmax.MANAP,ymin.MANAP,ymax.MANAP))
r.MANAP <- raster(ext=e.MANAP,crs="+init=epsg:32738")
r.MANAP.latlong <- projectRaster(r.MANAP,crs="+init=epsg:4326")
e.MANAP.latlong <- extent(r.MANAP.latlong)

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
## MANAP
## Import rasters
for1990 <- raster("gisdata/rast/for1990_MANAP.tif")
for1990_MANAP <- for1990
for2000 <- raster("gisdata/rast/for2000_MANAP.tif")
for2010 <- raster("gisdata/rast/for2010_MANAP.tif")
for2014 <- raster("gisdata/rast/for2014_MANAP.tif")
theta_MANAP <- raster("gisdata/rast_raw/theta_MANAP.tif")
## One raster for deforestation
defor_MANAP <- for2000
defor_MANAP[defor_MANAP==1 & is.na(for2010)] <- 2
defor_MANAP[defor_MANAP==1 & is.na(for2014)] <- 3
## Mean annual deforestation on the period 1990-2010
## Two scenarios: S1=1990-2010 or S2=1990-2010-2014
##===
## Conservative scenario: S1=1990-2010
defor.npix <- (sum(values(for1990)==1,na.rm=TRUE)-sum(values(for2010)==1,na.rm=TRUE))/20
defor.nha <- defor.npix*30*30/10000
## Number of pixels to be deforested on the period 2010-2050
pred.npix <- 40*defor.npix
pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
## Probability threshold
thres <- quantile(values(theta_MANAP),1-pred.prop,na.rm=TRUE)
## Forest in 2050
for2050.S1 <- for2010
for2050.S1[values(theta_MANAP)>thres] <- NA 
##===
## Worst-case scenario: S1=2000-2014
defor.npix <- (sum(values(for2000)==1,na.rm=TRUE)-sum(values(for2014)==1,na.rm=TRUE))/14
defor.nha <- defor.npix*30*30/10000
## Number of pixels to be deforested on the period 2010-2050
pred.npix <- 40*defor.npix
pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
## Probability threshold
thres <- quantile(values(theta_MANAP),1-pred.prop,na.rm=TRUE)
## Forest in 2050
for2050.S2 <- for2010
for2050.S2[values(theta_MANAP)>thres] <- NA 
##====
## One raster for projections
proj_MANAP <- for2010
proj_MANAP[proj_MANAP==1 & is.na(for2050.S1)] <- 2
proj_MANAP[proj_MANAP==1 & is.na(for2050.S2)] <- 3
##===
## Validation with S3: S3=2010-2014
defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2014)==1,na.rm=TRUE))/4
## Number of pixels to be deforested on the period 2010-2014
pred.npix <- defor.npix*4
pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
## Probability threshold
thres <- quantile(values(theta_MANAP),1-pred.prop,na.rm=TRUE)
## Forest in 2014
for2014.S3 <- for2010
for2014.S3[values(theta_MANAP)>thres] <- NA 
## Comparing observations and predictions (n_pred_obs)
defor.1014 <- for2010 ; defor.1014[values(defor.1014)==1] <- 0 ; defor.1014[values(for2010)==1 & is.na(values(for2014))] <- 1
defor.1014.S3 <- for2010 ; defor.1014.S3[values(defor.1014.S3)==1] <- 0 ; defor.1014.S3[values(for2010)==1 & is.na(values(for2014.S3))] <- 1
n00 <- sum(values(defor.1014.S3)==0 & values(defor.1014)==0 , na.rm=TRUE)
n10 <- sum(values(defor.1014.S3)==1 & values(defor.1014)==0 , na.rm=TRUE)
n01 <- sum(values(defor.1014.S3)==0 & values(defor.1014)==1 , na.rm=TRUE)
n11 <- sum(values(defor.1014.S3)==1 & values(defor.1014)==1 , na.rm=TRUE)
## Performance indices
OA_MANAP <- (n11+n00)/(n11+n10+n00+n01)
FOM_MANAP <- n11/(n11+n10+n01)
Sensitivity_MANAP <- n11/(n11+n01)
Specificity_MANAP <- n00/(n00+n10)
TSS <- Sensitivity_MANAP+Specificity_MANAP-1
N <- n11+n10+n00+n01
Observed.accuracy <- (n11+n00)/N
Expected.accuracy <- ((n11+n10)*((n11+n01)/N) + (n00+n01)*((n00+n10)/N)) / N
Kappa_MANAP <- (Observed.accuracy-Expected.accuracy)/(1-Expected.accuracy)
##==
## Percentage of 2010-2014 deforestation included in 2010-2050 deforestation
defor.1050.S1 <- for2010 ; defor.1050.S1[values(defor.1050.S1)==1] <- 0 ; defor.1050.S1[values(for2010)==1 & is.na(values(for2050.S1))] <- 1
defor.1050.S2 <- for2010 ; defor.1050.S2[values(defor.1050.S2)==1] <- 0 ; defor.1050.S2[values(for2010)==1 & is.na(values(for2050.S2))] <- 1
perc_MANAP.S1 <- 100*(sum(values(defor.1014)==1 & values(defor.1050.S1)==1,na.rm=TRUE)/sum(values(defor.1014)==1,na.rm=TRUE))
perc_MANAP.S2 <- 100*(sum(values(defor.1014)==1 & values(defor.1050.S2)==1,na.rm=TRUE)/sum(values(defor.1014)==1,na.rm=TRUE))

##=======================
## KMNP
## Import rasters
for1990 <- raster("gisdata/rast/for1990_KMNP.tif")
for1990_KMNP <- for1990
for2000 <- raster("gisdata/rast/for2000_KMNP.tif")
for2010 <- raster("gisdata/rast/for2010_KMNP.tif")
for2014 <- raster("gisdata/rast/for2014_KMNP.tif")
theta_KMNP <- raster("gisdata/rast_raw/theta_KMNP.tif")
## One raster for deforestation
defor_KMNP <- for2000
defor_KMNP[defor_KMNP==1 & is.na(for2010)] <- 2
defor_KMNP[defor_KMNP==1 & is.na(for2014)] <- 3
## Mean annual deforestation on the period 1990-2010
## Two scenarios: S1=1990-2010 or S2=1990-2010-2014
##===
## Conservative scenario: S1=1990-2010
defor.npix <- (sum(values(for1990)==1,na.rm=TRUE)-sum(values(for2010)==1,na.rm=TRUE))/20
defor.nha <- defor.npix*30*30/10000
## Number of pixels to be deforested on the period 2010-2050
pred.npix <- 40*defor.npix
pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
## Probability threshold
thres <- quantile(values(theta_KMNP),1-pred.prop,na.rm=TRUE)
## Forest in 2050
for2050.S1 <- for2010
for2050.S1[values(theta_KMNP)>thres] <- NA 
##===
## Worst-case scenario: S1=2000-2014
defor.npix <- (sum(values(for2000)==1,na.rm=TRUE)-sum(values(for2014)==1,na.rm=TRUE))/14
defor.nha <- defor.npix*30*30/10000
## Number of pixels to be deforested on the period 2010-2050
pred.npix <- 40*defor.npix
pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
## Probability threshold
thres <- quantile(values(theta_KMNP),1-pred.prop,na.rm=TRUE)
## Forest in 2050
for2050.S2 <- for2010
for2050.S2[values(theta_KMNP)>thres] <- NA 
##====
## One raster for projections
proj_KMNP <- for2010
proj_KMNP[proj_KMNP==1 & is.na(for2050.S1)] <- 2
proj_KMNP[proj_KMNP==1 & is.na(for2050.S2)] <- 3
##===
## Validation with S3: S3=2010-2014
defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2014)==1,na.rm=TRUE))/4
## Number of pixels to be deforested on the period 2010-2014
pred.npix <- defor.npix*4
pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
## Probability threshold
thres <- quantile(values(theta_KMNP),1-pred.prop,na.rm=TRUE)
## Forest in 2014
for2014.S3 <- for2010
for2014.S3[values(theta_KMNP)>thres] <- NA 
## Comparing observations and predictions (n_pred_obs)
defor.1014 <- for2010 ; defor.1014[values(defor.1014)==1] <- 0 ; defor.1014[values(for2010)==1 & is.na(values(for2014))] <- 1
defor.1014.S3 <- for2010 ; defor.1014.S3[values(defor.1014.S3)==1] <- 0 ; defor.1014.S3[values(for2010)==1 & is.na(values(for2014.S3))] <- 1
n00 <- sum(values(defor.1014.S3)==0 & values(defor.1014)==0 , na.rm=TRUE)
n10 <- sum(values(defor.1014.S3)==1 & values(defor.1014)==0 , na.rm=TRUE)
n01 <- sum(values(defor.1014.S3)==0 & values(defor.1014)==1 , na.rm=TRUE)
n11 <- sum(values(defor.1014.S3)==1 & values(defor.1014)==1 , na.rm=TRUE)
## Performance indices
OA_KMNP <- (n11+n00)/(n11+n10+n00+n01)
FOM_KMNP <- n11/(n11+n10+n01)
Sensitivity_KMNP <- n11/(n11+n01)
Specificity_KMNP <- n00/(n00+n10)
TSS <- Sensitivity_KMNP + Specificity_KMNP -1
N <- n11+n10+n00+n01
Observed.accuracy <- (n11+n00)/N
Expected.accuracy <- ((n11+n10)*((n11+n01)/N) + (n00+n01)*((n00+n10)/N)) / N
Kappa_KMNP <- (Observed.accuracy-Expected.accuracy)/(1-Expected.accuracy)
##==
## Percentage of 2010-2014 deforestation included in 2010-2050 deforestation
defor.1050.S1 <- for2010 ; defor.1050.S1[values(defor.1050.S1)==1] <- 0 ; defor.1050.S1[values(for2010)==1 & is.na(values(for2050.S1))] <- 1
defor.1050.S2 <- for2010 ; defor.1050.S2[values(defor.1050.S2)==1] <- 0 ; defor.1050.S2[values(for2010)==1 & is.na(values(for2050.S2))] <- 1
perc_KMNP.S1 <- 100*(sum(values(defor.1014)==1 & values(defor.1050.S1)==1,na.rm=TRUE)/sum(values(defor.1014)==1,na.rm=TRUE))
perc_KMNP.S2 <- 100*(sum(values(defor.1014)==1 & values(defor.1050.S2)==1,na.rm=TRUE)/sum(values(defor.1014)==1,na.rm=TRUE))

##== Synthesis of model performance in data-frames
## Data-frame for indices
mod.perf <- data.frame(model=c("MANAP","KMNP"),OA=NA,Sens=NA,Spec=NA,FOM=NA,K=NA)
mod.perf[1,c(2:6)] <- c(OA_MANAP,Sensitivity_MANAP,Specificity_MANAP,FOM_MANAP,Kappa_MANAP)
mod.perf[2,c(2:6)] <- c(OA_KMNP,Sensitivity_KMNP,Specificity_KMNP,FOM_KMNP,Kappa_KMNP)
## Data-frame for percentage
perc <- data.frame(model=c("MANAP","KMNP"),S1=NA,S2=NA)
perc[1,c(2:3)] <- c(perc_MANAP.S1,perc_MANAP.S2)
perc[2,c(2:3)] <- c(perc_KMNP.S1,perc_KMNP.S2)

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
## Compute land area
# KMNP
land.KMNP <- crop(mada,e.KMNP)
land_KMNP <- rasterize(land.KMNP,defor_KMNP,field=1)
# MANAP
land.MANAP <- crop(mada,e.MANAP)
land_MANAP <- rasterize(land.MANAP,defor_MANAP,field=1)

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
Belo.df <- as.data.frame(Belo); names(Belo.df)[12:13] <- c("x","y")
Obs.df <- as.data.frame(Obs); names(Obs.df)[3:4] <- c("x","y")
Lambokely_Kirindy.df <- as.data.frame(Lambokely_Kirindy); names(Lambokely_Kirindy.df)[3:4] <- c("x","y")
Morondava_BeloTsi.df <- as.data.frame(Morondava_BeloTsi); names(Morondava_BeloTsi.df)[12:13] <- c("x","y")
Morondava_BeloTsi.df$TOPONYME <- as.factor(c("Belo sur Tsiribihina","Morondava"))

##========================================
## Plot raster with gplot() from rasterVis

## Setting basic theme options for plot with ggplot2
theme_base <- theme(axis.line=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    legend.position="none",
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.border=element_blank())

##===========
## Madagascar
plot.Mada <- ggplot(data=mada.df,aes(x=long,y=lat,group=id)) +
  geom_polygon(colour=grey(0.4),fill=grey(0.8),size=0.2) +
  geom_rect(aes(xmin=xmin.KMNP,xmax=xmax.KMNP,ymin=ymin.KMNP,ymax=ymax.KMNP),
            fill="transparent",colour="black",size=0.2) +
  geom_rect(aes(xmin=xmin.MANAP,xmax=xmax.MANAP,ymin=ymin.MANAP,ymax=ymax.MANAP),
            fill="transparent",colour="black",size=0.2) +
  theme_bw() + theme_base + theme(plot.margin=unit(c(0,0,-6,-6),"mm")) +
  coord_equal()
## Grob
grob.Mada <- ggplotGrob(plot.Mada)

## Resolution of rasters
high.res <- TRUE
res.rast <- ifelse(high.res,10e5,10e3)

## MANAP
# Build deforestation plot
plot.defor.MANAP <- gplot(defor_MANAP,maxpixels=res.rast) + 
  annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="a",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen","orange","red")) +
  annotation_custom(grob=grob.Mada,xmin=xmin.MANAP+4000,
                    xmax=xmin.MANAP+4000+12500,
                    ymin=ymin.MANAP+25000,ymax=ymin.MANAP+25000+36000) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1500, hjust=0.5, nudge_x=3500) +
  geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
  coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"))
# Build projection plot
plot.proj.MANAP <- gplot(proj_MANAP,maxpixels=res.rast) + 
  annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="c",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen",grey(0.5),grey(0.3))) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1500, hjust=0.5, nudge_x=3500) +
  geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
  coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() + theme_base + theme(plot.margin=unit(c(0,0,0,0.2),"cm"))

## KMNP
# Build deforestation plot
plot.defor.KMNP <- gplot(defor_KMNP,maxpixels=res.rast) + 
  annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="b",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen","orange","red")) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3250) +
  geom_point(data=Obs.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Obs.df, aes(label=Obs), size=3, hjust=1, nudge_x=-1000) +
  coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"))
# Build projection plot
plot.proj.KMNP <- gplot(proj_KMNP,maxpixels=res.rast) + 
  annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="d",hjust=0,vjust=1,size=4,fontface="bold") +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values = c("forestgreen",grey(0.5),grey(0.3))) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3250) +
  coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() + theme_base + theme(plot.margin=unit(c(0,0,0,0.2),"cm"))

## Combine plots
plot.defor <- grid.arrange(plot.defor.MANAP, plot.proj.MANAP, plot.defor.KMNP, plot.proj.KMNP, ncol=2)
ggsave(filename="figs/deforestation.png",plot=plot.defor,width=14,height=20,unit=c("cm"))

##========================================
## Spatial probability of deforestation

## Colors
col.proba <- colorRampPalette(c("forestgreen","orange","red"))
v <- quantile(c(values(theta_MANAP),values(theta_KMNP)),c(0.75,0.90),na.rm=TRUE)

## MANAP
plot.proba.MANAP <- gplot(theta_MANAP,maxpixels=res.rast) + 
  geom_raster(aes(fill=value)) +
  scale_fill_gradientn(colours=c("forestgreen","orange","red","black"),values=c(0,v,1),limits=c(0,1)) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1500, hjust=0.5, nudge_x=3500) +
  geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
  coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() + theme_base + theme(legend.justification="left",legend.position=c(0,0.5)) +
  theme(legend.key.width=unit(0.35,"cm"), legend.title=element_blank()) +
  annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="a",hjust=0,vjust=1,size=4,fontface="bold") +
  theme(plot.margin=unit(c(0,0.2,0,0),"cm"))
## KMNP
plot.proba.KMNP <- gplot(theta_KMNP,maxpixels=res.rast) + 
  geom_raster(aes(fill=value)) +
  scale_fill_gradientn(colours=c("forestgreen","orange","red","black"),values=c(0,v,1),limits=c(0,1)) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="transparent", size=0.3) +
  geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3250) +
  geom_point(data=Obs.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Obs.df, aes(label=Obs), size=3, hjust=1, nudge_x=-1000) +
  coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() + theme_base + 
  annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="b",hjust=0,vjust=1,size=4,fontface="bold") +
  theme(plot.margin=unit(c(0,0,0,0.2),"cm"))

## Combine plots
plot.proba <- grid.arrange(plot.proba.MANAP, plot.proba.KMNP, ncol=2)
ggsave(filename="figs/probability.png",plot=plot.proba,width=15,height=10,unit="cm")

##========================================
## Forest cover evolution

## Annual deforestation in percentage
theta <- function(f2,f1,Y) {
  return(1-(1-(f1-f2)/f1)^(1/Y)) 
}
## Table of results
forest.cover <- data.frame(site=c("KMNP","MANAP"))
forest.cover$area <- c(sum(values(land_KMNP)==1,na.rm=TRUE),sum(values(land_MANAP)==1,na.rm=TRUE))
forest.cover$f1990 <- c(sum(values(for1990_KMNP),na.rm=TRUE),sum(values(for1990_MANAP),na.rm=TRUE))
forest.cover$f2000 <- c(sum(values(defor_KMNP) %in% c(1:3),na.rm=TRUE),sum(values(defor_MANAP) %in% c(1:3),na.rm=TRUE))
forest.cover$f2010 <- c(sum(values(defor_KMNP) %in% c(1,2),na.rm=TRUE),sum(values(defor_MANAP) %in% c(1,2),na.rm=TRUE))
forest.cover$f2014 <- c(sum(values(defor_KMNP)==1,na.rm=TRUE),sum(values(defor_MANAP)==1,na.rm=TRUE))
forest.cover$f2050.S1 <- c(sum(values(proj_KMNP) %in% c(1,3),na.rm=TRUE),sum(values(proj_MANAP) %in% c(1,3),na.rm=TRUE))
forest.cover$f2050.S2 <- c(sum(values(proj_KMNP)==1,na.rm=TRUE),sum(values(proj_MANAP)==1,na.rm=TRUE))
forest.cover$d9000.ha <- c(forest.cover$f1990-forest.cover$f2000)/10
forest.cover$d0010.ha <- c(forest.cover$f2000-forest.cover$f2010)/10
forest.cover$d1014.ha <- c(forest.cover$f2010-forest.cover$f2014)/4
forest.cover$d1050.S1.ha <- c(forest.cover$f2010-forest.cover$f2050.S1)/40
forest.cover$d1050.S2.ha <- c(forest.cover$f2010-forest.cover$f2050.S2)/40
## Deforestation rates in %
forest.cover$d9000.p <- round(theta(forest.cover$f2000,forest.cover$f1990,10)*100,2)
forest.cover$d0010.p <- round(theta(forest.cover$f2010,forest.cover$f2000,10)*100,2)
forest.cover$d1014.p <- round(theta(forest.cover$f2014,forest.cover$f2010,4)*100,2)
forest.cover$d1050.S1.p <- round(theta(forest.cover$f2050.S1,forest.cover$f2010,40)*100,2)
forest.cover$d1050.S2.p <- round(theta(forest.cover$f2050.S2,forest.cover$f2010,40)*100,2)
## Transform pixels in ha
forest.cover[,c(2:13)] <- round(forest.cover[,c(2:13)]*30*30/10000)

## Save objects
# load("menabe.rda")
save(forest.cover,e.KMNP.latlong,e.MANAP.latlong,mod.perf,perc,file="menabe.rda")

##========================
## Knit the document

## Assemble photos (ImageMagick need to be installed, see at www.imagemagick.org)
system("sh menabe.sh")

## Set knitr chunk default options
opts_chunk$set(echo=FALSE, cache=FALSE,
               results="hide", warning=FALSE,
               message=FALSE, highlight=TRUE,
               fig.show="hide", size="small",
               tidy=FALSE)

## Knit and translate to html and pdf
render("menabe.Rmd") # html output
#render("menabe.Rmd",output_format=c("html_document","pdf_document"))

##===========================================================================
## End of script
##===========================================================================

