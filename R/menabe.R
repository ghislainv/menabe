  #!/usr/bin/Rscript
  
  # ==============================================================================
  # author          :Ghislain Vieilledent
  # email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
  # web             :https://ghislainv.github.io
  # license         :CC-BY-SA 4.0
  # ==============================================================================
  
  ##= Libraries
  pkg <- c("broom","sp","rgdal","raster","ggplot2","gridExtra",
           "rasterVis","rgeos","dplyr","readr")
  ## broom: to convert map into data-frame with tiny()
  ## gridExtra: to combine several ggplots
  ## rasterVis: for gplot()
  ## rgeos: for crop()
  load.pkg <- function(x) {
    if(!require(x, character.only = T)) {
      install.packages(x)
      require(x, character.only = T)
    }
  }
  loaded <- lapply(pkg,load.pkg)
  ## Remove useless objects
  rm(pkg,load.pkg,loaded)
  
  ##======================================================================
  ## Download data (263.6 Mo): will have to be done from a Zenodo repository
  #d <- "http://bioscenemada.cirad.fr/githubdata/menabe/menabe_data.zip"
  #download.file(url=d,destfile="menabe_data.zip",method="wget",quiet=TRUE)
  #unzip("menabe_data.zip")
  
  ##===========================================================
  ## Create new directories to save figures and new raster data
  dir.create("figs")
  dir.create("rast")
  
  ##================================================
  ## Household income in MGA on the period 2006-2012
  ## Sources: The Gallup Organization and The World Bank
  
  household.income.usd <- 1013 ## The Gallup Organization
  rate <- c(2142.30,1873.88,1708.37,1956.21,2089.95,2025.12,2194.97)
  year <- c(2006,2007,2008,2009,2010,2011,2012)
  m.rate <- round(mean(rate))
  household.income.mga <- m.rate*household.income.usd
  
  ##========================================
  ## Prepare rasters for the three study areas
  
  ## Set region for Menabe Antimena New Protected Area (MANAP)
  xmin.MANAP <- 419600; xmax.MANAP <- 478890
  ymin.MANAP <- 7750744; ymax.MANAP <- 7834872
  prop <- (ymax.MANAP-ymin.MANAP)/(xmax.MANAP-xmin.MANAP)
  Extent.MANAP <- paste(xmin.MANAP,ymin.MANAP,xmax.MANAP,ymax.MANAP)
  e.MANAP <- extent(c(xmin.MANAP,xmax.MANAP,ymin.MANAP,ymax.MANAP))
  r.MANAP <- raster(ext=e.MANAP,crs="+init=epsg:32738")
  r.MANAP.latlong <- projectRaster(r.MANAP,crs="+init=epsg:4326")
  e.MANAP.latlong <- extent(r.MANAP.latlong)
  
  ## Set region for Kirindy-Mitea National Park (KMNP)
  xmin.KMNP <- 365000; xmax.KMNP <- 430010
  ymax.KMNP <- 7730000
  ymin.KMNP <- round(ymax.KMNP-prop*(xmax.KMNP-xmin.KMNP)) # 7640000
  Extent.KMNP <- paste(xmin.KMNP,ymin.KMNP,xmax.KMNP,ymax.KMNP)
  e.KMNP <- extent(c(xmin.KMNP,xmax.KMNP,ymin.KMNP,ymax.KMNP))
  r.KMNP <- raster(ext=e.KMNP,crs="+init=epsg:32738")
  r.KMNP.latlong <- projectRaster(r.KMNP,crs="+init=epsg:4326")
  e.KMNP.latlong <- extent(r.KMNP.latlong)
  
  ## Set region for Mikea New Protected Area (MIKEA)
  ymin.MIKEA <- 7461700; ymax.MIKEA <- 7603900
  xmin.MIKEA <- 310400; xmax.MIKEA <- round(xmin.MIKEA+(ymax.MIKEA-ymin.MIKEA)/prop) # 384800
  Extent.MIKEA <- paste(xmin.MIKEA,ymin.MIKEA,xmax.MIKEA,ymax.MIKEA)
  e.MIKEA <- extent(c(xmin.MIKEA,xmax.MIKEA,ymin.MIKEA,ymax.MIKEA))
  r.MIKEA <- raster(ext=e.MIKEA,crs="+init=epsg:32738")
  r.MIKEA.latlong <- projectRaster(r.MIKEA,crs="+init=epsg:4326")
  e.MIKEA.latlong <- extent(r.MIKEA.latlong)
  
  ## gdalwrap
  f <- c("for1990","for2000","for2010","for2017","prob2010_orig")
  for (i in 1:length(f)) {
    system(paste0("gdalwarp -overwrite -dstnodata 0 \\
            -r near -tr 30 30 -te ",Extent.MANAP," -of GTiff \\
            -co 'compress=lzw' -co 'predictor=2' \\
            gisdata/rasters/",f[i],".tif \\
            rast/",f[i],"_MANAP.tif"))
  }
  for (i in 1:length(f)) {
    system(paste0("gdalwarp -overwrite -dstnodata 0 \\
            -r near -tr 30 30 -te ",Extent.KMNP," -of GTiff \\
            -co 'compress=lzw' -co 'predictor=2' \\
            gisdata/rasters/",f[i],".tif \\
            rast/",f[i],"_KMNP.tif"))
  }
  for (i in 1:length(f)) {
    system(paste0("gdalwarp -overwrite -dstnodata 0 \\
            -r near -tr 30 30 -te ",Extent.MIKEA," -of GTiff \\
            -co 'compress=lzw' -co 'predictor=2' \\
            gisdata/rasters/",f[i],".tif \\
            rast/",f[i],"_MIKEA.tif"))
  }
  
  ##=======================
  ## Fill null values for probability of deforestation
  source("R/fillna_prob.R")
  ## Test
  # forest <- raster("rast/for2010_MANAP.tif")
  # theta <- raster("rast/prob2010_MANAP.tif")
  # sum(!is.na(values(forest)) & is.na(values(theta)))
  
  ##=======================
  ## MANAP
  ## Import rasters
  for1990 <- raster("rast/for1990_MANAP.tif")
  for1990_MANAP <- for1990
  for2000 <- raster("rast/for2000_MANAP.tif")
  for2010 <- raster("rast/for2010_MANAP.tif")
  for2017 <- raster("rast/for2017_MANAP.tif")
  theta_MANAP <- raster("rast/prob2010_MANAP.tif")
  ## One raster for deforestation
  defor_MANAP <- for2000
  defor_MANAP[values(defor_MANAP)==1 & is.na(values(for2010))] <- 2
  defor_MANAP[values(defor_MANAP)==1 & is.na(values(for2017))] <- 3
  writeRaster(defor_MANAP,filename="rast/defor_MANAP.tif",overwrite=TRUE)
  ## Two scenarios: S1=2000-2010 or S2=2010-2017
  ##===
  ## Conservative scenario: S1=2000-2010
  defor.npix <- (sum(values(for2000)==1,na.rm=TRUE)-sum(values(for2010)==1,na.rm=TRUE))/10
  defor.nha <- defor.npix*30*30/10000
  ## Number of pixels to be deforested on the period 2010-2050
  pred.npix <- 40*defor.npix
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  thres <- quantile(values(theta_MANAP),1-pred.prop,na.rm=TRUE)
  ## Forest in 2050
  for2050.S1 <- for2010
  for2050.S1[values(theta_MANAP)>thres] <- NA 
  writeRaster(for2050.S1,filename="rast/for2050_S1_MANAP.tif",overwrite=TRUE)
  ##===
  ## Worst-case scenario: S2=2010-2017
  defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2017)==1,na.rm=TRUE))/7
  defor.nha <- defor.npix*30*30/10000
  ## Number of pixels to be deforested on the period 2010-2050
  pred.npix <- 40*defor.npix
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_MANAP),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2050
  for2050.S2 <- for2010
  for2050.S2[values(theta_MANAP)>thres] <- NA 
  writeRaster(for2050.S2,filename="rast/for2050_S2_MANAP.tif",overwrite=TRUE)
  ##====
  ## One raster for projections
  proj_MANAP <- for2010
  proj_MANAP[values(proj_MANAP)==1 & is.na(values(for2050.S1))] <- 2
  proj_MANAP[values(proj_MANAP)==1 & is.na(values(for2050.S2))] <- 3
  ##===
  ## Validation with S3: S3=2010-2017
  defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2017)==1,na.rm=TRUE))/7
  ## Number of pixels to be deforested on the period 2010-2017
  pred.npix <- defor.npix*7
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_MANAP),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2017
  for2017.S3 <- for2010
  for2017.S3[values(theta_MANAP)>thres] <- NA 
  ## Comparing observations and predictions (n_pred_obs)
  defor.1017 <- for2010 ; defor.1017[values(defor.1017)==1] <- 0 ; defor.1017[values(for2010)==1 & is.na(values(for2017))] <- 1
  defor.1017.S3 <- for2010 ; defor.1017.S3[values(defor.1017.S3)==1] <- 0 ; defor.1017.S3[values(for2010)==1 & is.na(values(for2017.S3))] <- 1
  n00 <- sum(values(defor.1017.S3)==0 & values(defor.1017)==0 , na.rm=TRUE)
  n10 <- sum(values(defor.1017.S3)==1 & values(defor.1017)==0 , na.rm=TRUE)
  n01 <- sum(values(defor.1017.S3)==0 & values(defor.1017)==1 , na.rm=TRUE)
  n11 <- sum(values(defor.1017.S3)==1 & values(defor.1017)==1 , na.rm=TRUE)
  ## Performance indices
  OA_MANAP <- (n11+n00)/(n11+n10+n00+n01)
  FOM_MANAP <- n11/(n11+n10+n01)
  Sensitivity_MANAP <- n11/(n11+n01)
  Specificity_MANAP <- n00/(n00+n10)
  TSS <- Sensitivity_MANAP+Specificity_MANAP-1
  N <- n11+n10+n00+n01
  Observed_accuracy <- (n11+n00)/N
  Prob_1and1 <- ((n11 + n10) / N) * ((n11 + n01) / N)
  Prob_0and0 <- ((n00 + n01) / N) * ((n00 + n10) / N)
  Expected_accuracy <- Prob_1and1 + Prob_0and0
  Kappa_MANAP <- (Observed_accuracy-Expected_accuracy)/(1-Expected_accuracy)
  ##==
  ## Percentage of 2010-2017 deforestation included in 2010-2050 deforestation
  defor.1050.S1 <- for2010 ; defor.1050.S1[values(defor.1050.S1)==1] <- 0 ; defor.1050.S1[values(for2010)==1 & is.na(values(for2050.S1))] <- 1
  defor.1050.S2 <- for2010 ; defor.1050.S2[values(defor.1050.S2)==1] <- 0 ; defor.1050.S2[values(for2010)==1 & is.na(values(for2050.S2))] <- 1
  perc_MANAP.S1 <- 100*(sum(values(defor.1017)==1 & values(defor.1050.S1)==1,na.rm=TRUE)/sum(values(defor.1017)==1,na.rm=TRUE))
  perc_MANAP.S2 <- 100*(sum(values(defor.1017)==1 & values(defor.1050.S2)==1,na.rm=TRUE)/sum(values(defor.1017)==1,na.rm=TRUE))
  
  ##=======================
  ## KMNP
  ## Import rasters
  for1990 <- raster("rast/for1990_KMNP.tif")
  for1990_KMNP <- for1990
  for2000 <- raster("rast/for2000_KMNP.tif")
  for2010 <- raster("rast/for2010_KMNP.tif")
  for2017 <- raster("rast/for2017_KMNP.tif")
  theta_KMNP <- raster("rast/prob2010_KMNP.tif")
  ## One raster for deforestation
  defor_KMNP <- for2000
  defor_KMNP[values(defor_KMNP)==1 & is.na(values(for2010))] <- 2
  defor_KMNP[values(defor_KMNP)==1 & is.na(values(for2017))] <- 3
  writeRaster(defor_KMNP,filename="rast/defor_KMNP.tif",overwrite=TRUE)
  ## Two scenarios: S1=2000-2010 or S2=2010-2017
  ##===
  ## Conservative scenario: S1=2000-2010
  defor.npix <- (sum(values(for2000)==1,na.rm=TRUE)-sum(values(for2010)==1,na.rm=TRUE))/10
  defor.nha <- defor.npix*30*30/10000
  ## Number of pixels to be deforested on the period 2010-2050
  pred.npix <- 40*defor.npix
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_KMNP),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2050
  for2050.S1 <- for2010
  for2050.S1[values(theta_KMNP)>thres] <- NA 
  writeRaster(for2050.S1,filename="rast/for2050_S1_KMNP.tif",overwrite=TRUE)
  ##===
  ## Worst-case scenario: S2=2010-2017
  defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2017)==1,na.rm=TRUE))/7
  defor.nha <- defor.npix*30*30/10000
  ## Number of pixels to be deforested on the period 2010-2050
  pred.npix <- 40*defor.npix
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_KMNP),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2050
  for2050.S2 <- for2010
  for2050.S2[values(theta_KMNP)>thres] <- NA 
  writeRaster(for2050.S2,filename="rast/for2050_S2_KMNP.tif",overwrite=TRUE)
  ##====
  ## One raster for projections
  proj_KMNP <- for2010
  proj_KMNP[values(proj_KMNP)==1 & is.na(values(for2050.S1))] <- 2
  proj_KMNP[values(proj_KMNP)==1 & is.na(values(for2050.S2))] <- 3
  ##===
  ## Validation with S3: S3=2010-2017
  defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2017)==1,na.rm=TRUE))/7
  ## Number of pixels to be deforested on the period 2010-2017
  pred.npix <- defor.npix*7
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_KMNP),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2017
  for2017.S3 <- for2010
  for2017.S3[values(theta_KMNP)>thres] <- NA 
  ## Comparing observations and predictions (n_pred_obs)
  defor.1017 <- for2010 ; defor.1017[values(defor.1017)==1] <- 0 ; defor.1017[values(for2010)==1 & is.na(values(for2017))] <- 1
  defor.1017.S3 <- for2010 ; defor.1017.S3[values(defor.1017.S3)==1] <- 0 ; defor.1017.S3[values(for2010)==1 & is.na(values(for2017.S3))] <- 1
  n00 <- sum(values(defor.1017.S3)==0 & values(defor.1017)==0 , na.rm=TRUE)
  n10 <- sum(values(defor.1017.S3)==1 & values(defor.1017)==0 , na.rm=TRUE)
  n01 <- sum(values(defor.1017.S3)==0 & values(defor.1017)==1 , na.rm=TRUE)
  n11 <- sum(values(defor.1017.S3)==1 & values(defor.1017)==1 , na.rm=TRUE)
  ## Performance indices
  OA_KMNP <- (n11+n00)/(n11+n10+n00+n01)
  FOM_KMNP <- n11/(n11+n10+n01)
  Sensitivity_KMNP <- n11/(n11+n01)
  Specificity_KMNP <- n00/(n00+n10)
  N <- n11+n10+n00+n01
  Observed_accuracy <- (n11+n00)/N
  Prob_1and1 <- ((n11 + n10) / N) * ((n11 + n01) / N)
  Prob_0and0 <- ((n00 + n01) / N) * ((n00 + n10) / N)
  Expected_accuracy <- Prob_1and1 + Prob_0and0
  Kappa_KMNP <- (Observed_accuracy-Expected_accuracy)/(1-Expected_accuracy)
  ##==
  ## Percentage of 2010-2017 deforestation included in 2010-2050 deforestation
  defor.1050.S1 <- for2010 ; defor.1050.S1[values(defor.1050.S1)==1] <- 0 ; defor.1050.S1[values(for2010)==1 & is.na(values(for2050.S1))] <- 1
  defor.1050.S2 <- for2010 ; defor.1050.S2[values(defor.1050.S2)==1] <- 0 ; defor.1050.S2[values(for2010)==1 & is.na(values(for2050.S2))] <- 1
  perc_KMNP.S1 <- 100*(sum(values(defor.1017)==1 & values(defor.1050.S1)==1,na.rm=TRUE)/sum(values(defor.1017)==1,na.rm=TRUE))
  perc_KMNP.S2 <- 100*(sum(values(defor.1017)==1 & values(defor.1050.S2)==1,na.rm=TRUE)/sum(values(defor.1017)==1,na.rm=TRUE))
  
  ##=======================
  ## MIKEA
  ## Import rasters
  for1990 <- raster("rast/for1990_MIKEA.tif")
  for1990_MIKEA <- for1990
  for2000 <- raster("rast/for2000_MIKEA.tif")
  for2010 <- raster("rast/for2010_MIKEA.tif")
  for2017 <- raster("rast/for2017_MIKEA.tif")
  theta_MIKEA <- raster("rast/prob2010_MIKEA.tif")
  ## One raster for deforestation
  defor_MIKEA <- for2000
  defor_MIKEA[values(defor_MIKEA)==1 & is.na(values(for2010))] <- 2
  defor_MIKEA[values(defor_MIKEA)==1 & is.na(values(for2017))] <- 3
  writeRaster(defor_MIKEA,filename="rast/defor_MIKEA.tif",overwrite=TRUE)
  ## Two scenarios: S1=2000-2010 or S2=2010-2017
  ##===
  ## Conservative scenario: S1=2000-2010
  defor.npix <- (sum(values(for2000)==1,na.rm=TRUE)-sum(values(for2010)==1,na.rm=TRUE))/10
  defor.nha <- defor.npix*30*30/10000
  ## Number of pixels to be deforested on the period 2010-2050
  pred.npix <- 40*defor.npix
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_MIKEA),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2050
  for2050.S1 <- for2010
  for2050.S1[values(theta_MIKEA)>thres] <- NA 
  writeRaster(for2050.S1,filename="rast/for2050_S1_MIKEA.tif",overwrite=TRUE)
  ##===
  ## Worst-case scenario: S2=2010-2017
  defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2017)==1,na.rm=TRUE))/7
  defor.nha <- defor.npix*30*30/10000
  ## Number of pixels to be deforested on the period 2010-2050
  pred.npix <- 40*defor.npix
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_MIKEA),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2050
  for2050.S2 <- for2010
  for2050.S2[values(theta_MIKEA)>thres] <- NA 
  writeRaster(for2050.S2,filename="rast/for2050_S2_MIKEA.tif",overwrite=TRUE)
  ##====
  ## One raster for projections
  proj_MIKEA <- for2010
  proj_MIKEA[values(proj_MIKEA)==1 & is.na(values(for2050.S1))] <- 2
  proj_MIKEA[values(proj_MIKEA)==1 & is.na(values(for2050.S2))] <- 3
  ##===
  ## Validation with S3: S3=2010-2017
  defor.npix <- (sum(values(for2010)==1,na.rm=TRUE)-sum(values(for2017)==1,na.rm=TRUE))/7
  ## Number of pixels to be deforested on the period 2010-2017
  pred.npix <- defor.npix*7
  pred.prop <- pred.npix/sum(values(for2010)==1,na.rm=TRUE)
  ## Probability threshold
  if (pred.prop < 1) {
    thres <- quantile(values(theta_MIKEA),1-pred.prop,na.rm=TRUE)
  } else {
    thres <- 0
  }
  ## Forest in 2017
  for2017.S3 <- for2010
  for2017.S3[values(theta_MIKEA)>thres] <- NA 
  ## Comparing observations and predictions (n_pred_obs)
  defor.1017 <- for2010 ; defor.1017[values(defor.1017)==1] <- 0 ; defor.1017[values(for2010)==1 & is.na(values(for2017))] <- 1
  defor.1017.S3 <- for2010 ; defor.1017.S3[values(defor.1017.S3)==1] <- 0 ; defor.1017.S3[values(for2010)==1 & is.na(values(for2017.S3))] <- 1
  n00 <- sum(values(defor.1017.S3)==0 & values(defor.1017)==0 , na.rm=TRUE)
  n10 <- sum(values(defor.1017.S3)==1 & values(defor.1017)==0 , na.rm=TRUE)
  n01 <- sum(values(defor.1017.S3)==0 & values(defor.1017)==1 , na.rm=TRUE)
  n11 <- sum(values(defor.1017.S3)==1 & values(defor.1017)==1 , na.rm=TRUE)
  ## Performance indices
  OA_MIKEA <- (n11+n00)/(n11+n10+n00+n01)
  FOM_MIKEA <- n11/(n11+n10+n01)
  Sensitivity_MIKEA <- n11/(n11+n01)
  Specificity_MIKEA <- n00/(n00+n10)
  TSS <- Sensitivity_MIKEA+Specificity_MIKEA-1
  N <- n11+n10+n00+n01
  Observed_accuracy <- (n11+n00)/N
  Prob_1and1 <- ((n11 + n10) / N) * ((n11 + n01) / N)
  Prob_0and0 <- ((n00 + n01) / N) * ((n00 + n10) / N)
  Expected_accuracy <- Prob_1and1 + Prob_0and0
  Kappa_MIKEA <- (Observed_accuracy-Expected_accuracy)/(1-Expected_accuracy)
  ##==
  ## Percentage of 2010-2017 deforestation included in 2010-2050 deforestation
  defor.1050.S1 <- for2010 ; defor.1050.S1[values(defor.1050.S1)==1] <- 0 ; defor.1050.S1[values(for2010)==1 & is.na(values(for2050.S1))] <- 1
  defor.1050.S2 <- for2010 ; defor.1050.S2[values(defor.1050.S2)==1] <- 0 ; defor.1050.S2[values(for2010)==1 & is.na(values(for2050.S2))] <- 1
  perc_MIKEA.S1 <- 100*(sum(values(defor.1017)==1 & values(defor.1050.S1)==1,na.rm=TRUE)/sum(values(defor.1017)==1,na.rm=TRUE))
  perc_MIKEA.S2 <- 100*(sum(values(defor.1017)==1 & values(defor.1050.S2)==1,na.rm=TRUE)/sum(values(defor.1017)==1,na.rm=TRUE))
  
  ##== Synthesis of model performance in data-frames
  ## Data-frame for indices
  mod.perf <- data.frame(model=c("MANAP","KMNP","MIKEA"),OA=NA,Sens=NA,Spec=NA,FOM=NA,K=NA)
  mod.perf[1,c(2:6)] <- c(OA_MANAP,Sensitivity_MANAP,Specificity_MANAP,FOM_MANAP,Kappa_MANAP)
  mod.perf[2,c(2:6)] <- c(OA_KMNP,Sensitivity_KMNP,Specificity_KMNP,FOM_KMNP,Kappa_KMNP)
  mod.perf[3,c(2:6)] <- c(OA_MIKEA,Sensitivity_MIKEA,Specificity_MIKEA,FOM_MIKEA,Kappa_MIKEA)
  ## Data-frame for percentage
  perc <- data.frame(model=c("MANAP","KMNP","MIKEA"),S1=NA,S2=NA)
  perc[1,c(2:3)] <- c(perc_MANAP.S1,perc_MANAP.S2)
  perc[2,c(2:3)] <- c(perc_KMNP.S1,perc_KMNP.S2)
  perc[3,c(2:3)] <- c(perc_MIKEA.S1,perc_MIKEA.S2)
  
  ##=================================================================================
  ## Import maps (shapefiles) and convert to data-frame for ggplot with bloom::tiny()
  
  ## SAPM ("système d'aires protégées à Madagascar")
  sapm <- readOGR(dsn="gisdata/vectors/sapm",layer="AP-NAP_38s")
  sapm_region <- subset(sapm, NOM %in% c("Mikea", "Kirindy Mitea", "Menabe Antimena", "Andranomena", "Ranobe PK 32"))
  sapm.df <- tidy(sapm_region)
  
  ## Madagascar boundaries
  mada.latlong <- readOGR(dsn="gisdata/vectors/mada",layer="MAD_outline")
  proj4string(mada.latlong) <- "+init=epsg:4326"
  mada <- spTransform(mada.latlong,CRSobj=CRS("+init=epsg:32738"))
  mada.df <- tidy(mada)
  ## Compute land area
  # MANAP
  land.MANAP <- crop(mada,e.MANAP)
  area.land.MANAP <- round(sum(sapply(slot(land.MANAP, "polygons"), slot, "area"))/10000)
  # KMNP
  land.KMNP <- crop(mada,e.KMNP)
  area.land.KMNP <- round(sum(sapply(slot(land.KMNP, "polygons"), slot, "area"))/10000)
  # MIKEA
  land.MIKEA <- crop(mada,e.MIKEA)
  area.land.MIKEA <- round(sum(sapply(slot(land.MIKEA, "polygons"), slot, "area"))/10000)
  
  ## Roads
  roads.latlong <- readOGR(dsn="gisdata/vectors/roads",layer="tr_route_polyline")
  roads <- spTransform(roads.latlong,CRSobj=CRS("+init=epsg:32738"))
  roads.df <- tidy(roads)
  
  ## Localities and field observations
  Belo <- readOGR(dsn="gisdata/vectors/additional_points",layer="Belo")
  Lambokely_Kirindy <- readOGR(dsn="gisdata/vectors/additional_points",layer="Lambokely_Kirindy_Village")
  Morondava_BeloTsi <- readOGR(dsn="gisdata/vectors/additional_points",layer="Morondava_BeloTsi")
  Morombe <- readOGR(dsn="gisdata/vectors/additional_points",layer="Morombe")
  Obs <- readOGR(dsn="gisdata/vectors/additional_points",layer="Obs")
  ## df for localities and field observations
  Belo.df <- as.data.frame(Belo); names(Belo.df)[12:13] <- c("x","y")
  Morombe.df <- as.data.frame(Morombe); names(Morombe.df)[14:15] <- c("x","y")
  Obs.df <- as.data.frame(Obs); names(Obs.df)[3:4] <- c("x","y")
  Lambokely_Kirindy.df <- as.data.frame(Lambokely_Kirindy); names(Lambokely_Kirindy.df)[3:4] <- c("x","y")
  Morondava_BeloTsi.df <- as.data.frame(Morondava_BeloTsi); names(Morondava_BeloTsi.df)[12:13] <- c("x","y")
  Morondava_BeloTsi.df$TOPONYME <- as.factor(c("Belo sur Tsiribihina","Morondava"))
  
  ## Cyclones
  Fanele.latlong <- readOGR(dsn="gisdata/vectors/cyclones",layer="Fanele")
  Fanele <- spTransform(Fanele.latlong,CRSobj=CRS("+init=epsg:32738"))
  Fanele.df <- tidy(Fanele)
  Haruna.latlong <- readOGR(dsn="gisdata/vectors/cyclones",layer="Haruna")
  Haruna <- spTransform(Haruna.latlong,CRSobj=CRS("+init=epsg:32738"))
  Haruna.df <- tidy(Haruna)
  
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
    geom_rect(aes(xmin=xmin.MANAP,xmax=xmax.MANAP,ymin=ymin.MANAP,ymax=ymax.MANAP),
              fill="transparent",colour="black",size=0.2) +
    geom_rect(aes(xmin=xmin.KMNP,xmax=xmax.KMNP,ymin=ymin.KMNP,ymax=ymax.KMNP),
              fill="transparent",colour="black",size=0.2) +
    geom_rect(aes(xmin=xmin.MIKEA,xmax=xmax.MIKEA,ymin=ymin.MIKEA,ymax=ymax.MIKEA),
              fill="transparent",colour="black",size=0.2) +
    theme_bw() + theme_base + theme(plot.margin=unit(c(-0.25,-0.25,-0.5,-0.5),"line"),
                                    panel.background=element_rect(fill="azure")) +
    coord_equal()
  ## Grob
  grob.Mada <- ggplotGrob(plot.Mada)
  
  ## Resolution of rasters
  high.res <- TRUE
  res.rast <- ifelse(high.res,10e5,10e3)
  
  ## MANAP
  # Build deforestation plot
  plot.defor.MANAP <- gplot(defor_MANAP,maxpixels=res.rast) +
    annotation_custom(grob=grob.Mada,xmin=xmin.MANAP+3000,
                      xmax=xmin.MANAP+3000+12500,
                      ymin=ymin.MANAP+23000,ymax=ymin.MANAP+23000+36000) +
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=factor(value))) +
    scale_fill_manual(values=c("forestgreen","orange","red"),na.value="transparent") +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1500, hjust=0.5, nudge_x=3500) +
    geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
    geom_point(data=Obs.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Obs.df, aes(label=Obs), size=3, hjust=1, nudge_x=-1000) +
    coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="a",hjust=0,vjust=1,size=4,fontface="bold") +
    theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"), panel.background=element_rect(fill="azure"))
  ## KMNP
  # Build deforestation plot
  plot.defor.KMNP <- gplot(defor_KMNP,maxpixels=res.rast) + 
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=factor(value))) +
    scale_fill_manual(values=c("forestgreen","orange","red"),na.value="transparent") +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_path(data=Fanele.df, aes(x=long, y=lat, group=group), linetype="dashed", colour="black", size=0.4) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3250) +
    geom_point(data=Obs.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Obs.df, aes(label=Obs), size=3, hjust=1, nudge_x=-1000) +
    coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="b",hjust=0,vjust=1,size=4,fontface="bold") +
    theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"), panel.background=element_rect(fill="azure"))
  ## MIKEA
  # Build deforestation plot
  plot.defor.MIKEA <- gplot(defor_MIKEA,maxpixels=res.rast) + 
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=factor(value))) +
    scale_fill_manual(values=c("forestgreen","orange","red"),na.value="transparent") +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_path(data=Haruna.df, aes(x=long, y=lat, group=group), linetype="dashed", colour="black", size=0.4) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Morombe.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Morombe.df, aes(label=Name), size=3, vjust=0, nudge_y=2000, hjust=0.5, nudge_x=-3500) +
    geom_point(data=Obs.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Obs.df, aes(label=Obs), size=3, hjust=1, nudge_x=-1000) +
    coord_equal(xlim=c(xmin.MIKEA,xmax.MIKEA),ylim=c(ymin.MIKEA,ymax.MIKEA)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    annotate("text",x=xmin.MIKEA,y=ymax.MIKEA,label="c",hjust=0,vjust=1,size=4,fontface="bold") +
    theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"), panel.background=element_rect(fill="azure"))
  ## Combine plots
  plot.defor <- grid.arrange(plot.defor.MANAP, plot.defor.KMNP, plot.defor.MIKEA, ncol=3)
  ggsave(filename="manuscript/figures/deforestation.png",plot=plot.defor,width=21,height=10,unit=c("cm"))
  
  ## MANAP
  # Build projection plot
  plot.proj.MANAP <- gplot(proj_MANAP,maxpixels=res.rast) + 
    annotation_custom(grob=grob.Mada,xmin=xmin.MANAP+3000,
                      xmax=xmin.MANAP+3000+12500,
                      ymin=ymin.MANAP+23000,ymax=ymin.MANAP+23000+36000) +
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=factor(value))) +
    scale_fill_manual(values=c("#d1b45b","#8c4600"),na.value="transparent") +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1500, hjust=0.5, nudge_x=3500) +
    geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
    coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="a",hjust=0,vjust=1,size=4,fontface="bold") +
    theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"), panel.background=element_rect(fill="azure"))
  ## KMNP
  # Build projection plot
  plot.proj.KMNP <- gplot(proj_KMNP,maxpixels=res.rast) + 
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=factor(value))) +
    scale_fill_manual(values=c("forestgreen","#d1b45b","#8c4600"),na.value="transparent") +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3250) +
    coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="b",hjust=0,vjust=1,size=4,fontface="bold") +
    theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"), panel.background=element_rect(fill="azure"))
  ## MIKEA
  # Build projection plot
  plot.proj.MIKEA <- gplot(proj_MIKEA,maxpixels=res.rast) + 
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=factor(value))) +
    scale_fill_manual(values=c("forestgreen","#d1b45b","#8c4600"),na.value="transparent") +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Morombe.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Morombe.df, aes(label=Name), size=3, vjust=0, nudge_y=2000, hjust=0.5, nudge_x=-3500) +
    coord_equal(xlim=c(xmin.MIKEA,xmax.MIKEA),ylim=c(ymin.MIKEA,ymax.MIKEA)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    annotate("text",x=xmin.MIKEA,y=ymax.MIKEA,label="c",hjust=0,vjust=1,size=4,fontface="bold") +
    theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"), panel.background=element_rect(fill="azure"))
  ## Combine plots
  plot.scenarios <- grid.arrange(plot.proj.MANAP, plot.proj.KMNP, plot.proj.MIKEA, ncol=3)
  ggsave(filename="manuscript/figures/scenarios.png",plot=plot.scenarios,width=21,height=10,unit=c("cm"))
  
  ##========================================
  ## Spatial probability of deforestation
  
  ## Rescale function for legend
  rescale <- function(x,from.min,from.max,to.min=0,to.max=1) {
      a <- from.min; b <- from.max; c <- to.min; d <- to.max
      int <- (b*c-a*d)/(b-a) ; slope <- (d-c)/(b-a)
      return(int+slope*x)
  }
  
  ## Colors
  theta_val <- c(values(theta_MANAP),values(theta_KMNP))
  v <- quantile(theta_val,c(0.75,0.90),na.rm=TRUE)
  mi <- 1
  ma <- 65535
  vr <- rescale(v,mi,ma)
    
  ## MANAP
  plot.proba.MANAP <- gplot(theta_MANAP,maxpixels=res.rast) + 
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradientn(colours=c("forestgreen","orange","red","black"),na.value="transparent",
                         values=c(0,vr,1), limits=c(0,ma),
                         breaks=seq(mi,ma,length.out=5),labels=c("0.00","0.25","0.50","0.75","1.00")) +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Morondava_BeloTsi.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Morondava_BeloTsi.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1500, hjust=0.5, nudge_x=3500) +
    geom_point(data=Lambokely_Kirindy.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Lambokely_Kirindy.df, aes(label=Name), size=3, vjust=0, nudge_y=1000, hjust=0.5) +
    coord_equal(xlim=c(xmin.MANAP,xmax.MANAP),ylim=c(ymin.MANAP,ymax.MANAP)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() + theme_base + theme(legend.justification="left",legend.position=c(0,0.525)) +
    theme(legend.key.width=unit(0.35,"cm"), legend.title=element_blank(),
          legend.background=element_rect(fill="azure")) +
    annotate("text",x=xmin.MANAP,y=ymax.MANAP,label="a",hjust=0,vjust=1,size=4,fontface="bold") +
    theme(plot.margin=unit(c(0,0.2,0,0),"cm"),
          panel.background=element_rect(fill="azure"))
  ## KMNP
  plot.proba.KMNP <- gplot(theta_KMNP,maxpixels=res.rast) + 
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradientn(colours=c("forestgreen","orange","red","black"),na.value="transparent",
                         values=c(0,vr,1),limits=c(0,ma),
                         breaks=seq(mi,ma,length.out=5),labels=c("0.00","0.25","0.50","0.75","1.00")) +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3250) +
    coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() + theme_base + 
    annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="b",hjust=0,vjust=1,size=4,fontface="bold") +
    theme(plot.margin=unit(c(0,0.2,0,0),"cm"),
          panel.background=element_rect(fill="azure"))
  ## MIKEA
  plot.proba.MIKEA <- gplot(theta_MIKEA,maxpixels=res.rast) + 
    geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradientn(colours=c("forestgreen","orange","red","black"),na.value="transparent",
                         values=c(0,vr,1),limits=c(0,ma),
                         breaks=seq(mi,ma,length.out=5),labels=c("0.00","0.25","0.50","0.75","1.00")) +
    geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
    geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
    geom_point(data=Morombe.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
    geom_text(data=Morombe.df, aes(label=Name), size=3, vjust=0, nudge_y=2000, hjust=0.5, nudge_x=-3500) +
    coord_equal(xlim=c(xmin.MIKEA,xmax.MIKEA),ylim=c(ymin.MIKEA,ymax.MIKEA)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() + theme_base + 
    annotate("text",x=xmin.MIKEA,y=ymax.MIKEA,label="c",hjust=0,vjust=1,size=4,fontface="bold") +
    theme(plot.margin=unit(c(0,0.2,0,0),"cm"),
          panel.background=element_rect(fill="azure"))
  
  ## Combine plots
  plot.proba <- grid.arrange(plot.proba.MANAP, plot.proba.KMNP, plot.proba.MIKEA, ncol=3)
  ggsave(filename="manuscript/figures/probability.png",plot=plot.proba,width=21,height=10,unit="cm")
  
  ##========================================
  ## Forest cover evolution
  
  ## Annual deforestation in percentage
  theta <- function(f2,f1,Y) {
    return(1-(1-(f1-f2)/f1)^(1/Y)) 
  }
  ## Table of results
  forest.cover <- data.frame(site=c("MANAP","KMNP","MIKEA"))
  forest.cover$area <- c(area.land.MANAP,area.land.KMNP,area.land.MIKEA)
  forest.cover$f1990 <- c(sum(values(for1990_MANAP),na.rm=TRUE),
                          sum(values(for1990_KMNP),na.rm=TRUE),
                          sum(values(for1990_MIKEA),na.rm=TRUE))
  forest.cover$f2000 <- c(sum(values(defor_MANAP) %in% c(1,2,3),na.rm=TRUE),
                          sum(values(defor_KMNP) %in% c(1,2,3),na.rm=TRUE),
                          sum(values(defor_MIKEA) %in% c(1,2,3),na.rm=TRUE))
  forest.cover$f2010 <- c(sum(values(defor_MANAP) %in% c(1,3),na.rm=TRUE),
                          sum(values(defor_KMNP) %in% c(1,3),na.rm=TRUE),
                          sum(values(defor_MIKEA) %in% c(1,3),na.rm=TRUE))
  forest.cover$f2017 <- c(sum(values(defor_MANAP)==1,na.rm=TRUE),
                          sum(values(defor_KMNP)==1,na.rm=TRUE),
                          sum(values(defor_MIKEA)==1,na.rm=TRUE))
  forest.cover$f2050.S1 <- c(sum(values(proj_MANAP) %in% c(1,3),na.rm=TRUE),
                             sum(values(proj_KMNP) %in% c(1,3),na.rm=TRUE),
                             sum(values(proj_MIKEA) %in% c(1,3),na.rm=TRUE))
  forest.cover$f2050.S2 <- c(sum(values(proj_MANAP)==1,na.rm=TRUE),
                             sum(values(proj_KMNP)==1,na.rm=TRUE),
                             sum(values(proj_MIKEA)==1,na.rm=TRUE))
  forest.cover$d9000.ha <- c(forest.cover$f1990-forest.cover$f2000)/10
  forest.cover$d0010.ha <- c(forest.cover$f2000-forest.cover$f2010)/10
  forest.cover$d1017.ha <- c(forest.cover$f2010-forest.cover$f2017)/7
  ## Deforestation rates in %
  forest.cover$d9000.p <- round(theta(forest.cover$f2000,forest.cover$f1990,10)*100,2)
  forest.cover$d0010.p <- round(theta(forest.cover$f2010,forest.cover$f2000,10)*100,2)
  forest.cover$d1017.p <- round(theta(forest.cover$f2017,forest.cover$f2010,7)*100,2)
  ## Transform pixels in ha
  forest.cover[,c(3:11)] <- round(forest.cover[,c(3:11)]*30*30/10000)
  
  ## Save objects
  # load("menabe.rda")
  save(forest.cover,e.MANAP.latlong,e.KMNP.latlong,e.MIKEA.latlong,
       mod.perf,perc,file="manuscript/menabe.rda")

##========================
## Assemble photos (ImageMagick need to be installed, see at www.imagemagick.org)
system("sh scripts/photo.sh")

##=========================
## FAO and UN Comtrade stat

# FAO
fao_df <- read.table("data/FAOSTAT_data_4-25-2019.csv", header=TRUE, sep=",")
fao_res <- fao_df %>% 
  dplyr::mutate(Item=ifelse(Item=="Groundnuts, with shell",
                            "Peanut", "Maize")) %>%
  dplyr::filter(Year %in% c(2010:2017)) %>%
  dplyr::group_by(Item, Element) %>%
  dplyr::summarize(Mean=mean(Value)) %>%
  tidyr::spread(key=Element, value=Mean) %>%
  dplyr::rename(Crop=Item, Area=2) %>%
  dplyr::mutate(Yield=round(Yield/10000,1),
                Production=round(Production),
                Area=round(Area)) %>%
  dplyr::select(1,2,4,3) %>%
  as.data.frame()

# Change in production
prod_df <- fao_df %>% 
  dplyr::mutate(Item=ifelse(Item=="Groundnuts, with shell",
                            "Peanut", "Maize")) %>%
  dplyr::filter(Year %in% c(2009:2017), Element=="Production") %>%
  dplyr::rename(Crop=Item, Production=Value) %>%
  dplyr::select(Crop, Year, Production)
prod_df_peanut <- prod_df %>%
  dplyr::filter(Crop=="Peanut")

# Plot production peanut
p <- ggplot(prod_df_peanut, aes(Year, Production/1000)) + 
  geom_line(col="brown") + 
  scale_x_continuous(name="Year", limits=c(2009, 2017),
                     breaks=seq(2009, 2017, 1)) +
  scale_y_continuous(name="Production (KT/yr)", limits=c(0, 60),
                     breaks=seq(0, 60, 10)) +
  theme_bw() +
  theme(legend.position=c(0.05, 0.95),
        legend.background=element_blank(),
        legend.justification=c(0,1),
        legend.title=element_blank())
ggsave("manuscript/figures/production.png", p, width=5, height=4)

# UN Comtrade
comtrade_df <- read.table("data/comtrade.csv", header=TRUE, sep=",")
comtrade_res <- comtrade_df %>% 
  dplyr::mutate(Crop=ifelse(Commodity.Code==1005,"Maize","Peanut")) %>%
  dplyr::group_by(Crop, Year) %>%
  dplyr::summarize(Exports=sum(Alt.Qty.Unit)/1000)
comtrade_res_2010_2017 <- comtrade_res %>% 
  dplyr::filter(Year %in% c(2010:2017)) %>%
  dplyr::group_by(Crop) %>%
  dplyr::summarize(Mean=mean(Exports))

# Combine data
trade_df <- fao_res %>%
  dplyr::mutate(Exports=round(comtrade_res_2010_2017$Mean))
write_csv(trade_df, "manuscript/tables/faostat.csv")

# Plot exports
p <- ggplot(comtrade_res, aes(Year, Exports/1000, col=Crop)) + 
  scale_color_manual(values = c("Peanut"="brown", "Maize"="orange")) +
  geom_line() + 
  scale_x_continuous(name="Year", limits=c(2009, 2017),
                     breaks=seq(2009, 2017, 1)) +
  scale_y_continuous(name="Exports (KT/yr)", limits=c(0, 30),
                     breaks=seq(0, 30, 5)) +
  theme_bw() +
  theme(legend.position=c(0.05, 0.95),
        legend.background=element_blank(),
        legend.justification=c(0,1),
        legend.title=element_blank())
ggsave("manuscript/figures/exports.png", p, width=5, height=4)

##===========================================================================
## End of script
##===========================================================================

