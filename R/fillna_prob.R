##=====================================
## Create new grass location in UTM 38S
#dir.create("grassdata")
#system("grass72 -c epsg:32738 grassdata/menabe")  # Ignore errors

## Connect R to grass location
## Make sure that /usr/lib/grass72/lib is in your PATH in RStudio
## On Linux, find the path to GRASS GIS with: $ grass72 --config path
## It should be somethin like: "/usr/lib/grass72"
## On Windows, find the path to GRASS GIS with: C:\>grass72.bat --config path
## If you use OSGeo4W, it should be: "C:\OSGeo4W\apps\grass\grass-7.2"
Sys.setenv(LD_LIBRARY_PATH=paste("/usr/lib/grass72/lib", Sys.getenv("LD_LIBRARY_PATH"),sep=":"))

## Initialize GRASS
library(rgrass7)
initGRASS(gisBase="/usr/lib/grass72",home=tempdir(), 
          gisDbase="grassdata",
          location="menabe",mapset="PERMANENT",
          override=TRUE)

reg <- c("MANAP","KMNP","MIKEA")
nreg <- length(reg)
for (i in 1:nreg) {
  # Import into grass
  system(paste0("r.in.gdal --o input=rast/prob2010_orig_",reg[i],".tif output=prob2010_",reg[i]))
  system(paste0("r.in.gdal --o input=rast/for2010_",reg[i],".tif output=for2010_",reg[i]))
  # Set region
  system(paste0("g.region rast=for2010_",reg[i]," -ap"))
  # Compute neighborhood mean probability (-c for circular neighborhood)
  system(paste0("r.neighbors --o -c input=prob2010_",reg[i]," selection=for2010_",reg[i],
                " output=prob_neigh_",reg[i]," method=average size=33"))
  # Fill null with mean values
  system(paste0("r.mapcalc --o 'prob_fill_",reg[i]," = if(isnull(prob2010_",reg[i],
  "), prob_neigh_",reg[i],", prob2010_",reg[i],")'"))
  # Export
  execGRASS("r.out.gdal", flags=c("overwrite","f"),
            input=paste0("prob_fill_",reg[i]),
            nodata=-9999,
            output=paste0("rast/prob2010_",reg[i],".tif"),
            type="Int32", createopt="compress=lzw,predictor=2")
}

#=