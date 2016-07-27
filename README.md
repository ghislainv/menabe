# menabe

# Deforestation process in central Menabe, western Madagascar

## Data

Geographical data can be found in the `gisdata` folder:

- The `rast_raw` folder includes GeoTIFF raster files for Madagascar forest cover for the years 1990, 2000, 2010 and 2014 (`forXXXX.tif`) and the maps of the spatial probability of deforestation for the two study areas around the Kirindy-Mitea national park (`theta_KMNP.tif`) and the Menabe-Antimena new protected area (`theta_MANAP.tif`).
- The `vectors` folder includes some shapefiles for Madagascar boundaries, main localities, main roads, field observation points and the protected area network (SAPM). 
    
## R script

The R script `menabe.R` can be run to reproduce the results of the following study:

**Vieilledent G., C. Grinand, M. Pedrono, T. Rabetrano, J.-R. Rakotoarijaona, B. Rakotoarivelo, F. A. Rakotomalala, D. Razafimpahanana and F. Achard.** 2016. Increasing development aid and reducing poverty might not fix the inceasing deforestation problem in Madagascar dry tropical forests. _Conservation Letters_. Submitted.

To run the script, you can either execute the `menabe.sh` file or run the R script under a R GUI such as [RStudio](https://www.rstudio.com/).

## R Markdown file

The R Markdown file `menabe.Rmd` includes all the source code to combine text and computation results and generate the scientific report. It is called and compiled while executing the `menabe.R` R script.

## Additional folders

- The `bib` folder includes some bibliographic references (`biblio.bib`) and the bibliographic format (`*.csl`).
- The `photos` folder includes photos for illustration (biodiversity richness, causes of deforestation and field surveys).
- The `scripts` folder includes a bash script `photo.sh` to modify and combine photos for illustration with the [ImageMagick](http://www.imagemagick.org) software. 

## Results

All results are saved in the `figs` and `report` folders.

![Figure S1: **Spatial probability of deforestation for the year 2010.**](figs/probability.png)

Figure S1: **Spatial probability of deforestation for the year 2010.**


## License

Data and R script are available under the GNU General Public License version 3 (see `LICENSE` file).