# menabe

# Study of the deforestation process (causes, extent and modelling) around Kirindy-Mitea national park and Menabe-Antimena new protected area, western Madagascar

## Data

Geographical data can be found in the `gisdata` folder:

- The `rast_raw` folder includes GeoTIFF raster files for Madagascar forest cover for the years 1990, 2000, 2010 and 2014 (`forXXXX.tif`) and the maps of the spatial probability of deforestation for the two study areas around the Kirindy-Mitea national park (`theta_KMNP.tif`) and the Menabe-Antimena new protected area (`theta_MANAP.tif`).
- The `vectors` folder includes some shapefiles for Madagascar boundaries, main localities, main roads, field observation points and the protected area network (SAPM). 
- The `bib` folder includes some bibliographic references (`biblio.bib`) and the bibliographic format (`*.csl`).
    
## R script

The R script `menabe.R` can be run to reproduce the results of the following study:

**Vieilledent G., C. Grinand, M. Pedrono, T. Rabetrano, J.-R. Rakotoarijaona, B. Rakotoarivelo, F. A. Rakotomalala, D. Razafimpahanana and F. Achard.** 2016. Increasing development aid and reducing poverty might not fix the inceasing deforestation problem in Madagascar dry tropical forests. _Conservation Letters_. Submitted.

To run the script, you can either execute the `shellscript.sh` file or run the R script under a R GUI such as RStudio.

## R Markdown file

The R Markdown file `menabe.Rmd` includes all the source code to combine text and computation results and generate the scientific report. It is called and compiled while executing the `menabe.R` R script.

## Results

All results are saved in the `figs` and `report` folders.

![Figure 2: **Historical and forecasted deforestation in the study areas around the Kirindy-Mitea national park (KMNP) and Menabe-Antimena new protected area (MANAP).** Madagascar map is represented on the top left panel (a), with the Menabe-Antimena study area at the north and the Kinrindy-Mitea study area at the south (black rectangles). On each of the sub-panels, the boundaries of the protected areas are represented with black polygones (source: Rebioma project at http://rebioma.net). Main roads are represented with thin black lines (source: FTM BD500). Coast line is represented with a thin grey line. Morondava and Belo-sur-Tsiribihina are the main cities located near MANAP. Belo-sur-Mer is the main village located near KMNP. **a-b**: _Historical deforestation on the period 2000-2010-2014 for the Menabe-Antimena and Kirindy-Mitea study areas, respectively._ Green: forest cover in 2014, orange: 2000-2010 deforestation, red: 2010-2014 deforestation (source: BioSceneMada project at http://bioscenemada.net). In the Menabe-Antimena study area, the main cause of the deforestation is the slash-and-burn agriculture (_"hatsake"_) for maize and peanut crops. Most of the 2000-2014 deforestation occured around the villages of Kirindy and Lambokely. In the Kirindy-Mitea study area, the main causes of deforestation are (A) slash-and-burn agriculture, (B) cyclones followed by uncontrolled fires and (C) illegal logging. **c-d**: _Projected deforestation on the period 2010-2050._ Green: projected forest cover in 2050, light grey: 2010-2050 deforestation following conservative scenario S1 (projecting 1990-2010 mean annual deforestation), dark grey: 2010-2050 additional deforestation following scenario S2 (projecting 2000-2014 mean annual deforestation). Most of the 2010-2014 deforestation observed on panels (a-b) is included in the 2010-2050 projections. We predicted a loss of 36-55% of the forest cover in 2050 compared to 2000 depending on the scenario. Most of the remaining forest will be inside the protected areas.](figs/deforestation.png)

## License

Data and R script are available under the GNU General Public License version 3 (see `LICENSE` file).