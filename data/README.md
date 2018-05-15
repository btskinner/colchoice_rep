# Data

All data files should placed in this directory.

## Restricted data files

The following ELS restricted data files are needed:  

* `Geocodes.dat`  
* `byf3stu.dta`  
* `f2inst.dta`  
* `f3inst.dta`  
* `barrons2004.csv`  

## Public files

The following public files need to be downloaded:  

### IPEDS

* [`eap2004.csv`](https://nces.ed.gov/ipeds/datacenter/data/EAP2004.zip)  
* [`hd2004.csv`](https://nces.ed.gov/ipeds/datacenter/data/HD2004.zip)  
* [`ic2004.csv`](https://nces.ed.gov/ipeds/datacenter/data/IC2004.zip)  
* [`ic2004_ay.csv`](https://nces.ed.gov/ipeds/datacenter/data/IC2004_AY.zip)  
* [`sal2004_a.csv`](https://nces.ed.gov/ipeds/datacenter/data/SAL2004_A.zip)  

### Delta Cost Project

* [`delta_public_00_12.csv`](https://deltacostproject.org/delta-cost-project-database)  

### Census

* [`blkgrp_pop_centroid_withname.txt`](http://www2.census.gov/geo/docs/reference/cenpop2000/blkgrp/bg_popcen.zip)  
*
[`tract_pop.txt`](https://www2.census.gov/geo/docs/reference/cenpop2000/tract/tract_pop.txt)  

## Included

Other files already in the data directory:  

* `county_cen.txt` (From [county_centers.csv](https://github.com/btskinner/spatial))  
* `colfips2004.csv` (See note below)  

`colfips2004.csv` can be made from the file created by
`./scripts/ipedsgeo.r`:

```r
## library
library(tidyverse)

## read in file
load(../data/ipedsyeargeo.rda)

## rename
df <- geolist$y2004 %>% select(unitid, lon = longitud, lat = latitude)

## write
write_csv(df, '../data/colfips2004.csv)

```

## Simulation

To run the simulations and create the maps in the appendix, you will
need the following data files from the [American Community Survey](https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml):  

* `ACS_10_5YR_DP05_1.csv`  
* `ACS_10_5YR_DP05_2.csv`  
* `ACS_10_5YR_S1501_1.csv`  
* `ACS_10_5YR_S1501_2.csv`  
* `ACS_10_5YR_S1901_1.csv`  
* `ACS_10_5YR_S1901_2.csv`  
* `ACS_10_5YR_S2301_1.csv`  
* `ACS_10_5YR_S2301_2.csv`  

You will also need TIGER census tract shapefiles (one file for each
state) for the year 2010. They can be downloaded
[here](https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html). Once
unzipped, the files should come in threes and look like:  

* `gz_2010_01_140_00_500k.dbf`  
* `gz_2010_01_140_00_500k.prj`  
* `gz_2010_01_140_00_500k.shp`  
* `gz_2010_01_140_00_500k.shx`  
* `gz_2010_01_140_00_500k.xml`  
* `gz_2010_02_140_00_500k.dbf`  
* `gz_2010_02_140_00_500k.prj`  
* `gz_2010_02_140_00_500k.shp`  
* `gz_2010_02_140_00_500k.shx`  
* `gz_2010_02_140_00_500k.xml`  
* ...

A set of files for the United States as a whole (`gz_2010_us_040_00_500k.*`), can be downloaded
[here](https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_040_00_500k.zip).  

