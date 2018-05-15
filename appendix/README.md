# Appendix simulations

The simulations using synthetic students found in the paper appendix
were originally run using the [ACCRE computing cluster at Vanderbilt
University](https://www.vanderbilt.edu/accre/). While it is not
necessary to use a full cluster to run these simulations, the large
number of census tracks and the [embarrassingly
parallel](https://en.wikipedia.org/wiki/Embarrassingly_parallel)
nature of the problem means that splitting the task across multiple
cores is worthwhile.

The two core R scripts

1. `simulation_pred.R`
2. `simulation_maps_us.r`

create the predicted probabilities of enrollment for synthetic
students and then map those probabilities onto a map of the United
States, respectively.

## To Run

1. Make ACS data (`cleanacs.r`)  
2. Split census track centroids (`tractsplit.sh`)  
3. Make predictions (`simulation_pred.r`)  
4. Combine predictions into one file (`combinepreds.sh`)  
5. Make US maps (`simulation_maps_us.r`)  

Each step is below.

### 1. Make ACS data (`cleanacs.r`)

To make the synthetic "modal" students, first run `cleanacs.r` using
the appropriate ACS data files (see ../data/README.md).

### 2. Split census track centroids (`tractsplit.sh`)

To split the census tracks (approximately 66k) into files with only
1000 tracks in each (for parallelization), use `tractsplit.sh`:

```bash
tractsplit.sh ../data/tract_pop.txt
```

### 3. Make predictions (`simulation_pred.r`)

After splitting the census tracks, an example prediction script, run
from the command line would be:

```bash
Rscript --vanilla simulation_pred.R ../data/tract_split_<#>.txt
../<path>/<to>/<model>/<estimates> ../<path>/<to>/<model>/<estimates>
<type> asclogit_<estimate type I>_<type>_mat <estimate type II>_<type>_logit
```

where  

* `#` is the file number (0-65)  
* `type` is left blank for `attend`, `apply`, `apply_attend`  
* `estimate type I` is `main`, `hisat`, or `lowinc`  
* `estimate type II` is `main`, `main_marg`, `neartyr`, or
  `neartyr_marg`  

#### Example

```bash
Rscript --vanilla simulation_pred.R ../data/tract_split_0.txt ../models ../models
attend asclogit_main_mat main_logit
```

A simple bash script to loop or parallelize this command for each
census tract file split would be useful (see `simulation_pred.slurm`
for script used on cluster).

### 4. Combine predictions into one file (`combinepreds.sh`)

A prediction file will be created for each census tract split file. To
recombine them for mapping, use the following script and command:  

```bash
./combinepreds.sh ../<path>/<to>/<predictions> mostlikpred_attend_
```

**NB** If you made predictions using either the application or
attendance conditional on application estimates, change `attend` in
the command line call to whatever you used in step 3 (*e.g.*, `apply`).

### 5. Make US maps (`simulation_maps_us.r`)

```bash
Rscript --vanilla simulation_maps_us.R <cut>
../<path>/<to>/<predictions>/mostlikpred_<type>_us.csv
../<path>/<to>/<tract>/<shapefiles> ../<path>/<to>/<output> <type>
```

where  

* `cut` is t-test proportion used as single digit integer. For
  example, to test whether proportion is significantly different from
  even odds, 50/50, use `5`, which will be converted to `0.50` in the
  script.  
* `type` is same as above  

#### Example

```bash
Rscript --vanilla simulation_maps_us.R 5
../models/mostlikpred_attend_us.csv ../data ./ attend
```

The resulting map will be about 50Mb and may take a while to compile.


