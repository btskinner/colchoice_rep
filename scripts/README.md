# Scripts

Scripts to reproduce analyses and tables should be run in the
following order.  

1. `makedata.r`  
2. `analysis.do` (NB: this may take a long time to run)  
3. `descriptives.r`  
4. `predictions.r`  

Two helper scripts, `csv2tex.r` and `est2csv.do`, should remain in the
directory, but do not need to be called on their own.

After the scripts are run, paper tables can be reproduced using
`tables.rnw` in the `./tables` subdirectory.  
