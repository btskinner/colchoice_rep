
// PROJ: College choice
// FILE: est2csv.do
// AUTH: Benjamin Skinner
// DATE: 17 November 2015

// clear memory
clear all

// set directories
global mdir "../models/"
global tdir "../tables/csv/"

program define cleandat

// clear memory
clear

// take single *.ster argument, w/option for directory
syntax anything(name=est) [, estdir(str) outdir(str)]

// strip .ster from estimates name
gettoken stub : est, parse(" .")

// load estimates (conditional on directory path option)
if "`estdir'" != "" {
    local fname "`estdir'`est'"
}
else {
    local fname "`est'"
}

estimates use `fname'

// replay estimates
estimates replay

// store cases (unique students)
local cases = `e(N_case)'
di "Number of cases: `cases'"

// store observations (students by choice)
local obs = `e(N)'
di "Number of observations: `obs'"

// store transposed r(table)
mat outmat = r(table)'
mat li outmat

// make matrix into variables
svmat outmat

// keep only beta and z-score (columns 1 and 3)
keep outmat1 outmat3

// rename columns
rename outmat1 b
rename outmat3 z

// drop if z-score is missing (omitted category)
drop if z == .

// add N and cases columns
gen N = round(`obs',100)
gen case = round(`cases',10)

// save as csv file
if "`outdir'" != "" {
    local fname "`outdir'`stub'_mat.csv"
}
else {
    local fname "`stub'_mat.csv"
}

outsheet using `fname', comma replace

end

// convert estimates to csv
local ests : dir "$mdir" files "*.ster"

foreach f of local ests {
    di "Now working with: `f'"
    cleandat `f', estdir($mdir) outdir($tdir)
}

// end file
exit

