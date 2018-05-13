capture log close
log using "analysis.txt", replace text
//******************************************************************************
//
// PROJ: College choice conditional logit
// FILE: analysis.do
// NAME: Benjamin Skinner
// INIT: 16 November 2015
//
//******************************************************************************

// clear memory
clear all

// set matsize
set matsize 11000

// globals
global ddir "../data/"
global mdir "../models/"

// set up model
local v1 cost cost_sq dist dist_sq dist_cb instfte instfte_sq sfr ttfacpct
local v2 sathi sathi_sq satlo satlo_sq fte fte_sq i.twoyr i.twoyr#c.cost
local v3 i.twoyr#c.cost_sq i.twoyr#c.dist i.twoyr#c.dist_sq i.twoyr#c.fte i.twoyr#c.fte_sq

// =============================================================================
// FULL ESTIMATES: ATTEND
// =============================================================================

// read in data
use ${ddir}choice.dta, clear

// subset
keep if enroll2yr == 1

// attend
asclogit attend `v1' `v2' `v3', case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_main, replace

// apply
asclogit apply `v1' `v2' `v3', case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_main_apply, replace

// attend | apply
asclogit attend `v1' `v2' `v3' if apply == 1, case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_main_apply_attend, replace

// attend | accept
asclogit attend `v1' `v2' `v3' if accept == 1, case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_main_accept_attend, replace

// =============================================================================
// LOW INCOME ESTIMATES
// =============================================================================

// read in data
use ${ddir}choice.dta, clear

// subset
keep if enroll2yr == 1 & lowinc == 1

// attend
asclogit attend `v1' `v2' `v3', case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_lowinc, replace

// apply
asclogit apply `v1' `v2' `v3', case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_lowinc_apply, replace

// attend | apply
asclogit attend `v1' `v2' `v3' if apply == 1, case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_lowinc_apply_attend, replace

// attend | accept
asclogit attend `v1' `v2' `v3' if accept == 1, case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_lowinc_accept_attend, replace

// =============================================================================
// HIGH SAT ESTIMATES
// =============================================================================

// read in data
use ${ddir}choice.dta, clear

// subset
keep if enroll2yr == 1 & hisat == 1

// attend
asclogit attend `v1' `v2' `v3', case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_hisat, replace

// apply
asclogit apply `v1' `v2' `v3', case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_hisat_apply, replace

// attend | apply
asclogit attend `v1' `v2' `v3' if apply == 1, case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_hisat_apply_attend, replace

// attend | accept
asclogit attend `v1' `v2' `v3' if accept == 1, case(stu_id) alt(unitid) vce(cluster stu_id) nocons or
estimates save ${mdir}asclogit_hisat_accept_attend, replace

// =============================================================================
// Run file to convert estimates to csv files
// =============================================================================

do est2csv.do

log close
exit
// =============================================================================
// END FILE
// =============================================================================
