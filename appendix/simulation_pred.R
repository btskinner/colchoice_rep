#################################################################################
##
## <PROJ> College choice update
## <FILE> simulation_pred.R
## <AUTH> Benjamin Skinner
## <INIT> 03 February 2016
##
################################################################################

## clear memory
rm(list = ls())

## add local package directory
.libPaths('~/R/rlib/3.2.0')

## libraries
libs <- c('dplyr','geosphere','readr','tidyr')
lapply(libs, require, character.only = TRUE)

## options
options(scipen = 999, stringsAsFactors = FALSE)

## formulas
m2miles <- 0.0006214

## arguments
args <- commandArgs(trailingOnly = TRUE)
tfil <- args[1]                         # census tract file
idir <- args[2]                         # other input directory
odir <- args[3]                         # output directory
type <- args[4]                         # attend/apply
csv1 <- args[5]                         # c.logit beta
csv2 <- args[6]                         # logit beta/vcov (stub)
f <- gsub('.*_(\\d{1,2}).txt','\\1',tfil)

## quick functions
`%+%` <- function(a,b) paste0(a,b)
invlogit <- function(x) { 1 / (1 + exp(-x)) }

## ===================================================================
## COMPUTE DISTANCES
## ===================================================================

## read in institutional data
inst <- read_csv(idir %+% 'institutions.csv') %>% arrange(unitid)

## read in census tract data from arguments
trac <- read_csv(tfil, col_names = c('geoid','latitude','longitud')) %>%
    arrange(geoid)

## get vectors of names
unitid <- inst %>% select(unitid) %>% .[['unitid']]
geoid <- trac %>% select(geoid) %>% .[['geoid']]

## matrices of lon/lat
colmat <- data.matrix(inst %>% select(longitud,latitude))
trcmat <- data.matrix(trac %>% select(longitud,latitude))

## compute distances
distmat <- distm(colmat,trcmat)

## add row and column names
rownames(distmat) <- unitid
colnames(distmat) <- geoid

## convert to 100 miles
distmat <- distmat * m2miles / 100

message('\nDistance computation complete.')

## --------------------------------
## NEAREST PUBLIC TWO YEAR
## --------------------------------

## make public twoyear mask
ptmask <- replicate(nrow(trac),
                    as.integer(inst$public == 1 & inst$twoyr == 1))
ptmask <- ifelse(ptmask == 0, Inf, ptmask)

## mask distance matrix to only public twoyears
ptwmat <- distmat * ptmask

## get unitid and distance to nearest public twoyear
neartwo <- apply(ptwmat, 2, FUN = function(x){
    index <- which.min(x)
    return(cbind(names(x[index]), x[index]))
})

## arrange
neartwo <- as.data.frame(cbind(rownames(t(neartwo)),t(neartwo))) %>%
    mutate(geoid = as.numeric(as.character(V1)),
           unitid = as.integer(as.character(V2)),
           dist = as.numeric(as.character(V3))) %>%
    select(geoid,unitid,dist) %>%
    left_join(inst, by = 'unitid') %>%
    mutate(cost = ifelse(substr(geoid,1,2) == stfips,
                         instcost,
                         outscost)) %>%
    select(geoid, unitid, dist, fte, satpct, instfte, cost)

message('\nNearest public two-year association complete.')

## --------------------------------
## MOST LIKELY COLLEGE
## --------------------------------

## read in coefficient data (keep only betas (b))
b <- read_csv(idir %+% csv1 %+% '.csv') %>% select(b) %>% .[['b']]

########################################
## Coefficient Order
## (NB: no intercept b/c choice model [really a lot of intercepts])
##
## (01) cost
## (02) cost_sq
## (03) dist
## (04) dist_sq
## (05) dist_cb
## (06) instfte
## (07) instfte_sq
## (08) stf
## (09) ttfacpct
## (10) sathi
## (11) sathi_sq
## (12) satlo
## (13) satlo_sq
## (14) fte
## (15) fte_sq
## (16) twoyr
## (17) twoyr * cost
## (18) twoyr * cost_sq
## (19) twoyr * dist
## (20) twoyr * dist_sq
## (21) twoyr * fte
## (22) twoyr * fte_sq
##
########################################

## init vector
mostlikvec <- vector('list', length = length(geoid))

message('\nStarting most likely college computation...\n')

## loop through each census tract
for(gid in 1:length(geoid)){

    message('Working through tract: ' %+% geoid[gid])

    ## subset to single tract
    tr <- as.data.frame(distmat[,gid]) %>%
        add_rownames(var = 'unitid') %>%
        mutate(dist = distmat[,gid],
               unitid = as.integer(unitid)) %>%
        select(unitid, dist) %>%
        left_join(inst, by = 'unitid') %>%
        mutate(cost = ifelse(substr(geoid[gid], 1, 2) == stfips,
                             instcost,
                             outscost),
               satdiff1 = -4,
               satdiff2 = -2,
               satdiff3 = 0,
               satdiff4 = 2,
               satdiff5 = 4,
               sathi1 = ifelse(satdiff1 > 0, satdiff1, 0),
               sathi2 = ifelse(satdiff2 > 0, satdiff2, 0),
               sathi3 = ifelse(satdiff3 > 0, satdiff3, 0),
               sathi4 = ifelse(satdiff4 > 0, satdiff4, 0),
               sathi5 = ifelse(satdiff5 > 0, satdiff5, 0),
               satlo1 = ifelse(satdiff1 < 0, satdiff1, 0),
               satlo2 = ifelse(satdiff2 < 0, satdiff2, 0),
               satlo3 = ifelse(satdiff3 < 0, satdiff3, 0),
               satlo4 = ifelse(satdiff4 < 0, satdiff4, 0),
               satlo5 = ifelse(satdiff5 < 0, satdiff5, 0)) %>%
        arrange(unitid) %>%
        select(unitid,cost,dist,instfte,sfr,ttfacpct,twoyr,fte,
               sathi1,sathi2,sathi3,sathi4,sathi5,
               satlo1,satlo2,satlo3,satlo4,satlo5)

    ## get most likely for each SAT score
    varsatvec <- vector('list', 5)
    for(i in 1:5) {

        sathi <- eval(parse(text = 'tr$sathi' %+% i))
        satlo <- eval(parse(text = 'tr$satlo' %+% i))

        ## create design matrix (match coef order above)
        x <- cbind(tr$cost,
                   tr$cost^2,
                   tr$dist,
                   tr$dist^2,
                   tr$dist^3,
                   tr$instfte,
                   tr$instfte^2,
                   tr$sfr,
                   tr$ttfacpct,
                   sathi,
                   sathi^2,
                   satlo,
                   satlo^2,
                   tr$fte,
                   tr$fte^2,
                   tr$twoyr,
                   tr$twoyr*tr$cost,
                   tr$twoyr*tr$cost^2,
                   tr$twoyr*tr$dist,
                   tr$twoyr*tr$dist^2,
                   tr$twoyr*tr$fte,
                   tr$twoyr*tr$fte^2)

        ## make predictions
        p <- x %*% b
        pred <- invlogit(p)

        ## store
        varsat <- cbind(geoid[gid], tr$unitid, tr$dist, pred)
        varsat <- varsat[which.max(varsat[,4]),]
        varsatvec[[i]] <- t(varsat)
    }

    ## collapse
    mostlik <- data.frame(cbind(seq(1, 5, 1),
                                do.call('rbind', varsatvec)))
    names(mostlik) <- c('satdiff', 'geoid', 'unitid', 'dist', 'pred')

    ## store all in vector
    mostlikvec[[gid]] <- mostlik

}

## collapse
mostlik <- rbind_all(mostlikvec) %>%
    mutate(geoid = as.double(geoid),
           unitid = as.integer(unitid))

## merge with school data
mostlik <- mostlik %>%
    left_join(inst, by='unitid') %>%
    mutate(cost = ifelse(substr(geoid,1,2) == stfips,
                         instcost,
                         outscost),
           dist = as.double(dist)) %>%
    select(geoid,
           satdiff,
           unitid,
           dist,
           fte,
           satpct,
           instfte,
           cost,
           pred)

message('\nMost likely college computation complete.')

## ===================================================================
## MERGE WITH ACS DATA FOR PREDICTION
## ===================================================================

## read in ACS 2006-2010 data
acs <- read_csv(idir %+% 'acs0610.csv',
                col_types = cols(geoid = col_double()))

## join and drop missing (can't predict with missing values)
mostlik <- mostlik %>%
    left_join(acs, by = 'geoid') %>%
    mutate(stusatpct = NA,
           stusatpct = ifelse(satdiff == 1, .1, stusatpct),
           stusatpct = ifelse(satdiff == 2, .3, stusatpct),
           stusatpct = ifelse(satdiff == 3, .5, stusatpct),
           stusatpct = ifelse(satdiff == 4, .7, stusatpct),
           stusatpct = ifelse(satdiff == 5, .9, stusatpct))

## read in coefficient data (keep only betas (b)) and vcov
b <- read_csv(idir %+% csv2 %+% '_mat.csv') %>% select(b) %>% .[['b']]
vcov <- read_csv(idir %+% csv2 %+% '_vcov.csv') %>%
    .[,-1] %>%
    as.matrix(.)

## ===========================
## "Average" values
## ===========================

## female
female <- as.integer(mostlik$female / mostlik$pop >= .5)
female <- ifelse(is.na(female), 0, female)

## race/ethnicity
raceeth <- mostlik %>%
    select(white, hisp, black, amerind, asianpi, multrace)
raceeth <- apply(raceeth, 1, which.max)

hispanic <- as.integer(raceeth == 2)
black    <- as.integer(raceeth == 3)
amerind  <- as.integer(raceeth == 4)
asianpi  <- as.integer(raceeth == 5)
multrace <- as.integer(raceeth == 6)

## parental education
tracted <- mostlik %>%
    select(lt9, somehs, hsged, somecol, aa, ba, grad)
tracted <- apply(tracted,1,which.max)

pared <- rep(NA,nrow(mostlik))
pared[tracted %in% c(1,2)] <- 1
pared[tracted == 3] <- 2
pared[tracted == 4] <- 3
pared[tracted == 5] <- 4
pared[tracted == 6] <- 6
pared[tracted == 7] <- 7

## ===========================
## Design matrix
## ===========================

x <- cbind(1,                           # intercept
           mostlik$cost,                # cost
           mostlik$cost^2,              # cost^2
           mostlik$dist,                # distance
           mostlik$dist^2,              # distance^2
           mostlik$instfte,             # expenditures/fte
           mostlik$instfte^2,           # expenditures/fte^2
           mostlik$unemrate,            # tract-level unemployment %
           mostlik$stusatpct,           # student SAT %tile
           mostlik$inc,                 # ordered average income cat
           pared,                       # ordered cat. of parental ed
           female,                      # == 1 for female
           black,                       # == 1 for black
           hispanic,                    # == 1 for hispanic
           asianpi,                     # == 1 for asian/pacific isl.
           multrace,                    # == 1 for multiple races
           amerind,                     # == 1 for native american
           mostlik$fte,                 # # full-time equivalent
           mostlik$fte^2,               # fte^2
           mostlik$dist^3)              # distance^3

## linear combination
lincom <- x %*% b

## standard error of linear combination
se <- apply(x, 1, FUN = function(xx) {sqrt(t(xx) %*% vcov %*% xx)})

## 95% CI
lincom_lo95 <- lincom - qnorm(0.975) * se
lincom_hi95 <- lincom + qnorm(0.975) * se

## predictions
pp <- invlogit(lincom)
pp_lo95 <- invlogit(lincom_lo95)
pp_hi95 <- invlogit(lincom_hi95)

## bind it all together
mostlikpred <- data.frame(cbind(mostlik$geoid,
                                mostlik$stusatpct,
                                pp,
                                pp_lo95,
                                pp_hi95))

names(mostlikpred) <- c('geoid', 'stusatpct', 'pp', 'pp_lo95', 'pp_hi95')
mostlikpred$geoid <- sprintf('%011.0f', mostlikpred$geoid)

## ===================================================================
## WRITE FILES
## ===================================================================

write.csv(mostlikpred,
          file = odir %+% 'mostlikpred_' %+% type %+% '_' %+% f %+% '.csv',
          row.names = FALSE)
message('Wrote mostlikepred_' %+% type %+% '_' %+% f %+% '.csv to file.')

## =============================================================================
## END FILE
################################################################################
