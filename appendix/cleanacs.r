################################################################################
##
## <PROJ> College choice update
## <FILE> cleanacs.r
## <AUTH> Benjamin Skinner
## <INIT> 07 February 2016
##
################################################################################

## clear memory
rm(list=ls())

## libraries
libs <- c('dplyr','readr','tidyr')
lapply(libs, require, character.only = TRUE)

## directories
ddir <- '../data/'

## quick function
`%+%` <- function(a,b) paste0(a,b)
meanimp <- function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)

## ===============================================
## READ FILES
## ===============================================

## unemployment
unem <- read_csv(ddir %+% 'ACS_10_5YR_S2301_1.csv') %>%
    mutate(GEO.id2 = as.numeric(GEO.id2)) %>%
    rbind_list(read_csv(ddir %+% 'ACS_10_5YR_S2301_2.csv')) %>%
    select(geoid = GEO.id2,
           unemrate = HC04_EST_VC01) %>%
    arrange(geoid)

## income
inc <- read_csv(ddir %+% 'ACS_10_5YR_S1901_2.csv') %>%
    mutate(GEO.id2 = as.numeric(GEO.id2)) %>%
    rbind_list(read_csv(ddir %+% 'ACS_10_5YR_S1901_1.csv')) %>%
    select(geoid = GEO.id2,
           medinc = HC01_EST_VC13,
           meaninc = HC01_EST_VC15) %>%
    arrange(geoid)

## education
educ <- read_csv(ddir %+% 'ACS_10_5YR_S1501_1.csv') %>%
    mutate(GEO.id2 = as.numeric(GEO.id2)) %>%
    rbind_list(read_csv(ddir %+% 'ACS_10_5YR_S1501_2.csv')) %>%
    select(geoid = GEO.id2,
           pop = HC01_EST_VC07,
           female = HC03_EST_VC07,
           lt9 = HC01_EST_VC08,
           somehs = HC01_EST_VC09,
           hsged = HC01_EST_VC10,
           somecol = HC01_EST_VC11,
           aa = HC01_EST_VC12,
           ba = HC01_EST_VC13,
           grad = HC01_EST_VC14) %>%
    arrange(geoid)

## demographics: sex and race/ethnicity
demo <- read_csv(ddir %+% 'ACS_10_5YR_DP05_2.csv') %>%
    mutate(GEO.id2 = as.numeric(GEO.id2)) %>%
    rbind_list(read_csv(ddir %+% 'ACS_10_5YR_DP05_1.csv')) %>%
    mutate(asianpi = HC03_VC91 + HC03_VC92,
           multrace = HC03_VC93 + HC03_VC94) %>%
    select(geoid = GEO.id2,
           hisp = HC03_VC82,
           white = HC03_VC88,
           black = HC03_VC89,
           amerind = HC03_VC90,
           asianpi, multrace) %>%
    arrange(geoid)

## ===============================================
## JOIN DATA FRAMES
## ===============================================

dat <- unem %>%
    left_join(inc,  by = 'geoid') %>%
    left_join(educ, by = 'geoid') %>%
    left_join(demo, by = 'geoid')

## ===============================================
## NEW VARIABLES (TO MATCH)
## ===============================================

dat <- dat %>%
    arrange(geoid) %>%
    mutate(fips = round(geoid / 1000000),
           stfips = round(fips / 1000)) %>%
    mutate(inc = ifelse(medinc<=1000,2,inc),
           inc = ifelse(medinc>1000&medinc<=5000,3,inc),
           inc = ifelse(medinc>5000&medinc<=10000,4,inc),
           inc = ifelse(medinc>10000&medinc<=15000,5,inc),
           inc = ifelse(medinc>15000&medinc<=20000,6,inc),
           inc = ifelse(medinc>20000&medinc<=25000,7,inc),
           inc = ifelse(medinc>25000&medinc<=35000,8,inc),
           inc = ifelse(medinc>35000&medinc<=50000,9,inc),
           inc = ifelse(medinc>50000&medinc<=75000,10,inc),
           inc = ifelse(medinc>75000&medinc<=100000,11,inc),
           inc = ifelse(medinc>100000&medinc<=200000,12,inc),
           inc = ifelse(medinc>200000,13,inc),
           inc = as.integer(inc))

datmean <- dat %>%
    group_by(fips) %>%
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    mutate_(., .dots=setNames(names(dat), names(dat) %+% '_mean')) %>%
    select(contains('_mean')) %>%
    rename(fips = fips_mean) %>%
    select(-c(geoid_mean))

datmean2 <- dat %>%
    group_by(stfips) %>%
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    mutate_(., .dots=setNames(names(dat), names(dat) %+% '_mean2')) %>%
    select(contains('_mean2')) %>%
    rename(stfips = stfips_mean2) %>%
    select(-c(geoid_mean2,fips_mean2))

## mean impute: county level first, then state
dat <- dat %>%
    left_join(datmean, by = 'fips') %>%
    left_join(datmean2, by = 'stfips') %>%
    mutate(hisp = ifelse(is.na(hisp),hisp_mean,hisp),
           white = ifelse(is.na(white),white_mean,white),
           black = ifelse(is.na(black),black_mean,black),
           amerind = ifelse(is.na(amerind),amerind_mean,amerind),
           asianpi = ifelse(is.na(asianpi),asianpi_mean,asianpi),
           multrace = ifelse(is.na(multrace),multrace_mean,multrace),
           lt9 = ifelse(is.na(lt9),lt9_mean,lt9),
           somehs = ifelse(is.na(somehs),somehs_mean,somehs),
           hsged = ifelse(is.na(hsged),hsged_mean,hsged),
           somecol = ifelse(is.na(somecol),somecol_mean,somecol),
           aa = ifelse(is.na(aa),aa_mean,aa),
           ba = ifelse(is.na(ba),ba_mean,ba),
           grad = ifelse(is.na(grad),grad_mean,grad),
           unemrate = ifelse(is.na(unemrate),unemrate_mean,unemrate),
           medinc = ifelse(is.na(medinc),medinc_mean,medinc),
           inc = ifelse(is.na(inc),inc_mean,inc),
           meaninc = ifelse(is.na(meaninc),meaninc_mean,meaninc),
           female = ifelse(is.na(female),female_mean,female)) %>%
    mutate(hisp = ifelse(is.na(hisp),hisp_mean2,hisp),
           white = ifelse(is.na(white),white_mean2,white),
           black = ifelse(is.na(black),black_mean2,black),
           amerind = ifelse(is.na(amerind),amerind_mean2,amerind),
           asianpi = ifelse(is.na(asianpi),asianpi_mean2,asianpi),
           multrace = ifelse(is.na(multrace),multrace_mean2,multrace),
           lt9 = ifelse(is.na(lt9),lt9_mean2,lt9),
           somehs = ifelse(is.na(somehs),somehs_mean2,somehs),
           hsged = ifelse(is.na(hsged),hsged_mean2,hsged),
           somecol = ifelse(is.na(somecol),somecol_mean2,somecol),
           aa = ifelse(is.na(aa),aa_mean2,aa),
           ba = ifelse(is.na(ba),ba_mean2,ba),
           grad = ifelse(is.na(grad),grad_mean2,grad),
           unemrate = ifelse(is.na(unemrate),unemrate_mean2,unemrate),
           medinc = ifelse(is.na(medinc),medinc_mean2,medinc),
           inc = ifelse(is.na(inc),inc_mean2,inc),
           meaninc = ifelse(is.na(meaninc),meaninc_mean2,meaninc),
           female = ifelse(is.na(female),female_mean2,female)) %>%
    select(-matches('_mean'))

## ===============================================
## WRITE FILES
## ===============================================

write.csv(dat, file = paste0(ddir,'acs0610.csv'), row.names = FALSE)

## =============================================================================
## END FILE
################################################################################
