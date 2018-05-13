################################################################################
##
## PROJ: College choice update
## FILE: descriptives.r
## AUTH: Benjamin Skinner
## INIT: 11 November 2015
##
################################################################################

## clear memory
rm(list = ls())

## libraries
libs <- c('dplyr','foreign','readr')
lapply(libs, require, character.only = TRUE)

## dirs
ddir <- '../data/'
tdir <- '../tables/csv/'

## functions
`%+%` <- function(a,b) paste(a, b, sep = '')

checkmiss <- function(x) {
    out <- sapply(x, FUN = function(z) {mean(is.na(z))})
    return(out)
}

## =============================================================================
## READ IN DATA
## =============================================================================

df <- read.dta(ddir %+% 'choice.dta') %>%
    select(stu_id, hisat, lowinc, attend, apply, accept) %>%
    tbl_df() %>%
    group_by(stu_id) %>%
    summarise_each(funs(max))%>%
    ungroup() %>%
    left_join(read_csv(ddir %+% 'studentdat.csv'), by = 'stu_id')
gc()

apps <- read.dta(ddir %+% 'choice.dta') %>%
    select(stu_id, hisat, lowinc, apply) %>%
    tbl_df() %>%
    group_by(stu_id) %>%
    summarise(hisat = max(hisat),
              lowinc = max(lowinc),
              apply = sum(apply))%>%
    ungroup() %>%
    left_join(read_csv(ddir %+% 'studentdat.csv'), by = 'stu_id') %>%
    filter(enroll2yr == 1) %>%
    select(stu_id, hisat, lowinc, apply)
gc()

## =============================================================================
## DESCRIPTIVES
## =============================================================================

mat <- matrix(NA_real_, 15, 9)

## set masks
enr <- (df$enroll2yr == 1)
his <- (df$hisat == 1)
low <- (df$lowinc == 1)
apphis <- (apps$hisat == 1)
applow <- (apps$lowinc == 1)

## attend/apply
mat[1,c(2,3,5,6,8,9)] <- c(mean(enr),
                           mean(!enr),
                           mean(enr[his]),
                           mean(!enr[his]),
                           mean(enr[low]),
                           mean(!enr[low]))
## gender
mat[2,] <- c(mean(df$female),
             mean(df$female[enr]),
             mean(df$female[!enr]),
             mean(df$female[his]),
             mean(df$female[his & enr]),
             mean(df$female[his & !enr]),
             mean(df$female[low]),
             mean(df$female[low & enr]),
             mean(df$female[low & !enr]))

## black
mat[3,] <- c(mean(df$black),
             mean(df$black[enr]),
             mean(df$black[!enr]),
             mean(df$black[his]),
             mean(df$black[his & enr]),
             mean(df$black[his & !enr]),
             mean(df$black[low]),
             mean(df$black[low & enr]),
             mean(df$black[low & !enr]))

## hispanic
mat[4,] <- c(mean(df$hispanic),
             mean(df$hispanic[enr]),
             mean(df$hispanic[!enr]),
             mean(df$hispanic[his]),
             mean(df$hispanic[his & enr]),
             mean(df$hispanic[his & !enr]),
             mean(df$hispanic[low]),
             mean(df$hispanic[low & enr]),
             mean(df$hispanic[low & !enr]))

## Asian/PI
mat[5,] <- c(mean(df$asianpi),
             mean(df$asianpi[enr]),
             mean(df$asianpi[!enr]),
             mean(df$asianpi[his]),
             mean(df$asianpi[his & enr]),
             mean(df$asianpi[his & !enr]),
             mean(df$asianpi[low]),
             mean(df$asianpi[low & enr]),
             mean(df$asianpi[low & !enr]))

## amerind
mat[6,] <- c(mean(df$amerind),
             mean(df$amerind[enr]),
             mean(df$amerind[!enr]),
             mean(df$amerind[his]),
             mean(df$amerind[his & enr]),
             mean(df$amerind[his & !enr]),
             mean(df$amerind[low]),
             mean(df$amerind[low & enr]),
             mean(df$amerind[low & !enr]))

## multrace
mat[7,] <- c(mean(df$multrace),
             mean(df$multrace[enr]),
             mean(df$multrace[!enr]),
             mean(df$multrace[his]),
             mean(df$multrace[his & enr]),
             mean(df$multrace[his & !enr]),
             mean(df$multrace[low]),
             mean(df$multrace[low & enr]),
             mean(df$multrace[low & !enr]))

## parental education
mat[8,] <- c(mean(df$bypared),
             mean(df$bypared[enr]),
             mean(df$bypared[!enr]),
             mean(df$bypared[his]),
             mean(df$bypared[his & enr]),
             mean(df$bypared[his & !enr]),
             mean(df$bypared[low]),
             mean(df$bypared[low & enr]),
             mean(df$bypared[low & !enr]))

## parental education (sd)
mat[9,] <- c(sd(df$bypared),
             sd(df$bypared[enr]),
             sd(df$bypared[!enr]),
             sd(df$bypared[his]),
             sd(df$bypared[his & enr]),
             sd(df$bypared[his & !enr]),
             sd(df$bypared[low]),
             sd(df$bypared[low & enr]),
             sd(df$bypared[low & !enr]))

## family income
mat[10,] <- c(mean(df$byincome),
              mean(df$byincome[enr]),
              mean(df$byincome[!enr]),
              mean(df$byincome[his]),
              mean(df$byincome[his & enr]),
              mean(df$byincome[his & !enr]),
              mean(df$byincome[low]),
              mean(df$byincome[low & enr]),
              mean(df$byincome[low & !enr]))

## family income (sd)
mat[11,] <- c(sd(df$byincome),
              sd(df$byincome[enr]),
              sd(df$byincome[!enr]),
              sd(df$byincome[his]),
              sd(df$byincome[his & enr]),
              sd(df$byincome[his & !enr]),
              sd(df$byincome[low]),
              sd(df$byincome[low & enr]),
              sd(df$byincome[low & !enr]))

## composite SAT
mat[12,] <- c(mean(df$sat),
              mean(df$sat[enr]),
              mean(df$sat[!enr]),
              mean(df$sat[his]),
              mean(df$sat[his & enr]),
              mean(df$sat[his & !enr]),
              mean(df$sat[low]),
              mean(df$sat[low & enr]),
              mean(df$sat[low & !enr]))

## composite SAT (sd)
mat[13,] <- c(sd(df$sat),
              sd(df$sat[enr]),
              sd(df$sat[!enr]),
              sd(df$sat[his]),
              sd(df$sat[his & enr]),
              sd(df$sat[his & !enr]),
              sd(df$sat[low]),
              sd(df$sat[low & enr]),
              sd(df$sat[low & !enr]))

## applications for those who enroll (mean)
mat[14, c(2,5,8)] <- c(mean(apps$apply),
                       mean(apps$apply[apphis]),
                       mean(apps$apply[applow]))

## applications for those who enroll (sd)
mat[15, c(2,5,8)] <- c(sd(apps$apply),
                       sd(apps$apply[apphis]),
                       sd(apps$apply[applow]))


## multiple rows to get percentages instead of proportions
mat[1:7,] <- mat[1:7,] * 100

## round all to 2 decimal places
mat <- round(mat, 2)

## add column names
colnames(mat) <- c('all', 'all_enr', 'all_not',
                   'hisat', 'hisat_enr', 'hisat_not',
                   'lowinc', 'lowinc_enr', 'lowinc_not')

## save to csv
write.csv(mat, file = tdir %+% 'student_descriptives.csv',
          row.names = FALSE)

## =============================================================================
## END FILE
################################################################################
