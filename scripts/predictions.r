f################################################################################
##
## PROJ: College choice update
## FILE: predictions.r
## AUTH: Benjamin Skinner
## INIT: 30 January 2016
##
################################################################################

## clear memory
rm(list = ls())

## libraries
libs <- c('dplyr','foreign','readr','tidyr')
lapply(libs, require, character.only = TRUE)

## dirs
ddir <- '../data/'
mdir <- '../models/'
tdir <- '../tables/csv/'

## functions
`%+%` <- function(a,b) paste(a, b, sep = '')

## turn off scientific notation
options(scipen = 999)

## =============================================================================
## DATA
## =============================================================================

## read in full choice data
df <- read.dta(ddir %+% 'choice.dta') %>% tbl_df()
gc()

## get enrollment university for later
en <- df %>%
    filter(attend == 1) %>%
    select(stu_id, att_unitid = unitid)

## get application universities for later
ap <- df %>%
    filter(apply == 1 & enroll2yr == 1) %>%
    select(stu_id, app_unitid = unitid)

## get smaller two-year public dataframe for later
ty <- df %>%
    select(unitid, twoyr) %>%
    distinct(unitid) %>%
    left_join(read_csv(ddir %+% 'hd2004.csv'), by = 'unitid') %>%
    select(unitid, twoyr, sector) %>%
    mutate(pubtwo = as.integer(twoyr == 1 & sector %in% c(1,4), 1, 0)) %>%
    select(unitid, pubtwo)

## get student data
studat <- read_csv(ddir %+% 'studentdat.csv') %>%
    ## create fips for merge
    mutate(fips = state %+% county) %>%
    ## left join with county-level unemployment
    left_join(read_csv(ddir %+% 'unemployrate.csv') %>%
              filter(year == 2002) %>%
              select(fips, unemrate),
              by = 'fips') %>%
    ## set indicator for marginal: SAT < 900 OR income < $25k/year
    mutate(marg = ifelse(sat < 900 | byincome < 8, 1, 0))

## =============================================================================
## PREDICTIONS
## =============================================================================

## get different model coefficients + near two-year
mods <- c(grep('asclogit_main', list.files(tdir), value = TRUE), 'neartyr')

## init lists
pred_list <- list()
sen_list <- list()

## loop through each model + nearest two year
for (i in 1:length(mods)) {

    if (i == length(mods)) {
        ## set model name
        modname <- 'neartyr'
        message('Now working with: ' %+%  modname)
        ## get information from nearest two-year
        d <- df %>%
            ## join vector of two-year indicators
            left_join(ty, by = 'unitid') %>%
            ## filter
            filter(pubtwo == 1) %>%
            ## summarize
            group_by(stu_id) %>%
            filter(dist == min(dist)) %>%
            ungroup() %>%
            ## add model name
            mutate(modname = modname,
                   pred = NA) %>%
            ## subset
            select(modname, stu_id, unitid, cost, cost_sq, dist, dist_sq, instfte,
                   instfte_sq, fte, fte_sq, dist_cb, pred)
    } else {
        ## get model name
        modname <- gsub('asclogit_(.*)_mat.csv', '\\1', mods[i])
        message('Now working with: ' %+%  modname)
        ## read in coefficient data (keep only betas)
        b <- read_csv(tdir %+% mods[i])$b
        ## get individual linear combinations
        d <- df %>%
            mutate(lincom = cost * b[1]
                   + cost_sq     * b[2]
                   + dist        * b[3]
                   + dist_sq     * b[4]
                   + dist_cb     * b[5]
                   + instfte     * b[6]
                   + instfte_sq  * b[7]
                   + sfr         * b[8]
                   + ttfacpct    * b[9]
                   + sathi       * b[10]
                   + sathi_sq    * b[11]
                   + satlo       * b[12]
                   + satlo_sq    * b[13]
                   + fte         * b[14]
                   + fte_sq      * b[15]
                   + twoyr       * b[16]
                   + twoyr * cost    * b[17]
                   + twoyr * cost_sq * b[18]
                   + twoyr * dist    * b[19]
                   + twoyr * dist_sq * b[20]
                   + twoyr * fte     * b[21]
                   + twoyr * fte_sq  * b[22]) %>%
            select(stu_id, unitid, lincom)
        ## get sum of expoentiated preds for each student (denominator)
        den <- d %>%
            group_by(stu_id) %>%
            summarise(den = sum(exp(lincom)))

        ## merge with data frame and compute predictions
        d <- d %>%
            left_join(den, by = 'stu_id') %>%
            mutate(pred = exp(lincom) / den) %>%
            select(stu_id, unitid, pred) %>%
            group_by(stu_id) %>%
            mutate(rank = dense_rank(desc(pred))) %>%
            arrange(rank) %>%
            ungroup()

        if (modname == 'main_apply') {

            dd <- d %>%
                left_join(ap, by = 'stu_id') %>%
                ## drop students who didn't attend
                filter(!is.na(app_unitid)) %>%
                mutate(app = as.integer(unitid == app_unitid)) %>%
                filter(app == 1 | rank == 1) %>%
                select(-app_unitid) %>%
                group_by(stu_id) %>%
                arrange(rank, desc(app)) %>%
                group_by(stu_id, unitid) %>%
                filter(row_number() == 1) %>%
                ungroup() %>%
                group_by(stu_id) %>%
                mutate(match = as.integer(rank == app),
                       pred_diff_2 = pred - lead(pred),
                       pred_diff_max = pred - min(pred)) %>%
                summarise(match = max(match),
                          rank_pred_min = min(rank[rank > 1]),
                          rank_pred_max = max(rank),
                          pred_diff_2 = max(pred_diff_2, na.rm = TRUE),
                          pred_diff_max = max(pred_diff_max, na.rm = TRUE)) %>%
                mutate(rank_pred_min = ifelse(rank_pred_max == 1, 1, rank_pred_min),
                       pred_diff_max = ifelse(pred_diff_max == 0, NA, pred_diff_max)) %>%
                summarise(match_mean = mean(match),
                          match_sd = sd(match),
                          rank_pred_min_mean = mean(rank_pred_min[rank_pred_min > 1]),
                          rank_pred_min_med = median(rank_pred_min[rank_pred_min > 1]),
                          rank_pred_max_mean = mean(rank_pred_max[rank_pred_max > 1]),
                          rank_pred_max_med = median(rank_pred_max[rank_pred_max > 1]),
                          rank_pred_min_sd = sd(rank_pred_min[rank_pred_min > 1]),
                          rank_pred_max_sd = sd(rank_pred_max[rank_pred_max > 1]),
                          pred_diff_2_mean = mean(pred_diff_2, na.rm = TRUE),
                          pred_diff_2_med = median(pred_diff_2, na.rm = TRUE),
                          pred_diff_2_sd = sd(pred_diff_2, na.rm = TRUE),
                          pred_diff_max_mean = mean(pred_diff_max, na.rm = TRUE),
                          pred_diff_max_med = median(pred_diff_max, na.rm = TRUE),
                          pred_diff_max_sd = sd(pred_diff_max, na.rm = TRUE)) %>%
                mutate(modname = modname)

        } else {

            dd <- d %>%
                left_join(en, by = 'stu_id') %>%
                ## drop students who didn't attend
                filter(!is.na(att_unitid)) %>%
                mutate(att = as.integer(unitid == att_unitid)) %>%
                filter(att == 1 | rank == 1) %>%
                select(-att_unitid) %>%
                group_by(stu_id) %>%
                arrange(rank) %>%
                mutate(match = as.integer(rank == att),
                       pred_diff = pred - lead(pred, order_by = stu_id)) %>%
                summarise(match = max(match),
                          rank_pred = max(rank),
                          pred_diff = max(pred_diff, na.rm = TRUE)) %>%
                summarise(match_mean = mean(match),
                          match_sd = sd(match),
                          rank_pred_mean = mean(rank_pred[rank_pred > 1]),
                          rank_pred_med = median(rank_pred[rank_pred > 1]),
                          rank_pred_sd = sd(rank_pred[rank_pred > 1]),
                          pred_diff_mean = mean(pred_diff, na.rm = TRUE),
                          pred_diff_med = median(pred_diff, na.rm = TRUE),
                          pred_diff_sd = sd(pred_diff, na.rm = TRUE)) %>%
                mutate(modname = modname)
        }

        ## add to list
        sen_list[[i]] <- dd

        rm(dd)
        gc()

        ## get highest prediction college
        d <- d %>%
            group_by(stu_id) %>%
            ## get college with highest prediction
            filter(rank == 1) %>%
            ## join with choice data for covariates
            left_join(df, by = c('stu_id', 'unitid')) %>%
            ## add model name
            mutate(modname = modname) %>%
            ## subset
            select(modname, stu_id, unitid, cost, cost_sq, dist, dist_sq, instfte,
                   instfte_sq, fte, fte_sq, dist_cb, pred)
    }

    ## add df to list
    pred_list[[i]] <- d
    rm(d)
    gc()
}

## drop full df; garbage collect
rm(df)
gc()

## bind into single data table
preddf <- bind_rows(pred_list) %>%
    ## arrange
    arrange(stu_id, modname)

sendf <- bind_rows(sen_list[c(1,2,4)]) %>%
    arrange(modname)

write.csv(sendf, tdir %+% 'prediction_sensitivity_p1.csv', row.names = FALSE)

write.csv(sen_list[[3]], tdir %+% 'prediction_sensitivity_p2.csv', row.names = FALSE)

rm(list = c('pred_list', 'sen_list'))
gc()
Sys.sleep(5)

## =============================================================================
## LOGISTIC REGRESSIONS: PREDICT ENROLLMENT
## =============================================================================

## formula
x <- c('cost','cost_sq','dist','dist_sq','instfte','instfte_sq','unemrate',
       'satpct','byincome','bypared','female','black','hispanic','asianpi',
       'amerind','multrace','fte','fte_sq','dist_cb')
fmla <- as.formula(paste('enroll2yr ~ ', paste(x, collapse = '+')))

## mods
mods <- preddf %>% distinct(modname) %>% .[['modname']]

## loop through each model
for (i in 1:length(mods)) {

    ## data
    df <- preddf %>%
        ## filter to model
        filter(modname == mods[i]) %>%
        ## join student data
        left_join(studat, by = 'stu_id')

    ## all students then those on the margin
    for (j in 1:2) {
        if (j == 2) {
            type <- '_marg'
            df <- df %>% filter(marg == 1)
        } else {
            type <- ''
        }

        message('Working with: ' %+% mods[i] %+% type)

        ## fit
        fit <- glm(fmla, family = binomial(link = 'logit'), data = df)

        ## get output
        b <- summary(fit)$coefficients[,1]
        z <- summary(fit)$coefficients[,3]
        or <- exp(b)
        obs <- round(nobs(fit), -1)
        vals <- cbind(b, z, or, obs)
        vcov <- vcov(fit)

        ## write tables to disk for table making
        write.csv(vals, file = tdir %+% mods[i] %+% type %+% '_logit_mat.csv')
        write.csv(vcov, file = tdir %+% mods[i] %+% type %+% '_logit_vcov.csv')

        ## save model
        save(fit, file = mdir %+% mods[i] %+% type %+% '_logit_fit.rda')
    }
}

## =============================================================================
## Probability stats
## =============================================================================

predstat <- preddf %>%
    ## drop near two-year b/c no prediction measure
    filter(modname != 'neartyr') %>%
    ## group by model type
    group_by(modname) %>%
    ## get quantiles
    summarise(min = min(pred),
              first_q = quantile(pred, probs = 0.25),
              median = median(pred),
              mean = mean(pred),
              third_q = quantile(pred, probs = 0.75),
              max = max(pred))

write.csv(predstat, tdir %+% 'mostlikprobs.csv', row.names = FALSE)

## =============================================================================
## COMPARE PREDICTIONS TO ACTUAL VALUES
## =============================================================================

## subset preddf
preddf <- preddf %>%
    select(stu_id, modname, pred_unitid = unitid)

## read back in choice data and subset to choices
choice <- read.dta(ddir %+% 'choice.dta') %>%
    tbl_df() %>%
    filter(enroll2yr == 1) %>%
    select(stu_id, unitid, attend, apply, accept)
gc()

## ---------------------------
## unconditional attend (main)
## ---------------------------

att <- choice %>%
    filter(attend == 1) %>%
    left_join(preddf %>% filter(modname == 'main'), by = 'stu_id') %>%
    mutate(match = ifelse(unitid == pred_unitid, 1, 0)) %>%
    summarize(match = mean(match)) %>%
    .[['match']]

## ---------------------------
## application
## ---------------------------

app <- choice %>%
    filter(apply == 1) %>%
    left_join(preddf %>% filter(modname == 'main_apply'), by = 'stu_id') %>%
    mutate(match = ifelse(unitid == pred_unitid, 1, 0)) %>%
    group_by(stu_id) %>%
    summarize(match = max(match)) %>%
    ungroup() %>%
    summarize(match = mean(match)) %>%
    .[['match']]

## ---------------------------
## attend|application
## ---------------------------

## those with more than one application
mask <- choice %>%
    group_by(stu_id) %>%
    summarise(apply = sum(apply)) %>%
    filter(apply > 1) %>%
    .[['stu_id']]

aca <- choice %>%
    filter(stu_id %in% mask, attend == 1) %>%
    left_join(preddf %>% filter(modname == 'main_apply_attend'), by = 'stu_id') %>%
    mutate(match = ifelse(unitid == pred_unitid, 1, 0)) %>%
    summarize(match = mean(match)) %>%
    .[['match']]

## ---------------------------
## attend|accept
## ---------------------------

## those with more than one acceptance
mask <- choice %>%
    group_by(stu_id) %>%
    summarise(accept = sum(accept)) %>%
    filter(accept > 1) %>%
    .[['stu_id']]

acc <- choice %>%
    filter(stu_id %in% mask, attend == 1) %>%
    left_join(preddf %>% filter(modname == 'main_accept_attend'), by = 'stu_id') %>%
    mutate(match = ifelse(unitid == pred_unitid, 1, 0)) %>%
    summarize(match = mean(match)) %>%
    .[['match']]

## ---------------------------
## near two-year
## ---------------------------

nty <- choice %>%
    filter(attend == 1) %>%
    left_join(preddf %>% filter(modname == 'neartyr'), by = 'stu_id') %>%
    mutate(match = ifelse(unitid == pred_unitid, 1, 0)) %>%
    summarize(match = mean(match)) %>%
    .[['match']]

## ---------------------------
## matrix and save
## ---------------------------

out <- data.frame('model' = c(mods[c(1,3,4,2,5)]),
                  'pred_pct' = round(c(att, app, aca, acc, nty), 4) * 100,
                  stringsAsFactors = FALSE) %>%
    tbl_df()

write.csv(out,
          file = tdir %+% 'correctly_predicted.csv',
          quote = FALSE,
          row.names = FALSE)

rm(list = c('out', 'nty', 'acc', 'aca', 'app', 'att', 'choice'))
gc()

## =============================================================================
## COMPARE PREDICTED INSTITUTIONS TO ACTUAL ATTENDED INSTITUTIONS
## =============================================================================

## read back in choice data and subset to enrollees
choice <- read.dta(ddir %+% 'choice.dta') %>%
    tbl_df() %>%
    filter(enroll2yr == 1)

## get model names
mods <- preddf %>% distinct(modname) %>% .[['modname']]

## init lists
dif_list <- list()
ind_list <- list()

## loops
for(i in 1:length(mods)) {

    message('Working with: ' %+% mods[i])

    ## subset to model
    df <- preddf %>%
        filter(modname == mods[i]) %>%
        right_join(choice, by = 'stu_id') %>%
        mutate(pred = ifelse(unitid == pred_unitid, 1, 0)) %>%
        filter(pred == 1 | attend == 1, pred != attend)

    ## differences in cost and distance
    diffs <- df %>%
        select(stu_id, attend, pred, cost, dist) %>%
        arrange(stu_id, pred, attend) %>%
        mutate(cost = cost * 1000,
               dist = dist * 100) %>%
        group_by(stu_id) %>%
        mutate(cost_pct = (cost + 1) / lead((cost + 1)), # b/c zeros
               cost_diff = cost - lead(cost),
               dist_pct = dist / lead(dist),
               dist_diff = dist - lead(dist)) %>%
        filter(pred == 0) %>%
        ungroup() %>%
        select(-c(stu_id, attend, pred, cost, dist)) %>%
        summarise_each(funs(mean, median)) %>%
        gather(stat, value) %>%
        mutate(modname = mods[i])

    ## correct on indicators
    ind <- df %>%
        select(stu_id, attend, unitid) %>%
        left_join(read_csv(tdir %+% 'institutions.csv'), by = 'unitid') %>%
        select(stu_id, attend, unitid, stfips, public, privnp, privfp, twoyr) %>%
        group_by(stu_id) %>%
        arrange(desc(attend)) %>%
        mutate(same_st = ifelse(stfips == lead(stfips), 1, 0),
               same_sec = ifelse(public == lead(public)
                                 & privnp == lead(privnp)
                                 & privfp == lead(privfp)
                                 & twoyr == lead(twoyr), 1, 0),
               same_lev = ifelse(twoyr == lead(twoyr), 1, 0),
               same_con = ifelse(public == lead(public)
                                 & privnp == lead(privnp)
                                 & privfp == lead(privfp), 1, 0)) %>%
        filter(attend == 1) %>%
        ungroup() %>%
        select(starts_with('same')) %>%
        mutate(matches = same_st + same_sec + same_lev + same_con) %>%
        summarise_each(funs(mean)) %>%
        mutate(modname = mods[i])

    ## add to lists
    dif_list[[i]] <- diffs
    ind_list[[i]] <- ind

    rm(list = c('diffs', 'ind'))
    gc()
}

rm(choice)
gc()

## bind into single data table
diffs <- bind_rows(dif_list) %>%
    ## select
    select(modname, stat, value)

ind <- bind_rows(ind_list) %>%
    ## select
    select(modname, starts_with('same'))

## write to csv
write.csv(diffs,
          file = tdir %+% 'school_prediction_diffs_1.csv',
          quote = FALSE,
          row.names = FALSE)

write.csv(ind,
          file = tdir %+% 'school_prediction_diffs_2.csv',
          quote = FALSE,
          row.names = FALSE)

## =============================================================================
## END FILE
################################################################################
