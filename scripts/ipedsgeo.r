################################################################################
##
## IPEDS geocode
## Benjamin Skinner
## INIT: 15 January 2015
##
################################################################################

## clear memory
rm(list=ls())

## libraries
library(zipcode); data(zipcode)

## raw file location
raw <- '../ipeds/'

## cleaned data location
cln <- '../data/'

################################################################################
## FUNCTIONS
################################################################################

ipedsgeo <- function(f){

    ## file names
    dest <- paste0(raw, paste0(f,'.zip')); flat <- paste0(tolower(f),'.csv')

    ## check to see if file exists on in raw directory
    if(file.exists(dest)){

        ## message
        message(paste0('\n ',f, '...get local file...'))

        ## unzip and store data
        ipeds <- read.csv(unz(dest, flat), header = TRUE)

    } else {

        ## message
        message(paste0('\n ', f, '...downloading file...'))

        ## download the file, save locally, and open
        url <- paste0('nces.ed.gov/ipeds/datacenter/data/', paste0(f,'.zip'))
        download.file(url, dest, method = 'curl')
        ipeds <- read.csv(unz(dest, flat), header = TRUE)
        
    }

    ## get year; correct 1990s years
    year <- as.numeric(gsub('\\D','',f))
    if (year < 1000) {year <- as.numeric(paste0('19',year))}
    if (year > 9000) {year <- as.numeric(paste0('19',substr(year,1,2)))}

    ## lower names
    names(ipeds) <- tolower(names(ipeds))

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## SUBSET LISTS
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ## areas that aren't lower 48 + DC (need for below)
    outstates <- c('AK','HI','AS','FM','GU','MH','MP','PW','PR','VI')

    ## states with zip codes that begin with zero
    zip0st <- c('CT','MA','ME','NH','NJ','RI','VT')

    ## Title IV eligibility codes and associated years (not consistent)
    opecode <- list(c(1),c(1,2),c(1,2,4),c(1,2,8))
    opflyrs <- list(c(2002),c(1997:1999,2001,2011,2013),c(2000,2003:2005),
                    c(2006:2010,2012))

    ## fix bad zipcodes: tuple => unitid,corrected zip
    fix <- list(c(219578,37205),c(246974,94578),c(371830,92841),c(400187,92843),
                c(438063,79903),c(438674,39401),c(439279,63043),c(444884,24016),
                c(122269,90275),c(180142,59405),c(192855,10458),c(203951,43614),
                c(249779,19114),c(407009,85306),c(420574,85212),c(434399,82930),
                c(437574,30033),c(440615,31023),c(443313,98109),c(445106,90804),
                c(445805,91203),c(448886,85004),c(448187,75007),c(178022,64156),
                c(178785,64111),c(192475,11201),c(193317,10801),c(204547,44131),
                c(384360,30907),c(384379,31605),c(384388,30213),c(384397,31093),
                c(423476,31909),c(440305,64057),c(442000,64120),c(445009,19464),
                c(448549,46410),c(451680,33868),c(103945,86004),c(112455,92866),
                c(138497,34450),c(217907,29550),c(367981,60465),c(431035,92264),
                c(406608,92612),c(157827,42701),c(137078,33781),c(109785,91702),
                c(116439,92618),c(138309,34474),c(262448,92868),c(372718,33764),
                c(434885,33607),c(381334,92590),c(373951,48210),c(146685,62794),
                c(140711,30297),c(119128,92845),c(120537,92831),c(123943,92831),
                c(133085,33759),c(136491,33760),c(377421,20110))

    ## check for lon/lat
    if('longitud' %in% colnames(ipeds)){

        ## message
        message('\n lon/lat exist in this file...adding...')

        ## subset vars
        vars <- c('unitid','zip','stabbr','longitud','latitude','sector',
                  'opeflag'); subset <- ipeds[,vars]

        ## drop if not 48 + DC
        subset <- subset[!(subset$stabbr %in% outstates),]

        ## drop if an administrative unit
        subset <- subset[subset$sector != 0,]

        ## add vars for sector
        subset$pub4yr <- ifelse(subset$sector == 1, 1, 0)
        subset$pub2yr <- ifelse(subset$sector == 4, 1, 0)
        subset$public <- ifelse(subset$sector == 1 | subset$sector == 4, 1, 0)
        
        ## drop if not Title IV
        if (year %in% opflyrs[[1]]){
            subset <- subset[(subset$opeflag %in% opecode[[1]]),]
        } else if (year %in% opflyrs[[2]]){
            subset <- subset[(subset$opeflag %in% opecode[[2]]),]
        } else if (year %in% opflyrs[[3]]){
            subset <- subset[(subset$opeflag %in% opecode[[3]]),]
        } else {
            subset <- subset[(subset$opeflag %in% opecode[[4]]),]
        }

        ## clean zips: remove hyphens; strip ZIP+4; add leading zeros
        subset$zip <- gsub('-', '', subset$zip)
        ind <- (subset$stabbr %in% zip0st)
        subset$zip[ind] <- substr(as.numeric(subset$zip[ind]), 1, 4)
        subset$zip[!ind] <- substr(as.numeric(subset$zip[!ind]), 1, 5)
        ind <- (as.numeric(subset$zip) < 1000 & !is.na(as.numeric(subset$zip)))
        subset$zip[ind] <- paste0('00',as.numeric(subset$zip[ind]))
        ind <- (as.numeric(subset$zip) < 10000 & as.numeric(subset$zip) > 1000
                & !is.na(as.numeric(subset$zip)))
        subset$zip[ind] <- paste0('0',as.numeric(subset$zip[ind]))

        ## loop to fix bad zipcodes
        for(i in 1:length(fix)){
            ind <- (subset$unitid == fix[[i]][[1]])
            subset$zip[ind] <- fix[[i]][[2]]
        }

        ## subset and add to list
        vars <- c('unitid','zip','longitud','latitude','stabbr','public',
                  'pub4yr','pub2yr'); subset <- subset[,vars]
        
        ## message
        message('\n adding dataframe to list...')

        ## return dataframe as list element
        gls <- list(subset); names(gls) <- paste0('y', year); return(gls)
        
    } else {

        ## message
        message('\n no lon/lat...comparing to 2013 data...')

        ## subset data (NB: variables the same for 1997-2008...except once...)
        if (year == 1997){
            vars <- c('unitid','stabbr','zip','sector','opeind')
            subset <- ipeds[,vars]
            names(subset)[names(subset) == 'opeind'] <- 'opeflag'
        } else {
            vars <- c('unitid','stabbr','zip','sector','opeflag')
            subset <- ipeds[,vars]
        }
        
        ## drop if not 48
        subset <- subset[!(subset$stabbr %in% outstates),]

        ## drop if an administrative unit
        subset <- subset[subset$sector != 0,]

        ## add vars for sector
        subset$pub4yr <- ifelse(subset$sector == 1, 1, 0)
        subset$pub2yr <- ifelse(subset$sector == 4, 1, 0)
        subset$public <- ifelse(subset$sector == 1 | subset$sector == 4, 1, 0)

        ## drop if not Title IV
        if (year %in% opflyrs[[1]]){
            subset <- subset[(subset$opeflag %in% opecode[[1]]),]
        } else if (year %in% opflyrs[[2]]){
            subset <- subset[(subset$opeflag %in% opecode[[2]]),]
        } else if (year %in% opflyrs[[3]]){
            subset <- subset[(subset$opeflag %in% opecode[[3]]),]
        } else {
            subset <- subset[(subset$opeflag %in% opecode[[4]]),]
        }

        ## message
        message('\n fixing bad zipcodes...')

        ## clean zipcodes: only 5 ZIP, add leading 0
        subset$zip <- gsub('-', '', subset$zip)
        ind <- (subset$stabbr %in% zip0st)
        subset$zip[ind] <- substr(as.numeric(subset$zip[ind]), 1, 4)
        subset$zip[!ind] <- substr(as.numeric(subset$zip[!ind]), 1, 5)
        ind <- (as.numeric(subset$zip) < 1000 & !is.na(as.numeric(subset$zip)))
        subset$zip[ind] <- paste0('00',as.numeric(subset$zip[ind]))
        ind <- (as.numeric(subset$zip) < 10000 & as.numeric(subset$zip) > 1000
                & !is.na(as.numeric(subset$zip)))
        subset$zip[ind] <- paste0('0',as.numeric(subset$zip[ind]))

        ## loop to fix bad zipcodes
        for(i in 1:length(fix)){
            ind <- (subset$unitid == fix[[i]][[1]])
            subset$zip[ind] <- fix[[i]][[2]]
        }

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## ATTEMPT (1): FILL IN
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ## message
        message('\n first attempt: backfill from future matches...')
        
        ## init comb data; create new lon/lat columns; merge
        vars <- c('unitid','zip','stabbr','public','pub4yr','pub2yr')
        comb <- subset[,vars]; comb$lat <- comb$lon <- NA

        ## loop through latest 4 years that have lon/lat
        for(i in 1:4){

            ## get year; message
            y <- 2014 - i; message(paste0('\n trying year: ', y,'...'))

            ## get data from newer year; don't need stabbr or sector
            n <- geolist[[i]]
            n <- n[,!colnames(n) %in% c('stabbr','public','pub4yr','pub2yr')]

            ## merge        
            comb <- merge(comb, n, by = 'unitid', all.x = T)

            ## fill in if zip is the same
            ind <- (is.na(comb$lon) & !is.na(comb$zip.y)
                    & comb$zip.x == comb$zip.y)
            comb$lon[ind] <- comb$longitud[ind]
            ind <- (is.na(comb$lat) & !is.na(comb$zip.y)
                    & comb$zip.x == comb$zip.y)
            comb$lat[ind] <- comb$latitude[ind]

            ## clean up
            vars <- c('unitid','zip.x','lon','lat','stabbr','public',
                      'pub4yr','pub2yr'); comb <- comb[,vars]
            names(comb)[names(comb) == 'zip.x'] <- 'zip'

            ## show proportion missing
            message('\n proportion missing...'); print(propmiss(comb))
        }

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## ATTEMPT (2): ZIP CODE CENTROID
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ## message
        message('\n second attempt: use zipcode centroids...')
        
        ## try to geocode zipcode next (could use google geocode...)    
        comb <- merge(comb, zipcode, by = 'zip', all.x = T)

        ## replace missing lon/lat with zip centroid lon/lat
        ind <- (is.na(comb$lon)); comb$lon[ind] <- comb$longitude[ind]
        ind <- (is.na(comb$lat)); comb$lat[ind] <- comb$latitude[ind]

        ## show proportion missing
        vars <- c('unitid','zip','lon','lat','stabbr','public','pub4yr','pub2yr')
        comb <- comb[,vars]
        message('\n proportion missing...'); print(propmiss(comb))

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## OUTPUT
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        ## final subset of data; rename
        names(comb)[names(comb) == 'lon'] <- 'longitud'
        names(comb)[names(comb) == 'lat'] <- 'latitude'

        ## message
        message('\n adding dataframe to list...')
        
        ## return dataframe as list element
        gls <- list(comb); names(gls) <- paste0('y', year); return(gls)
        
    }
}

## from: https://gist.github.com/stephenturner/841686
propmiss <- function(dataframe) {
    m <- sapply(dataframe, function(x) {
        data.frame(
            nmiss=sum(is.na(x)),
            n=length(x),
            propmiss=sum(is.na(x))/length(x)
        )
    })
    d <- data.frame(t(m))
    d <- sapply(d, unlist)
    d <- as.data.frame(d)
    d$variable <- row.names(d)
    row.names(d) <- NULL
    d <- cbind(d[ncol(d)],d[-ncol(d)])
    return(d[order(d$propmiss), ])
}

################################################################################
## RUN
################################################################################

## IPEDS files
ipedsfiles <- list('HD2013','HD2012','HD2011','HD2010','HD2009','HD2008',
                   'HD2007','HD2006','HD2005','HD2004','HD2003','HD2002',
                   'FA2001HD','FA2000HD','IC99_HD','ic98hdac','ic9798_HDR')

## init final list (needs to be called geolist...kludge)s
geolist <- list()

## iterate through IPEDS files; add to list
for(i in 1:length(ipedsfiles)){
    f <- ipedsfiles[i]
    geolist <- c(geolist, ipedsgeo(f))
}

################################################################################
## CHECK MISSING
################################################################################

## apply across list items
lapply(geolist, FUN = function(x){propmiss(x)})

################################################################################
## SAVE
################################################################################

save(geolist, file = paste0(cln, 'ipedsyeargeo.rda'))
