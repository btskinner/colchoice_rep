#################################################################################
##
## <PROJ> College choice update
## <FILE> simulation_maps_us.r
## <AUTH> Benjamin Skinner
## <INIT> 27 February 2016
##
################################################################################

## clear memory
rm(list=ls())

## libraries
library(dplyr)
library(ggplot2)
library(grid)
library(mapproj)
library(maptools)
library(readr)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(scales)
library(sp)

## quick functions
`%+%` <- function(a,b) paste0(a,b)

## arguments
args <- commandArgs(trailingOnly = TRUE)
idir <- args[3]                                 # shapefile directory
odir <- args[4]                                 # output directory
typ <- args[5]                                  # type
tfil <- args[2]                                 # combined prediction file
bench <- as.double(args[1]) / 10                # cut for t-test

## read in census tract data and subset to state
dat <- read_csv(tfil) %>%
    mutate(stfips = substr(geoid,1,2)) %>%
    mutate(col = ifelse(pp < bench & pp_hi95 < bench, -2,
                 ifelse(pp < bench & pp_hi95 >= bench, -1,
                 ifelse(pp > bench & pp_lo95 <= bench, 1,
                 ifelse(pp > bench & pp_hi95 > bench, 2,
                        0))))) %>%
    select(id = geoid, stfips, sat = stusatpct, col)

stfips <- dat %>% select(stfips) %>%
    unique %>%
    filter(stfips != '72',
           stfips != '02',
           stfips != '15') %>%
    .[['stfips']]

## read in shape files and put data in list
stlist <- list()
for (st in stfips) {

    ## get layer
    layer <- 'gz_2010_' %+% st %+% '_140_00_500k'
    map <- readOGR(dsn = idir, layer = layer)

    ## add GEOID
    map@data$geoid <- as.character(map@data$STATE) %+%
        as.character(map@data$COUNTY) %+%
        as.character(map@data$TRACT)

    ## keep only geoid
    map <- map[,'geoid']

    ## make unique IDs for merge
    new_id <- map@data$geoid %+% sprintf('%03d', round(runif(1, 1, 999)),0)
    map <- spChFIDs(map, new_id)

    ## stick in list
    stlist[[st]] <- map

}

## bind into one big map
map <- do.call(rbind, stlist)

## get state maps
stmap <- readOGR(dsn = idir, layer = 'gz_2010_us_040_00_500k')

## fortify
message('Fortifying maps.\n')
map <- fortify(map, region = 'geoid')
stmap <- fortify(stmap, region = 'STATE')

## clean stmap to drop non-48
stmap <- stmap[!(stmap$id %in% c('02','15','72')),]

## loop through SAT levels
for (satlev in c(0.1, 0.3, 0.5, 0.7, 0.9)) {

    message('\nWorking with SAT level: ' %+% satlev %+% '\n')

    ## subset data to SAT level
    d <- dat %>% filter(sat == satlev)

    ## join data
    m <- map %>% left_join(d, by = 'id')

    ## create map
    message('Creating map.\n')
    g <- ggplot() +
        theme_bw() +
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              plot.margin = unit(c(0,0,0,0),'lines'),
	      legend.position = c(.94,.23),
	      legend.key.size = unit(2, 'lines'),
	      axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank()) +
        geom_polygon(data = m,
                     aes(x = long, y = lat, group = group, fill = factor(col)),
                     color = NA) +
        geom_polygon(data = stmap,
                     aes(x = long, y = lat, group = group),
                     fill = NA, color = 'black', size = 0.25) +
        scale_fill_manual(values = brewer.pal(4, 'RdBu'),
                          labels = c('No (sig)', 'No', 'Yes', 'Yes (sig)'),
                          na.value = 'grey10',
                          name = 'Prob > ' %+% round(bench * 100) %+% '%') +
        guides(fill = guide_legend(reverse = TRUE)) +
        coord_map('polyconic', xlim = c(-120,-73.5), ylim = c(25,50))

    ## save map
    message('Save map to file.\n')
    ggsave(filename = 'us_' %+% typ %+% '_' %+% bench %+% '_' %+% satlev %+% '.eps',
           plot = g,
           path = odir,
           width = 13,
           height = 9)

    rm(list = c('d','m','g'))

}

message('Finished!')

## =============================================================================
## END FILE
################################################################################
