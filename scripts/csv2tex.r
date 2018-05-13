################################################################################
##
## CONVERT CSV MATRIX TO TEX FORMAT
## Benjamin Skinner
##
################################################################################

## PURPOSE

## If you have a matrix that has been stored as as csv file, e.g.,

## col1,col2,col3
## 1,a,2
## 3,b,4
## 5,c,7

## and want to convert it to the content of a TeX table, e.g.,

## col1 & col2 & col3 \\
## 1 & a & 2 \\
## (3) & (b) & (4) \\
## 5 & c & 7 \\

## that can be wrapped within table/tabular environments, this function
## allows you to:

## choose the csv file
## decide if you want to keep the header
## decide how much you want to round the cells (if any)
## decide which row cells to add parentheses to (e.g., standard errors)
## add rownames
## add hlines (or booktabs midrule lines if using booktabs package)

csv2tex <- function(csvfile, header = FALSE, rounding = NULL,
                    parenrows = NULL, rnames = NULL, hlines = NULL,
                    btmidrule = NULL) {
    
    ## read the csv as lines
    foo <- readLines(csvfile)
    
    ## drop the header line (assumes yes)
    if (!header) {
        foo <- foo[-1]
    }
    
    ## clean each line
    foo <- unlist(lapply(foo, function(x) {
        
        ## split row by comma
        x <- strsplit(x, split = ',')
        
        ## convert each element to numeric so can be rounded
        x <- sapply(x, function(xx) {
            xx <- as.numeric(xx)
            if(!is.null(rounding)) {
                xx <- round(xx, rounding)
            }
            xx <- as.character(xx)
        })
        
        ## paste back together with & between
        x <- paste(x, collapse = '&')
        
    }))
    
    ## add parentheses to selected rows (if any)
    if (!is.null(parenrows)) {
        for (r in parenrows) {
            foo[r] <- strsplit(foo[r], split = '&')
            foo[[r]] <- gsub('(.*)', '(\\1)', foo[[r]])
            foo[[r]] <- gsub('\\(NA\\)', ' ', foo[[r]])
            foo[[r]] <- paste(foo[[r]], collapse = '&')
            foo <- unlist(foo)
        }
    }
    
    ## clean out NA
    foo <- unlist(lapply(foo, function(x) {gsub('NA', ' ', x)}))
    
    ## add rownames (if any)
    if (!is.null(rownames)) {
        for (n in 1:length(rnames)) {
            foo[n] <- gsub('(.*)', paste(rnames[n], '\\1', sep = '&'), foo[n])
        }
    }

    ## add \\\\ to the end of each line
    foo <- unlist(lapply(foo, function(x) paste0(x, '\\\\')))
    
    ## add horizontal lines (if any wanted)
    if (!is.null(hlines) | !is.null(btmidrule)) {

        ## append hlines after rows selected
        for (hl in hlines) {
            foo <- append(foo, '\\hline', hl)
        }
        ## append midrule after rows selected
        for (mr in btmidrule) {
            foo <- append(foo, '\\midrule', mr)
        }   
    }
            
    ## return
    return(foo)
    
}

## returned object can used within knitr chuck using writeLines() or
## saved to a file using the same command.
