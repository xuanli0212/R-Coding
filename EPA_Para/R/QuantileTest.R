#' Quantile Test 
#' The Quantile test looks at number of potentially impacted data among the largest data in the pooled basin-wide dataset. 
#'@param x is the string of the Background Measurements. 
#'@param y is string of the Site Measurements 
#'@param Epsilon is the proportion of a site at which chemicals are present at concentrations greater than background levels. 
#'@examples
#'QuanTest(x,y,Epsilon)
#'set.seed(123)
#'Site <- floor(runif(30,1,100)) # input data for site measures
#'Bkg <- floor(runif(30,1,101))  # input data for background measures
#'tail((sort(c(Site,Bkg))),4) 
#'QuanTest(Site,Bkg,.4)

QuanTest <- function(x,y,Epsilon) {
m <- c(80,35,20,15,10,10,10,10,10,10)
names(m) <- c(".1",".2",".3",".4",".5",".6",".7",".8",".9","1") 
# each epsilon value corresponds to a minimum number of measurement
z <- m["Epsilon"]
if (isTRUE(length(x)<z)) stop('minimum number of measurements is not satisfied') # use verifying argument in the function
r <- tail((sort(c(x,y))),4) # combine, sort and then take the last four measures
if (table(r%in%x)["TRUE"]<4) {cat("With alpha=.05, power=.80, the data are insufficient to conclude the chemical is a COPC, and the WRS test should be conducted") 
                              } else {cat("With alpha=.05, power=.80, the data are sufficient to conclude the chemical is a COPC")
}
}
#'@export
# an executable example
set.seed(123)
Site <- floor(runif(30,1,100)) # input data for site measures
Bkg <- floor(runif(30,1,101))  # input data for background measures

Site
Bkg
tail((sort(c(Site,Bkg))),4) 
QuanTest(Site,Bkg,.4)
