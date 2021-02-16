# tile_latlon.R
#
# Make lat/lon information lists from tile names of the SGLI L2 products 
#
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)

rm(list=ls())

# output directory
diro <- "/Users/output_directory/"

# constants
lin <- 1200      # total number of pixels in a vertical direction
col <- 1200      # total number of pixels in a horizontal direction
vtn <- 18        # total number of tiles in a vertical direction
htn <- 36        # total number of tiles in a horizontal direction 

d <- 180.0/lin/vtn     # [deg/pixel]
NL <- 180.0/d          # total number of pixels from the N.P to S.P.
NP0 <- 2* round(NL)    # total number of pixels along the equator

nlin <- array(1:lin) - 1    # line numbers
ncol <- array(1:col) - 1    # column numbers

# get existing tile names
fi <- "/Users/input_directory/tile_names.txt"
tiles <- read.table(fi, header=FALSE, skip=0)
tnam <- unique(tiles)      # tile names
rm(tiles)

vv <- as.numeric(substr(tnam[,1], 2, 3))     # vertical number　 
hh <- as.numeric(substr(tnam[,1], 4, 5))     # horizontal number

for (ntile in 1:nrow(tnam)) { 
  #print(paste("File #", ntile))
  lin_tot <- nlin + (vv[ntile]*lin)
  col_tot <- ncol + (hh[ntile]*col)
  
  lat_tmp <- 90.0-(lin_tot+0.5)*d            # latitude
  lat_2d <- array(lat_tmp,dim = c(col,lin))
  
  NP <- round(NP0*cos(lat_tmp*pi/180.0))
  
  lon_tmp <- array(dim = c(col,lin))
  for (i in 1:lin) {
    for (j in 1:col) {
      lon_tmp[j,i] <- (360.0/NP[i])*(col_tot[j]-NP0/2.0+0.5)    # longitude
    }
  }
  
  # eliminate invalid data
  lon_out <- lon_tmp[(lon_tmp>-180.0)&(lon_tmp<180.0)]
  lat_out <- lat_2d[(lon_tmp>-180.0)&(lon_tmp<180.0)]
  
  bf <- cbind(lat_out, lon_out)
  colnames(bf) <- c("latitude", "longitude")
  rm(lat_tmp,lat_2d,lat_out,lon_tmp,lon_out)
  
  # output
  #print(paste("Number of valid points:", nrow(bf)))  
  fout <- paste0(tnam[ntile,1],"_latlon.csv")
  write.csv(bf,paste0(diro,fout), row.names = FALSE)
  rm(bf)
}


