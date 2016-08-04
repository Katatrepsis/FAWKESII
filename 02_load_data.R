############################################################################
### Purpose of this skript module 03 is to:
###
### 02.1. Get Natura 2000 data
###
### General comments:
### * Should load all files needed from the web. Best to place a file somewhere in your dropbox and copy the share link here (right click). You need to change dl=0 to dl=1 at the end of the link to make it work.
###
### Authors: CH, MB, ...
############################################################################

# set to temp wd suitable for downloading and extracting data files
setwd(path2temp %+% "/") 

############################################################################
### 02.1. Get Natura 2000 data
############################################################################

if (file.exists("PublicNatura2000End2015_csv.zip")==FALSE){
  download.file("https://www.dropbox.com/s/yaujzwuijyzluc6/PublicNatura2000End2015_csv.zip?dl=1", "PublicNatura2000End2015_csv.zip", mode="wb")
  unzip("PublicNatura2000End2015_csv.zip")
} else {unzip("PublicNatura2000End2015_csv.zip")}

############################################################################
### 02.2. Get management plans for text analysis 
############################################################################

# CH: Currently the 05 script pulls the PDFs from the web directly (as GZ was doing) and puts them in
# the temp directory, but we can put the code here if you like?


