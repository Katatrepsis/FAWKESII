############################################################################
### Purpose of this skript module 03 is to:
###
### 02.1. Get Natura 2000 data
###
### General comments:
### * 
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
### 02.2. Get literature for text analysis (to be replaced for management plans)
############################################################################

if (file.exists("Literature.zip")==FALSE){
  download.file("https://www.dropbox.com/sh/chnaavz6mj83ng0/AAAZElcg1VeWLzqUP8c3AkaCa?dl=1", "Literature.zip", mode="wb")
  unzip("Literature.zip")
} else {unzip("Literature.zip")}


