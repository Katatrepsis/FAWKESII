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
### 02.2. Get threat/service translation table directly from google docs
############################################################################

### authorize with google docs, the first time or in a new session:
### follow the displayed url, go to browser and enter your login credentials click accept and copy key back into R
gs_ls() #once authorized, this will list the files you have in GS

### load mapping table
threats_services_gsheet<- gs_title("Mapping pressures/impacts to ESS")
MappingData <- gs_read(threats_services_gsheet, ws = "Leeds_mapping",check.names=TRUE, range = cell_rows(8:424)) #consume data from "Leeds_mapping", skip first rows
MappingData<-MappingData[-c(1,2),]
MappingData<-as.data.frame(MappingData) #some functions don't like the tbl.df data type
### check variable types 
str(MappingData)

### make factors
MappingData$ACT_Code <- as.factor(MappingData$ACT_Code)
MappingData$relation <- as.factor(MappingData$relation)

### remove uneeded rows
MappingData<-MappingData[!(is.na(MappingData$relation) | MappingData$relation==""), ]


########################################
### load Guy's new 2017 mapping table
### load mapping table
GuyMappingData <- gs_read(threats_services_gsheet, ws = "GuyMapping2017",check.names=TRUE) #consume data from "GuyMapping2017"
GuyMappingData<-as.data.frame(GuyMappingData) #some functions don't like the tbl.df data type
### check variable types 
str(GuyMappingData)

### make factors
GuyMappingData$ACT_Code <- as.factor(GuyMappingData$ACT_Code)



############################################################################
### 02.2. Get management plans for text analysis 
############################################################################

# CH: Currently the 05 script pulls the PDFs from the web directly (as GZ was doing) and puts them in
# the temp directory, but we can put the code here if you like?


