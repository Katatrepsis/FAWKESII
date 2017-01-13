############################################################################
### Purpose of this skript module 03 is to:
### 
### 03.1. Analysis of species trends
### 03.2. 
###
### Authors: CH ...
############################################################################


############################################################################
### 03.1. Analysis of species trends
###
### Can download IUCN data and quantify trends using R package "letsR":
### https://cran.r-project.org/web/packages/letsR/letsR.pdf. The script
### works well, but struggles with subspecies - there don't seem to be
### IUCN listings for some of the subspecies in the Natura 2000 dataset.
###
############################################################################

# set wd to path2temp where files have been downloaded and extracted
setwd(path2temp %+% "/") 

# Load data
speciesTable<-read.csv("DIRECTIVESPECIES.csv")

# Create character vectors into which to extract IUCN data
Species<-Family<-Status<-Criteria<-Population<-DescriptionYear<-character()

ptm <- proc.time()
# Loop through each Natura 2000 species to extract data and save to the vector
for(x in 1:nrow(speciesTable)){
  
  # Create a temporary object to hold the data from each web request
  #spData <- try(lets.iucn(speciesTable$SPECIESNAME[x]))
  spData <- tryCatch(lets.iucn(speciesTable$SPECIESNAME[x]), error=function(e) "Not available")
  if(spData=="Not available") {Species[x]<-Family[x]<-Status[x]<-Criteria[x]<-Population[x]<-DescriptionYear[x]<-"Not available";next}
  # Extract data from that object into the vectors
  Species[x]<-as.character(spData[1,1])
  Family[x]<-as.character(spData[1,2])
  Status[x]<-as.character(spData[1,3] )
  Criteria[x]<-as.character(spData[1,4])
  Population[x]<-as.character(spData[1,5])
  DescriptionYear[x]<-as.character(spData[1,6])
  
  # Extra bit of code to report the loop (for sanity!), print loop on multiples of 10
  if(x%%10==0) {print(x)}
  print(x)
  flush.console()
  
}
proc.time() - ptm
statusTable<-cbind(Species,Family,Status,Criteria,Population,DescriptionYear)
speciesTable<-cbind(speciesTable,statusTable)



# Now rotate through N2000 sites to establish (I) the number of species listed in directives, (II) the
# number of those species that are increasing, declining, or stable.

# Create character vectors into which to extract IUCN data
NumberDirectiveSp<-IncreasingSp<-DecreasingSp<-StableSp<-integer()

# Find list of unique N2000 sites
N2000Sites<-read.csv("NATURA2000SITES.csv",header=TRUE)
N2000SiteCodes<-N2000Sites$SITECODE

# Load big list of species with presence at each site
N2000Species<-read.csv("SPECIES.csv",header=TRUE)


# Loop through each Natura 2000 site to extract data and save to the vector
ptm <- proc.time()
for(x in 1:length(N2000SiteCodes)){
  
  # Create a temporary object to hold the data from each web request
  spData <- subset(N2000Species,N2000Species$SITECODE %in% N2000SiteCodes[x])
  spTrends<-subset(statusTable,as.vector(statusTable[,1]) %in% as.vector(spData$SPECIESNAME))
  # Extract data from that object into the vectors
  NumberDirectiveSp[x]<-nrow(spTrends)
  IncreasingSp[x]<-nrow(subset(spTrends,spTrends[,5]=="Increasing"))
  DecreasingSp[x]<-nrow(subset(spTrends,spTrends[,5]=="Decreasing"))
  StableSp[x]<-nrow(subset(spTrends,spTrends[,5]=="Stable"))

  # Extra bit of code to report the loop (for sanity!), print loop on multiples of 10
  if(x%%10==0) {print(x)}
  print(x)
  flush.console()
  
}
proc.time() - ptm

SiteTrends<-data.frame(Site=N2000SiteCodes,SpeciesN=NumberDirectiveSp,Inc=IncreasingSp,Dec=DecreasingSp,Stable=StableSp)
head(SiteTrends)
plot(SiteTrends[,3],SiteTrends[,4])
