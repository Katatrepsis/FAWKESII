# Analysis of species trends ####

# Can download IUCN data and quantify trends using R package "letsR":
# https://cran.r-project.org/web/packages/letsR/letsR.pdf. The script
# works well, but struggles with subspecies - there don't seem to be
# IUCN listings for some of the subspecies in the Natura 2000 dataset.

# Install and load packages
install.packages("letsR")
library(letsR)

# Load data
setwd("C:\\Users\\fbscha\\Dropbox\\FAWKES II\\Resources\\PublicNatura2000End2015_csv")
speciesTable<-read.csv("DIRECTIVESPECIES.csv")

# Create character vectors into which to extract IUCN data
Species<-Family<-Status<-Criteria<-Population<-DescriptionYear<-character()

# Loop through each Natura 2000 species to extract data and save to the vector
for(x in 1:nrow(speciesTable)){
  
  # Create a temporary object to hold the data from each web request
  spData <- lets.iucn(speciesTable$SPECIESNAME[x])
  
  # Extract data from that object into the vectors
  Species[x]<-as.character(spData[1,1])
  Family[x]<-as.character(spData[1,2])
  Status[x]<-as.character(spData[1,3] )
  Criteria[x]<-as.character(spData[1,4])
  Population[x]<-as.character(spData[1,5])
  DescriptionYear[x]<-as.character(spData[1,6])
  
  # Extra bit of code to report the loop (for sanity!), print loop on multiples of 10
  if(x%%10==0) {print(x)}
  flush.console()
  
}

statusTable<-cbind(Species,Family,Status,Criteria,Population,DescriptionYear)
speciesTable<-cbind(speciesTable,statusTable)


