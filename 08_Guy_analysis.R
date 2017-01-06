############################################################################
### Purpose of this script module 06 is to:
###
### Replicate Guy's Excel analysis
### ecosystem services
### 
### Authors: CH, AC, MB, AK
###
### Run skript modules 00, 01, 02 before
############################################################################

############################################################################
### To do:
### 
###  [CH] Still need to do the "dominant habitat" analysis
###  [CH] Attempt the rose diagrams that Guy suggested
### 
############################################################################

############################################################################
### 08.1 N2000Impact data frame
###
### Create N2000Impact data frame as a base for further data analysis
############################################################################

# set wd to path2temp where files have been downloaded and extracted
setwd(path2temp %+% "/") 

# Load data
N2000Impact<-read.csv("IMPACT.csv")


#### DATA CLEANING ####

# Change column name from "ï..SITECODE" to "SITECODE"
colnames(N2000Impact)[1] <- "SITECODE"

# Convert lower case to upper case
N2000Impact$IMPACTCODE<-gsub("j", "J", N2000Impact$IMPACTCODE)
N2000Impact$IMPACTCODE<-gsub("k", "K", N2000Impact$IMPACTCODE)

# Convert letter "O" to number "0"
N2000Impact$IMPACTCODE<-gsub("O", "0", N2000Impact$IMPACTCODE)

# Replace comma with period
N2000Impact$IMPACTCODE<-gsub(",", ".", N2000Impact$IMPACTCODE)

# Remove spaces
N2000Impact$IMPACTCODE<-gsub(" ", "", N2000Impact$IMPACTCODE)

# Some impact codes had a period as the final character, which is also invalid
for(x in 1:nrow(N2000Impact)){
  if(substr(N2000Impact$IMPACTCODE[x],nchar(N2000Impact$IMPACTCODE[x]),nchar(N2000Impact$IMPACTCODE[x]))==".")
  {N2000Impact$IMPACTCODE[x]<-substr(N2000Impact$IMPACTCODE[x],1,nchar(N2000Impact$IMPACTCODE[x])-1)}
}

# Remove codes that do not exist in definitions, i.e. beginning with 0, 6, 8, O and P (n=102)
FirstChar<-substr(N2000Impact$IMPACTCODE,1,1)
N2000Impact<-subset(N2000Impact,is.na(match(FirstChar,c("0","6", "8", "O","P"))))

# Remove NULL impact codes (n=5494)
N2000Impact<-subset(N2000Impact,N2000Impact$IMPACTCODE!="NULL")

# And some very specific mistakes
N2000Impact<-subset(N2000Impact,N2000Impact$IMPACTCODE!="D014.01") # Not possible to establish whether D01.01 or D04.01, so delete
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="D2.01")]<-"D02.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="F.03.01.01")]<-"F03.01.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="K.02.01")]<-"K02.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="G.01.04.03")]<-"G01.04.03" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="F3.01.01")]<-"F03.01.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="C3.03")]<-"C03.03"

# Remove duplicate lines
N2000Impact<-N2000Impact[!duplicated(N2000Impact),]

# Convert to factor
N2000Impact$IMPACTCODE<-as.factor(N2000Impact$IMPACTCODE)


###############################################
# 08.2 Subsetting data 
# Just working with subset of N2000Impact with the following characteristics:
#     (i) SITE_TYPE = A or C (SPA sites only)
#     (ii) INTENSITY = MEDIUM or HIGH
#     (iii) OCCURRENCE = IN or BOTH
###############################################

# First, subset N2000Impact by intensity and occurrence
N2000Impact<-subset(N2000Impact,N2000Impact$INTENSITY!="LOW" & N2000Impact$OCCURRENCE!="OUT")

# Assign site type
# Load data
N2000Sites <- read.csv("NATURA2000SITES.csv")
N2000Sites[,4] <- as.character(N2000Sites[,4])
N2000Impact$SITE_TYPE <- NA

# Add site type to N2000Impact
# Running time 117 seconds
#ptm2 <- proc.time()
for(x in 1:nrow(N2000Impact)){
  N2000Impact$SITE_TYPE[x] <- N2000Sites[match(N2000Impact$SITECODE[x],N2000Sites$SITECODE),4]
}
#proc.time() - ptm2
# Now subset to exclude SITE_TYPE="B"
N2000Impact<-subset(N2000Impact,N2000Impact$SITE_TYPE!="B")


############################################################################
### 08.3 Associations of services
###
### Simplere here - just link Guy's mapping
############################################################################

# Bind Guy's mapping to the threats table
N2000Impact<-cbind(N2000Impact,GuyMappingData[match(N2000Impact$IMPACTCODE,GuyMappingData$ACT_Code),])

# Create a list of services based on Guy's mapping 
ServiceList<-names(GuyMappingData[,c(2:12)])

ServiceBySite<-matrix(ncol=length(ServiceList)*4,nrow=length(unique(N2000Impact$SITECODE)))
rownames(ServiceBySite)<-unique(N2000Impact$SITECODE)
colnames(ServiceBySite)<-c(paste(ServiceList,"POS"),paste(ServiceList,"NEG"),paste(ServiceList,"BOTH"),paste(ServiceList,"NET"))

# Run through Guy's mapping and tally the positive (in the first 11 columns) and negative (second 11 columns)
# ESS associated with each site. Then calculate the difference between the two to give a net score for each
# ESS on each site
ptm <- proc.time()
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteServices<-subset(N2000Impact,N2000Impact$SITECODE==rownames(ServiceBySite)[x])
  # For each service group (defined by Guy), sum the number of times it was positive or negative
  for(y in 1:length(ServiceList)){
    if(nrow(subset(SiteServices,SiteServices[,9+y]=="x" & SiteServices$IMPACT_TYPE=="P"))>0) {ServiceBySite[x,y] <- 1} else {ServiceBySite[x,y] <- 0}
    if(nrow(subset(SiteServices,SiteServices[,9+y]=="x" & SiteServices$IMPACT_TYPE=="N"))>0) {ServiceBySite[x,y+11] <- 1} else {ServiceBySite[x,y+11] <- 0}
    if("P"%in%subset(SiteServices,SiteServices[,9+y]=="x")$IMPACT_TYPE & "N"%in%subset(SiteServices,SiteServices[,9+y]=="x")$IMPACT_TYPE) {ServiceBySite[x,y+22]<-1} else {ServiceBySite[x,y+22]<-0}
    ServiceBySite[x,y+33]<-ServiceBySite[x,y]-ServiceBySite[x,y+11]
  }
  # Timer to track progress of loop
  if(x %% 100 == 0) {print(x/nrow(ServiceBySite));flush.console()}
}
proc.time() - ptm

# Final "net" value for all ESS across each site
NetESS<-rowSums(ServiceBySite[,c(34:44)])
ServiceBySite<-cbind(ServiceBySite,NetESS)

# Add Bioregion
BIOREGION<-read.csv("BIOREGION.csv")
ServiceBySite<-data.frame(ServiceBySite,Biogeog=as.factor(BIOREGION[match(rownames(ServiceBySite),BIOREGION$ï..SITECODE),2]))



############################################################################
### 08.4 Bar plots
###
### Creates bar plots of types of ESS by biogeographical region
############################################################################

par(mfrow=c(3,2))

# Figure 2A (all SPAs)
All_BarData<-cbind(colSums(ServiceBySite[,c(1:11)]),colSums(ServiceBySite[,c(23:33)]),colSums(ServiceBySite[,c(12:22)]))
rownames(All_BarData) <- ServiceList
colnames(All_BarData) <- c("POS","NEG","BOTH")
barplot(t(All_BarData)/38.80,las=2, legend=T, main="(A) All SPAs")

# Figure 2B (Atlantic)
Atlantic_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Atlantic")
Atlantic_BarData<-cbind(colSums(Atlantic_ServiceBySite[,c(1:11)]),colSums(Atlantic_ServiceBySite[,c(23:33)]),colSums(Atlantic_ServiceBySite[,c(12:22)]))
rownames(Atlantic_BarData) <- ServiceList
colnames(Atlantic_BarData) <- c("POS","NEG","BOTH")
barplot(t(Atlantic_BarData)/(nrow(Atlantic_ServiceBySite)/100),las=2, legend=T, main="(B) Atlantic SPAs")

# Figure 2C (Continental)
Continental_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Continental")
Continental_BarData<-cbind(colSums(Continental_ServiceBySite[,c(1:11)]),colSums(Continental_ServiceBySite[,c(23:33)]),colSums(Continental_ServiceBySite[,c(12:22)]))
rownames(Continental_BarData) <- ServiceList
colnames(Continental_BarData) <- c("POS","NEG","BOTH")
barplot(t(Continental_BarData)/(nrow(Continental_ServiceBySite)/100),las=2, legend=T, main="(C) Continental SPAs")

# Figure 2D (Mediterranean)
Mediterranean_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Mediterranean")
Mediterranean_BarData<-cbind(colSums(Mediterranean_ServiceBySite[,c(1:11)]),colSums(Mediterranean_ServiceBySite[,c(23:33)]),colSums(Mediterranean_ServiceBySite[,c(12:22)]))
rownames(Mediterranean_BarData) <- ServiceList
colnames(Mediterranean_BarData) <- c("POS","NEG","BOTH")
barplot(t(Mediterranean_BarData)/(nrow(Mediterranean_ServiceBySite)/100),las=2, legend=T, main="(D) Mediterranean SPAs")

# Figure 2E (Boreal)
Boreal_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Boreal")
Boreal_BarData<-cbind(colSums(Boreal_ServiceBySite[,c(1:11)]),colSums(Boreal_ServiceBySite[,c(23:33)]),colSums(Boreal_ServiceBySite[,c(12:22)]))
rownames(Boreal_BarData) <- ServiceList
colnames(Boreal_BarData) <- c("POS","NEG","BOTH")
barplot(t(Boreal_BarData)/(nrow(Boreal_ServiceBySite)/100),las=2, legend=T, main="(E) Boreal SPAs")

# Figure 2F (Alpine)
Alpine_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Alpine")
Alpine_BarData<-cbind(colSums(Alpine_ServiceBySite[,c(1:11)]),colSums(Alpine_ServiceBySite[,c(23:33)]),colSums(Alpine_ServiceBySite[,c(12:22)]))
rownames(Alpine_BarData) <- ServiceList
colnames(Alpine_BarData) <- c("POS","NEG","BOTH")
barplot(t(Alpine_BarData)/(nrow(Alpine_ServiceBySite)/100),las=2, legend=T, main="(F) Alpine SPAs")


############################################################################
### ESS by core habitat
############################################################################

# Still to do

############################################################################
### Bird and IUCN conservation by net ESS
############################################################################

# Convert the A, B, C CONSERVATION code to a numeric score
SPECIES<-read.csv("SPECIES.csv")
ConvertToNumber<-data.frame(Letters=c("A","B","C"),Numbers=c(2,1,0))
SpeciesIndex<-ConvertToNumber[match(SPECIES$CONSERVATION,ConvertToNumber[,1]),2]
SPECIES$SpeciesIndex<-SpeciesIndex

BIRDSPECIES<-subset(SPECIES,SPECIES$SPGROUP=="Birds")

# Add bird and all species scores to the sites
AllSpeciesIndex<-BirdIndex<-numeric(length=nrow(ServiceBySite))
for(x in 1:nrow(ServiceBySite)){
  BirdIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(ServiceBySite)[x]))
}

par(mfrow=c(1,2))
plot(NetESS,BirdIndex)

SummaryBirdData<-matrix(ncol=4,nrow=13)
colnames(SummaryBirdData)<-c("NetESS","MeanBirdStatus","SE","N")
for(x in 1:13){
  SummaryBirdData[x,1]<-x-9
  SummaryBirdData[x,2]<-mean(subset(BirdIndex,NetESS==x-9),na.rm=TRUE)
  SummaryBirdData[x,3]<-sd(subset(BirdIndex,NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(BirdIndex,NetESS==x-9)))
  SummaryBirdData[x,4]<-length(subset(BirdIndex,NetESS==x-9))
}
plot(SummaryBirdData[,1],SummaryBirdData[,2],ylim=c(0.7,1.7),ylab="Bird Index",xlab="Net ESS")
arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]+SummaryBirdData[,3],length=0)
arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]-SummaryBirdData[,3],length=0)
text(SummaryBirdData[,1],SummaryBirdData[,2]+SummaryBirdData[,3]+0.05,labels=SummaryBirdData[,4])
cor.test(NetESS,ServiceBySite$IUCN,method="spearman")


