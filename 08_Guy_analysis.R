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
### Simpler here - just link Guy's mapping
############################################################################

# Bind Guy's mapping to the threats table
N2000Impact<-cbind(N2000Impact,GuyMappingData[match(N2000Impact$IMPACTCODE,GuyMappingData$ACT_Code),])

# Create a list of services based on Guy's mapping 
ServiceList<-names(GuyMappingData[,c(3:13)])

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
    if(nrow(subset(SiteServices,SiteServices[,10+y]=="x" & SiteServices$IMPACT_TYPE=="P"))>0) {ServiceBySite[x,y] <- 1} else {ServiceBySite[x,y] <- 0}
    if(nrow(subset(SiteServices,SiteServices[,10+y]=="x" & SiteServices$IMPACT_TYPE=="N"))>0) {ServiceBySite[x,y+11] <- 1} else {ServiceBySite[x,y+11] <- 0}
    if("P"%in%subset(SiteServices,SiteServices[,10+y]=="x")$IMPACT_TYPE & "N"%in%subset(SiteServices,SiteServices[,10+y]=="x")$IMPACT_TYPE) {ServiceBySite[x,y+22]<-1;ServiceBySite[x,y] <- 0;ServiceBySite[x,y+11] <- 0} else {ServiceBySite[x,y+22]<-0}
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
ServiceBySite<-data.frame(ServiceBySite,Biogeog=as.factor(BIOREGION[match(rownames(ServiceBySite),BIOREGION$SITECODE),2]))



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

# Load HABITATCLASS
HABITATCLASS<-read.csv("HABITATCLASS.csv")
# Convert % cover to a numeric variable
HABITATCLASS$PERCENTAGECOVER<-as.numeric(as.vector(HABITATCLASS$PERCENTAGECOVER))
# Extract a subset where the % cover is >=50%
HABITATCLASS<-subset(HABITATCLASS,HABITATCLASS$PERCENTAGECOVER>=50)
# Add that dominant habitat to the main table
ServiceBySite<-cbind(ServiceBySite,DomHab=HABITATCLASS$HABITATCODE[match(rownames(ServiceBySite),HABITATCLASS$ï..SITECODE)])

# Subset the data to just include those sites with a dominant class
DomHabData<-subset(ServiceBySite,ServiceBySite$DomHab%in%names(which(table(ServiceBySite$DomHab)>30)))
# Guy used "any mention" as the response
AnyMention<-matrix(nrow=nrow(DomHabData),ncol=11)
for(x in 1:nrow(DomHabData)){
  for(y in 1:11){
    if(sum(DomHabData[x,c(y,y+11,y+22)])>0) {AnyMention[x,y]<-1} else {AnyMention[x,y]<-0}
  }
}

# Find the average number of ESS per dominant habitat
aggregate(rowSums(AnyMention), list(DomHabData$DomHab), mean)

# Find the proportion of SPAs in each dominant habitat that mention each ESS
xtabs(DomHabData$DomHab~AnyMention[,1])
ESSByHab<-aggregate(AnyMention, list(DomHabData$DomHab), mean)

# Tables of each habitat, with positive, negative, and both
# First, N01
N01_Data<-subset(DomHabData,DomHabData$DomHab=="N01")
N02_Data<-subset(DomHabData,DomHabData$DomHab=="N02")
N05_Data<-subset(DomHabData,DomHabData$DomHab=="N05")
N06_Data<-subset(DomHabData,DomHabData$DomHab=="N06")
N07_Data<-subset(DomHabData,DomHabData$DomHab=="N07")
N08_Data<-subset(DomHabData,DomHabData$DomHab=="N08")
N10_Data<-subset(DomHabData,DomHabData$DomHab=="N10")
N12_Data<-subset(DomHabData,DomHabData$DomHab=="N12")
N14_Data<-subset(DomHabData,DomHabData$DomHab=="N14")
N15_Data<-subset(DomHabData,DomHabData$DomHab=="N15")
N16_Data<-subset(DomHabData,DomHabData$DomHab=="N16")
N17_Data<-subset(DomHabData,DomHabData$DomHab=="N17")
N19_Data<-subset(DomHabData,DomHabData$DomHab=="N19")
N23_Data<-subset(DomHabData,DomHabData$DomHab=="N23")

# Find proportions
N01_ESS<-data.frame(Pos=t(unname(aggregate(N01_Data[,c(1:11)], list(N01_Data$DomHab), mean))),
                   Neg=t(unname(aggregate(N01_Data[,c(12:22)], list(N01_Data$DomHab), mean))),
                   Both=t(unname(aggregate(N01_Data[,c(23:33)], list(N01_Data$DomHab), mean))))[-1,]
N02_ESS<-data.frame(Pos=t(unname(aggregate(N02_Data[,c(1:11)], list(N02_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N02_Data[,c(12:22)], list(N02_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N02_Data[,c(23:33)], list(N02_Data$DomHab), mean))))[-1,]
N05_ESS<-data.frame(Pos=t(unname(aggregate(N05_Data[,c(1:11)], list(N05_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N05_Data[,c(12:22)], list(N05_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N05_Data[,c(23:33)], list(N05_Data$DomHab), mean))))[-1,]
N06_ESS<-data.frame(Pos=t(unname(aggregate(N06_Data[,c(1:11)], list(N06_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N06_Data[,c(12:22)], list(N06_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N06_Data[,c(23:33)], list(N06_Data$DomHab), mean))))[-1,]
N07_ESS<-data.frame(Pos=t(unname(aggregate(N07_Data[,c(1:11)], list(N07_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N07_Data[,c(12:22)], list(N07_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N07_Data[,c(23:33)], list(N07_Data$DomHab), mean))))[-1,]
N08_ESS<-data.frame(Pos=t(unname(aggregate(N08_Data[,c(1:11)], list(N08_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N08_Data[,c(12:22)], list(N08_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N08_Data[,c(23:33)], list(N08_Data$DomHab), mean))))[-1,]
N10_ESS<-data.frame(Pos=t(unname(aggregate(N10_Data[,c(1:11)], list(N10_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N10_Data[,c(12:22)], list(N10_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N10_Data[,c(23:33)], list(N10_Data$DomHab), mean))))[-1,]
N12_ESS<-data.frame(Pos=t(unname(aggregate(N12_Data[,c(1:11)], list(N12_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N12_Data[,c(12:22)], list(N12_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N12_Data[,c(23:33)], list(N12_Data$DomHab), mean))))[-1,]
N14_ESS<-data.frame(Pos=t(unname(aggregate(N14_Data[,c(1:11)], list(N14_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N14_Data[,c(12:22)], list(N14_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N14_Data[,c(23:33)], list(N14_Data$DomHab), mean))))[-1,]
N15_ESS<-data.frame(Pos=t(unname(aggregate(N15_Data[,c(1:11)], list(N15_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N15_Data[,c(12:22)], list(N15_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N15_Data[,c(23:33)], list(N15_Data$DomHab), mean))))[-1,]
N16_ESS<-data.frame(Pos=t(unname(aggregate(N16_Data[,c(1:11)], list(N16_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N16_Data[,c(12:22)], list(N16_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N16_Data[,c(23:33)], list(N16_Data$DomHab), mean))))[-1,]
N17_ESS<-data.frame(Pos=t(unname(aggregate(N17_Data[,c(1:11)], list(N17_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N17_Data[,c(12:22)], list(N17_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N17_Data[,c(23:33)], list(N17_Data$DomHab), mean))))[-1,]
N19_ESS<-data.frame(Pos=t(unname(aggregate(N19_Data[,c(1:11)], list(N19_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N19_Data[,c(12:22)], list(N19_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N19_Data[,c(23:33)], list(N19_Data$DomHab), mean))))[-1,]
N23_ESS<-data.frame(Pos=t(unname(aggregate(N23_Data[,c(1:11)], list(N23_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N23_Data[,c(12:22)], list(N23_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N23_Data[,c(23:33)], list(N23_Data$DomHab), mean))))[-1,]

# Convert to numeric
N01_ESS<-transform(N01_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N02_ESS<-transform(N02_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N05_ESS<-transform(N05_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N06_ESS<-transform(N06_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N07_ESS<-transform(N07_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N08_ESS<-transform(N08_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N10_ESS<-transform(N10_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N12_ESS<-transform(N12_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N14_ESS<-transform(N14_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N15_ESS<-transform(N15_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N16_ESS<-transform(N16_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N17_ESS<-transform(N17_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N19_ESS<-transform(N19_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N23_ESS<-transform(N23_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))

# Plot barcharts
par(mfrow=c(4,4),mar=c(2,5.5,2,1))

Services<-c("Crop","Fodder","Fibre","Livestock","Wild food","Aquaculture","Water","Erosion","Flow","Carbon seq","Recreation")

barplot(t(as.matrix(N01_ESS)),main="N01: Marine",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N02_ESS)),main="N02: Intertidal",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N05_ESS)),main="N05: Shore",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N06_ESS)),main="N06: Inland water",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N07_ESS)),main="N07: Marshes",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N08_ESS)),main="N08: Heath",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N10_ESS)),main="N10: Grassland",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N12_ESS)),main="N12: Cropland",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N14_ESS)),main="N14: Improv grass",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N15_ESS)),main="N15: Other arable",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N16_ESS)),main="N16: Broad wood",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N17_ESS)),main="N17: Conif wood",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N19_ESS)),main="N19: Mixed wood",names.arg=Services,horiz=TRUE,las=1)
barplot(t(as.matrix(N23_ESS)),main="N23: Other land",names.arg=Services,horiz=TRUE,las=1)

# Now we plot the other way around: bar charts for each ESS across the different core habitats
AllESS<-as.data.frame(rbind(N01_ESS,N02_ESS,N05_ESS,N06_ESS,N07_ESS,N08_ESS,N10_ESS,N12_ESS,N14_ESS,N15_ESS,N16_ESS,N17_ESS,N19_ESS,N23_ESS))
AllESS$ESS<-rep(Services,14)
Habitats<-c("N01","N02","N05","N06","N07","N08","N10","N12","N14","N15","N16","N17","N19","N23")
AllESS$Habitat<-rep(Habitats,each=11)
AllESS<-AllESS[,c(1,3,2,4,5)]

par(mfrow=c(3,4))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Crop"))),main="Crop",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Fodder"))),main="Fodder",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Fibre"))),main="Fibre",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Livestock"))),main="Livestock",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Wild food"))),main="Wild food",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Aquaculture"))),main="Aquaculture",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Water"))),main="Water",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Erosion"))),main="Erosion",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Flow"))),main="Flow",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Carbon seq"))),main="Carbon seq",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Recreation"))),main="Recreation",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))


############################################################################
### Bird and IUCN conservation by net ESS
############################################################################

# Convert the A, B, C CONSERVATION code to a numeric score
SPECIES<-read.csv("SPECIES.csv")
ConvertToNumber<-data.frame(Letters=c("A","B","C"),Numbers=c(2,1,0))
SpeciesIndex<-ConvertToNumber[match(SPECIES$CONSERVATION,ConvertToNumber[,1]),2]
SPECIES$SpeciesIndex<-SpeciesIndex

BIRDSPECIES<-subset(SPECIES,SPECIES$SPGROUP=="Birds")

# Add bird scores to the sites
IUCNIndex<-BirdIndex<-IUCNNumber<-BirdNumber<-numeric(length=nrow(ServiceBySite))
for(x in 1:nrow(ServiceBySite)){
  BirdIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(ServiceBySite)[x]))
  BirdNumber[x]<-length(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(ServiceBySite)[x]))
  IUCNIndex[x]<-mean(c(rep(2,subset(SiteTrends$Inc,SiteTrends$Site==rownames(ServiceBySite)[x])),
                       rep(1,subset(SiteTrends$Stable,SiteTrends$Site==rownames(ServiceBySite)[x])),
                       rep(0,subset(SiteTrends$Dec,SiteTrends$Site==rownames(ServiceBySite)[x]))))
  IUCNNumber[x]<-sum(subset(SiteTrends$Inc,SiteTrends$Site==rownames(ServiceBySite)[x]),
                     subset(SiteTrends$Stable,SiteTrends$Site==rownames(ServiceBySite)[x]),
                     subset(SiteTrends$Dec,SiteTrends$Site==rownames(ServiceBySite)[x]))
  if(x%%100==0) {print(x)}
  #print(x)
  flush.console()
}

par(mfrow=c(2,2))
plot(NetESS,BirdIndex)
plot(NetESS,IUCNIndex)


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
cor.test(NetESS,BirdIndex,method="spearman")
cor.test(SummaryBirdData[,2],SummaryBirdData[,1],method="pearson")
abline(lm(SummaryBirdData[,2]~SummaryBirdData[,1],weights=SummaryBirdData[,4]))

SummaryIUCNData<-matrix(ncol=4,nrow=13)
colnames(SummaryIUCNData)<-c("NetESS","MeanBirdStatus","SE","N")
for(x in 1:13){
  SummaryIUCNData[x,1]<-x-9
  SummaryIUCNData[x,2]<-mean(subset(IUCNIndex,NetESS==x-9),na.rm=TRUE)
  SummaryIUCNData[x,3]<-sd(subset(IUCNIndex,NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(IUCNIndex,NetESS==x-9)))
  SummaryIUCNData[x,4]<-length(subset(IUCNIndex,NetESS==x-9))
}
plot(SummaryIUCNData[,1],SummaryIUCNData[,2],ylim=c(0.4,0.73),ylab="IUCN Index",xlab="Net ESS")
arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]+SummaryIUCNData[,3],length=0)
arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]-SummaryIUCNData[,3],length=0)
text(SummaryIUCNData[,1],SummaryIUCNData[,2]+SummaryIUCNData[,3]+0.05,labels=SummaryIUCNData[,4])
cor.test(NetESS,IUCNIndex,method="spearman")
cor.test(SummaryIUCNData[,2],SummaryIUCNData[,1],method="pearson")
cor.test(SummaryIUCNData[c(1:12),2],SummaryIUCNData[c(1:12),1])
abline(lm(SummaryIUCNData[,2]~SummaryIUCNData[,1],weights=SummaryIUCNData[,4]))
cor.test(subset(NetESS,NetESS<4),subset(IUCNIndex,NetESS<4),method="spearman")
abline(lm(SummaryIUCNData[c(1:12),2]~SummaryIUCNData[c(1:12),1],weights=SummaryIUCNData[c(1:12),4]),lty=2)


############################################################################
### Bird conservation (from N2000) and IUCN status by mean net ESS
############################################################################

# (i) the mean net ESS that each species experiences vs the mean “Bird index” score 
# (based on the conservation status of each site)
BirdSpeciesNames<-unique(BIRDSPECIES$SPECIESNAME)
BirdSpeciesIndex<-numeric(length=length(BirdSpeciesNames))
for(x in 1:length(BirdSpeciesNames)){
  BirdSpeciesIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SPECIESNAME==BirdSpeciesNames[x]))
}

BirdNetESS<-numeric(length=length(BirdSpeciesNames))
for(x in 1:length(BirdSpeciesNames)){
  BirdSites<-subset(BIRDSPECIES$SITECODE,BIRDSPECIES$SPECIESNAME==BirdSpeciesNames[x])
  BirdNetESS[x]<-mean(subset(ServiceBySite$NetESS,rownames(ServiceBySite)%in%BirdSites))
}

plot(BirdSpeciesIndex,BirdNetESS)
cor.test(BirdSpeciesIndex,BirdNetESS,method="spearman")


# (ii) the IUCN classification for each species (increasing/decreasing/stable as 
# well as VU, EN, etc) vs mean net ESS

BirdSpeciesOutput<-data.frame(BirdSpecies=BirdSpeciesNames,ConservationIndex=BirdSpeciesIndex,
                              NetESS=BirdNetESS,)

statusTable[match(statusTable[,1],BirdSpeciesNames),1]

# Data for map
MikaOutput<-ServiceBySite[,c(1:44,50,52)]
write.table(MikaOutput,"MikaOutput.txt")
