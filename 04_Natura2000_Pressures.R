############################################################################
### Purpose of this skript module 04 is to:
### 
### 04.1. Statistics on Natura2000 pressures
### 04.2. ...
###
### Authors: CH ...
############################################################################

############################################################################
### 04.1. Statistics on Natura2000 pressures
###
###
############################################################################

# set wd to path2temp where files have been downloaded and extracted
setwd(path2temp %+% "/") 

# Load data
N2000Impact<-read.csv("IMPACT.csv")
head(N2000Impact)

## DATA CLEANING ####

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

## SUMMARY STATISTICS ####

# Create harmonised impact categories where possible at the four tiers
Tier1Impact<-substr(N2000Impact$IMPACTCODE,1,1)
Tier2Impact<-substr(N2000Impact$IMPACTCODE,1,3)
Tier3Impact<-substr(N2000Impact$IMPACTCODE,1,6)
Tier4Impact<-substr(N2000Impact$IMPACTCODE,1,9)
# Remove the value if the site does not have sufficient details
Tier2Impact[nchar(Tier2Impact)!=3]<-NA
Tier3Impact[nchar(Tier3Impact)!=6]<-NA
Tier4Impact[nchar(Tier4Impact)!=9]<-NA

# Add those new harmonised impacts to the main table
N2000Impact<-cbind(N2000Impact,Tier1Impact,Tier2Impact,Tier3Impact,Tier4Impact)

# Plot distributions of frequencies of impacts
barplot(table(unique(N2000Impact[,c(1,8)])$Tier1Impact))
barplot(table(unique(N2000Impact[,c(1,9)])$Tier2Impact))
barplot(table(unique(N2000Impact[,c(1,10)])$Tier3Impact))
barplot(table(unique(N2000Impact[,c(1,11)])$Tier4Impact))

# 10 most common impacts
# Download definitions of impact codes for simplicity
download.file("http://bd.eionet.europa.eu/activities/Natura_2000/Folder_Reference_Portal/Ref_threats_pressures_FINAL_20110330.xls", "Ref_threats_pressures_FINAL_20110330.xls", mode="wb")
ImpactDefinitions<-readWorksheetFromFile("Ref_threats_pressures_FINAL_20110330.xls",2)[,c(1,2)]

# Rank Tier1 Impacts and give definitions of codes
Tier1Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,8)])$Tier1Impact),decreasing=TRUE))
DefineTier1<-ImpactDefinitions$Descript_EN[match(rownames(Tier1Ranked),ImpactDefinitions$ACT_Code)]
Tier1Summary<-cbind(Tier1Ranked,DefineTier1)
colnames(Tier1Summary)<-c("Number of sites","Type of impact")

# Rank Tier2 Impacts and give definitions of codes
Tier2Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,9)])$Tier2Impact),decreasing=TRUE))
DefineTier2<-ImpactDefinitions$Descript_EN[match(rownames(Tier2Ranked),ImpactDefinitions$ACT_Code)]
Tier2Summary<-cbind(Tier2Ranked,DefineTier2)
colnames(Tier2Summary)<-c("Number of sites","Type of impact")

# Rank Tier3 Impacts and give definitions of codes
Tier3Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,10)])$Tier3Impact),decreasing=TRUE))
DefineTier3<-ImpactDefinitions$Descript_EN[match(rownames(Tier3Ranked),ImpactDefinitions$ACT_Code)]
Tier3Summary<-cbind(Tier3Ranked,DefineTier3)
colnames(Tier3Summary)<-c("Number of sites","Type of impact")

# Rank Tier4 Impacts and give definitions of codes
Tier4Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,11)])$Tier4Impact),decreasing=TRUE))
DefineTier4<-ImpactDefinitions$Descript_EN[match(rownames(Tier4Ranked),ImpactDefinitions$ACT_Code)]
Tier4Summary<-cbind(Tier4Ranked,DefineTier4)
colnames(Tier4Summary)<-c("Number of sites","Type of impact")

############################################################################
### 04.2. parsing database using lookup table from g.docs
### (run 04.1 to clean N2000Impact data first)
### (deprecated, based on 04.4?)
############################################################################

# Condense threats according to relation
library(plyr)
counttab<-count(N2000Impact$IMPACTCODE)
names(counttab)<-c("ACT_Code","count" )

# subdata<-MappingData[which(MappingData$Cultivated.crops=='x'),1:11 ]
# mergedata<-merge(subdata, counttab, by="ACT_Code")
# aggdata<-aggregate(mergedata$count, by=list(mergedata$relation), FUN=sum)
# pie(aggdata$x, labels=aggdata$Group.1, main="Cultivated crops")


#subdata<-MappingData[which(MappingData$Cultivated.crops=='x'), ]
par(mfrow=c(6,3),mar=c(1,2,2,1))
for(i in 11:28) {

  subdata<-MappingData[which(MappingData[,i]=='x'),1:11 ]
  mergedata<-merge(subdata, counttab, by="ACT_Code")
  
  #if (length(mergedata$relation)>1) { 
  aggdata<-aggregate(mergedata$count, by=list(mergedata$relation), FUN=sum)
  #} else {
  #}
  pie(aggdata$x, radius=log(sum(aggdata$x))/log(32177),labels=aggdata$Group.1, main=paste(names(data[i]), "\n", sum(aggdata$x)))
}


length(N2000Impact$IMPACTCODE[N2000Impact$IMPACTCODE=="A01"])


subdata<-MappingData[which(MappingData[,i]=='x'),1:11 ]
mergedata<-merge(subdata, counttab, by="ACT_Code")

MappingData$relation


setwd(path2wd)

############################################################################
### 04.3. Networks showing associations between threats and services
### Useful tutorial here: http://kateto.net/network-visualization
###
############################################################################

### 04.3.1: Associations of threats
# Create a table of threats by sites
ThreatAssoc<-xtabs(~N2000Impact$ï..SITECODE+N2000Impact$Tier1Impact)
ThreatAssoc<-as.data.frame.matrix(ThreatAssoc)

# Create a co-occurrence matrix of threats
ThreatCount<-numeric(length=15*15)
 for (x in 1:15){
   for(y in 1:15){
     TempDat<-subset(ThreatAssoc,ThreatAssoc[,x]!="0" & ThreatAssoc[,y]!="0")
     ThreatCount[(x-1)*15+y]<-nrow(TempDat)
   }
 }
ThreatAssocEdgeTable<-data.frame(Threat1=rep(colnames(ThreatAssoc),each=15),
                                 Threat2=rep(colnames(ThreatAssoc),15),
                                 Weight=ThreatCount)

# Remove duplicates, as well as X (no threats or pressures, or threats and pressures from outside 
# of the EU), U (unknown threats), and L (natural catastrophes)
NoDup<-t(combn(colnames(ThreatAssoc)[-c(12,14,15)],m=2))
NoDupWeight<-numeric(length=nrow(NoDup))
for(x in 1:nrow(NoDup)){
  NoDupWeight[x]<-subset(ThreatAssocEdgeTable,ThreatAssocEdgeTable$Threat1==NoDup[x,1] 
                      & ThreatAssocEdgeTable$Threat2==NoDup[x,2])[,3]
}
ThreatAssocEdgeTable<-data.frame(Threat1=NoDup[,1],Threat2=NoDup[,2],Weight=NoDupWeight)

# There are two ways to visualise the association
# ...first, a network graph
graph <- graph.data.frame(ThreatAssocEdgeTable, directed = FALSE)
E(graph)$width <- E(graph)$Weight/500 # Set edge width based on weight
plot(graph) 

# ...second, a heat matrix
netm <- as_adjacency_matrix(graph, attr="Weight", sparse=F)
colnames(netm) <- V(graph)$media
rownames(netm) <- V(graph)$media
netm[lower.tri(netm)]<-NA # Remove upper triangle
palf <- colorRampPalette(c("blue", "red")) 
heatmap(netm[,12:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )


### 04.3.2: Associations of services
# Convert threats to harmonised threats that all have services associated (the "relation" column)
# in the Google Sheet (sorry that this uses some ugly loops)
ThreatWithService<-character(length=nrow(N2000Impact))
for (x in 1:nrow(N2000Impact)){
  if(!is.na(match(N2000Impact$IMPACTCODE[x],MappingData$ACT_Code))) 
  {ThreatWithService[x]<-as.character(MappingData$relation[which(MappingData$ACT_Code==N2000Impact$IMPACTCODE[x])])} else {ThreatWithService[x]<-NA}
}
# Add that new harmonised threat to the N2000Impact table
N2000Impact2<-cbind(N2000Impact,ThreatWithService)

# Create a vector of simplified service groups
ServiceGroups<-c("CultCrop","WildPlants","PlantMaterialAg","Fibre","PlantEnergy","RearedAnimal","WildAnimal","Aquaculture","SurfWaterDrink",
"SurfWaterNonDrink","GroundWaterDrink","GroundWaterNonDrink","ErosionPrevent","HydroMaintain","FloodProtect","GHGReduction",
"Experiential","Physical","Scientific")

# Create a matrix of site x service group
ServiceBySite<-matrix(ncol=length(ServiceGroups),nrow=length(unique(N2000Impact2$ï..SITECODE)))
rownames(ServiceBySite)<-unique(N2000Impact2$ï..SITECODE)
colnames(ServiceBySite)<-ServiceGroups
# This is the ugly bit, and takes a few minutes to run
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteThreats<-na.omit(subset(N2000Impact2,N2000Impact2$ï..SITECODE==rownames(ServiceBySite)[x])$ThreatWithService)
  # Remove the rows that did not have services
  SiteThreats<-SiteThreats[which(SiteThreats!="NA")]
  # Extract the rows from the mapping table that contain the services associated with those threats
  SiteThreatsMapped<-subset(MappingData,MappingData$relation %in% SiteThreats)
  # For each service group, check whether the site had a threat that indicates that service is present
  for(y in 1:ncol(ServiceBySite)){
    if("x" %in% SiteThreatsMapped[,9+y]) {ServiceBySite[x,y]<-1} else {ServiceBySite[x,y]<-0}
  }
  # Utility function to trace progress (ca. 20,000 sites total)
  if(x %% 100 == 0) {print(x);flush.console()}
}

# Find the total number of times each service occurs
ServiceFreq<-colSums(ServiceBySite)

# Create a co-occurrence matrix of threats
ServiceCount<-numeric(length=15*15)
for (x in 1:19){
  for(y in 1:19){
    TempDat<-subset(ServiceBySite,ServiceBySite[,x]!="0" & ServiceBySite[,y]!="0")
    ServiceCount[(x-1)*19+y]<-nrow(TempDat)
  }
}
ServiceBySiteEdgeTable<-data.frame(Service1=rep(colnames(ServiceBySite),each=19),
                                   Service2=rep(colnames(ServiceBySite),19),
                                   Weight=ServiceCount)

# Remove duplicates
NoDup<-t(combn(colnames(ServiceBySite),m=2))
NoDupWeight<-numeric(length=nrow(NoDup))
for(x in 1:nrow(NoDup)){
  NoDupWeight[x]<-subset(ServiceBySiteEdgeTable,ServiceBySiteEdgeTable$Service1==NoDup[x,1] 
                         & ServiceBySiteEdgeTable$Service2==NoDup[x,2])[,3]
}
ServiceBySiteEdgeTable<-data.frame(Service1=NoDup[,1],Service2=NoDup[,2],Weight=NoDupWeight)

# Plot two different forms of association
par(mfrow=c(1,2))

# ...first, a network graph
graph <- graph.data.frame(ServiceBySiteEdgeTable, directed = FALSE) # create an igraph object
E(graph)$width <- E(graph)$Weight/1000 # Set edge width based on weight
V(graph)$size <- sqrt(ServiceFreq)/3 # Set vertex size based on frequency of service
tkid <- tkplot(graph) # tkid is the id of the tkplot that will open, and allows manual rearragement of nodes
l <- tkplot.getcoords(tkid) # grab the coordinates from final tkplot after rearrangement
plot(graph, layout=l)


# ...and a second network graph excluding weak links
ServiceBySiteEdgeTable2<-subset(ServiceBySiteEdgeTable,ServiceBySiteEdgeTable$Weight>500)
graph <- graph.data.frame(ServiceBySiteEdgeTable2, directed = FALSE)
E(graph)$width <- E(graph)$Weight/1000 # Set edge width based on weight
ServiceFreq2<-ServiceFreq[names(ServiceFreq)%in%names(V(graph))] # extract frequencies from subset of services
V(graph)$size <- sqrt(ServiceFreq2)/3 # Set node size based on frequency of service
tkid <- tkplot(graph) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(graph, layout=l)







############################################################################
### 04.4. Visualisation of threats by services provided
### 
###
############################################################################


# Take the N2000Impact table modified from above (which adds a column for the
# threats that are known to have services associated with them) and remove
# any threats that do not have a service
N2000Impact3<-subset(N2000Impact2,N2000Impact2$ThreatWithService!="NA")

# Calculate the number of services across the site network
SumThreats<-as.matrix(table(N2000Impact3$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each threat into the number of each service provided
ThreatByService<-matrix(ncol=19,nrow=nrow(SumThreats))
colnames(ThreatByService)<-ServiceGroups
rownames(ThreatByService)<-rownames(SumThreats)
# For each of the threats, we specify the number of times that that threat indicates
# a service, using a matrix
for(x in 1:nrow(ThreatByService)){
  # Extract the rows from the mapping table that contain the services associated with those threats
  SiteThreatsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreats)[x])
  # For each service group, check whether the site had a threat that indicates that service is present
  for(y in 1:ncol(ThreatByService)){
    # The cells in the matrix correspond to the frequency with which each threat leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each threat.
    if("x" %in% SiteThreatsMapped[,10+y]) {ThreatByService[x,y]<-SumThreats[x]} else {ThreatByService[x,y]<-0}
  }
}

# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatByService<-matrix(nrow=ncol(ThreatByService)*nrow(ThreatByService),ncol=3)
ThreatFreq<-as.vector(ThreatByService)
LongThreat<-rep(rownames(ThreatByService),19)
LongService<-rep(colnames(ThreatByService),each=nrow(ThreatByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatByService<-as.data.frame(cbind(LongService,LongThreat,ThreatFreq=as.vector(as.numeric(ThreatFreq))))
LongThreatByService<-transform(LongThreatByService, ThreatFreq = as.numeric(ThreatFreq))
treemap(LongThreatByService,c("LongService","LongThreat"),vSize="ThreatFreq")








