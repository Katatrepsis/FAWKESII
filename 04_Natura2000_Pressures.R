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
###
###
############################################################################

N2000Impact<-read.csv("IMPACT.csv")

#condense threats according to relation



library(plyr)
counttab<-count(N2000Impact$IMPACTCODE)
names(counttab)<-c("ACT_Code","count" )

# subdata<-data[which(data$Cultivated.crops=='x'),1:11 ]
# mergedata<-merge(subdata, counttab, by="ACT_Code")
# aggdata<-aggregate(mergedata$count, by=list(mergedata$relation), FUN=sum)
# pie(aggdata$x, labels=aggdata$Group.1, main="Cultivated crops")


#subdata<-data[which(data$Cultivated.crops=='x'), ]
for(i in 11:28) {

  subdata<-data[which(data[,i]=='x'),1:11 ]
  mergedata<-merge(subdata, counttab, by="ACT_Code")
  
  #if (length(mergedata$relation)>1) { 
  aggdata<-aggregate(mergedata$count, by=list(mergedata$relation), FUN=sum)
  #} else {
  #}
  pie(aggdata$x, labels=aggdata$Group.1, main=paste(names(data[i]), "\n", sum(aggdata$x)))
}


length(N2000Impact$IMPACTCODE[N2000Impact$IMPACTCODE=="A01"])





data$relation


setwd(path2wd)
