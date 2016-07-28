# Statistics on pressures ####
setwd("/Users/christopherhassall/Dropbox/FAWKES II/Resources/PublicNatura2000End2015_csv")
N2000Impact<-read.csv("IMPACT.csv")
head(N2000Impact)

# DATA CLEANING
# Remove some lowercase letters
N2000Impact$IMPACTCODE<-gsub("j", "J", N2000Impact$IMPACTCODE)
N2000Impact$IMPACTCODE<-gsub("k", "K", N2000Impact$IMPACTCODE)

# Remove codes that do not exist in definitions (0, 6, 8, O and P)
N2000Impact<-subset(N2000Impact,!is.na(match(N2000Impact$IMPACTCODE,c("0","6", "8", "O","P"))))

table(match(N2000Impact$IMPACTCODE,c("0","6", "8", "O","P")))

table(match(Tier1Impact,c("0","6", "8", "O","P")))

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
