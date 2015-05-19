#Set up address file from master first
#Working companies into regional bins

setwd("~/git/open-business")
options(stringsAsFactors = FALSE)

#This is an epically epically large file - load at your own peril
#will need local version
nspl <- read.csv("NSPL_AUG_2014_DO_NOT_COMMIT/Data/NSPL_AUG_2014_UK.csv")


postcode.reg <- read.csv("data/master-postcodes.csv")

#-----------------------
#-----------------------
#Read extra postcodes
postcode.reg.ex <- read.csv("data/extra-postcodes.csv")

#Bind these onto the original postcodes
postcode.reg.all <- rbind(postcode.reg, postcode.reg.ex)

#To run all - set: postcode.reg <- postcode.reg.all

#-----------------------
#-----------------------

#-----------------------

#Match the postcodes to the entries in nspl and get the approriate entry in 
postcode.reg$reg.code <- nspl$gor[match(postcode.reg$x, nspl$pcds)]

ref.code <- c("E12000001", "E12000002", "E12000003", "E12000004", "E12000005", "E12000006",
          "E12000007", "E12000008", "E12000009", "W99999999", "S99999999", 'N99999999')  
	
ref.region <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands",
            "East of England", "London", "South East", "South West",  "Wales", "Scotland", "Northern Ireland")

postcode.reg$region <- ref.region[match(postcode.reg$reg.code, ref.code)]

#--------------------------------
#Stats

#Set percentages as a data frame
regions <- as.data.frame(round(table(postcode.reg$region) / length(na.omit(postcode.reg$region)) * 100, digits=0))

#Order
#As character
regions$Var1 <- as.character(regions$Var1)
regions <- regions[order(-regions$Freq),]
row.names(regions) = NULL


#Set counts as a data frame
regions.count <- as.data.frame(table(postcode.reg$region))

#Order
#As character
regions.count$Var1 <- as.character(regions.count$Var1)
regions.count <- regions.count[order(-regions.count$Freq),]
row.names(regions.count) = NULL


#regions.count$label <- reorder(regions.count$label, regions.count$Freq) #if plot

#---------------------------------------------------
#Bins for London boroughs

#Get local authority code
postcode.reg$la.code <- nspl$laua[match(postcode.reg$x, nspl$pcds)]

#Get the names attatched to these - it is in this csv (converted) documents
lads <- read.csv("NSPL_AUG_2014_DO_NOT_COMMIT/Documents/LA_UA.csv")

#name these local authorities
postcode.reg$la <- lads$LAD13NM[match(postcode.reg$la.code, lads$LAD13CD)]

#is London?
postcode.reg$islondon <- ifelse(postcode.reg$region == "London", TRUE, FALSE)


#Get the boroughs
borough <- postcode.reg$la[postcode.reg$islondon]

#--------------------------------
#Stats


#Set percentages as a data frame
borough.percent <- as.data.frame(round(table(borough) / length(na.omit(borough)) * 100, digits=0))

#Order
#As character
borough.percent$borough <- as.character(borough.percent$borough)
borough.percent <- borough.percent[order(-borough.percent$Freq),]
row.names(borough.percent) = NULL


#Set counts as a data frame
borough.count <- as.data.frame(table(borough))

#Order
#As character
borough.count$borough <- as.character(borough.count$borough)
borough.count <- borough.count[order(-borough.count$Freq),]
row.names(borough.count) = NULL



