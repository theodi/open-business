setwd("~/git/open-business")
library(car)
library(RGoogleDocs)
library(ggplot2)
library(lubridate)
library(treemap)
source('~/git/ODI colour Scheme.R')
theme_set(theme_minimal(base_family = "Helvetica", base_size = 18))

options(stringsAsFactors = FALSE)

# This loads the Google password
source('~/git/SENSITIVE DO NOT COMMIT.R')
# Get the data directly
sheets.con <- getGoogleDocsConnection(getGoogleAuth("jamie.fawcett@theodi.org", gmailpw, service = "wise"))
spreadsheets <- getDocs(sheets.con)
# Remove passwords
rm(list = c('gmailpw'))

#------------------------------

# Load the master database
list.sheet <- getWorksheets(spreadsheets[["1. Master Database"]], sheets.con)
list.all <- sheetAsMatrix(list.sheet[["Data"]], header = TRUE)
sic.lookup <- sheetAsMatrix(list.sheet[["SIC Code Lookup"]], header = TRUE)
#--------------------------------------------------
# CLEAN DATA
#--------------------------------------------------

# Takes list as only observations (companies) which passed inclusion criteria
list <- list.all[list.all[, "Confident in inclusion on public OBUK"] == "Y", ] 

#Sets up columns of master - so can use headers
column.list <- function(var, x = list){
  var_index <- grep(var, names(x))
  return(var_index)
}


#Naming columns - structuring the data

#Displays head of specific column
#head(list[, column.list("Reason for inculsion\\/exclusion on public OBUK \\(without survey\\)")])
#head(list[, column.list("Primary UK Address")])
#head(list[, column.list("Primary Field\\/Industry")])
#head(list[, column.list("Incorporated Date")])
#head(list[, column.list("SICCode1 Section")])
#head(list[, column.list("SICCode2 Section")])
#head(list[, column.list("SICCode3 Section")])
#head(list[, column.list("SICCode4 Section")])


# name columns
col.incl <- column.list("Reason for inculsion\\/exclusion on public OBUK \\(without survey\\)")
col.addr <- column.list("Primary UK Address")
col.adpl <- column.list("Registered Address postcode")
col.indu <- column.list("Primary Field\\/Industry")
col.date <- column.list("Incorporated Date")

#Sector analysis by SIC section
sic.1 <- column.list("SICCode1 Section")
sic.2 <- column.list("SICCode2 Section")
sic.3 <- column.list("SICCode3 Section")
sic.4 <- column.list("SICCode4 Section")

#SIC analysis by ONS broad industry grouping
onssic.1 <- column.list("SIC1 ONS class")
onssic.2 <- column.list("SIC2 ONS class")
onssic.3 <- column.list("SIC3 ONS class")
onssic.4 <- column.list("SIC4 ONS class")
#---------------------------------------------------
# Analysis
#---------------------------------------------------

# Numbers

#shows observations in each category for specific column
#table(list[, col.incl])
#table(list[, col.incl]) / length(na.omit(list[, col.incl])) * 100


#---------------------------------------------------

# Location
# Based on "Primary UK Address"

#Location Plot - trading address postcodes
postcodes <- na.omit(list[, "Postcode"])
write.csv(postcodes, "data/master-postcodes.csv", , row.names = FALSE)

#Additional postcodes - registered address postcodes for those missing trading
ad.post <- ifelse(is.na(list[, "Postcode"]) == TRUE, list[, col.adpl], NA)
postcodes.ex <- na.omit(ad.post)
write.csv(postcodes.ex, "data/extra-postcodes.csv", , row.names = FALSE)

#RUN "Postcode geo plot.R" to plot maps
#FOR regional data run "regional_bins.R"


#---------------------------------------------------

#Sectors

#Sectors BASED ON ONS BROAD INDUSTRY GROUPS

#All comparisons made
onssic.codes <- ifelse(is.na(list[,onssic.1]), NA, 
                    ifelse(is.na(list[,onssic.2]), list[,onssic.1], ifelse(list[,onssic.1] == list[,onssic.2],
                                                                     #Add in branch for this occassion - where 1 = 2 - instead of just paste(list[,onssic.1])
                                                                     ifelse(is.na(list[,onssic.3]), paste(list[,onssic.1]), ifelse(list[,onssic.1] == list[,onssic.3], paste(list[,onssic.1]), 
                                                                                                                             ifelse(is.na(list[,onssic.4]), paste(list[,onssic.1], ",", list[,onssic.3]), ifelse(list[,onssic.1] == list[,onssic.4], paste(list[,onssic.1], ",", list[,onssic.3]),                                                               
                                                                                                                                                                                                        ifelse(list[,onssic.3] == list[,onssic.4], paste(list[,onssic.1], ",", list[,onssic.3]), paste(list[,onssic.1], ",", list[,onssic.3], ",", list[,onssic.4])))))), 
                                                                     ifelse(is.na(list[,onssic.3]), paste(list[,onssic.1], ",", list[,onssic.2]), ifelse(list[,onssic.2] == list[,onssic.3], 
                                                                                                                                                #Add in branch for this occassion - where 2 = 3 - instead of just paste(list[,onssic.1], list[,onssic.2]),
                                                                                                                                                ifelse(is.na(list[,onssic.4]), paste(list[,onssic.1], ",", list[,onssic.2]), ifelse(list[,onssic.2] == list[,onssic.4], paste(list[,onssic.1], ",", list[,onssic.2]), 
                                                                                                                                                                                                                           ifelse(list[,onssic.1] == list[,onssic.4], paste(list[,onssic.1], ",", list[,onssic.2]), paste(list[,onssic.1], ",", list[,onssic.2], ",", list[,onssic.4])))),
                                                                                                                                                ifelse(is.na(list[,onssic.4]), paste(list[,onssic.1], ",", list[,onssic.2], ",", list[,onssic.3]), ifelse(list[,onssic.3] == list[,onssic.4], paste(list[,onssic.1], ",", list[,onssic.2], ",", list[,onssic.3]),
                                                                                                                                                                                                                                              paste(list[,onssic.1], ",", list[,onssic.2], ",", list[,onssic.3], ",", list[,onssic.4]))))))))                                          

#Split the string up into all observations and unlist it - so companies can have multiple in same vector
onssic.codes.all <- unlist(strsplit(na.omit(onssic.codes), " , "))

#Get counts of each observation - i.e. which sectors
table(onssic.codes.all)
# Percentages
round(table(onssic.codes.all) / length(na.omit(onssic.codes.all)) * 100, digits = 0)

#To use

#Set percentages as a data frame
onssectors <- as.data.frame(round(table(onssic.codes.all) / length(na.omit(onssic.codes.all)) * 100, digits=0))
#Easier colname
colnames(onssectors) <- c('label', 'Freq')
#As character
onssectors$label <- as.character(onssectors$label)
#Order
onssectors <- onssectors[order(-onssectors$Freq),]
row.names(onssectors) = NULL
onssectors$label <- reorder(onssectors$label, onssectors$Freq) #Order to plot


#Set percentages as a data frame
onssectors.count <- as.data.frame(table(onssic.codes.all))
#Easier colname
colnames(onssectors.count) <- c('label', 'Freq')
#As character
onssectors.count$label <- as.character(onssectors.count$label)
#Order
onssectors.count <- onssectors.count[order(-onssectors.count$Freq),]
row.names(onssectors.count) = NULL
onssectors.count$label <- reorder(onssectors.count$label, onssectors.count$Freq) #Order to plot


#plot options

#tree map

#Hack for colours - unlist odi_pallette and times to make same number?
#onssectors$colours <- c(odi_pallette, "white")

#treemap(onssectors, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
#        algorithm = "pivotSize",fontsize.labels = 15, fontface.labels = "bold", fontfamily.labels = "Helvetica Neue",
#        border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = TRUE, 
#        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 0.7)

#Cut down the labels for long ones which are small
#As character
onssectors.count$label <- as.character(onssectors.count$label)
onssectors.count$label[onssectors.count$label == "Transport & storage (inc postal)"] <- as.character("Transport & storage")
onssectors.count$label[onssectors.count$label == "Public administration & defence"] <- as.character("Public admin & defence")

#Fixing sizes by creating "other category" and "other treemap"
#hack as not dynamic - fix this
#Take observations which are low out
onssectors.count.other <- onssectors.count[9:13,]
row.names(onssectors.count.other) = NULL

#Take the other observations as main treemap
onssectors.count.main <- onssectors.count[1:8,]
#Create a row to summarize 
other.onssectors <- c("Other industries", sum(onssectors.count.other$Freq))
#bind this onto the main sectors
onssectors.count.main <- rbind(onssectors.count.main, other.onssectors)
#make numeric
onssectors.count.main$Freq <- as.numeric(onssectors.count.main$Freq)
#Order
onssectors.count.main <- onssectors.count.main[order(-onssectors.count.main$Freq),]
row.names(onssectors.count.main) = NULL
onssectors.count.main$label <- reorder(onssectors.count.main$label, onssectors.count.main$Freq) #Order to plot


#colours for main - odi pallette 1
onssectors.count.main$colours <- c(odi_2dBlue, odi_dBlue, odi_mdBlue, odi_2mBlue, odi_mBlue, 
                                   odi_lBlue, odi_2pBlue, odi_turquoise, odi_lGreen)

#   , odi_dGreen

#colours for other
onssectors.count.other$colours <- c(odi_yellow, odi_orange, odi_crimson, odi_red, odi_lPink)
#.... ...odi_dPink, , odi_purple


#main treemap
treemap(onssectors.count.main, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold" , fontfamily.labels = "Helvetica Neue",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)

#Other treemap
treemap(onssectors.count.other, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold", fontfamily.labels = "Helvetica Neue",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)



#Save Main
#PNG
png("graphics/Master/sector_treemap_main.png")
treemap(onssectors.count.main, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold" , fontfamily.labels = "Helvetica Neue",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)
dev.off()

#EPS
setEPS()
postscript("graphics/Master/sector_treemap_main.eps")
treemap(onssectors.count.main, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold" , fontfamily.labels = "Helvetica",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)
dev.off()


#SVG
svg("graphics/Master/sector_treemap_main.svg")
treemap(onssectors.count.main, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold" , fontfamily.labels = "Helvetica Neue",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)
dev.off()



#Save other

#PNG
png("graphics/Master/sector_treemap_other.png")
treemap(onssectors.count.other, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold", fontfamily.labels = "Helvetica Neue",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)
dev.off()

#EPS
setEPS()
postscript("graphics/Master/sector_treemap_other.eps")
treemap(onssectors.count.other, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold", fontfamily.labels = "Helvetica",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)
dev.off()


#SVG
svg("graphics/Master/sector_treemap_other.svg")
treemap(onssectors.count.other, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 50, fontface.labels = "bold", fontfamily.labels = "Helvetica Neue",
        fontcolor.labels = "white", border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = FALSE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 1.3)
dev.off()


#OTHER PLOTS
#Non-stacked bar - will need to do more on this
ggplot(onssectors, aes(y = Freq, x = label)) + geom_bar(stat = "identity", fill = odi_mBlue) +
  xlab("") + ylab("Percentage of companies") + coord_flip() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())

#ggsave("graphics/Master/ons_sector_bar.png", height = 6, width = 14)

#Pie - don't do it
#Add percentages to labels
lbls <- paste(paste(onssectors$label, onssectors$Freq), "%", sep= "")
pie(onssectors$Freq, labels = lbls, col = odi_pallette)

#ggsave("graphics/Master/ons_sector_pie.png", height = 6, width = 14)

#-------------------
#UNUSED

#BASED ON SIC SECTIONS

#All comparisons made
sic.codes <- ifelse(is.na(list[,sic.1]), NA, 
                    ifelse(is.na(list[,sic.2]), list[,sic.1], ifelse(list[,sic.1] == list[,sic.2],
                                                                     #Add in branch for this occassion - where 1 = 2 - instead of just paste(list[,sic.1])
                                                                     ifelse(is.na(list[,sic.3]), paste(list[,sic.1]), ifelse(list[,sic.1] == list[,sic.3], paste(list[,sic.1]), 
                                                                                                                             ifelse(is.na(list[,sic.4]), paste(list[,sic.1], ",", list[,sic.3]), ifelse(list[,sic.1] == list[,sic.4], paste(list[,sic.1], ",", list[,sic.3]),                                                               
                                                                                                                                                                                                        ifelse(list[,sic.3] == list[,sic.4], paste(list[,sic.1], ",", list[,sic.3]), paste(list[,sic.1], ",", list[,sic.3], ",", list[,sic.4])))))), 
                                                                     ifelse(is.na(list[,sic.3]), paste(list[,sic.1], ",", list[,sic.2]), ifelse(list[,sic.2] == list[,sic.3], 
                                                                                                                                                #Add in branch for this occassion - where 2 = 3 - instead of just paste(list[,sic.1], list[,sic.2]),
                                                                                                                                                ifelse(is.na(list[,sic.4]), paste(list[,sic.1], ",", list[,sic.2]), ifelse(list[,sic.2] == list[,sic.4], paste(list[,sic.1], ",", list[,sic.2]), 
                                                                                                                                                                                                                           ifelse(list[,sic.1] == list[,sic.4], paste(list[,sic.1], ",", list[,sic.2]), paste(list[,sic.1], ",", list[,sic.2], ",", list[,sic.4])))),
                                                                                                                                                ifelse(is.na(list[,sic.4]), paste(list[,sic.1], ",", list[,sic.2], ",", list[,sic.3]), ifelse(list[,sic.3] == list[,sic.4], paste(list[,sic.1], ",", list[,sic.2], ",", list[,sic.3]),
                                                                                                                                                                                                                                              paste(list[,sic.1], ",", list[,sic.2], ",", list[,sic.3], ",", list[,sic.4]))))))))                                          

#Split the string up into all observations and unlist it - so companies can have multiple in same vector
sic.codes.all <- unlist(strsplit(na.omit(sic.codes), " , "))

#Get counts of each observation - i.e. which sectors
table(sic.codes.all)
# Percentages
round(table(sic.codes.all) / length(na.omit(sic.codes.all)) * 100, digits = 0)



#---------------------------
# USING SUMMARY DATA

#Make a reference dataframe of labels
sector.names <- na.omit(data.frame(sic.lookup[, "Section"], sic.lookup[, "Label"]))
colnames(sector.names) <- c('Section', 'Label')

#Set percentages as a data frame
sectors <- as.data.frame(round(table(sic.codes.all) / length(na.omit(sic.codes.all)) * 100, digits=0))

#Lookup the values so that if the 'section' matches the 'sic.code.all' attach the definition
#potentially easier to use match here
sectors$label <- na.omit(ifelse(sector.names$Section %in% sectors$sic.codes.all, sector.names$Label, NA))

#remake without section letter
sectors <- data.frame(sectors$Freq, sectors$label)
colnames(sectors) <- c('Freq', 'label')
#As character
sectors$label <- as.character(sectors$label)

#Order
sectors <- sectors[order(-sectors$Freq),]
row.names(sectors) = NULL
sectors$label <- reorder(sectors$label, sectors$Freq) #Order to plot


#Print percentage - sectors
#print count
sectors.count <- as.data.frame(table(sic.codes.all))

#Lookup the values so that if the 'section' matches the 'sic.code.all' attach the definition
sectors.count$label <- na.omit(ifelse(sector.names$Section %in% sectors.count$sic.codes.all, sector.names$Label, NA))
#remake without section letter
sectors.count <- data.frame(sectors.count$Freq, sectors.count$label)
colnames(sectors.count) <- c('Freq', 'label')
#As character
sectors.count$label <- as.character(sectors.count$label)
#Order
sectors.count <- sectors.count[order(-sectors.count$Freq),]
row.names(sectors.count) = NULL



#plot

#treemap
#Hack for colours - unlist odi_pallette and times to make same number?
#sectors$colours <- c(odi_pallette, "black")

#treemap(sectors, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
#        algorithm = "pivotSize",fontsize.labels = 15, fontface.labels = "bold", fontfamily.labels = "Helvetica Neue",
#        border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = TRUE, 
#        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 0.7)

#Use count for treemap so that all are represented

#Hack for colours - unlist odi_pallette and times to make same number?
sectors.count$colours <- c(odi_pallette, "black")

treemap(sectors.count, index = "label", vSize = "Freq", vColor = "colours", type = "color", title = "",
        algorithm = "pivotSize",fontsize.labels = 15, fontface.labels = "bold", fontfamily.labels = "Helvetica Neue",
        border.col = "white", border.lwds = 1, lowerbound.cex.labels = 0, inflate.labels = TRUE, 
        align.labels = c("center", "center"), overlap.labels = 0.5, aspRatio = 0.7)

# Save to "graphics/Master/sic_sector_treemap.png"
#PNG

#EPS

#SVG



#Non-stacked bar - will need to do more on this
ggplot(sectors, aes(y = Freq, x = label)) + geom_bar(stat = "identity", fill = odi_mBlue) +
  xlab("") + ylab("Percentage of companies") + coord_flip() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())

#ggsave("graphics/master/sector_bar.png", height = 6, width = 14)

#Pie - don't do it
#Add percentages to labels
lbls <- paste(paste(sectors$label, sectors$Freq), "%", sep= "")
pie(sectors$Freq, labels = lbls, col = odi_pallette)

#ggsave("graphics/master/sector_pie.png", height = 6, width = 14)



#Getting to sic.code.all

#Guide - select only where the value exists - i.e. test not na, paste previous if is, else continue
#ifelse(is.na(list[,sic.1]), "", 
#       ifelse(is.na(list[,sic.2]), list[,sic.1], 
#              ifelse(is.na(list[,sic.3]), paste(list[,sic.1], list[,sic.2]), 
#                     ifelse(is.na(list[,sic.4]), paste(list[,sic.1], list[,sic.2], list[,sic.3]), 
#                            paste(list[,sic.1], list[,sic.2], list[,sic.3], list[,sic.4])))))

#Add in tests for duplicates - this does however stop when a duplicate is found in the next SIC code i.e. sic.1 = sic.2 - it will paste sic.1 BUT if sic.3 != sic.1 lose sic.3
#ifelse(is.na(list[,sic.1]), "", 
#  ifelse(is.na(list[,sic.2]), list[,sic.1], ifelse(list[,sic.1] == list[,sic.2], paste(list[,sic.1]), 
#    ifelse(is.na(list[,sic.3]), paste(list[,sic.1], list[,sic.2]), ifelse(list[,sic.2] == list[,sic.3], paste(list[,sic.1], list[,sic.2]), 
#          ifelse(is.na(list[,sic.4]), paste(list[,sic.1], list[,sic.2],list[,sic.3]), ifelse(list[,sic.3] == list[,sic.4], paste(list[,sic.1], list[,sic.2],list[,sic.3]),                                                               
#                    paste(list[,sic.1], list[,sic.2],list[,sic.3], list[,sic.4]))))))))                                          

#Fix this - add in 2 extra branches to catch the places where we missed the analysis before


#Using full data

#Alternative whole string replace
#sectors.alt <- data.frame(sic.codes.all, c(rep(1,length(sic.codes.all))))
#colnames(sectors.alt) <- c('section', 'x')

#Make a reference dataframe of labels
#sector.names <- na.omit(data.frame(sic.lookup[, "Section"], sic.lookup[, "Label"]))
#colnames(sector.names) <- c('Section', 'Label')

#replace with grepl


#order of section - set levels - manual hack atm

#levels(sectors.alt$section)
#sectors.alt$section <- factor(sectors.alt$section, levels = rev(c("J", "M", "N", "S", "P", "R", "K", "G", "O", "Q", "F", "U", "C")))
#levels(sectors.alt$section)

#ggplot(sectors.alt, aes(x = x)) + geom_bar(aes(weight=100, fill = section), position = 'fill', binwidth = 50)

#need to remove a minus if want to 
#sectors$label <- factor(sectors$label, levels = sectors$label)

#Stacked bar - fail
ggplot(sectors, aes(x = 1, y = Freq, fill = label)) + geom_bar(stat = "identity")

barplot(as.matrix(sectors$Freq),beside = FALSE, col = odi_pallette) 

#----------------------------------------------------
#Incorporated date

#Is incorporated date treated as a date?
is.Date(list[, col.date])
#Treat incorporated date as a date?
list[, col.date] <- as.Date(list[, col.date], format("%d/%m/%Y"))
#Worked?
is.Date(list[, col.date])
# Load today's date (might not want to use this but choose a significant day)
tdate <- today()

#Calculate the interval of time - that is start date (incorporated date) until now (today atm) USING POSIXct format
age_int <- new_interval(as.POSIXct(list[, col.date]), as.POSIXct(tdate))
#Calculate the duration in seconds (this is more accurate/workable than difftime)
age_sec <- as.duration(age_int)

#Printing age solutions - make character variable of years and months
# Create standard durations for a year and a month
one.year <- duration(1, units = "years")
one.month <- duration(1, units = "months")
# Calculate the difference in years as a whole number - i.e. rounded
years.inc <- floor(new_interval(list[, col.date], tdate) / one.year )
# Calculate left over rounded number of months (first full number of months then remainder)
months.inc <- round(new_interval(list[, col.date], tdate) / one.month ) %% 12
# Paste the years and months together with year and date included - for output = good, otherwise quite silly 
list$age.char <- ifelse(is.na(years.inc), NA, paste(years.inc, ifelse(years.inc == 1,"year","years"), months.inc, ifelse(months.inc == 1, "month", "months")))


#Save these ages to add to master database
list.ages <- as.data.frame(cbind(list[,"Unique Identifier"], list[,"Trading/Correspondence Name"], list[,"UK Company Number"],  list$age.char))
write.csv(list.ages, "data/character_ages.csv", , row.names = FALSE)
#Analysis

#Mean age of companies in seconds
duration(mean(na.omit(age_sec)), "seconds")
#Printing mean age in years and months format
# Calculate the difference in years as float and integer
mean.years <- floor(duration(mean(na.omit(age_sec)), "seconds") / one.year )
# Calculate left over rounded number of months
mean.months <- round(duration(mean(na.omit(age_sec)), "seconds")/ one.month) %% 12
# Paste the years and months together with year and date included - for output = good, otherwise quite silly 
mean.age <- paste(mean.years, "years", mean.months, "months")

#Median age of companies in seconds
duration(median(na.omit(age_sec)), "seconds")
#Printing mean age in years and months format
# Calculate the difference in years as float and integer
median.years <- floor(duration(median(na.omit(age_sec)), "seconds") / one.year )
# Calculate left over rounded number of months
median.months <- round(duration(median(na.omit(age_sec)), "seconds")/ one.month) %% 12
# Paste the years and months together with year and date included - for output = good, otherwise quite silly 
median.age <- paste(median.years, "years", median.months, "months")


#Find oldest/youngest
#Calculate the duration in seconds (this is more accurate/workable than difftime)
list$age_sec <- as.duration(age_int)
sort.date <- list[order(age_sec),] 

#Total age
duration(sum(na.omit(age_sec)), "seconds")
#Printing mean age in years and months format
# Calculate the difference in years as float and integer
total.years <- floor(duration(sum(na.omit(age_sec)), "seconds") / one.year )
# Calculate left over rounded number of months
total.months <- round(duration(sum(na.omit(age_sec)), "seconds")/ one.month) %% 12
# Paste the years and months together with year and date included - for output = good, otherwise quite silly 
total.age <- paste(total.years, "years", total.months, "months")

#Put ages into categories
#Have to use days as is numeric - define cutoffs by one.year

#Alternative categories - less than two years, 2 to 4, 4 to 6, 6 to 8, 8 to 10, 10, 10+
#biz$age_cat <-cut(age_sec, c(0,2*one.year,4*one.year,6*one.year,8*one.year,10*one.year, 15*one.year, 100*one.year), right = FALSE,
#labels = c("less than 2 years", "2 to 4 years", "4 to 6 years", "6 to 8 years", "8 to 10 years", "10 to 15 years", "more than 15 years"))

#Using ONS data categories
#10+ should end at value of highest
high <- as.duration(max(list$age_sec, na.rm = TRUE)) + duration(1, units = "seconds") #one second to capture top value
list$age_cat <-cut(age_sec, c(0, 2*one.year,4*one.year,10*one.year, high), right = FALSE,
                  labels = c("less than 2 years", "2 to 3 years", "4 to 9 years", "more than 10 years"))


#What does this now look like? For print
age.count <- t(as.data.frame(table(list$age_cat)))
age.percent <- t(as.data.frame(round(table(list$age_cat) / length(na.omit(list$age_cat)) * 100 , digits=0)))

#Plot

#Need histogram
#use age in years
list$age_year <- list$age_sec /one.year

#Use high again but over year
high.hist <- high/one.year

#Put lines on to show ONS bins - try later
#y <- rep(0, 4)
#x1 <- c(0, 2, 4 , 10)
#x2 <- c(2, 4 , 10, high.hist)
#bin.lines <- data.frame(y, x1, x2)
#geom_linerange(aes(data = bin.lines, y = 0, xmin = x1, xmax= x2)) +

#simple histogram
ggplot(list, aes(x = age_year)) + stat_bin(binwidth = 2, position = "identity", colour = odi_dBlue, fill = odi_mBlue) +
  xlab("Age of companies") + ylab("Number of companies") + coord_cartesian(xlim = c(0, high.hist)) + scale_x_continuous(breaks = seq(0, 110, by = 10)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())

#c(seq(0, 40, by = 5), ) 

#Save this histogram
#set image = 
#PNG
ggsave(file = "graphics/Master/age-histogram-simple.png", plot=image, height = 6, width = 12)
#EPS
ggsave(file = "graphics/Master/age-histogram-simple.eps", plot=image, height = 6, width = 12)
#SVG
ggsave(file = "graphics/Master/age-histogram-simple.svg", plot=image, height = 6, width = 12)


 

#Density histogram
ggplot(list, aes(x = age_year)) + stat_bin(aes(y = ..density..), breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, high.hist),
                                           position = "identity", colour = odi_dBlue, fill = odi_mBlue) +
            xlab("Age of companies") + ylab("Density") + coord_cartesian(xlim = c(0, 60)) + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, high.hist)) +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())


#Simpler version
#category - breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, high.hist)
#scale - breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, high.hist)

#more complicated
#category - breaks = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16, 18, 20, 24, 28, 32, 40, 48, high.hist)
#scale - breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, high.hist)

#Save this histogram
#set image = 
#PNG
ggsave(file = "graphics/Master/age-histogram-density.png", plot=image, height = 6, width = 12)
#EPS
ggsave(file = "graphics/Master/age-histogram-density.eps", plot=image, height = 6, width = 12)
#SVG
ggsave(file = "graphics/Master/age-histogram-density.svg", plot=image, height = 6, width = 12)



#bin percentages as dataframe
age <- as.data.frame(round(table(list$age_cat) / length(na.omit(list$age_cat)) * 100, digits = 0))

#barchart
ggplot(age, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue)  +
  geom_text(aes(label = paste0(Freq, "%") , y = Freq+1), stat = "identity", color = "black", size = 4) + 
  xlab("") + ylab("Percentage of companies") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())

ggsave("graphics/Master/age-percentages.png", height = 6, width = 12)

#coord_flip() +
#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +


#Timeline - Cumulative number of companies founded

#By everyday
#What about doing it every day!
#Get everyday into a dataframe
day.num <- c(1:as.numeric(as.duration(new_interval(as.POSIXct(ymd(19900101)), as.POSIXct(tdate)))/ddays(1)))
exist <- data.frame(day.num)
exist$day <- as.Date(ymd(19900101) + days(exist$day.num))

#ensure still have interval for age
age_int <- new_interval(as.POSIXct(list[, col.date]), as.POSIXct(tdate))
#Now to get the number of companies that existed on that day
for(i in 1:as.numeric(as.duration(new_interval(as.POSIXct(ymd(19900101)), as.POSIXct(tdate)))/ddays(1)))
{exist$exist.then[i] <- sum(exist$day[i] %within% age_int + 0, na.rm = TRUE)}

#plot

ggplot(exist, aes(y = exist.then, x = day)) + geom_area(aes(y = exist.then), stat = "identity", fill = odi_mBlue) +
          xlab("Year") + ylab("Number of Companies") + 
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(), 
                axis.title.x = element_text(size = 20, vjust= -0.3), axis.title.y = element_text(size = 20, vjust= 1),
                axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18))

ggsave("graphics/Master/incorporation_timeline.png", height = 6, width = 12)





#---------------------------------------------------

#Saving EPS and SVG

#EPS
ggsave(file = "graphics/test.eps", plot=image, width=10, height=8)

#SVG
image= plot

  ggsave(file="graphics/test.svg", plot=image, width=10, height=8)




#----------------------------------------------------
#Sectors based on manual coded "Primary Field/Industry"

# Sectors all organisations
#table(list[, col.indu])
#table(list[, col.indu]) / length(na.omit(list[, col.indu]))

# Export count of sectors
#write.csv(table(list[, col.indu]), "data/master-sectors.csv", row.names = FALSE)


#----------------------------------------------------
#IGNORE AS NOW HAVE MORE SCIENTIFIC METHOD - BINS
#Is the address in London?
#list$London <- ifelse(is.na(list[,col.addr]), NA, grepl("London", list[, col.addr], TRUE, FALSE, ignore.case = TRUE))

# Remove missings
#list[is.na(list$London), list$London] <- ""

#Number in London = TRUE, other= FALSE
#table(list$London)
#table(list$London) / length(na.omit(list$London)) * 100

#Look at other ecosystems using same method

#----------------------------------------------------
#To save to SVG

#save the plot in a variable image to be able to export to svg
#image= "plot"
#This actually save the plot in a image
#ggsave(file="test.svg", plot=image, width=10, height=8)

#svg("graphics/test.svg")
#ggplot(sectors, aes(y = Freq, x = label)) + geom_bar(stat = "identity", fill = odi_mBlue) +
#  xlab("") + ylab("Percentage of companies") + coord_flip() +
#  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())
#dev.off()

#setEPS()
#postscript("graphics/test.eps", horizontal = TRUE)
#dev.off()
