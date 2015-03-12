setwd("~/git/open-business")
library(car)
library(RGoogleDocs)

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

#--------------------------------------------------
# CLEAN DATA
#--------------------------------------------------

# Takes list as only observations (companies) which passed inclusion criteria
# change as and when we change criteria 
list <- list.all[list.all[, "Confident in inclusion on public OBUK"] != "No", ]

#Sets up columns of master - so can use headers
column.list <- function(var, x = list){
  var_index <- grep(var, names(x))
  return(var_index)
}


#Naming columns - structuring the data

#Displays head of specific column
head(list[, column.list("Reason for inculsion\\/exclusion on public OBUK \\(without survey\\)")])
head(list[, column.list("Primary UK Address")])
head(list[, column.list("Primary Field\\/Industry")])

# name columns
col.incl <- column.list("Reason for inculsion\\/exclusion on public OBUK \\(without survey\\)")
col.addr <- column.list("Primary UK Address")
col.indu <- column.list("Primary Field\\/Industry")


#---------------------------------------------------
# Analysis
#---------------------------------------------------

# Numbers

#shows observations in each category for specific column
table(list[, col.incl])
table(list[, col.incl]) / length(na.omit(list[, col.incl])) * 100


#---------------------------------------------------

# Location 
# Based on "Primary UK Address"

#Is the address in London?
list$London <- ifelse(is.na(list[,col.addr]), NA, grepl("London", list[, col.addr], TRUE, FALSE, ignore.case = TRUE))

# Remove missings
list[is.na(list$London), list$London] <- ""

#Number in London = TRUE, other= FALSE
table(list$London)
table(list$London) / length(na.omit(list$London)) * 100


#Location Plot

#Plot addresses from Master - 
#write.csv(table(list[, "Postcode"]), "master-postcodes.csv", , row.names = FALSE)
write.csv(list[, "Postcode"], "master-postcodes.csv", , row.names = FALSE)

#RUN "Postcode geo plot.R" to plot maps

#---------------------------------------------------

#Sectors
#Based on SIC codes and/or "Primary Field/Industry"

# Sectors all organisations
table(list[, col.indu])
table(list[, col.indu]) / length(na.omit(list[, col.indu]))

# Export count of sectors
write.csv(table(list[, col.indu]), "data/master-sectors.csv", row.names = FALSE)


#----------------------------------------------------
#----------------------------------------------------
#To save to SVG

#save the plot in a variable image to be able to export to svg
#image= "plot"
#This actually save the plot in a image
#ggsave(file="test.svg", plot=image, width=10, height=8)


