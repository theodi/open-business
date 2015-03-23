setwd("~/git/open-business")
library(car)
library(RGoogleDocs)
library(ggplot2)
source('~/git/ODI colour Scheme.R')
theme_set(theme_minimal(base_family = "Helvetica Neue", base_size = 18))


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
head(list[, column.list("Incorporated Date")])

# name columns
col.incl <- column.list("Reason for inculsion\\/exclusion on public OBUK \\(without survey\\)")
col.addr <- column.list("Primary UK Address")
col.indu <- column.list("Primary Field\\/Industry")
col.date <- column.list("Incorporated Date")

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


#Put ages into categories
#Have to use days as is numeric - define cutoffs by one.year

#Alternative categories - less than two years, 2 to 4, 4 to 6, 6 to 8, 8 to 10, 10, 10+
#biz$age_cat <-cut(age_sec, c(0,2*one.year,4*one.year,6*one.year,8*one.year,10*one.year, 15*one.year, 100*one.year), right = FALSE,
#labels = c("less than 2 years", "2 to 4 years", "4 to 6 years", "6 to 8 years", "8 to 10 years", "10 to 15 years", "more than 15 years"))

#Using ONS data categories
list$age_cat <-cut(age_sec, c(0,2*one.year,4*one.year,10*one.year,100*one.year), right = FALSE,
                  labels = c("less than 2 years", "2 to 3 years", "4 to 9 years", "more than 10 years"))


#What does this now look like?
table(list$age_cat)
table(list$age_cat) / length(na.omit(list$age_cat)) * 100 


#Plot
#as dataframe
age <- as.data.frame(round(table(list$age_cat) / length(na.omit(list$age_cat)) * 100, digits = 2))

ggplot(age, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue)  +
  geom_text(aes(label = Freq, y = - 2.5), stat = "identity", color = "black", size = 4) + 
  xlab("") + ylab("Percentage of companies") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())

#coord_flip() +
#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

ggsave("graphics/survey/age-percentages.png", height = 6, width = 12)


#Timeline - Cumulative number of companies founded

#By everyday
#What about doing it every day!
#Get everyday into a dataframe
day.num <- c(1:as.numeric(as.duration(new_interval(as.POSIXct(ymd(20000101)), as.POSIXct(tdate)))/ddays(1)))
exist <- data.frame(day.num)
exist$day <- as.Date(ymd(20000101) + days(exist$day.num))

#ensure still have interval for age
age_int <- new_interval(as.POSIXct(biz[,colIncD]), as.POSIXct(tdate))
#Now to get the number of companies that existed on that day
for(i in 1:as.numeric(as.duration(new_interval(as.POSIXct(ymd(20000101)), as.POSIXct(tdate)))/ddays(1)))
{exist$exist.then[i] <- sum(exist$day[i] %within% age_int + 0, na.rm = TRUE)}

#plot

ggplot(exist, aes(y = exist.then, x = day)) + geom_line(stat = "identity", colour = odi_mBlue, size = 1)

ggsave("graphics/master/incorporation_timeline_byday.png", height = 6, width = 14)






#----------------------------------------------------
#----------------------------------------------------
#To save to SVG

#save the plot in a variable image to be able to export to svg
#image= "plot"
#This actually save the plot in a image
#ggsave(file="test.svg", plot=image, width=10, height=8)


