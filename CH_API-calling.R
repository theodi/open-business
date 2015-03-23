setwd("~/git/open-business")
library(RGoogleDocs)
library(jsonlite)

# This loads the Google password
source('~/git/SENSITIVE DO NOT COMMIT.R')
# Get the data directly
sheets.con <- getGoogleDocsConnection(getGoogleAuth("jamie.fawcett@theodi.org", gmailpw, service = "wise"))
spreadsheets <- getDocs(sheets.con)
# Remove passwords
rm(list = c('gmailpw'))

#------------------------
#Get the company numbers

#Load master database

list.sheet <- getWorksheets(spreadsheets[["1. Master Database"]], sheets.con)
list.all <- sheetAsMatrix(list.sheet[["Data"]], header = TRUE)

# Takes list as only observations (companies) which have UK company numbers 
list.num.full <- list.all[complete.cases(list.all[, "UK Company Number"]), ]

list.num <- list.num.full[list.num.full[, "UK Company Number"] != "y", ]

#Sets up columns
column.num <- function(var, x = list.num){
  var_index <- grep(var, names(x))
  return(var_index)
}

#head(list.num[, column.num("UK Company Number")])
#finds the column headed UK company number and references it to col.num
col.num<- column.num("UK Company Number")

#this is our list of company numbers
list.num$col.num <- list.num[, col.num]


#------------------------

#JSON TESTING - IGNORE

#URL of JSON call to companies house API
#url <- 'http://data.companieshouse.gov.uk/doc/company/07049166.json'

#USING JSONLITE
#Retrieving the JSON object into a structure R can understand
#document1 <- fromJSON(url, flatten = TRUE)

#Objects within JSON call
#Top level is pretty useless
#names(document1)
#Displays objects of interest
#names(document1$primaryTopic)
#Displays the values of those objects
#document1$primaryTopic$IncorporationDate
#document1$primaryTopic$CompanyName
#document1$primaryTopic$CompanyNumber

#Issue with SIC codes as they are nested in a different way
#document1$primaryTopic$SICCodes
#how to get sic codes out
#document1$primaryTopic$SICCodes$SicText

#produces dataframe of all data - unfortunately gives 4 lines as SICCodes create multiple lines
#data <- as.data.frame(document$primaryTopic)

#headings <- c("Company Name", "Company Number", "Incorporation date")
#company1 <- c(document1$primaryTopic$CompanyName, document1$primaryTopic$CompanyNumber, document1$primaryTopic$IncorporationDate)

#turn it into a matrix
#rbind(company1)

#---------------------------

#Lets try querying

#this is out list of company numbers
#list.num$col.num <- list.num[, col.num]

#This is for a single call
#number1 <- list.num$col.num[1]
#url <- paste0('http://data.companieshouse.gov.uk/doc/company/',number1,'.json')
#document1 <- fromJSON(url)
#company1 <- c(document1$primaryTopic$CompanyName, document1$primaryTopic$CompanyNumber, document1$primaryTopic$IncorporationDate)


#-----------------------------

#As a function

#getdate <- function(num){
 # urlx <- paste0('http://data.companieshouse.gov.uk/doc/company/',num,'.json')
 # document <- fromJSON(urlx)
 # com.name <- document$primaryTopic$CompanyName
 # com.num <- document$primaryTopic$CompanyNumber
 # ifelse(!is.null(document$primaryTopic$IncorporationDate), date <- document$primaryTopic$IncorporationDate, date <- NA)
 # record <- c(com.num, com.name, date)
#return(record)
#}

#raw.data <- readLines(urlx, warn="F")

#----------------------------
#Testing the function
#works for individual call
#answer1 <- getdate(number1)

#add extra numbers
#number2 <- list.num$col.num[2]
#number3 <- list.num$col.num[3]

#Works for them
#answer2 <- getdate(number2)
#answer3 <- getdate(number3)

#practice list
#numbers <- c(number1, number2, number3) 

#carry out on practice list
#answers <- t(sapply(as.matrix(numbers), getdate, USE.NAMES = FALSE))
#naming the columns
#colnames(answers) <- c("Company Number", "Company Name", "Incorporation date")

#-----------------------------
#FULL GO

#describe at whole list
#company.numbers <- as.vector(list.num$col.num)
#len <- length(company.numbers)

#split list up into managable chunks
#company.numbers1 <- company.numbers[1:25]
#company.numbers.sp <- split(company.numbers, ceiling(seq_along(company.numbers)/25))

#Carrying out the function on the full list
#company.date1 <- sapply(company.numbers1, getdate, USE.NAMES = FALSE)

#Turns it into a dataframe
#comp_det <- as.data.frame(t(company.date1))
#Name columns
#colnames(data_as_df) <- c("CompanyNumber", "CompanyName", "IncorporationDate")
#make the date column into date format
#data_as_df$IncorporationDate <- as.Date(data_as_df$IncorporationDate, format("%d/%m/%Y"))


#-------------
# THIS is the final method
#Includes a progress bar and error check

#whole list
company.numbers <- as.vector(list.num$col.num)

#Column names get lost in loop
final <- data.frame(CompanyNumber = character(), CompanyName = character(), CompanyCategory = character(), CompanyStatus = character(),
                    IncorporationDate = character(), RegPostcode = character(), SICCodes = character(),
                    SICCode2 = character(), SICCode3 = character(), SICCode4 = character(), row.names = NULL)
#adds a progress bar 
pb <- txtProgressBar(min = 0, max = length(company.numbers), style = 3)

#for loop performed on individual elements of full list, sets url and retrives company name, number and incorp date
for(num in company.numbers){
  tryCatch({
    url <- paste0('http://data.companieshouse.gov.uk/doc/company/',num,'.json')
    document <- fromJSON(url)
    com.num <- document$primaryTopic$CompanyNumber
    com.name <- document$primaryTopic$CompanyName
    ifelse(!is.null(document$primaryTopic$CompanyCategory), com.cat <- document$primaryTopic$CompanyCategory, com.cat <- NA)
    ifelse(!is.null(document$primaryTopic$CompanyStatus), com.stat <- document$primaryTopic$CompanyStatus, com.stat <- NA)
    ifelse(!is.null(document$primaryTopic$IncorporationDate), date <- document$primaryTopic$IncorporationDate, date <- NA)
    ifelse(!is.null(document$primaryTopic$RegAddress$Postcode), com.post <- document$primaryTopic$RegAddress$Postcode, post <- NA)
    ifelse(!is.null(document$primaryTopic$SICCodes$SicText), sic.code <- document$primaryTopic$SICCodes$SicText, sic.code <- NA)
    value <- vector(,length =(4-length(sic.code)))
    sic.codes <- append(as.vector(sic.code), value)
    record <- as.data.frame(t(c(com.num, com.name, com.cat, com.stat, con.org, date, com.post, sic.codes)))
    final <<- rbind(final, record)
  },
  error = function(e) cat("MESSAGE:", conditionMessage(e), "\n"))
  setTxtProgressBar(pb, match(num, company.numbers))
}
close(pb)


#Name columns
colnames(final) <- c("CompanyNumber", "CompanyName", "CompanyCategory", "CompanyStatus", "IncorporationDate", "CompanyAdd", "SICCode1", "SICCode2", "SICCode3", "SICCode4")

#make the date column into date format
final$IncorporationDate <- as.Date(final$IncorporationDate, format("%d/%m/%Y"))

#creates a csv
write.csv(final, "CH_full_info.csv", , row.names = FALSE)
