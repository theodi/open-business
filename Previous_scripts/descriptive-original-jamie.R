# Path to working folder
setwd("~/git/open-business")
library(car)
library(RGoogleDocs)
library(gmodels)

options(stringsAsFactors = FALSE)
   
#----------------------------------------------------
#Loading specfic sheets

# Survey responses
biz.sheet <- getWorksheets(spreadsheets[["5. Responses Database "]], sheets.con)
biz <- sheetAsMatrix(biz.sheet[["Form responses 1"]], header = TRUE)

# Master Database
list.sheet <- getWorksheets(spreadsheets[["1. Master Database"]], sheets.con)
list.all <- sheetAsMatrix(list.sheet[["Data"]], header = TRUE)

# If also want to take from business models sheet
#list.models <- sheetAsMatrix(list.sheet[["Business models"]], header = TRUE)

#---------------------------------------------------
# CLEAN DATA

#Clean Master

# Takes list as only observations (companies) which passed inclusion criteria 
list <- list.all[list.all[, "Confident in inclusion on public OBUK"] != "No", ]


#Sets up columns of master - so can use headers
column.list <- function(var, x = list){
               var_index <- grep(var, names(x))
               return(var_index)
               }


#Clean Responses

#Sets up columns of responses
column <- function(var, x = biz){
                var_index <- grep(var, names(x))
                return(var_index)
                }

#Turn size of company into factor/categorical data
clean.biz <- function(x = biz){
                size <- column("2. What is the size of your company?")
                x[, size] <- as.factor(x[, size])
                return(x)
                }

#creates named rows
drop.vars <- function(x = biz, list){
                dropvars <- names(x) %in% list 
                x <- x[!dropvars]
                }

biz <- clean.biz() #clean.biz() is returned to biz
biz <- biz[biz[, column("Timestamp logged in Staging Database?")] != "No", ] #chooses only surveys recorded as valid
biz <- drop.vars(list = c("Timestamp", "Timestamp logged in Staging Database?", "Name", "Job title", "Email")) #removes columns cited



#create information from master for those who have answered survey 

#Take a list of only the responses 
list.biz <- list[!is.na(list[, "Survey Response? (batch)"]), ]
list.biz <- list.biz[list.biz[,"Survey Response? (batch)"] != "No",]

#Sets up columns of list.biz - so can use headers
column.list.biz <- function(var, x = list.biz){
  var_index <- grep(var, names(x))
  return(var_index)
}
# Could merge biz and list.biz but - Very complicated to merge them as text fields don't line up could use this - but can't see how
#biz.full <- merge(biz, list.biz, by "")





#---------------------------------------------------
#---------------------------------------------------
# Survey Response - Analysis - biz
#---------------------------------------------------
#---------------------------------------------------

#Naming columns - structuring the data

#shows the top rows of column - to check it is correct (Don't run every time)
head(biz[, column("2. What is the size of your company?")])
head(biz[, column("3. Which category best describes your company's area of business?")])
head(biz[, column("4. Which of the following are significant sources of revenue for your company?")])
head(biz[, column("5. How does your company currently use open data?")])
head(biz[, column("6. What types of open data does your company use?")])
head(biz[, column("7. Does your company currently use open government datasets?")])
head(biz[, column("9. Does your company currently use other open datasets, such as those provided by businesses, charities, or community projects??")])
head(biz[, column("12. Which pricing mechanism\\(s\\) does your company use for its open data products and\\/or services?")])

head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Provenance of data\\]")])
head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Licensing of datasets\\]")])
head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accuracy of data\\]")])
head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Timeliness of data\\]")])
head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Ease of access to datasets\\]")])
head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Format of data\\]")])
head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accompanying documentation\\]")])
head(biz[, column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Help and support from publisher\\]")])


# name columns
colQ2 <- column("2. What is the size of your company?")
colQ3 <- column("3. Which category best describes your company's area of business?")
colQ4 <- column("4. Which of the following are significant sources of revenue for your company?")
colQ5 <- column("5. How does your company currently use open data?")
colQ6 <- column("6. What types of open data does your company use?")
colQ7 <- column("7. Does your company currently use open government datasets?")
colQ9 <- column("9. Does your company currently use other open datasets, such as those provided by businesses, charities, or community projects??")
colQ12 <- column("12. Which pricing mechanism\\(s\\) does your company use for its open data products and\\/or services?")

colQ14_prov <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Provenance of data\\]")
colQ14_lice <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Licensing of datasets\\]")
colQ14_accu <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accuracy of data\\]")
colQ14_time <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Timeliness of data\\]")
colQ14_ease <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Ease of access to datasets\\]")
colQ14_form <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Format of data\\]")
colQ14_docu <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accompanying documentation\\]")
colQ14_help <- column("14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Help and support from publisher\\]")


#----------------------------------------------------------
# Basic Statistics - Specific Column stats
#----------------------------------------------------------

#Q2 Number of employees 

#check what the distribution looks like in terms of employees 
table(biz[, colQ2])
#shows percentage in each category for column
table(biz[, colQ2]) / length(na.omit(biz[, colQ2])) * 100 


#---------------------------------------------------
#Q3 Area of business
#check what the distribution looks like in terms of employees
table(biz[, colQ3])
table(biz[, colQ3]) / length(na.omit(biz[, colQ3])) * 100 


#---------------------------------------------------
#Q4 Sources of revenue

# Resolve multiple responses - there are loads in this question - ignore for now


#---------------------------------------------------
# Q5 Use of open data

# Resolve multiple response 
answers_Q5 <- c("My company provides infrastructure for others to publish open data \\(e.g. platforms, portals, data stores\\)", 
                "My company processes open data \\(e.g aggregation, classification, anonymization, cleaning, refining, enriching\\)", 
                "My company develops products based on open data \\(e.g. APIs, apps\\)",
                "My company publishes open data",
                "My company provides insights based on open data \\(e.g. analytics, visualisations\\)")

# Remove missings (zero at the moment)
biz[is.na(biz[, colQ5]), colQ5] <- ""

#Create dummies
biz$Q5_infrastructure <- grepl(answers_Q5[1], biz[, colQ5])
biz$Q5_processes <- grepl(answers_Q5[2], biz[, colQ5])
biz$Q5_develops <- grepl(answers_Q5[3], biz[, colQ5])
biz$Q5_publish <- grepl(answers_Q5[4], biz[, colQ5])
biz$Q5_insights <- grepl(answers_Q5[5], biz[, colQ5])

Q5_dummies <- c('Q5_infrastructure', 'Q5_processes', 'Q5_develops', 'Q5_publish', 'Q5_insights')

#Analysing Q5 answers 
sapply(biz[, Q5_dummies], table)
sapply(biz[, Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100)

# How many companies checked more than one box?
table(rowSums(biz[, Q5_dummies]))
sum(table(rowSums(biz[, Q5_dummies]))[3:6])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q5_dummies]))[3:6]) / sum(table(rowSums(biz[, Q5_dummies]))) * 100

# Only checked on box - which box?
sapply(biz[rowSums(biz[, Q5_dummies]) == 1, Q5_dummies], table)
sapply(biz[rowSums(biz[, Q5_dummies]) == 1, Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100)


# Crossproduct - which appear together
crossprod(as.matrix(biz[, Q5_dummies]))
# % of total 
crossprod(as.matrix(biz[, Q5_dummies])) / nrow(biz[, Q5_dummies]) * 100

# working out how often two variables occur together by % in column 
True_Q5 <-rep(diag(crossprod(as.matrix(biz[, Q5_dummies]))), 5)
True.m_Q5 <- matrix(True_Q5, 5, byrow = T)
crossprod(as.matrix(biz[, Q5_dummies])) / True.m_Q5 * 100

#Does above but for individual variables
sapply(biz[biz[, "Q5_processes"], Q5_dummies], table)
sapply(biz[biz[, "Q5_processes"], Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100)



#Extra stuff trying to determine co-occurance
cor(biz[, Q5_dummies]) # DANGER very misleading
hetcor(crossprod(as.matrix(biz[, Q5_dummies])), ML = TRUE, std.err = FALSE)
polychor(biz$Q5_processes, biz$Q5_insights, ML = TRUE)
# Also look at phi coefficient
mapply(function(x, y) polychor(x, y, ML = TRUE), biz[, Q5_dummies], biz[, Q5_dummies])

#--------------------------------------------------
#Question 6 - types of data



#---------------------------------------------------
#Question 7 - open gov data?

#check what the distribution looks like in terms of employees 
table(biz[, colQ7])
#shows percentage in each category for column
table(biz[, colQ7]) / length(na.omit(biz[, colQ7])) * 100 

#--------------------------------------------------
#Question 9 - Open business data?

# Can but shouldn't - Replace missings with no - for the moment - NOT REALLY ALLOWED
biz[is.na(biz[, colQ9]), colQ9] <- "No"

#check what the distribution looks like in terms of employees 
table(biz[, colQ9])
#shows percentage in each category for column
table(biz[, colQ9]) / length(na.omit(biz[, colQ9])) * 100 


#--------------------------------------------------
#Questions 7 and 9

#set up as binary
biz$Q7 <- ifelse(biz[, colQ7] == "No", FALSE, TRUE)
biz$Q9 <- ifelse(biz[, colQ9] == "No", FALSE, TRUE)
crossQ7.Q9 <- c( 'Q7', 'Q9')

#Carry out crosstable 
CrossTable(biz$Q7, biz$Q9, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE, format=c("SPSS"))


#data as above - independently how many true/false for each
sapply(biz[, crossQ7.Q9], table)
sapply(biz[, crossQ7.Q9], function(x) table(x) / length(na.omit(x)) * 100)

# How many boxes did each company check?
table(rowSums(biz[, crossQ7.Q9]))
sum(table(rowSums(biz[, crossQ7.Q9])))

# Percentage who answered question!
sum(table(rowSums(biz[, crossQ7.Q9]))[2:3]) / sum(table(rowSums(biz[, crossQ7.Q9]))) * 100

# Only checked on box - which box?
sapply(biz[rowSums(biz[, crossQ7.Q9]) == 1, crossQ7.Q9], table)
sapply(biz[rowSums(biz[, crossQ7.Q9]) == 1, crossQ7.Q9], function(x) table(x) / length(na.omit(x)) * 100)



crossprod(as.matrix(biz[, crossQ7.Q9]))
crossprod(as.matrix(biz[, Q12_dummies])) / nrow(biz[, Q12_dummies]) * 100


#---------------------------------------------------
# Question 12 - pricing mechanisms

# Resolve multiple responses - 
    #create list answer_Q12  
    answers_Q12 <- c("Provide unlimited free access to everyone", 
                      "Provide limited free access to everyone \\(e\\.g\\. rate or volume limited\\)", 
                      "Provide free access to only a subset of people", 
                      "Provide only paid-for access")
       
# Remove missings
biz[is.na(biz[, colQ12]), colQ12] <- ""

#Setting up dummy variables - i.e. where Q12 is a specific answer dummy variable = TRUE, otherwise = FALSE
    biz$Q12_unlimited <- grepl(answers_Q12[1], biz[, colQ12])
    biz$Q12_limited <- grepl(answers_Q12[2], biz[, colQ12])
    biz$Q12_subset <- grepl(answers_Q12[3], biz[, colQ12])
    biz$Q12_paid <- grepl(answers_Q12[4], biz[, colQ12])

# Only OTHER text for Q12
     #Extracts other answers out
      biz$Q12_other_text <- gsub(answers_Q12[1], "", biz[, colQ12])
      for (i in 2:4) biz$Q12_other_text <- gsub(answers_Q12[i], "", biz[, "Q12_other_text"])
      biz$Q12_other_text <- gsub("^(, ){1,4}", "", biz$Q12_other_text)

#Creates dummy for other text
      biz$Q12_other <- ifelse(biz$Q12_other_text == "", 0, 1)


# Analysing the dummy variables for Q12 - including 'other' 
Q12_dummies <- c('Q12_unlimited', 'Q12_limited', 'Q12_subset', 'Q12_paid', 'Q12_other')
sapply(biz[, Q12_dummies], table)
sapply(biz[, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)

# Analysing the dummy variables for Q12 - without including 'other' as this doesnt really make sense here
Q12_dummies.x <- c('Q12_unlimited', 'Q12_limited', 'Q12_subset', 'Q12_paid')
sapply(biz[, Q12_dummies.x], table)
sapply(biz[, Q12_dummies.x], function(x) table(x) / length(na.omit(x)) * 100)


# How many companies checked more than one box?
table(rowSums(biz[, Q12_dummies]))
sum(table(rowSums(biz[, Q12_dummies]))[3:5])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q12_dummies]))[3:5]) / sum(table(rowSums(biz[, Q12_dummies]))) * 100

# How many companies checked more than one box? - excluding other
table(rowSums(biz[, Q12_dummies.x]))
sum(table(rowSums(biz[, Q12_dummies.x]))[3:5])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q12_dummies.x]))[3:5]) / sum(table(rowSums(biz[, Q12_dummies.x]))) * 100

# Only checked on box - which box?
sapply(biz[rowSums(biz[, Q12_dummies]) == 1, Q12_dummies], table)
sapply(biz[rowSums(biz[, Q12_dummies]) == 1, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)



# Crossproduct
crossprod(as.matrix(biz[, Q12_dummies]))
crossprod(as.matrix(biz[, Q12_dummies])) / nrow(biz[, Q12_dummies]) * 100

# working out how often two variables occur together by % in column - i.e. [2,1] 7 answered both limited and unlimited - or 19% of those who replied unlimited ([2,1] gives 50% of those who answered limited also answered unlimited)  
True_Q12 <-rep(diag(crossprod(as.matrix(biz[, Q12_dummies]))), 5)
True.m_Q12 <- matrix(True_Q12, 5, byrow = T)
crossprod(as.matrix(biz[, Q12_dummies])) / True.m_Q12 * 100



#---------------------------------------------------
#---------------------------------------------------
# Master - Analysis - list
#---------------------------------------------------
#---------------------------------------------------
#Naming columns - structuring the data

#Displays head of specific column
head(list[, column.list("Reason for inculsion\\/exclusion on public OBUK \\(without survey\\)")])
head(list[, column.list("Primary UK Address")])

# name columns
col.incl <- column.list("Reason for inculsion\\/exclusion on public OBUK \\(without survey\\)")
col.add <- column.list("Primary UK Address")
 
#---------------------------------------------------
# Basic column stats


#shows observations in each category for specific column
table(list[, col.incl])
table(list[, col.incl]) / length(na.omit(list[, col.incl])) * 100


#Primary UK Address
# Remove missings
#list[is.na(list$London), list$London] <- ""

#Is the address in London?
list$London <- ifelse(is.na(list[,col.add]), NA, grepl("London", list[, col.add], TRUE, FALSE, ignore.case = TRUE))

#Number in London = TRUE, other= FALSE
table(list$London)
table(list$London) / length(na.omit(list$London)) * 100

#---------------------------------------------------
#---------------------------------------------------
# Data on responses from master - list.biz
#---------------------------------------------------
#---------------------------------------------------

#Check Columns 
#Displays head of specific column
head(list.biz[, column.list.biz("ODI Relationship")])

#Name Columns
col.ODI <- column.list.biz("ODI Relationship")
col.b.add <- column.list.biz("Primary UK Address")



#------------------------------------------------------
#Column stats


# Address in London?
list.biz$London.biz <- ifelse(is.na(list.biz[,col.b.add]), NA, grepl("London", list.biz[,col.b.add], TRUE, FALSE, ignore.case = TRUE))

#Number in London = TRUE, other= FALSE
table(list.biz$London.biz)
table(list.biz$London.biz) / length(na.omit(list.biz$London.biz)) * 100








