# Path to working folder
setwd("~/git/open-business")
library(car)
library(RGoogleDocs)
library(gmodels)
library(ggplot2)
library(likert)
library(plyr)
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


#----------------------------------------------------
# Load survey responses

biz.sheet <- getWorksheets(spreadsheets[["5. Responses Database "]], sheets.con)
biz <- sheetAsMatrix(biz.sheet[["Recorded responses"]], header = TRUE)


#---------------------------------------------------
# CLEANING THE DATA
#----------------------------------------------------

#Sets up columns
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
biz <- biz[biz[, column("Exclude")] != "Yes", ] #chooses only surveys recorded as valid
biz <- drop.vars(list = c("Timestamp", "Date completed", "Time completed", "Exclude", "Is Q14 complete?", "How many missing answers in Q14 are there?", "Survey Respondant Name"
                          ,"Survey Respondant Job title", "Survey Respondant Email", "UK Company Number")) #removes columns cited
row.names(biz) <- NULL

#Remove qual questions
biz <- drop.vars(list = c("Q8. If yes, which open government datasets does it currently use?",
                          "Q10. If yes, which other open datasets does it currently use?", 
                          "Q11. Please name and briefly describe your company's products and/or services that utilise open data.",
                          "Q13. If your company has previously used open data in particular products and/or services but ceased to do so, could you please describe why?",                          "Q15. If you wish to elaborate on any of these issues, please do so.")) 


#Structuring the (interesting) data

#shows the top rows of column - to check it is correct (Don't run every time)
#head(biz[, column("Q2. What is the size of your company?")])
#head(biz[, column("Q5. How does your company currently use open data?")])
#head(biz[, column("Q6. What types of open data does your company use?")])
#head(biz[, column("Q7. Does your company currently use open government datasets?")])
#head(biz[, column("Q9. Does your company currently use other open datasets, such as those provided by businesses, charities, or community projects??")])
#head(biz[, column("Q12. Which pricing mechanism\\(s\\) does your company use for its open data products and\\/or services?")])

#head(biz[, column("Q3. Which category best describes your company's area of business?")])
#head(biz[, column("Q4. Which of the following are significant sources of revenue for your company?")])

# name columns (that is - the variable takes the number of the position of that column so it can be used with reference to biz)
colQ2 <- column("Q2. What is the size of your company?")
colQ3 <- column("3. Which category best describes your company's area of business?")
colQ5 <- column("Q5. How does your company currently use open data?")
colQ6 <- column("Q6. What types of open data does your company use?")
colQ7 <- column("Q7. Does your company currently use open government datasets?")
colQ9 <- column("Q9. Does your company currently use other open datasets, such as those provided by businesses, charities, or community projects??")
colQ12 <- column("Q12. Which pricing mechanism\\(s\\) does your company use for its open data products and\\/or services?")


colIncD <- column("Incorporated Date")


#colQ4 <- column("Q4. Which of the following are significant sources of revenue for your company?")

#If using Q14
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Provenance of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Licensing of datasets\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accuracy of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Timeliness of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Ease of access to datasets\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Format of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accompanying documentation\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Help and support from publisher\\]")])

colQ14_prov <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Provenance of data\\]")
colQ14_lice <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Licensing of datasets\\]")
colQ14_accu <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accuracy of data\\]")
colQ14_time <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Timeliness of data\\]")
colQ14_ease <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Ease of access to datasets\\]")
colQ14_form <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Format of data\\]")
colQ14_docu <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accompanying documentation\\]")
colQ14_help <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Help and support from publisher\\]")

#---------------------------------------------------

#FINDING and extracting free text OTHER answers 
#Extracts other answers out, set to blank first
#biz$Q5_other_text <- ""
#biz$Q5_other_text <- gsub(answers_Q5[1], "", biz[, colQ5])
#for (i in 2:5) biz$Q5_other_text <- gsub(answers_Q5[i], "", biz[, "Q5_other_text"])
#biz$Q5_other_text <- gsub("^(, ){1,4}", "", biz$Q5_other_text)
#---------------------------------------------------

#Printing output to RTF - Not worth the effort
#test.rtf <- RTF(file="ODM_stats", width=8.5, height=11, omi=c(1, 1, 1, 1), font.size=10)
#addHeader(test.rtf, "ODM Statistics Output")
#startParagraph(test.rtf)
#addText(test.rtf, "Q2 - Employees")
#Q2 - Employees
#addTable(test.rtf, table(biz[, colQ2]))
#shows percentage in each category for column
#addTable(test.rtf, table(biz[, colQ2]) / length(na.omit(biz[, colQ2])) * 100)
#endParagraph(test.rtf)
#done(test.rtf)
#---------------------------------------------------
#Print output
# output <- as.data.frame(STAT)

#For percentages - round to 2 DP
# output <- as.data.frame(round(STAT, digits = 2))


#---------------------------------------------------
#---------------------------------------------------
# ANALYSIS
#---------------------------------------------------
#---------------------------------------------------

#GENERATE PROFILES OF SURVEY RESPONDENTS

#Q2 Number of employees 

#check what the distribution looks like in terms of employees 
table(biz[, colQ2])
#shows percentage in each category for column
table(biz[, colQ2]) / length(na.omit(biz[, colQ2])) * 100 

#Creates a new data frame - so we can print and plot
employ.count <- as.data.frame(table(biz[, colQ2]))
employ <- as.data.frame(table(biz[, colQ2]) / length(na.omit(biz[, colQ2])) * 100)

#Order them:
#Set Target order
target.Q2 <- c("fewer than 10 employees", "10 - 50 employees", "51 - 250 employees",
               "251 - 1000 employees",  "More than 1000 employees")
# order Var1 by the target
employ$Var1 <- factor(employ$Var1, levels=target.Q2)
employ <- employ[order(employ$Var1, employ$Freq),]


#Plot them
ggplot(employ, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue) +
  ylim(0, 100) + 
  geom_text(aes(label = paste(round(Freq, digits=2), "%"), y = (Freq+3)), stat = "identity", color = "black", size = 5) + 
  xlab("") + ylab("Percentage responses") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())
# In theme to move the x label axis.title.x = element_text(vjust=-0.1)
# geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1)

ggsave("graphics/employees.png", height = 6, width = 14)

#INC analysis of SIC Codes and incorporated date


#------------------------------------------------

#Incorporated date
library(lubridate)

#Is incorporated date treated as a date?
is.Date(biz[, colIncD])
#Treat incorporated date as a date?
biz[,colIncD] <- as.Date(biz[,colIncD], format("%d/%m/%Y"))
#Worked?
is.Date(biz[, colIncD])

#Any missings?

# Load today's date (might not want to use this but choose a significant day)
tdate <- now()

#Calculate the interval of time - that is start date (incorporated date) until now (today atm)
age_int <- new_interval(biz[,colIncD], tdate)

age_sec <- as.duration(age_int)

calc_age <- function(birthDate, refDate = today()) {
  
  require(lubridate)
  
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  
  period$year
  
}

calc_age(biz[,colIncD])



#--------------------------------------------------


# Create standard durations for a year and a month
one.year <- duration(num = 1, units = "years")
one.month <- duration(num = 1, units = "months")

# Calculate the difference in years as float and integer
diff.years <- new_interval(biz[,colIncD], tdate) / one.year
years <- floor( new_interval(biz[,colIncD], tdate) / one.year )

# Calculate the modulo for number of months
diff.months <- round( new_interval(biz[,colIncD], tdate) / one.month )
months <- diff.months %% 12

# Paste the years and months together with year and date included - for output = good, otherwise quite silly 
biz$age_char <- ifelse(is.na(years), NA, paste(years, ifelse(years == 1,"year","years"), months, ifelse(months == 1, "month", "months")))


#Work out how old companies are in years
age_days <- tdate - biz[,colIncD]




#---------------------------------------------------
#IGNORE
#Q3 Area of business
#check what the distribution looks like in terms of sectors - now inconsequential - but not for ODM
table(biz[, colQ3])
table(biz[, colQ3]) / length(na.omit(biz[, colQ3])) * 100 


#Save counts as data frame
sectors.count <- as.data.frame(table(biz[, colQ3]))
#Save percentages as data frame
sectors <- as.data.frame(round(table(biz[, colQ3]) / length(na.omit(biz[, colQ3])) * 100, digits = 2))


#Rename mistakes
sectors$Var1 <- as.character(sectors$Var1)
sectors$Var1[sectors$Var1 == "Environment  & Weather"] <- "Environment & Weather"
sectors$Var1[sectors$Var1 == "Food and agriculture"] <- "Food and Agriculture"

#To print
#To order 
sectors.count <- sectors.count[order(-sectors.count$Freq),]

#To order 
sectors <- sectors[order(-sectors$Freq),]




#THIS IS IRRELEVANT AS I HAVE GOT IT TO WORK!!!

#Attempt 1
#sectors2 <- sectors
#sectors2$Freq <- sectors$Freq <- as.factor(sectors$Freq)
#sectors2$Freq <-factor(sectors$Freq, levels=sectors[order(sectors$Freq), "Var1"])
#sectors2 <- sectors[order(-sectors$Freq),]
#row.names(sectors) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#Doesnt work

#Attempt 3
#levels=sectors[order(sectors1$Freq),]
#Order them:
# order Var1 by the target
#sectors <- sectors[order(sectors$Var1, sectors$Freq),]
#sectors$Var2 <- factor(sectors$Var1, as.factor(sectors$Freq))
#Doesnt work

#Attempt 4
#sectors <- transform(sectors, Var1 = reorder(Var1, order(Freq, decreasing = TRUE))
#Doesnt work




# Plot

# WORKING!!!!!!!
# Attempt 2 - this works if you treat it as character not factor!!!!

sectors$Var1 <- reorder(sectors$Var1, sectors$Freq)

ggplot(sectors, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue)  +
 geom_text(aes(label = Freq, y = - 2.5), stat = "identity", color = "black", size = 4) + 
 xlab("") + ylab("Percentage of companies") + coord_flip() +
 theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())

#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

ggsave("graphics/sector-percentages.png", height = 6, width = 12)



#---------------------------------------------------
#IGNORE
#Q4 Sources of revenue

# Resolve multiple responses - there are loads in this question

#---------------------------------------------------
#HOW DO THEY USE OPEN DATA
# Q5 Use of open data

# Resolve multiple response 
answers_Q5 <- c("My company provides infrastructure for others to publish open data \\(e.g. platforms, portals, data stores\\)", 
                "My company processes open data \\(e.g aggregation, classification, anonymization, cleaning, refining, enriching\\)", 
                "My company develops products based on open data \\(e.g. APIs, apps\\)",
                "My company publishes open data",
                "My company provides insights based on open data \\(e.g. analytics, visualisations\\)")


#Any missings? (zero at the moment)
length(which(is.na(biz[, colQ5])))
# Remove missings (if necessary)
#biz[is.na(biz[, colQ5]), colQ5] <- ""

#Create dummies - returns LOGICAL for each column
biz$Q5_infrastructure <- grepl(answers_Q5[1], biz[, colQ5])
biz$Q5_processes <- grepl(answers_Q5[2], biz[, colQ5])
biz$Q5_develops <- grepl(answers_Q5[3], biz[, colQ5])
biz$Q5_publish <- grepl(answers_Q5[4], biz[, colQ5])
biz$Q5_insights <- grepl(answers_Q5[5], biz[, colQ5])

Q5_dummies <- c('Q5_infrastructure', 'Q5_processes', 'Q5_develops', 'Q5_publish', 'Q5_insights')

#TEXT HAS BEEN EXTRACTED OTHER CATEGORY CAN BE IGNORED
#FINDING and extracting free text OTHER answers 
#Extracts other answers out, set to blank first
#biz$Q5_other_text <- ""
#biz$Q5_other_text <- gsub(answers_Q5[1], "", biz[, colQ5])
#for (i in 2:5) biz$Q5_other_text <- gsub(answers_Q5[i], "", biz[, "Q5_other_text"])
#biz$Q5_other_text <- gsub(",", "", biz$Q5_other_text, perl=T) #remove commas
#biz$Q5_other_text <- gsub("^\\s+|\\s+$", "", biz$Q5_other_text) #remove leading and trailing spaces

#Creates dummy for other text
#biz$Q5_other <- ifelse(biz$Q5_other_text == "", FALSE, TRUE)


#Analyse

# SIMPLE

#Number and percentage of answers in each column
sapply(biz[, Q5_dummies], table) # TRUE/FALSE by column
sapply(biz[, Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100) # percentage using each of these models 

# How many companies chose only one description?
table(rowSums(biz[, Q5_dummies])) #Count of number of descriptions chosen
sum(table(rowSums(biz[, Q5_dummies]))[2:5]) # number who chose more than one description
sum(table(rowSums(biz[, Q5_dummies]))[2:5]) / sum(table(rowSums(biz[, Q5_dummies]))) * 100 # Percentage of who ticked more than one box

# Only checked on box - which box?
sapply(biz[rowSums(biz[, Q5_dummies]) == 1, Q5_dummies], table) #Gives TRUE/FALSE for each column out of all the one box ticked
sapply(biz[rowSums(biz[, Q5_dummies]) == 1, Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100) # percentage of those who ticked that box from all who ticked one box


#COMPLEX

#Relationship between any 2 variables
# Crossproduct - which 2 variables appear together - although this doesnt really capture 'profiles'
crossprod(as.matrix(biz[, Q5_dummies]))
# where two variables co-occur as a % of the total 
crossprod(as.matrix(biz[, Q5_dummies])) / nrow(biz[, Q5_dummies]) * 100

# working out how often a variable occurs (rows) with respect another variable (column) given as a % of the column variable
True_Q5 <-rep(diag(crossprod(as.matrix(biz[, Q5_dummies]))), 5)
True.m_Q5 <- matrix(True_Q5, 5, byrow = T)
crossprod(as.matrix(biz[, Q5_dummies])) / True.m_Q5 * 100

#Extracts the values from the above by variable - i.e. what number/ % of all observations in a variable (column/named) where another variable occurs
sapply(biz[biz[, "Q5_processes"], Q5_dummies], table)
sapply(biz[biz[, "Q5_processes"], Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100)


#Ideally do some multivariate analysis or clustering


#Extra stuff trying to determine co-occurance
cor(biz[, Q5_dummies]) # DANGER very misleading
hetcor(crossprod(as.matrix(biz[, Q5_dummies])), ML = TRUE, std.err = FALSE)
polychor(biz$Q5_processes, biz$Q5_insights, ML = TRUE)
# Also look at phi coefficient
mapply(function(x, y) polychor(x, y, ML = TRUE), biz[, Q5_dummies], biz[, Q5_dummies])

#--------------------------------------------------
#Question 6 - what types of data

#Free text recode and analysis by dummies/multiple responses

answers_Q6 <- c("Agriculture & food", "Business", "Consumer", "Demographics & social", "Economics", "Education", "Energy",
               "Environment", "Finance", "Geospatial/Mapping", "Government operations", "Health/Healthcare", "Housing",
               "International/Global development", "Legal", "Manufacturing", "Science and research", "Public safety", 
               "Tourism", "Transportation", "Weather")

#Any missings? 
length(which(is.na(biz[, colQ6])))
# Remove missings - dangerous?
#biz[is.na(biz[, colQ6]), colQ6] <- ""

#Create dummies - returns LOGICAL for each column - Returns NA if existing value is NA
biz$Q6_agri <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[1], biz[, colQ6]))
biz$Q6_busi <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[2], biz[, colQ6]))
biz$Q6_cons <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[3], biz[, colQ6]))
biz$Q6_demo <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[4], biz[, colQ6]))
biz$Q6_econ <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[5], biz[, colQ6]))
biz$Q6_educ <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[6], biz[, colQ6]))
biz$Q6_ener <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[7], biz[, colQ6]))
biz$Q6_envi <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[8], biz[, colQ6]))
biz$Q6_fina <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[9], biz[, colQ6]))
biz$Q6_geom <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[10], biz[, colQ6]))
biz$Q6_govt <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[11], biz[, colQ6]))
biz$Q6_heal <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[12], biz[, colQ6]))
biz$Q6_hous <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[13], biz[, colQ6]))
biz$Q6_intg <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[14], biz[, colQ6]))
biz$Q6_lega <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[15], biz[, colQ6]))
biz$Q6_manu <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[16], biz[, colQ6]))
biz$Q6_scie <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[17], biz[, colQ6]))
biz$Q6_safe <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[18], biz[, colQ6]))
biz$Q6_tour <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[19], biz[, colQ6]))
biz$Q6_tran <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[20], biz[, colQ6]))
biz$Q6_weat <- ifelse(is.na(biz[, colQ6]), NA, grepl(answers_Q6[21], biz[, colQ6]))


#FINDING and extracting free text OTHER answers 
#Extracts other answers out, set to blank first
biz$Q6_other_text <- ""
biz$Q6_other_text <- gsub(answers_Q6[1], "", biz[, colQ6])
for (i in 2:21) biz$Q6_other_text <- gsub(answers_Q6[i], "", biz[, "Q6_other_text"])
biz$Q6_other_text <- gsub(",", "", biz$Q6_other_text, perl=T) #remove commas
biz$Q6_other_text <- gsub("^\\s+|\\s+$", "", biz$Q6_other_text) #remove leading and trailing spaces

#biz$Q5_other_text <- gsub("^ *|(?<= ) | *$", "", biz$Q5_other_text, perl=T)
#biz$Q5_other_text <- gsub("^( , ){1,4}/g", "", biz[, "Q5_other_text"], perl = T)
#biz$Q5_other_text <- gsub("^,*|(?<=,),|,*$", "", biz[, "Q5_other_text"], perl=T)

#Creates dummy for other text
biz$Q6_other <- ifelse(is.na(biz[, colQ6]), NA, ifelse(biz$Q6_other_text == "", FALSE, TRUE))

Q6_dummies <- c('Q6_agri', 'Q6_busi', 'Q6_cons', 'Q6_demo', 'Q6_econ', 'Q6_educ', 'Q6_ener',
                'Q6_envi', 'Q6_fina', 'Q6_geom', 'Q6_govt', 'Q6_heal', 'Q6_hous', 'Q6_intg',
                'Q6_lega', 'Q6_manu', 'Q6_scie', 'Q6_safe', 'Q6_tour', 'Q6_tran', 'Q6_weat',
                'Q6_other')



#--------------------------------------------
#Printing example here!
#Save percentages as data frame (transpose to get )
output <- t(as.data.frame(round(sapply(biz[, Q6_dummies], function(x) table(x) / length(na.omit(x)) * 100), digits=2)))
#Rename the rows - add other category
cat_Q6 <- c(answers_Q6, "Other")
row.names(output) <- cat_Q6
#To order
output <- output[order(-output[,"TRUE"]),]

#----------------------------------------------

#Analyse

#Number and percentage of answers in each column
sapply(biz[, Q6_dummies], table) 
sapply(biz[, Q6_dummies], function(x) table(x) / length(na.omit(x)) * 100) # percentage TRUE/FALSE by column

# How many companies chose only one type of data?
table(rowSums(biz[, Q6_dummies])) #Count of data types chosen

sum(table(rowSums(biz[, Q6_dummies]))[2:16]) # number who chose more than one data type
sum(table(rowSums(biz[, Q6_dummies]))[2:16]) / sum(table(rowSums(biz[, Q6_dummies]))) * 100 # Percentage of who ticked more than one box

sum(table(rowSums(biz[, Q6_dummies]))[11:16]) # number who use over 10 types of data
sum(table(rowSums(biz[, Q6_dummies]))[11:16]) / sum(table(rowSums(biz[, Q6_dummies]))) * 100 # Percentage of who ticked more than ten boxes

sum(table(rowSums(biz[, Q6_dummies]))[1]) # number who chose only one data type
sum(table(rowSums(biz[, Q6_dummies]))[1]) / sum(table(rowSums(biz[, Q6_dummies]))) * 100 # Percentage of who ticked only one box

mean(table(rowSums(biz[, Q6_dummies]))) # mean number of data types chosen
median(table(rowSums(biz[, Q6_dummies]))) # median number of data types chosen


# Only checked on box - which box? - this returns a list because some don't have any TRUE - can't fix
sapply(biz[rowSums(biz[, Q6_dummies]) == 1, Q6_dummies], table) #Gives TRUE/FALSE for each column out of all the one box ticked
sapply(biz[rowSums(biz[, Q6_dummies]) == 1, Q6_dummies], function(x) table(x) / length(na.omit(x)) * 100) # percentage of those who ticked that box from all who ticked one box

#This is alright but produces a hell of a lot of data

# Crossproduct - which appear together
crossprod(na.omit(as.matrix(biz[, Q6_dummies])))
# % of total 
crossprod(na.omit(as.matrix(biz[, Q6_dummies]))) / nrow(biz[, Q6_dummies]) * 100

# working out how often two variables occur together by % in column 
True_Q6 <-rep(diag(crossprod(na.omit(as.matrix(biz[, Q6_dummies])))), 22)
True.m_Q6 <- matrix(True_Q6, 22, byrow = T)
crossprod(na.omit(as.matrix(biz[, Q6_dummies]))) / True.m_Q6 * 100

#Does above but for individual variables
sapply(biz[biz[, "Q6_agri"], Q6_dummies], table)
sapply(biz[biz[, "Q6_agri"], Q6_dummies], function(x) table(x) / length(na.omit(x)) * 100)




#---------------------------------------------------
#Question 7 - open gov data?

#Any Missings?
length(which(is.na(biz[, colQ7])))

#check what the distribution looks like
table(biz[, colQ7])
#shows percentage in each category for column
table(biz[, colQ7]) / length(na.omit(biz[, colQ7])) * 100 

#--------------------------------------------------
#Question 9 - Open business data?

#Any missings? (Yes)
length(which(is.na(biz[, colQ9])))
# Remove missings
#biz[is.na(biz[, colQ9]), colQ9] <- ""

#check what the distribution looks like
table(biz[, colQ9])
#shows percentage in each category for column
table(biz[, colQ9]) / length(na.omit(biz[, colQ9])) * 100 


#--------------------------------------------------
#Questions 7 and 9

#set up as LOGICAL:  TRUE = YES, FALSE = No
biz$Q7 <- ifelse(biz[, colQ7] == "No", FALSE, TRUE)
biz$Q9 <- ifelse(biz[, colQ9] == "No", FALSE, TRUE)

#Are they the same length? (Yes this works on length not actual values and is therefore not ideal but I know I wont need to do this again so whatever)
ifelse(length(which(is.na(biz[, colQ7]))) - length(which(is.na(biz[, colQ9]))) == 0, TRUE, FALSE) 
# If FALSE must remove the cases (answers to Q7 and Q9) where either Q7 or Q9 is NA
biz$Q7 <-ifelse(is.na(biz$Q9),NA, biz$Q7)
biz$Q9 <-ifelse(is.na(biz$Q7),NA, biz$Q9)
# Did it work?
ifelse(length(which(is.na(biz$Q7))) - length(which(is.na(biz$Q9))) == 0, TRUE, FALSE) 

#Create a varaible for refering to them
crossQ7.Q9 <- c( 'Q7', 'Q9')

#Carry out crosstable 
CrossTable(biz$Q7, biz$Q9, prop.r=FALSE, prop.c=FALSE,prop.t=TRUE, prop.chisq=FALSE, format=c("SPSS"))


#data as above - independently how many true/false for each
sapply(biz[, crossQ7.Q9], table)
sapply(biz[, crossQ7.Q9], function(x) table(x) / length(na.omit(x)) * 100)

# How many boxes did each company check?
table(rowSums(biz[, crossQ7.Q9]))
sum(table(rowSums(biz[, crossQ7.Q9]))[2:3])
sum(table(rowSums(biz[, crossQ7.Q9]))[2:3]) / sum(table(rowSums(biz[, crossQ7.Q9]))) * 100 # Percentage who checked one box or more i.e. users
sum(table(rowSums(biz[, crossQ7.Q9]))[1]) / sum(table(rowSums(biz[, crossQ7.Q9]))) * 100 # Percentage who checked no box i.e. not users

# Only checked on box - which box? - bit unneccessary
sapply(biz[rowSums(biz[, crossQ7.Q9]) == 1, crossQ7.Q9], table)
sapply(biz[rowSums(biz[, crossQ7.Q9]) == 1, crossQ7.Q9], function(x) table(x) / length(na.omit(x)) * 100)

#Dont really know!
crossprod(na.omit(as.matrix(biz[, crossQ7.Q9])))
crossprod(na.omit(as.matrix(biz[, crossQ7.Q9]))) / nrow(biz[, crossQ7.Q9]) * 100


#---------------------------------------------------
# Question 12 - pricing mechanisms

# Resolve multiple responses - 
#create list answer_Q12  
answers_Q12 <- c("Provide unlimited free access to everyone", 
                 "Provide limited free access to everyone \\(e\\.g\\. rate or volume limited\\)", 
                 "Provide free access to only a subset of people", 
                 "Provide only paid-for access")

#Any missings?
length(which(is.na(biz[, colQ12])))
# Remove missings
#biz[is.na(biz[, colQ12]), colQ12] <- ""

#Setting up dummy variables - i.e. where Q12 is a specific answer dummy variable = TRUE, otherwise = FALSE
biz$Q12_unlimited <- ifelse(is.na(biz[, colQ12]), NA, grepl(answers_Q12[1], biz[, colQ12]))
biz$Q12_limited <- ifelse(is.na(biz[, colQ12]), NA, grepl(answers_Q12[2], biz[, colQ12]))
biz$Q12_subset <- ifelse(is.na(biz[, colQ12]), NA, grepl(answers_Q12[3], biz[, colQ12]))
biz$Q12_paid <- ifelse(is.na(biz[, colQ12]), NA, grepl(answers_Q12[4], biz[, colQ12]))


#Deal with this in recoding
# Only OTHER text for Q12
#Extracts other answers out
biz$Q12_other_text <- gsub(answers_Q12[1], "", biz[, colQ12])
for (i in 2:4) biz$Q12_other_text <- gsub(answers_Q12[i], "", biz[, "Q12_other_text"])
biz$Q12_other_text <- gsub("^(, ){1,4}", "", biz$Q12_other_text)

#Creates dummy for other text
biz$Q12_other <- ifelse(is.na(biz[, colQ12]), NA, ifelse(biz$Q12_other_text == "", FALSE, TRUE))

# Analysing the dummy variables for Q12 - including 'other' - although this should be recategorised at least in part
Q12_dummies <- c('Q12_unlimited', 'Q12_limited', 'Q12_subset', 'Q12_paid', 'Q12_other')
sapply(biz[, Q12_dummies], table)
sapply(biz[, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)

# How many companies checked more than one box?
table(rowSums(biz[, Q12_dummies]))
sum(table(rowSums(biz[, Q12_dummies]))[2:5])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q12_dummies]))[2:5]) / sum(table(rowSums(biz[, Q12_dummies]))) * 100

# Can also analyse the dummy variables excluding 'other' - as these require more thinking - can be useful in some places
Q12_dummies.x <- c('Q12_unlimited', 'Q12_limited', 'Q12_subset', 'Q12_paid')


# How many companies checked more than one box? - excluding other
table(rowSums(biz[, Q12_dummies.x]))
sum(table(rowSums(biz[, Q12_dummies.x]))[3:5])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q12_dummies.x]))[3:5]) / sum(table(rowSums(biz[, Q12_dummies.x]))) * 100

# Only checked on box - which box?
sapply(biz[rowSums(biz[, Q12_dummies]) == 1, Q12_dummies], table)
sapply(biz[rowSums(biz[, Q12_dummies]) == 1, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)


# Crossproduct
crossprod(na.omit(as.matrix(biz[, Q12_dummies])))
crossprod(na.omit(as.matrix(biz[, Q12_dummies]))) / nrow(biz[, Q12_dummies]) * 100


# working out how often two variables occur together by % in column - i.e. [2,1] gives % of those who answered limited of those who answered unlimited
                                                                          #[1,2] gives % of those who answered unlimited of those who answered limited)  
True_Q12 <-rep(diag(crossprod(na.omit(as.matrix(biz[, Q12_dummies])))), 5)
True.m_Q12 <- matrix(True_Q12, 5, byrow = T)
crossprod(na.omit(as.matrix(biz[, Q12_dummies]))) / True.m_Q12 * 100

#----------------------------------------------------------
# Q14 Issues with/in open data

#Check missings for each column - they should all be the same length for the %ages to match be useful
#length(which(is.na(biz[, colQ14_prov])))
#length(which(is.na(biz[, colQ14_lice])))
#length(which(is.na(biz[, colQ14_accu])))
#length(which(is.na(biz[, colQ14_time])))
#length(which(is.na(biz[, colQ14_ease])))
#length(which(is.na(biz[, colQ14_form])))
#length(which(is.na(biz[, colQ14_docu])))
#length(which(is.na(biz[, colQ14_help])))

#colQ14 <- c('colQ14_prov', 'colQ14_lice', 'colQ14_accu', 'colQ14_time', 'colQ14_ease', 'colQ14_form', 'colQ14_docu', 'colQ14_help')

# Remove cases where there is missing data after imputation
#ifelse(is.na(biz[, colQ14_prov]), biz[, colQ14] == NA, biz[, colQ14]) 


#Values have been imputated where necessary into original document
#Imputate values of missings - where 1 or 2 is missing
#q14_miss <- biz[, substr(names(biz), 1, 4) == "Q14."]
#q14_miss <- as.data.frame(lapply(q14_miss, factor, ordered = TRUE))
#q14_miss <- as.data.frame(lapply(q14_miss, function(x) revalue(x, c("1 (little or no influence)"="1", "2.0"="2", "3.0"="3", "4.0"="4", "5 (great influence)"="5"))))

#q14_miss <- rename(q14_miss, c( 
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Provenance.of.data. = "Provenance of data",
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Licensing.of.datasets. = "Licensing of datasets",          
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Accuracy.of.data. = "Accuracy of data",              
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Timeliness.of.data. = "Timeliness of data",            
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Ease.of.access.to.datasets.  = "Ease of access to datasets",    
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Format.of.data. = "Format of data",                 
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Accompanying.documentation.  = "Accompanying documentation",    
  #Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Help.and.support.from.publisher. = "Help and support from publishers"))

#Median values for each of the columns in order to imputate
#median(as.numeric(na.omit(q14_miss[, "Provenance of data"])))
#median(as.numeric(na.omit(q14_miss[, "Licensing of datasets"])))
#median(as.numeric(na.omit(q14_miss[, "Accuracy of data"])))
#median(as.numeric(na.omit(q14_miss[, "Timeliness of data"])))
#median(as.numeric(na.omit(q14_miss[, "Ease of access to datasets"])))
#median(as.numeric(na.omit(q14_miss[, "Format of data"])))
#median(as.numeric(na.omit(q14_miss[, "Accompanying documentation"])))
#median(as.numeric(na.omit(q14_miss[, "Help and support from publishers"])))


q14 <- biz[, substr(names(biz), 1, 4) == "Q14."]
q14 <- as.data.frame(lapply(q14, factor, ordered = TRUE))

q14 <- rename(q14, c( 
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Provenance.of.data. = "Provenance of data",
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Licensing.of.datasets. = "Licensing of datasets",          
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Accuracy.of.data. = "Accuracy of data",              
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Timeliness.of.data. = "Timeliness of data",            
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Ease.of.access.to.datasets.  = "Ease of access to datasets",    
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Format.of.data. = "Format of data",                 
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Accompanying.documentation.  = "Accompanying documentation",    
  Q14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Help.and.support.from.publisher. = "Help and support from publishers"))

#Remove cases that still have missing data
q14 <- na.omit(q14)

q14 <- as.data.frame(lapply(q14, function(x) revalue(x, c("2.0"="2", "3.0"="3", "4.0"="4"))))


#To get frequencies
#Transform annoyingly names columns numbers which is a pain
q14.count <- t(sapply(q14, table))
q14.percent <- round(t(sapply(q14, function(x) table(x) / length(na.omit(x)) * 100)), digits = 2)

#To order in same way likert does (THIS IS A TERRIBLE HACK)
# create a new column to order them by - in the same way likert does by combo of agree, strongly agree. Named order
q14.count <- transform(q14.count, order = q14.count[,"4"] + q14.count[,"5 (great influence)"])
q14.percent <- transform(q14.percent, order = q14.percent[,"4"] + q14.percent[,"5 (great influence)"])

#Order by that aptly named column "order"
q14.count <- q14.count[order(-q14.count$order),]
q14.percent <- q14.percent[order(-q14.percent$order),]


#To plot
#apply likert analysis
lik.q14 <- likert(q14)

#Centred
plot(lik.q14, low.color = odi_red, high.color = odi_dGreen, text.size = 6)

ggsave("graphics/q14-responses-centred.png")

#Filled
plot(lik.q14, low.color = odi_red, high.color = odi_dGreen, text.size = 5, centered = FALSE) 
ggsave("graphics/q14-responses-filled.png")

#plot(lik.q14, low.color = , neutral.color = , high.color = , text.size = 5)
#+ geom_hline(aes(yintercept = c(25, 50)), colour = "white")
#+ ggtitle("Please indicate the extent to which each of the following issues influence your company's decision to use open data")
