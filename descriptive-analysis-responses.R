# Path to working folder
setwd("~/git/open-business")
library(car)
library(RGoogleDocs)
library(gmodels)
library(ggplot2)
library(likert)
library(plyr)
library(lubridate)
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
colQ3 <- column("Q3. Which category best describes your company's area of business?")
#colQ4 <- column("Q4. Which of the following are significant sources of revenue for your company?")
colQ4_c <- column("Q4. recoded")
colQ5 <- column("Q5. How does your company currently use open data?")
colQ6 <- column("Q6. What types of open data does your company use?")
colQ7 <- column("Q7. Does your company currently use open government datasets?")
colQ9 <- column("Q9. Does your company currently use other open datasets, such as those provided by businesses, charities, or community projects??")
#colQ12 <- column("Q12. Which pricing mechanism\\(s\\) does your company use for its open data products and\\/or services?")
colQ12_c <- column("Q12. Recoded")

#If using Q14 - currently analysis chooses all the columns by type
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Provenance of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Licensing of datasets\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accuracy of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Timeliness of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Ease of access to datasets\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Format of data\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accompanying documentation\\]")])
#head(biz[, column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Help and support from publisher\\]")])

#colQ14_prov <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Provenance of data\\]")
#colQ14_lice <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Licensing of datasets\\]")
#colQ14_time <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Timeliness of data\\]")
#colQ14_accu <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accuracy of data\\]")
#colQ14_ease <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Ease of access to datasets\\]")
#colQ14_form <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Format of data\\]")
#colQ14_docu <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Accompanying documentation\\]")
#colQ14_help <- column("Q14. Please indicate the extent to which each of the following issues influence your company's decision to use open data: \\[Help and support from publisher\\]")

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
# output <- as.data.frame(round(STAT, digits = 0))


#Plots as many forms
#set image = 
#PNG
ggsave(file = "graphics/Survey/plot.png", plot=image, height = "y", width = "x")
#EPS
ggsave(file = "graphics/Survey/plot.eps", plot=image, height = "y", width = "x")
#SVG
ggsave(file = "graphics/Survey/plot.svg", plot=image, height = "y", width = "x")

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
employ <- as.data.frame(round(table(biz[, colQ2]) / length(na.omit(biz[, colQ2])) * 100, digits=0))
employ.count <- as.data.frame(table(biz[, colQ2])) #for printing count

#Lower case "More than..." to "more than..."
employ$Var1 <- as.character(employ$Var1)
employ$Var1[employ$Var1 == "More than 1000 employees"] <- "more than 1000 employees"

#Order them:
#Set Target order
target.Q2 <- c("fewer than 10 employees", "10 - 50 employees", "51 - 250 employees",
               "251 - 1000 employees",  "more than 1000 employees")
# order Var1 by the target
employ$Var1 <- factor(employ$Var1, levels=target.Q2)
employ <- employ[order(employ$Var1, employ$Freq),]
rownames(employ) = NULL
#Adding line breaks for two longer labels
levels(employ$Var1) <- gsub(" employees", "\n employees", levels(employ$Var1))

#levels(employ$Var1) <- gsub("more than 1000 employees", "more than \n 1000 employees", levels(employ$Var1))

#Plot
ggplot(employ, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue) +
  ylim(0, 80) +
  geom_text(aes(label = paste(round(Freq, digits = 0), "%"), y = (Freq+3)), stat = "identity", color = "black", size = 6) + 
  xlab("") + ylab("Percentage of companies")  +
  theme(axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20, vjust=1.2), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), axis.ticks = element_blank())

# In theme:
# to move the x axis title axis.title.x = element_text(vjust=-0.1)
# to angle the x labels axis.text.x = element_text(angle=90, vjust=1)
# geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1)

#set image = 
#PNG
ggsave(file = "graphics/Survey/employees.png", plot=image, height = 6, width = 12)
#EPS
ggsave(file = "graphics/Survey/employees.eps", plot=image, height = 6, width = 12)
#SVG
ggsave(file = "graphics/Survey/employees.svg", plot=image, height = 6, width = 12)


#------------------------------------------------

#---------------------------------------------------
#Q3 Area of business
#check what the distribution looks like in terms of sectors - now inconsequential - but not for ODM
table(biz[, colQ3])
table(biz[, colQ3]) / length(na.omit(biz[, colQ3])) * 100 


#Save counts as data frame
sectors.count <- as.data.frame(table(biz[, colQ3]))
#To order 
sectors.count <- sectors.count[order(-sectors.count$Freq),]


#Save percentages as data frame
sectors <- as.data.frame(round(table(biz[, colQ3]) / length(na.omit(biz[, colQ3])) * 100, digits=0))

#Rename mistakes
sectors$Var1 <- as.character(sectors$Var1)
sectors$Var1[sectors$Var1 == "Environment  & Weather"] <- "Environment & Weather"
sectors$Var1[sectors$Var1 == "Food and agriculture"] <- "Food and Agriculture"

#To order 
sectors <- sectors[order(-sectors$Freq),]
sectors$Var1 <- reorder(sectors$Var1, sectors$Freq)

# Plot

ggplot(sectors, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue)  + ylim(0, 70) +
 geom_text(aes(label = paste0(round(Freq, digits = 0), "%"), y = (Freq+4.5)), stat = "identity", color = "black", size = 6) + 
 xlab("") + ylab("Percentage of companies") + coord_flip() +
 theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(),
       axis.title.x = element_text(size = 20, vjust=-0.3), axis.text.x = element_text(size = 16),
       axis.text.y = element_text(size = 19))


#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

#set image = 
#PNG
ggsave(file = "graphics/Survey/business-area.png", plot=image, height = 6, width = 12)
#EPS
ggsave(file = "graphics/Survey/business-area.eps", plot=image, height = 6, width = 12)
#SVG
ggsave(file = "graphics/Survey/business-area.svg", plot=image, height = 6, width = 12)

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

#---------------------------------------------------
#IGNORE
#Q4 Sources of revenue
# Resolve multiple responses - there are loads in this question

answers_Q4 <- c("Advertising", "Consulting", "Contributions/Donations", "Data analysis for clients",
                "Database licensing", "Government contracts", "Lead generation to other businesses", 
                "Membership fees", "Grants", "Software licensing", "Subscriptions", 
                "User fees for web or mobile access", "Software development")

#Any missings? NONE right now
length(which(is.na(biz[, colQ4_c])))
# Remove missings - dangerous?
#biz[is.na(biz[, colQ6]), colQ6] <- ""

#Create dummies - returns LOGICAL for each column - Returns NA if existing value is NA
biz$Q4_advt <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[1], biz[, colQ4_c]))
biz$Q4_cons <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[2], biz[, colQ4_c]))
biz$Q4_dona <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[3], biz[, colQ4_c]))
biz$Q4_clie <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[4], biz[, colQ4_c]))
biz$Q4_dbse <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[5], biz[, colQ4_c]))
biz$Q4_govc <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[6], biz[, colQ4_c]))
biz$Q4_lead <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[7], biz[, colQ4_c]))
biz$Q4_memb <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[8], biz[, colQ4_c]))
biz$Q4_gran <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[9], biz[, colQ4_c]))
biz$Q4_sofl <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[10], biz[, colQ4_c]))
biz$Q4_subs <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[11], biz[, colQ4_c]))
biz$Q4_user <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[12], biz[, colQ4_c]))
biz$Q4_sofd <- ifelse(is.na(biz[, colQ4_c]), NA, grepl(answers_Q4[13], biz[, colQ4_c]))

#FINDING and extracting free text OTHER answers 
#Extracts other answers out, set to blank first
biz$Q4_other_text <- ""
biz$Q4_other_text <- gsub(answers_Q4[1], "", biz[, colQ4_c])
for (i in 2:13) biz$Q4_other_text <- gsub(answers_Q4[i], "", biz[, "Q4_other_text"])
biz$Q4_other_text <- gsub(",", "", biz$Q4_other_text, perl=T) #remove commas
biz$Q4_other_text <- gsub("^\\s+|\\s+$", "", biz$Q4_other_text) #remove leading and trailing spaces

#Creates dummy for other text
biz$Q4_other <- ifelse(is.na(biz[, colQ4_c]), NA, ifelse(biz$Q4_other_text == "", FALSE, TRUE))

Q4_dummies <- c('Q4_advt', 'Q4_cons', 'Q4_dona', 'Q4_clie', 'Q4_dbse',
                'Q4_govc', 'Q4_lead', 'Q4_memb', 'Q4_gran', 'Q4_sofl',
                'Q4_subs', 'Q4_user', 'Q4_sofd', 'Q4_other')

#Analyse

# SIMPLE

#Number and percentage of answers in each column
sapply(biz[, Q4_dummies], table) # TRUE/FALSE by column
sapply(biz[, Q4_dummies], function(x) table(x) / length(na.omit(x)) * 100) # percentage using each of these models 

# How many companies chose only one description?
table(rowSums(biz[, Q4_dummies])) #Count of number of descriptions chosen
sum(table(rowSums(biz[, Q4_dummies]))[2:6]) # number who chose more than one description
sum(table(rowSums(biz[, Q4_dummies]))[2:6]) / sum(table(rowSums(biz[, Q4_dummies]))) * 100 # Percentage of who ticked more than one box


#Order for plot and print
#Save percentages as data frame (transpose to get )
Q4_table <- t(as.data.frame(sapply(biz[, Q4_dummies], function(x) table(x) / length(na.omit(x)) * 100)))
#extract only TRUE 
Q4_table <- as.data.frame(Q4_table[,"TRUE"])
#name column
colnames(Q4_table) <- "Percentage"
#Rename the rows - add other category
cat_Q4 <- c(answers_Q4, "Other")
row.names(Q4_table) <- cat_Q4
#take out row names as a column
Q4_table$Var1 = rownames(Q4_table)
#Set as character to allow ordering
Q4_table$Var1 <- as.character(Q4_table$Var1)
#To order
Q4_table <- Q4_table[order(-Q4_table[, 'Percentage']),]
# Reset the `rownames` of your original data
rownames(Q4_table) = NULL
#Reorder for plot
Q4_table$Var1 <- reorder(Q4_table$Var1, Q4_table$Percentage)

#Print table


#Plot

ggplot(Q4_table, aes(y = Percentage, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue)  + ylim(0, 65) +
  geom_text(aes(label = paste0(round(Percentage, digits = 0), "%"), y = (Percentage+5)), stat = "identity", color = "black", size = 6) + 
  xlab("") + ylab("Percentage of companies") + coord_flip() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_text(size = 20, vjust=-0.3),
        axis.text.y = element_text(size = 19), axis.text.x = element_text(size = 16))


#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

ggsave("graphics/survey/revenue.png", height = 6, width = 12)


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


#Plot
#Save percentages as data frame (transpose to get )
Q5_table <- t(as.data.frame(sapply(biz[, Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100)))

#extract only TRUE 
Q5_plot <- as.data.frame(Q5_table[,"TRUE"])
#name it
colnames(Q5_plot) <- "percentage"
#label rownames without brackets
row.names(Q5_plot)<- c("Provide infrastructure", 
                      "Process open data", 
                      "Develop products",
                      "Publish open data",
                      "Provide insights")
#take out row names as a column
Q5_plot$Var1 = rownames(Q5_plot)
# Reset the `rownames` of your original data
rownames(Q5_plot) = NULL

#Set as character to allow ordering
Q5_plot$Var1 <- as.character(Q5_plot$Var1)
#To order
Q5_plot <- Q5_plot[order(-Q5_plot[, 'percentage']),]
# Reset the `rownames` of your original data
rownames(Q5_plot) = NULL

#Reorder for plot
Q5_plot$Var1 <- reorder(Q5_plot$Var1, -Q5_plot$percentage)

#Adding breaks in between words then fix for 3 word ones so 'open data' appears on one line
#This requires setting levels for the plot 
levels(Q5_plot$Var1) <- gsub(" ", "\n", Q5_plot$Var1)
levels(Q5_plot$Var1) <- gsub("open\ndata", "open data", levels(Q5_plot$Var1))

#Plot
ggplot(Q5_plot, aes(y = percentage, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue) + ylim(0, 70) +
  geom_text(aes(label = paste0(round(percentage, digits = 0), "%"), y = (percentage + 4)), stat = "identity", color = "black", size = 6) + 
  xlab("") + ylab("Percentage of companies") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(),
        axis.title.y = element_text(size = 20, vjust= 1.5), axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 19))

#+ coord_flip()
#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

#ggsave("graphics/survey/roles.png", height = 6, width = 12)

#To save
#set image = 
#PNG
ggsave(file = "graphics/Survey/roles.png", plot=image, height = 6, width = 12)
#EPS
ggsave(file = "graphics/Survey/roles.eps", plot=image, height = 6, width = 12)
#SVG
ggsave(file = "graphics/Survey/roles.svg", plot=image, height = 6, width = 12)

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


#To save this - a bit hacky

#Save percentages as data frame (transpose to get )
data.type <- t(as.data.frame(round(sapply(biz[, Q6_dummies], function(x) table(x) / length(na.omit(x)) * 100), digits = 0)))
#extract only TRUE 
data.type <- as.data.frame(data.type[,"TRUE"])
#name it
colnames(data.type) <- "percentage"
#Rename the rows - add other category
cat_Q6 <- c(answers_Q6, "Other")
row.names(data.type) <- cat_Q6
#take out row names as a column
data.type$Var1 = rownames(data.type)
# Reset the `rownames` of your original data
rownames(data.type) = NULL
#make them characters 
data.type$Var1 <- as.character(data.type$Var1)
#To order
data.type <- data.type[order(-data.type[,"percentage"]),]
# Reset the `rownames` again
rownames(data.type) = NULL

#Save percentages as data frame (transpose to get )
data.type.count <- t(as.data.frame(sapply(biz[, Q6_dummies], table))) 
#extract only TRUE 
data.type.count <- as.data.frame(data.type.count[,"TRUE"])
#name it
colnames(data.type.count) <- "count"
#Rename the rows - add other category
cat_Q6 <- c(answers_Q6, "Other")
row.names(data.type.count) <- cat_Q6
#take out row names as a column
data.type.count$Var1 = rownames(data.type.count)
# Reset the `rownames` of your original data
rownames(data.type.count) = NULL
#make them characters 
data.type.count$Var1 <- as.character(data.type.count$Var1)
#To order
data.type.count <- data.type.count[order(-data.type.count[,"count"]),]
# Reset the `rownames` again
rownames(data.type.count) = NULL


#---------------------------------------------
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

#---------------------------------------------

#Plotting!!!

#Order for plot
data.type$Var1 <- reorder(data.type$Var1, data.type$percentage)

image = ggplot(data.type, aes(y = percentage, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue) + ylim(0, 70) +
  geom_text(aes(label = paste0(round(percentage, digits = 0), "%"), y = (percentage+5)), stat = "identity", color = "black", size = 6) + 
  xlab("") + ylab("Percentage of companies") + coord_flip()  +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(), 
        axis.title.x = element_text(size = 20, vjust= -0.3), axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18))


#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

#set image = 
#PNG
ggsave(file = "graphics/Survey/data_types.png", plot=image, height = 8, width = 12)
#EPS
ggsave(file = "graphics/Survey/data_types.eps", plot=image, height = 8, width = 12)
#SVG
ggsave(file = "graphics/Survey/data_types.svg", plot=image, height = 8, width = 12)
 


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
# Use recoded values - colQ12_c

# Resolve multiple responses - 
#create list answer_Q12  
answers_Q12 <- c("Provide unlimited free access to everyone", 
                 "Provide limited free access to everyone \\(e\\.g\\. rate or volume limited\\)", 
                 "Provide free access to only a subset of people", 
                 "Provide only paid-for access"
                 )
#", Depends upon client"

#Any missings?
length(which(is.na(biz[, colQ12_c])))
# Remove missings
#biz[is.na(biz[, colQ12]), colQ12] <- ""

#Setting up dummy variables - i.e. where Q12 is a specific answer dummy variable = TRUE, otherwise = FALSE
biz$Q12_unlimited <- ifelse(is.na(biz[, colQ12_c]), NA, grepl(answers_Q12[1], biz[, colQ12_c]))
biz$Q12_limited <- ifelse(is.na(biz[, colQ12_c]), NA, grepl(answers_Q12[2], biz[, colQ12_c]))
biz$Q12_subset <- ifelse(is.na(biz[, colQ12_c]), NA, grepl(answers_Q12[3], biz[, colQ12_c]))
biz$Q12_paid <- ifelse(is.na(biz[, colQ12_c]), NA, grepl(answers_Q12[4], biz[, colQ12_c]))
#biz$Q12_client <- ifelse(is.na(biz[, colQ12_c]), NA, grepl(answers_Q12[5], biz[, colQ12_c]))

# Only OTHER text for Q12
#Extracts other answers out
biz$Q12_other_text <- gsub(answers_Q12[1], "", biz[, colQ12_c])
for (i in 2:4) biz$Q12_other_text <- gsub(answers_Q12[i], "", biz[, "Q12_other_text"])
biz$Q12_other_text <- gsub("^(, ){1,4}", "", biz$Q12_other_text)

#Creates dummy for other text
biz$Q12_other <- ifelse(is.na(biz[, colQ12_c]), NA, ifelse(biz$Q12_other_text == "", FALSE, TRUE))

# Set up dummy variables
Q12_dummies <- c('Q12_unlimited', 'Q12_limited', 'Q12_subset', 'Q12_paid', 'Q12_other')

#'Q12_client',



#Analysis

# Analysing the dummy variables for Q12 - including 'other'
sapply(biz[, Q12_dummies], table)
sapply(biz[, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)
# to print - round: round(sapply(biz[, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100), digits=2)

# How many companies checked more than one box?
table(rowSums(biz[, Q12_dummies]))
sum(table(rowSums(biz[, Q12_dummies]))[2:5])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q12_dummies]))[2:5]) / sum(table(rowSums(biz[, Q12_dummies]))) * 100

# Only checked on box - which box? - especially salient here
sapply(biz[rowSums(biz[, Q12_dummies]) == 1, Q12_dummies], table)
sapply(biz[rowSums(biz[, Q12_dummies]) == 1, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)

#for output

prices.count <- as.data.frame(sapply(biz[, Q12_dummies], table))
#extract only TRUE 
prices.count <- as.data.frame(prices.count["TRUE",])
#name it
row.names(prices.count) <- "percentage"
#Rename the rows - add other category
cat_Q12 <- c(answers_Q12, "Other")
colnames(prices.count) <- cat_Q12


prices <- t(as.data.frame(sapply(biz[, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)))
#extract only TRUE 
prices <- as.data.frame(prices[,"TRUE"])
#name it
colnames(prices) <- "percentage"
#Rename the rows - add other category
cat_Q12 <- c("Unlimited free access", "Limited free access", "Free access for subset", 
             "Paid for access", "Other")
prices$Var1 <- cat_Q12
row.names(prices) <- NULL

#Reorder for plot
prices$Var1 <- reorder(prices$Var1, -prices$percentage)

#Adding breaks in between words then fix for 3 word ones so 'open data' appears on one line
#This requires setting levels for the plot 
levels(prices$Var1) <- gsub("imited ", "imited\n ", prices$Var1)
levels(prices$Var1) <- gsub("access ", "access\n", levels(prices$Var1))
levels(prices$Var1) <- gsub("for access", "for\naccess", levels(prices$Var1))


#Plot
image = ggplot(prices, aes(y = percentage, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue) + ylim(0, 70) +
  geom_text(aes(label = paste0(round(percentage, digits = 0), "%"), y = (percentage + 4)), stat = "identity", color = "black", size = 6) + 
  xlab("") + ylab("Percentage of companies") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(),
        axis.title.y = element_text(size = 20, vjust= 1.5), axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 19))


# + coord_flip() 
#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

#set image = 
#PNG
ggsave(file = "graphics/Survey/pricing.png", plot=image, height = 6, width = 12)
#EPS
ggsave(file = "graphics/Survey/pricing.eps", plot=image, height = 6, width = 12)
#SVG
ggsave(file = "graphics/Survey/pricing.svg", plot=image, height = 6, width = 12)



#-----------------------------------------------------

# Crossproduct - only makes sense with original 4 variables use.x
Q12_dummies.x <- c('Q12_unlimited', 'Q12_limited', 'Q12_subset', 'Q12_paid')

crossprod(na.omit(as.matrix(biz[, Q12_dummies.x])))
crossprod(na.omit(as.matrix(biz[, Q12_dummies.x]))) / nrow(biz[, Q12_dummies.x]) * 100

#Save percentages as data frame (transpose to get )
cross.12x <- as.data.frame(crossprod(na.omit(as.matrix(biz[, Q12_dummies.x]))))

#Rename the columns - using rename of limited and add other category
cat_Q12 <- c("Provide unlimited free access to everyone", 
             "Provide limited free access to everyone", 
             "Provide free access to only a subset of people", 
             "Provide only paid-for access")
colnames(cross.12x) <- cat_Q12
row.names(cross.12x) <- cat_Q12

# DONT THINK THIS MAKES SENSE IN CONTEXT
# working out how often two variables occur together by % in column - i.e. [2,1] gives % of those who answered limited of those who answered unlimited
                                                                          #[1,2] gives % of those who answered unlimited of those who answered limited)  
#True_Q12 <-rep(diag(crossprod(na.omit(as.matrix(biz[, Q12_dummies.x])))), 4)
#True.m_Q12 <- matrix(True_Q12, 4, byrow = T)
#crossprod(na.omit(as.matrix(biz[, Q12_dummies.x]))) / True.m_Q12 * 100

# Can also analyse the dummy variables excluding 'other' - as these require more thinking - can be useful in some places
# How many companies checked more than one box? - excluding other
#table(rowSums(biz[, Q12_dummies.x]))
#sum(table(rowSums(biz[, Q12_dummies.x]))[3:5])
# Percentage of 2 or more pricing mechanisms
#sum(table(rowSums(biz[, Q12_dummies.x]))[3:5]) / sum(table(rowSums(biz[, Q12_dummies.x]))) * 100

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



#IGNORE
#q14 <- as.data.frame(lapply(q14, function(x) revalue(x, c("2.0"="2", "3.0"="3", "4.0"="4"))))


#To get frequencies
#Transform annoyingly names columns numbers which is a pain
q14.count <- t(sapply(q14, table))
q14.percent <- round(t(sapply(q14, function(x) table(x) / length(na.omit(x)) * 100)), digits = 0)

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
image = plot(lik.q14, low.color = odi_red, high.color = odi_dGreen, text.size = 7) + ylab("Percentage of companies") +
       theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text.x = element_text(size = 20),
             axis.title.x = element_text(size = 22, vjust= -0.3), axis.text.y = element_text(size = 18),
             legend.title = element_blank())

# for a panel grid with just vertical lines
#panel.grid.minor.x = element_line(), panel.grid.major.x = element_line(), 
#panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),

#set image = 
#PNG
ggsave(file = "graphics/Survey/influence_centred.png", plot=image, height = 8, width = 14)
#EPS
ggsave(file = "graphics/Survey/influence_centred.eps", plot=image, height = 8, width = 14)
#SVG
ggsave(file = "graphics/Survey/influence_centred.svg", plot=image, height = 8, width = 14)


#Filled
plot(lik.q14, low.color = odi_red, high.color = odi_dGreen, text.size = 7, centered = FALSE) + ylab("Percentage of companies") +
        theme(panel.grid = element_blank(), rect = element_blank(),
              axis.ticks = element_blank(), axis.text.x = element_text(size = 20),
              axis.title.x = element_text(size = 22, vjust= -0.3), axis.text.y = element_text(size = 20),
              legend.title = element_blank())

#set image = 
#PNG
ggsave(file = "graphics/Survey/influence_filled.png", plot=image, height = 8, width = 14)
#EPS
ggsave(file = "graphics/Survey/influence_filled.eps", plot=image, height = 8, width = 14)
#SVG
ggsave(file = "graphics/Survey/influence_filled.svg", plot=image, height = 8, width = 14)


#plot(lik.q14, low.color = , neutral.color = , high.color = , text.size = 5)
#+ geom_hline(aes(yintercept = c(25, 50)), colour = "white")
#+ ggtitle("Please indicate the extent to which each of the following issues influence your company's decision to use open data")


#Other graphs to try
#Density - interesting but would want to look at more
likert.density.plot(lik.q14, facet = TRUE, bw = 0.5)

#Heat plot
likert.heat.plot(lik.q14, low.color = odi_red, high.color = odi_dBlue,
                 text.color = "black", text.size = 4, wrap = 10)




#----------------------------------------------------------------------

#Incorporated date

colIncD <- column("Incorporated Date")


#Is incorporated date treated as a date?
is.Date(biz[, colIncD])
#Treat incorporated date as a date?
biz[,colIncD] <- as.Date(biz[,colIncD], format("%d/%m/%Y"))
#Worked?
is.Date(biz[, colIncD])
# Load today's date (might not want to use this but choose a significant day)
tdate <- today()

#Calculate the interval of time - that is start date (incorporated date) until now (today atm) USING POSIXct format
age_int <- new_interval(as.POSIXct(biz[,colIncD]), as.POSIXct(tdate))
#Calculate the duration in seconds (this is more accurate/workable than difftime)
age_sec <- as.duration(age_int)


#Printing age solutions - make character variable of years and months
# Create standard durations for a year and a month
one.year <- duration(1, units = "years")
one.month <- duration(1, units = "months")
# Calculate the difference in years as a whole number - i.e. rounded
years <- floor(new_interval(biz[,colIncD], tdate) / one.year )
# Calculate left over rounded number of months (first full number of months then remainder)
months <- round(new_interval(biz[,colIncD], tdate) / one.month ) %% 12
# Paste the years and months together with year and date included - for output = good, otherwise quite silly 
biz$age_char <- ifelse(is.na(years), NA, paste(years, ifelse(years == 1,"year","years"), months, ifelse(months == 1, "month", "months")))


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

#Categories - less than two years, 2 to 4, 4 to 6, 6 to 8, 8 to 10, 10, 10+
#biz$age_cat <-cut(age_sec, c(0,2*one.year,4*one.year,6*one.year,8*one.year,10*one.year, 15*one.year, 100*one.year), right = FALSE,
#labels = c("less than 2 years", "2 to 4 years", "4 to 6 years", "6 to 8 years", "8 to 10 years", "10 to 15 years", "more than 15 years"))

#Using ONS data categories
biz$age_cat <-cut(age_sec, c(0,2*one.year,4*one.year,10*one.year,100*one.year), right = FALSE,
                  labels = c("less than 2 years", "2 to 3 years", "4 to 9 years", "more than 10 years"))


#What does this now look like?
table(biz$age_cat)
table(biz$age_cat) / length(na.omit(biz$age_cat)) * 100 


#Plot
#as dataframe
age <- as.data.frame(round(table(biz$age_cat) / length(na.omit(biz$age_cat)) * 100, digits = 0))

ggplot(age, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_mBlue)  +
  geom_text(aes(label = Freq, y = - 2.5), stat = "identity", color = "black", size = 4) + 
  xlab("") + ylab("Percentage of companies") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())

#coord_flip() +
#geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +

ggsave("graphics/survey/age-percentages.png", height = 6, width = 12)




#TIMELINE
#Cumulative number of companies founded

#By everyday
#What about doing it every day!
#Get everyday into a dataframe
everyday.num <- c(1:as.numeric(as.duration(new_interval(as.POSIXct(ymd(20000101)), as.POSIXct(tdate)))/ddays(1)))
exist.date <- data.frame(everyday.num)
exist.date$everyday <- as.Date(ymd(20000101) + days(exist.date$everyday.num))

#ensure still have interval for age
age_int <- new_interval(as.POSIXct(biz[,colIncD]), as.POSIXct(tdate))
#Now to get the number of companies that existed on that day
for(i in 1:as.numeric(as.duration(new_interval(as.POSIXct(ymd(20000101)), as.POSIXct(tdate)))/ddays(1)))
{exist.date$exist.then[i] <- sum(exist.date$everyday[i] %within% age_int + 0, na.rm = TRUE)}

#plot

ggplot(exist.date, aes(y = exist.then, x = everyday)) + geom_line(stat = "identity", colour = odi_mBlue, size = 1)

ggsave("graphics/survey/incorporation_timeline_byday.png", height = 6, width = 14)


#By month
#Set up months as a dataframe - arbitrary choice of date - could use open data timeline?
list.months <- c("January", "February", "March", "April", "May", "June", "July", 
                 "August", "September", "October", "November", "December")
years.then <- c(2000:2014)
month.num <- c(0:179)
exist <- data.frame(month.num, rep(list.months,15), rep(years.then, each=12))
#Include Jan 2015 for sake of one company which changed since survey
jan <- c(180, "January", 2015)
exist <<- rbind(exist,jan)
#make those fields that need to be numeric - numeric
exist$month.num <- as.numeric(exist$month.num)
colnames(exist) <- c("month.num", "Month", "Year")

#Because months are difficult beasts - first day must always return first of month so adds whole months
# and last day must always get last day so gets last logical day

first.day <- as.Date(ymd(20000101) + months(exist$month.num))
last.day <- as.Date(ymd(20000131) %m+% months(exist$month.num))

exist$month.span <- new_interval(as.POSIXct(first.day), as.POSIXct(last.day))
#For plots - midpoint when the value can be plotted against
exist$mid.day <- first.day + (last.day - first.day)/2

#ensure still have interval for age
age_int <- new_interval(as.POSIXct(biz[,colIncD]), as.POSIXct(tdate))
#Now to get the number of companies that existed during that month - i.e. takes first whole month
for(i in 1:181){exist$exist.then[i] <- sum(exist$month.span[i] %within% age_int + 0, na.rm = TRUE)}




# Now to plot this baby

#Get the things we need - simpler to make a new df
date.point <- exist$mid.day
exist.then <- exist$exist.then
exist.plot <- data.frame(date.point, exist.then)

ggplot(exist.plot, aes(y = exist.then, x = date.point)) + geom_line(stat = "identity", colour = odi_mBlue, size = 1)

ggsave("graphics/survey/incorporation_timeline.png", height = 6, width = 14)


#+ xlab("Number of companies") + ylab("Date")
#  ylim(0, 100) + 
#  geom_text(aes(label = paste(round(Freq, digits=2), "%"), y = (Freq+3)), stat = "identity", color = "black", size = 5) +  
#  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())


#Could do it with a date
#date <- as.Date("2010-12-13")
#exist.date <- sum(date %within% age_int + 0, na.rm = TRUE)

#PERIOD function not really working - bit of a hack to use duration and set athematical values for months and years but oh well
#Calculate ther period of time 
#age_period <- as.period(biz$age_sec)
#Less precise
#Work out how old companies are in days (difftime)
#biz$age_days <- tdate - biz[,colIncD]
#Mean in days
#mean(na.omit(age_days))


