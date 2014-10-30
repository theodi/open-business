setwd("~/git/open-business")
library(car)
library(RGoogleDocs)
source('~/git/R-projects/data-portal-analysis/functions.R')

options(stringsAsFactors = FALSE)

# This loads the Google password
source('~/git/R-playground/sensitive-data-never-commit.R')
# Get the data directly
sheets.con <- getGoogleDocsConnection(getGoogleAuth("ulrich.atz@theodi.org", gmailpw, service = "wise"))
spreadsheets <- getDocs(sheets.con)
# Remove passwords
rm(list = c('gmailpw', 'plotlykey'))

# EXPERIMENTAL: testing
library(testthat)

# Survey responses
biz.sheet <- getWorksheets(spreadsheets[["5. Responses Database "]], sheets.con)
biz <- sheetAsMatrix(biz.sheet[["Form responses 1"]], header = TRUE)

list.sheet <- getWorksheets(spreadsheets[["1. Master Database"]], sheets.con)
list.all <- sheetAsMatrix(list.sheet[["Data"]], header = TRUE)
# list.models <- sheetAsMatrix(list.sheet[["Business models"]], header = TRUE)
# Only those that passed the criterion
list <- list.all[list.all[, 3] != "No", ]

# List may include missing


#---------------------------------------------------
# CLEAN DATA
column <- function(var, x = biz){
  var_index <- grep(var, names(x))
  return(var_index)
}

clean.biz <- function(x = biz){
  size <- column("2. What is the size of your company?")
  x[, size] <- as.factor(x[, size])
  return(x)
}

drop.vars <- function(x = biz, list){
  dropvars <- names(x) %in% list 
  x <- x[!dropvars]
}



biz <- clean.biz()
biz <- biz[biz[, column("Timestamp logged in Staging Database?")] == "Yes", ]
biz <- drop.vars(list = c("Timestamp", "Timestamp logged in Staging Database?", "Name", "Job title", "Email"))



#---------------------------------------------------
# STATISTICS

# Survey: size and sector
sapply(biz[, 2:3], table)
sapply(biz[, 2:3], function(x) table(x) / length(na.omit(x)))

# Sectors all organisations
table(list[, 20])
table(list[, 20]) / length(na.omit(list[, 20]))

# Export count of sectors
write.csv(table(list[, 20]), "data/master-sectors.csv", row.names = FALSE)

#---------------------------------------------------
# Newspaper contribution
#---------------------------------------------------

head(biz[, column("5. How does your company currently use open data?")])
head(biz[, column("12. Which pricing mechanism\\(s\\) does your company use for its open data products and\\/or services?")])

# Q12 Pricing mechanisms
colQ12 <- column("12. Which pricing mechanism\\(s\\) does your company use for its open data products and\\/or services?")
# Resolve multiple response 
answers_Q12 <- c("Provide unlimited free access to everyone", 
                 "Provide limited free access to everyone \\(e\\.g\\. rate or volume limited\\)", 
                 "Provide free access to only a subset of people", 
                 "Provide only paid-for access")


# Remove missings
biz[is.na(biz[, colQ12]), colQ12] <- ""

biz$Q12_unlimited <- grepl(answers_Q12[1], biz[, colQ12])
biz$Q12_limited <- grepl(answers_Q12[2], biz[, colQ12])
biz$Q12_subset <- grepl(answers_Q12[3], biz[, colQ12])
biz$Q12_paid <- grepl(answers_Q12[4], biz[, colQ12])

biz$Q12_other_text <- gsub("^(, ){1,4}", "", biz$Q12_other_text)
biz$Q12_other <- ifelse(biz$Q12_other_text == "", 0, 1)

# Only OTHER for Q12
biz$Q12_other_text <- gsub(answers_Q12[1], "", biz[, colQ12])
for (i in 2:4) biz$Q12_other_text <- gsub(answers_Q12[i], "", biz[, "Q12_other"])


# Analyse the dummy variables for Q12
Q12_dummies <- c('Q12_unlimited', 'Q12_limited', 'Q12_subset', 'Q12_paid', 'Q12_other')
sapply(biz[, Q12_dummies], table)
sapply(biz[, Q12_dummies], function(x) table(x) / length(na.omit(x)) * 100)

# How many companies checked more than one box?
table(rowSums(biz[, Q12_dummies]))
sum(table(rowSums(biz[, Q12_dummies]))[3:5])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q12_dummies]))[3:5]) / sum(table(rowSums(biz[, Q12_dummies]))) * 100


#---------------------------------------------------
# Q5 Use of open data
colQ5 <- column("5. How does your company currently use open data?")
# Resolve multiple response 
answers_Q5 <- c("My company provides infrastructure for others to publish open data \\(e.g. platforms, portals, data stores\\)", 
                "My company processes open data \\(e.g aggregation, classification, anonymization, cleaning, refining, enriching\\)", 
                "My company develops products based on open data \\(e.g. APIs, apps\\)",
                "My company publishes open data",
                "My company provides insights based on open data \\(e.g. analytics, visualisations\\)")

# Remove missings (zero at the moment)
biz[is.na(biz[, colQ5]), colQ5] <- ""

biz$Q5_infrastructure <- grepl(answers_Q5[1], biz[, colQ5])
biz$Q5_processes <- grepl(answers_Q5[2], biz[, colQ5])
biz$Q5_develops <- grepl(answers_Q5[3], biz[, colQ5])
biz$Q5_publish <- grepl(answers_Q5[4], biz[, colQ5])
biz$Q5_insights <- grepl(answers_Q5[5], biz[, colQ5])


Q5_dummies <- c('Q5_infrastructure', 'Q5_processes', 'Q5_develops', 'Q5_publish', 'Q5_insights')
sapply(biz[, Q5_dummies], table)
sapply(biz[, Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100)

# How many companies checked more than one box?
table(rowSums(biz[, Q5_dummies]))
sum(table(rowSums(biz[, Q5_dummies]))[3:6])
# Percentage of 2 or more pricing mechanisms
sum(table(rowSums(biz[, Q5_dummies]))[3:6]) / sum(table(rowSums(biz[, Q5_dummies]))) * 100

# Crossproduct to see which usage co-occur
crossprod(as.matrix(biz[, Q5_dummies]))
crossprod(as.matrix(biz[, Q5_dummies])) / nrow(biz[, Q5_dummies]) * 100

# Correlations - DANGER very misleading
cor(biz[, Q5_dummies]) 
require(polycor)
hetcor(crossprod(as.matrix(biz[, Q5_dummies])), ML = TRUE, std.err = FALSE)
polychor(biz$Q5_processes, biz$Q5_insights, ML = TRUE)

# FILTER on process open data
sapply(biz[biz[, "Q5_processes"], Q5_dummies], table)
sapply(biz[biz[, "Q5_processes"], Q5_dummies], function(x) table(x) / length(na.omit(x)) * 100)

# Only checked on box
sapply(biz[rowSums(biz[, Q5_dummies]) == 1, Q5_dummies], table)
# >> Infrastructure seems to be its on cluster

#---------------------------------------------------
# Q14 Issues for using open data
library(likert)
library(plyr)

q14 <- biz[, substr(names(biz), 1, 3) == "14."]
q14 <- as.data.frame(lapply(q14, factor, ordered = TRUE))

q14 <- as.data.frame(lapply(q14, function(x) revalue(x, c("2.0"="2", "3.0"="3", "4.0"="4"))))

q14 <- rename(q14, c( 
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Provenance.of.data. = "Provenance of data",
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Licensing.of.datasets. = "Licensing of datasets",          
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Accuracy.of.data. = "Accuracy of data",              
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Timeliness.of.data. = "Timeliness of data",            
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Ease.of.access.to.datasets.  = "Ease of access to datasets",    
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Format.of.data. = "Format of data",                 
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Accompanying.documentation.  = "Accompanying documentation",    
  X14..Please.indicate.the.extent.to.which.each.of.the.following.issues.influence.your.company.s.decision.to.use.open.data...Help.and.support.from.publisher. = "Help and support from publishers"))


lik.q14 <- likert(q14)

plot(lik.q14, low.color = odi_turquoise, high.color = odi_red, text.size = 5) 
ggsave("graphics/q14-responses.png")
plot(lik.q14, low.color = odi_turquoise, high.color = odi_red, text.size = 5, centered = FALSE) 
ggsave("graphics/q14-responses-centred.png")


# + geom_hline(aes(yintercept = c(25, 50)), colour = "white")
# + ggtitle("Please indicate the extent to which each of the following issues influence your company's decision to use open data")
  
#---------------------------------------------------
# TESTS
test_file('tests.R')



