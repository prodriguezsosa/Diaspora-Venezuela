# VisaDoorScraper.R
rm(list=ls())

# libraries
library(XML)
library(rvest)
library(data.table)
library(qdap)
library(stringr)
library(xlsx)
library(dplyr)
library(magrittr)
library(tidyr)

# set folder
setwd("/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/VisaDoor")

# Summary of steps:
# Step 1: scrapes summary tables of applications for each year (2008 - 2017) and respective hyperlinks
# Step 2: uses output from Step 1, extracts relevants hyperlinks (Step 2 follows Step 1 without cleaning workstation)
# Step 3: uses output from Step 2 (the output from Step 2 is saved such that Step 3 can be run independently once, 1 and 2 are run)

# define years of interest
years <- seq(2008, 2018, 1)

#######################
# 
# STEP 1: Description
#
#######################
# scrape summary tables of applications for each year (2008 - 2017)
# variables on summary table: Id, Decision Date, Employer, City-State, Case Status, Job Title, Wage Offer.
# "Id" and "Employer" have attached hyperlinks that should also be scraped
# OUTPUT 1: visa.table = list of yearly tables 
# OUTPUT 2: visa.hlinks = list of yearly hyperlinks

#######################
# 
# STEP 1: Code
#
#######################
ScrapeSummaryTables <- function(country = "Venezuela", years = seq(2008, 2018, 1)){
  # specify base link
  # the year is pasted in between the two components that make up the url
  url_base_1 <- paste0("http://visadoor.com/greencards/index?company=&job=&country=", country, "&state=&year=")
  url_base_2 <- "&submit=Search"
  # create empty list to be filled with tables
  visa.table <- rep(list(NA), length(years))
  names(visa.table) <- years
  
  # create empty list to be filled with links
  visa.hlinks <- rep(list(NA), length(years))
  names(visa.hlinks) <- years
  
  # loop through years
  for(i in 1:length(years)){
    url <- paste0(url_base_1, years[i], url_base_2)        # create url for respective year
    visa.table[[i]] <- readHTMLTable(url, header=T, which=1, stringsAsFactors=F)  # scrape table
    visa.table[[i]]$`Country of Origin` <- country 
    visa.hlinks[[i]] <- url %>% read_html() %>% html_nodes("a") %>% html_attr('href') # scrape hyperlinks
  }
  return(list(visa.table, visa.hlinks))
}

#######################
# 
# STEP 2: Description
#
#######################
# for each year extract relevant links from visa.hlinks and add to visa.table
# hyperlink attached to "Id" directs you to more specific information pertaining to the application 
# (e.g. http://visadoor.com/greencards-A-16161-19849/set-and-exhibit-designers-cisneros-studios-llc) & 
# "Link to iCert Registry" which is the complete application,
# (e.g. https://lcr-pjr.doleta.gov/index.cfm?event=ehLCJRExternal.dspCert&doc_id=3&visa_class_id=6&id=1118801).
# hyperlink attached to "Employer" directs you to yearly summary information on the employer's overall visa applications &
# interestingly, it provides a list of "Top Nationalities working at..." (e.g. http://visadoor.com/companies/cisneros-studios-llc).
# OUTPUT: VisaDoorOutput1.rds = visa.table with two additional columns: employee.links = "Id" hyperlinks; employer.links = "Employer" hyperlinks

#######################
# 
# STEP 2: Code
#
#######################
# define function to extract relevant hyperlinks from visa.hlinks and attach to respective visa.table
# INPUT 1: visa_table = table in visa.table list to which function will be applied
# INPUT 2: visa_hlinks = associated list of hyperlinks in visa.hlinks
# OUTPUT: visa.table list with two additional columns for each year: employee.links = "Id" hyperlinks; employer.links = "Employer" hyperlinks
add_hyperlinks <- function(visa_table, visa_hlinks){
  id_first <- visa_table[1,"Id"]                   # find first Id on table, this will allow us to identify the index of the first item of the hyperlinks list that we are interested in
  id_last <- visa_table[nrow(visa_table),"Id"]     # find last Id on table, this will allow us to identify the index of the last item of the hyperlinks list that we are interested in
  # below: use regular expressions to identify the index of the hyperlinks that mention id_first and id_last
  # these will define the range of hyperlinks we are interested in
  # note: each "Id" hyperlink is followed by its corresponding "Employer" hyperlink, we are interested in both
  # the + 1 makes sure we include the company of id_last
  hyperlinks <- visa_hlinks[which(grepl(pattern = id_first, visa_hlinks, fixed = TRUE) == TRUE):(which(grepl(pattern = id_last, visa_hlinks, fixed = TRUE) == TRUE) + 1)] 
  employee_hlinks <- hyperlinks[seq(1, (length(hyperlinks) - 1), 2)]  # seperate employee links (recall: "Id" link is followed by its corresponding "Employer" link)
  employer_links <- hyperlinks[seq(2, length(hyperlinks), 2)]   # seperate employer links
  return(data.table(visa_table, "Employee Hyperlinks" = employee_hlinks, "Employer Hyperlinks" = employer_links)) # bind each set of hyperlinks to original table
}

# create empty list to be filled with new expanded table
visa.table.hlinks <- rep(list(NA), length(years))
names(visa.table.hlinks) <- years

# apply above function to each year
for(j in 1:length(visa.table.hlinks)){
  visa.table.hlinks[[j]] <- add_hyperlinks(visa_table = visa.table[[j]], visa_hlinks = visa.hlinks[[j]])
  visa.table.hlinks[[j]] <- data.table(visa.table.hlinks[[j]], "Year" = years[j]) # add year as an additional column
}

# bind yearly tables to create final table
visa.table.hlinks <- rbindlist(visa.table.hlinks)

# save data
saveRDS(visa.table.hlinks, "VisaDoorOutputAll1.rds")
write.csv(visa.table.hlinks, "VisaDoorOutputAll1.csv")

#######################
# 
# STEP 3: Description
#
#######################
# scrape all "Employee Hyperlinks" in VisaDoorOutput1.rds
# OUTPUT: VisaDoorOutput2.rds = more detailed information on each application including "Link to iCert Registry" = link to the actual application (not all cases have these links)

#######################
# 
# STEP 3: Code
#
#######################
rm(list = ls())

# load data
VisaDoorOutput1 <- readRDS("VisaDoorOutputAll1.rds")
# base url
url_base <- "http://visadoor.com"
# i-cert identifier
identifier <- "https://icert.doleta.gov/"

# create empty list to be filled with tables
application.table <- list()
# loop over Ids
start_time <- proc.time() # measure run time
# do the following for each observation in VisaDoorOutput1 (1 obs = 1 employee = 1 application)
for(i in 1:nrow(VisaDoorOutput1)){
  url <- paste0(url_base, VisaDoorOutput1[i,"Employee Hyperlinks"]) # specify full url
  temp <- data.table(readHTMLTable(url, header=T, which=1, stringsAsFactors=F))  # scrape table
  temp <- temp[,ID:=colnames(temp)[2]]
  setnames(temp, c("Variable", "Value", "Id")) # set column names
  temp <- dcast(temp, Id ~ Variable, value.var = "Value") # convert to wide format
  temphyper <- url %>% read_html() %>% html_nodes("a") %>% html_attr('href') # scrape hyperlink
  if("Link to iCert Registry" %in% colnames(temp)){temp <- temp[, "Link to iCert Registry":=temphyper[grepl(identifier, temphyper)]]} # if a hyperlink exists, include in "Link to iCert Registry" column
  application.table[[i]] <- temp # save individual data to list
  rm(temp, temphyper) # clean up
}
proc.time() - start_time

# bind individual lists
application.table <- rbindlist(application.table, fill = TRUE)
# save data
saveRDS(application.table, "VisaDoorOutputAll2.rds")
write.csv(application.table, "VisaDoorOutputAll2.csv")

#######################
# 
# STEP 4: Description
#
#######################
# scrape all "Link to iCert Registry" links in VisaDoorOutput2.rds
# these are the links to the actual applications (complete forms)
# Note: the forms contain images with relevant information (boxes with/without check marks)
# to process these we extract the name of each image which includes information on whether the box is checked (on) or unchecked (off)
# we then manually created a key that tags each image with its respective question such that we can merge this information 
# this key is the dataset labeled: gif_keys.csv
# OUTPUT: VisaDoorOutput3.rds = all the information contained in the forms by individual level observations

#######################
# 
# STEP 4: Code
#
#######################
rm(list = ls())

# load data
VisaDoorOutput2 <- readRDS("VisaDoorOutputAll2.rds")
gif_key <- read.csv(file="gif_key.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)  # image key
# keep observations with non-missing "Link to iCert Registry"
VisaDoorOutput2 <- na.omit(VisaDoorOutput2, cols =  c('Link to iCert Registry'))

# create empty list to be filled with tables
certification.table <- list()
# loop over hyperlinks
start_time <- proc.time() # measure run time
for(i in 1:nrow(VisaDoorOutput2)){
  url <- unname(unlist(VisaDoorOutput2[i,'Link to iCert Registry'])) # specify full url
  app.page <- url %>% read_html() # scrape url
  # extract text
  app.answer <- html_nodes(app.page,'#detail p') %>% html_text(.) %>% .[-102] # extract answers and delete row mismatch with questions
  app.question <- html_nodes(app.page,'#detail h5') %>% html_text(.) # extract questions
  app.comb <- data.table("QuestionID" = seq(1,length(app.question)), "Question" = app.question, "Answer" = app.answer) # create data.table
  # images
  app.img <- html_nodes(app.page,'#detail img') %>% html_attr(., "src") # extract images
  gif_on <- unlist(lapply(app.img, function(x) str_extract(x, "_on"))) # identify checked boxes
  gif_off <- unlist(lapply(app.img, function(x) str_extract(x, "_off"))) # identify unchecked boxes
  gif_redacted <- unlist(lapply(app.img, function(x) str_extract(x, "redacted"))) # identify image of "redacted"
  gif <- ifelse(!(is.na(gif_on)), "On", ifelse(!(is.na(gif_off)), "Off", "Redacted")) # drop NAs (lines on application without an image)
  gif <- data.table(gif_key, gif) # create image data.table
  gif <- gif[gif %in% c("On", "Redacted"), c("QuestionID", "AnswerType")] # subset gif data.table to keep relevant variables
  # merge text and image tables
  setkey(app.comb, QuestionID)
  setkey(gif, QuestionID)
  app.comb <- merge(app.comb, gif, all.x=TRUE)
  # apply pre-processing to clean questions
  app.comb[,"Question"] <- sapply(app.comb[,"Question"], function(x) mgsub(c("\\\\r", "\\\\t", "\\\\n"), c(" "," ", " "), x)) # delete special characters
  app.comb <- app.comb[,"Question":=str_replace(Question, "^[:digit:]+-[:alpha:].", "")]       # remove current numbering
  app.comb <- app.comb[,"Question":=str_replace(Question, "^[:digit:]+.", "")]                 # remove current numbering (different pattern)
  app.comb[,"Question"] <- sapply(app.comb[,"Question"], function(x) gsub("^ +| +$|( ) +", "\\1", x))  # trim extra white spacing
  app.comb[,"Question"] <- sapply(app.comb[,"Question"], function(x) gsub("^\\s+|\\s+$", " ", x)) # trim beginning and ending white space
  app.comb <- app.comb[,"Question":=paste(paste0("Q", seq(1,nrow(app.comb),1)), Question, sep = ": ")]  # replace with new numbering
  # apply pre-processing to clean answers
  app.comb[,"Answer"] <- sapply(app.comb[,"Answer"], function(x) mgsub(c("\\\\r", "\\\\t", "\\\\n"), c(" "," ", " "), x))  # delete special characters
  app.comb[,"Answer"] <- sapply(app.comb[,"Answer"], function(x) gsub("^ +| +$|( ) +", "\\1", x))  # trim extra white spacing
  app.comb[,"Answer"] <- sapply(app.comb[,"Answer"], function(x) gsub("^\\s+|\\s+$", " ", x)) # trim beginning and ending white space
  # add in image based answers
  app.comb$Answer[!(is.na(app.comb$AnswerType))] <- as.character(app.comb$AnswerType[!(is.na(app.comb$AnswerType))])
  # relabel columns
  app.comb <- app.comb[,c("QuestionID", "Question", "Answer")]
  # replace empty answers with NA
  app.comb$Answer[app.comb$Answer==" "] <- NA 
  # convert from long to wide format
  app.comb[,"ETA Case Number":=VisaDoorOutput2[i,"Id"]] # set Id variable
  app.comb <- app.comb[,c("ETA Case Number", "Question", "Answer")]
  question_order <- c("ETA Case Number", unname(unlist(app.comb[,"Question"]))) # store ordering of questions (spread changes ordering for some reason)
  app.comb <- spread(app.comb, Question, Answer) %>% setcolorder(.,question_order)
  # store
  certification.table[[i]] <- app.comb # store individual entry
}
proc.time() - start_time

# bind data
certification.table <- rbindlist(certification.table, fill = TRUE)
#certification.table[,.N,by="Q-130-6. Country of birth"]
#certification.table[,.N,by="Q-129-5. Country of citizenship"]

# save data
saveRDS(certification.table, "VisaDoorOutputAll3.rds")
write.csv(certification.table, "VisaDoorOutputAll3.csv")








