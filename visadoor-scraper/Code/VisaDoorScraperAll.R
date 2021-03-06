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
library(pbapply)
library(progress)

# set folder
setwd("/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/VisaDoor")

# Summary of steps:
# Step 1: scrapes summary tables of applications for each year (2008 - 2017) and respective hyperlinks
# Step 2: uses output from Step 1, extracts relevants hyperlinks (Step 2 follows Step 1 without cleaning workstation)
# Step 3: uses output from Step 2 (the output from Step 2 is saved such that Step 3 can be run independently once, 1 and 2 are run)

# define years of interest
years <- seq(2008, 2017, 1)

# scrape list of countries available
url_base <- "http://visadoor.com/greencards/index?company=&job=&country=&state=&year=2008&submit=Search"
countries <- read_html(url_base) %>% html_nodes("option") %>% html_attr('value') # scrape option menus
countries <- countries[(which(countries == "")[1] + 1):(which(countries == "")[2] - 1)] # extract countries
countries <- str_replace_all(string = countries, pattern = " ", replacement = "+") # replace spaces with +

# check list of countries are the same every year
#for(i in 2008:2018){
#  url_base <- paste0("http://visadoor.com/greencards/index?company=&job=&country=&state=&year=",i,"&submit=Search")
#  countries <- read_html(url_base) %>% html_nodes("option") %>% html_attr('value')
#  countries <- countries[(which(countries == "")[1] + 1):(which(countries == "")[2] - 1)]
#  print(length(countries))
#}

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
ScrapeSummaryTables <- function(country = "Venezuela", years = 2008){
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
    url <- paste0(url_base_1, years[i], url_base_2)  # create url for respective year
    page <- read_html(url)  # scrape html
    table_check <- page %>% html_nodes("table") # is there are a table?
    if(length(table_check) > 0){ # if TRUE
    visa.table[[i]] <- page %>% html_table(header = TRUE, trim = TRUE) %>% .[[1]]  # scrape table
    visa.table[[i]]$Year <- years[i] # add country of origin
    visa.table[[i]]$`Country of Origin` <- country # add country of origin
    visa.hlinks[[i]] <- page %>% html_nodes("a") %>% html_attr('href') # scrape hyperlinks
    }
  }
  output <- list(visa.table, visa.hlinks)
  names(output) <- c("visa.table", "visa.hlinks")
  return(output)
}

#start_time <- Sys.time()
# loop format (good for debugging)
#VisaDoorSummary <- list()
#for(j in 1:length(countries)){
#  VisaDoorSummary[[j]] <- ScrapeSummaryTables(country = countries[j], years = years)
#}
#Sys.time() - start_time

# apply function to list of countries
VisaDoorSummary <- pblapply(countries, function(x) ScrapeSummaryTables(country = x, years = years))

# extract table
visa.table <- lapply(VisaDoorSummary, function(x) x[["visa.table"]]) %>% unlist(recursive = FALSE) %>% do.call(rbind, .) %>% data.table
rownames(visa.table) <- NULL

# extract links
visa.hlinks <- lapply(VisaDoorSummary, function(x) x[["visa.hlinks"]]) %>% unlist(recursive = FALSE)

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
  visa_table <- data.table(visa_table) # convert to data.table
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

# function extension to apply add_hyperlinks to a list of countries
AddHyperlinks <- function(SummaryOutput){
  visa.table <- SummaryOutput$visa.table
  visa.hlinks <- SummaryOutput$visa.hlinks
  # check which years have data
  index_years <- !is.na(visa.table)
  if(any(index_years)){ # if any of the years have data
  # create empty list to be filled with new expanded table
  visa.table.hlinks <- rep(list(NA), length(which(index_years == TRUE)))
  names(visa.table.hlinks) <- names(visa.table[index_years])
  # apply above function to each year
  # loop version is good for debugging
  #for(j in 1:length(visa.table.hlinks)){
  #  visa.table.hlinks[[j]] <- add_hyperlinks(visa_table = visa.table[[j]], visa_hlinks = visa.hlinks[[j]])
  #}
  visa.table.hlinks <- lapply(names(visa.table.hlinks), function(x) add_hyperlinks(visa_table = visa.table[[x]], visa_hlinks = visa.hlinks[[x]]))
  # bind yearly tables to create final table
  visa.table.hlinks <- rbindlist(visa.table.hlinks)
  return(visa.table.hlinks)}else{
    return(NA)
    }
}

visa.table.hlinks.all <- lapply(1:length(countries), function(k) AddHyperlinks(VisaDoorSummary[[k]]))
visa.table.hlinks.all <- do.call(rbind, visa.table.hlinks.all)

# save data
saveRDS(visa.table.hlinks.all, "VisaDoorOutputAll1.rds")
write.csv(visa.table.hlinks.all, "VisaDoorOutputAll1.csv")

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
VisaDoorOutput1 <- VisaDoorOutput1[`Country of Origin` == "Venezuela",]
# base url
url_base <- "http://visadoor.com"
# i-cert identifier
identifier <- "https://icert.doleta.gov/"

# create empty list to be filled with tables
application.table <- list()
# do the following for each observation in VisaDoorOutput1 (1 obs = 1 employee = 1 application)
GetEmployerInfo <- function(applicant = NULL){  
  url <- paste0(url_base, applicant[1,"Employee Hyperlinks"]) # specify full url
  page <- read_html(url)  # scrape html
  table_check <- page %>% html_nodes("table") # is there are a table?
  if(length(table_check) > 0){ # if TRUE
    temp <- page %>% html_table(header = TRUE, trim = TRUE) %>% .[[1]] %>% data.table  # scrape table
    temp <- temp[,ID:=colnames(temp)[2]]
    setnames(temp, c("Variable", "Value", "Id")) # set column names
    temp <- dcast(temp, Id ~ Variable, value.var = "Value") # convert to wide format
    temphyper <- page %>% html_nodes("a") %>% html_attr('href') # scrape hyperlink
    if("Link to iCert Registry" %in% colnames(temp)){temp <- temp[, "Link to iCert Registry":=temphyper[grepl(identifier, temphyper)]]} # if a hyperlink exists, include in "Link to iCert Registry" column
    return(temp) # save individual data to list
  }
}

#start_time <- Sys.time() # measure run time
#application.table <- list()
#for(i in 3167:nrow(VisaDoorOutput1)){  
#  application.table[[i]] <- GetEmployerInfo(applicant = VisaDoorOutput1[i,])
#}
#Sys.time() - start_time

# apply version
#start_time <- Sys.time() # measure run time
application.table <- pblapply(1:nrow(VisaDoorOutput1), function(x) GetEmployerInfo(applicant = VisaDoorOutput1[x,]))
#Sys.time() - start_time
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
# apply to hyperlinks
GetiCert <- function(applicant = NULL, j){ 
  url <- unname(unlist(applicant[1,'Link to iCert Registry'])) # specify full url
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
  app.comb[,"ETA Case Number":=VisaDoorOutput2[j,"Id"]] # set Id variable
  app.comb <- app.comb[,c("ETA Case Number", "Question", "Answer")]
  question_order <- c("ETA Case Number", unname(unlist(app.comb[,"Question"]))) # store ordering of questions (spread changes ordering for some reason)
  app.comb <- spread(app.comb, Question, Answer) %>% setcolorder(.,question_order)
  # store
  return(app.comb)
}

# loop format for debugging
pb <- progress_bar$new(total = nrow(VisaDoorOutput2))
start_time <- Sys.time() # measure run time
certification.table <- list()
for(i in 27:nrow(VisaDoorOutput2)){
  certification.table[[i]] <- GetiCert(applicant = VisaDoorOutput2[i,], j = i)
  pb$tick()
}
Sys.time()  - start_time

# lapply does not seem to work in this case
#start_time <- Sys.time() # measure run time
#certification.table <- pblapply(1:nrow(VisaDoorOutput2), function(x) GetiCert(applicant = VisaDoorOutput2[x,], i = x))
#Sys.time()  - start_time

# bind data
certification.table <- rbindlist(certification.table, fill = TRUE)
#certification.table[,.N,by="Q-130-6. Country of birth"]
#certification.table[,.N,by="Q-129-5. Country of citizenship"]

# save data
saveRDS(certification.table, "VisaDoorOutputAll3.rds")
write.csv(certification.table, "VisaDoorOutputAll3.csv")








