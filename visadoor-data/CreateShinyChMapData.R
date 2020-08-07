rm(list = ls())
library(data.table)
library(magrittr)

# load data
df <- readRDS("/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/VisaDoor/VisaDoorOutputClean.rds")
StateCodes <- readRDS("/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/VisaDoor/StateCodes.rds")

#######################
# 
# FUNCTIONS
#
#######################

###################################
# name: top_N_by_state
# task: provides top N (according to count) of a user defined variable for a user defined state
# 
# inputs: 
# var_int = variable of interest 
# N = number of top N
# count_data = count data by state-var-int
# state = state
#
# output: a data.table with State Code, State Name, Total Applicants, Top1, Top2,...,TopN
top_N_by_state <- function(var_int, N, count_data, state){ 
  labels <- unlist(count_data[State == state,..var_int])[1:N]
  counts <- unlist(count_data[State == state, "N"])[1:N]
  # create data table for state state
  dt <- data.table("Code" = state, "State" = unlist(StateCodes$State[which(StateCodes$Code==state)]))
  # add to N occupations                    
  for (n in 1:N) {
    dt <- cbind(dt, ifelse(is.na(labels[n]), "", paste(labels[n], counts[n], sep = ": ")))
  }
  # assign column names
  colnames(dt)[3:(2+N)] <- paste0("Top", seq(1,N,1))
  # return
  return(dt)
}

#######################
# 
# TOTAL APPLICATIONS
#
#######################
# total applicants count by state
VisaDoorMaps <- na.omit(df[,.N,by = c("State")])
# get list of missing states (0 applicants)
missing_states <- setdiff(StateCodes$Code, unlist(unique(df$State)))
# fill in missing states
for(s in missing_states){
  VisaDoorMaps <- rbind(VisaDoorMaps, data.table("State" = s, "N" = 0))
}
# rename columns
colnames(VisaDoorMaps) <- c("Code", "TotalApps")
# merge with full state names
VisaDoorMaps <- merge(VisaDoorMaps, StateCodes, by = "Code")
# re-order columns
VisaDoorMaps <- VisaDoorMaps[,c("Code", "State", "TotalApps")]
# sort data
VisaDoorMaps <- VisaDoorMaps[order(TotalApps)]
# create groups by deciles
VisaDoorMaps$Decile <- as.integer(cut(VisaDoorMaps$TotalApps, breaks = quantile(VisaDoorMaps$TotalApps, probs = seq(0, 1, length = 11)), include.lowest = TRUE, labels = 1:10))
# create color pallete (see: http://colorbrewer2.org/#type=sequential&scheme=PuBu&n=9)
VisaDoorMapsPallete <- c("#ffffff", "#eff3ff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b")

#######################
# 
# OCCUPATION BROAD
#
#######################
# total count by occupation-state (ordered by occupation count within state)
occupation_counts <- na.omit(df[,.N,by = c("State", "Occupation Broad")][order(State,-N)])
# get list of missing states
missing_states <- setdiff(StateCodes$Code, unlist(unique(occupation_counts$State)))
# fill in missing states
for(s in missing_states){
  occupation_counts <- rbind(occupation_counts, data.table("State" = s, "Occupation Broad" = NA, "N" = NA))
}
# obs with data on variable of interest
occupation_info <- occupation_counts[,sum(N), by = "State"] %>% set_colnames(c("Code", "InfoSet"))
# top N occupations types
top_occupations <- lapply(StateCodes$Code, function(s) top_N_by_state(var_int = "Occupation Broad", N = 3, count_data = occupation_counts, state = s))
top_occupations <- do.call(rbind, top_occupations)
# rename columns
colnames(top_occupations) <- c("Code", "State", "OccupationBroad1", "OccupationBroad2", "OccupationBroad3")
# merge with VisaDoorMaps
VisaDoorMaps <- merge(VisaDoorMaps, top_occupations[,c("Code", "OccupationBroad1", "OccupationBroad2", "OccupationBroad3")], by = "Code")
VisaDoorMaps <- merge(VisaDoorMaps, occupation_info[,c("Code", "InfoSet")], by = "Code")
# define hover text
VisaDoorMaps$HoverOccupationBroad <- with(VisaDoorMaps, ifelse(TotalApps == 0, 
                                                               paste(State,
                                                              '<br>', "Total Applicants: ", TotalApps),
                                                               paste(State,
                                                              '<br>', "Total Applicants: ", TotalApps,
                                                              '<br>', "Info Set :", InfoSet,
                                                              '<br>', OccupationBroad1,
                                                              '<br>', OccupationBroad2,
                                                              '<br>', OccupationBroad3)))
# keep only hover text
VisaDoorMaps <- VisaDoorMaps[,c("Code", "State", "TotalApps", "Decile", "HoverOccupationBroad")]
# spring cleaning
rm(occupation_counts, occupation_info, top_occupations)

#######################
# 
# OCCUPATION NARROW
#
#######################
# total count by occupation-state (ordered by occupation count within state)
occupation_counts <- na.omit(df[,.N,by = c("State", "Occupation Narrow")][order(State,-N)])
# get list of missing states
missing_states <- setdiff(StateCodes$Code, unlist(unique(occupation_counts$State)))
# fill in missing states
for(s in missing_states){
  occupation_counts <- rbind(occupation_counts, data.table("State" = s, "Occupation Narrow" = NA, "N" = NA))
}
# obs with data on variable of interest
occupation_info <- occupation_counts[,sum(N), by = "State"] %>% set_colnames(c("Code", "InfoSet"))
# top N occupations types
top_occupations <- lapply(StateCodes$Code, function(s) top_N_by_state(var_int = "Occupation Narrow", N = 3, count_data = occupation_counts, state = s))
top_occupations <- do.call(rbind, top_occupations)
# rename columns
colnames(top_occupations) <- c("Code", "State", "OccupationNarrow1", "OccupationNarrow2", "OccupationNarrow3")
# merge with VisaDoorMaps
VisaDoorMaps <- merge(VisaDoorMaps, top_occupations[,c("Code", "OccupationNarrow1", "OccupationNarrow2", "OccupationNarrow3")], by = "Code")
VisaDoorMaps <- merge(VisaDoorMaps, occupation_info[,c("Code", "InfoSet")], by = "Code")
# define hover text
VisaDoorMaps$HoverOccupationNarrow <- with(VisaDoorMaps, ifelse(TotalApps == 0, 
                                                               paste(State,
                                                                     '<br>', "Total Applicants: ", TotalApps),
                                                               paste(State,
                                                                     '<br>', "Total Applicants: ", TotalApps,
                                                                     '<br>', "Info Set: ", InfoSet,
                                                                     '<br>', OccupationNarrow1,
                                                                     '<br>', OccupationNarrow2,
                                                                     '<br>', OccupationNarrow3)))
# keep only hover text
VisaDoorMaps <- VisaDoorMaps[,c("Code", "State", "TotalApps", "Decile", "HoverOccupationBroad", "HoverOccupationNarrow")]
# spring cleaning
rm(occupation_counts, occupation_info, top_occupations)

#######################
# 
# ECONOMIC SECTOR
#
#######################
# total count by economic sector-state (ordered by economic sector count within state)
sector_counts <- na.omit(df[,.N,by = c("State", "Economic Sector")][order(State,-N)])
# get list of missing states
missing_states <- setdiff(StateCodes$Code, unlist(unique(sector_counts$State)))
# fill in missing states
for(s in missing_states){
  sector_counts <- rbind(sector_counts, data.table("State" = s, "Economic Sector" = NA, "N" = NA))
}
# obs with data on variable of interest
sector_info <- sector_counts[,sum(N), by = "State"] %>% set_colnames(c("Code", "InfoSet"))
# top N sectors
top_sectors <- lapply(StateCodes$Code, function(s) top_N_by_state(var_int = "Economic Sector", N = 3, count_data = sector_counts, state = s))
top_sectors <- do.call(rbind, top_sectors)
# rename columns
colnames(top_sectors) <- c("Code", "State", "EconomicSector1", "EconomicSector2", "EconomicSector3")
# merge with VisaDoorMaps
VisaDoorMaps <- merge(VisaDoorMaps, top_sectors[,c("Code", "EconomicSector1", "EconomicSector2", "EconomicSector3")], by = "Code")
VisaDoorMaps <- merge(VisaDoorMaps, sector_info[,c("Code", "InfoSet")], by = "Code")
# define hover text
VisaDoorMaps$HoverEconomicSector <- with(VisaDoorMaps, ifelse(TotalApps == 0, 
                                                                paste(State,
                                                                      '<br>', "Total Applicants: ", TotalApps),
                                                                paste(State,
                                                                      '<br>', "Total Applicants: ", TotalApps,
                                                                      '<br>', "Info Set: ", InfoSet,
                                                                      '<br>', EconomicSector1,
                                                                      '<br>', EconomicSector2,
                                                                      '<br>', EconomicSector3)))
# keep only hover text
VisaDoorMaps <- VisaDoorMaps[,c("Code", "State", "TotalApps", "Decile", "HoverOccupationBroad", "HoverOccupationNarrow", "HoverEconomicSector")]
# spring cleaning
rm(sector_counts, sector_info, top_sectors)

#######################
# 
# EDUCATION
#
#######################
# change "none" to "other"
df$`Max Education`[df$`Max Education`=="none"] <- "other"
# total count by education-state (ordered by education count within state)
education_counts <- na.omit(df[,.N,by = c("State", "Max Education")][order(State,-N)])
# get list of missing states
missing_states <- setdiff(StateCodes$Code, unlist(unique(education_counts$State)))
# fill in missing states
for(s in missing_states){
  education_counts <- rbind(education_counts, data.table("State" = s, "Max Education" = NA, "N" = NA))
}
# adjust column names
colnames(education_counts) <- c("Code", "Max Education", "N")
# obs with data on variable of interest
education_info <- education_counts[,sum(N), by = "Code"] %>% set_colnames(c("Code", "InfoSet"))
# convert to wide format
education_counts <- dcast(education_counts, Code ~ `Max Education`, value.var = "N") 
# merge with VisaDoorMaps
VisaDoorMaps <- merge(VisaDoorMaps, education_counts[,1:7], by = "Code")
VisaDoorMaps <- merge(VisaDoorMaps, education_info[,c("Code", "InfoSet")], by = "Code")
# define hover text
VisaDoorMaps$HoverEducation <- with(VisaDoorMaps, ifelse(TotalApps == 0, 
                                                                paste(State,
                                                                      '<br>', "Total Applicants: ", TotalApps),
                                                                paste(State,
                                                                      '<br>', "Total Applicants: ", TotalApps,
                                                                      '<br>', "Info Set: ", InfoSet,
                                                                      '<br>', "High school: ", `high school`,
                                                                      '<br>', "Associate's: ", `associate's`,
                                                                      '<br>', "Bachelor's: " , `bachelor's`,
                                                                      '<br>', "Master's: " , `master's`,
                                                                      '<br>', "Doctorate: ", `doctorate`,
                                                                      '<br>', "Other: ", `other`)))
# keep only hover text
VisaDoorMaps <- VisaDoorMaps[,c("Code", "State", "TotalApps", "Decile", "HoverOccupationBroad", "HoverOccupationNarrow", "HoverEconomicSector", "HoverEducation")]
# spring cleaning
rm(education_counts, education_info)

#######################
# 
# VISA STATUS
#
#######################
# total count by visa status-state (ordered by visa status count within state)
status_counts <- na.omit(df[,.N,by = c("State", "Visa Status")][order(State,-N)])
# get list of missing states
missing_states <- setdiff(StateCodes$Code, unlist(unique(status_counts$State)))
# fill in missing states
for(s in missing_states){
  status_counts <- rbind(status_counts, data.table("State" = s, "Visa Status" = NA, "N" = NA))
}
# adjust column names
colnames(status_counts) <- c("Code", "Visa Status", "N")
# obs with data on variable of interest
status_info <- status_counts[,sum(N), by = "Code"] %>% set_colnames(c("Code", "InfoSet"))
# convert to wide format
status_counts <- dcast(status_counts, Code ~ `Visa Status`, value.var = "N") 
# merge with VisaDoorMaps
VisaDoorMaps <- merge(VisaDoorMaps, status_counts[,1:5], by = "Code")
VisaDoorMaps <- merge(VisaDoorMaps, status_info[,c("Code", "InfoSet")], by = "Code")
# define hover text
VisaDoorMaps$HoverVisaStatus <- with(VisaDoorMaps, ifelse(TotalApps == 0, 
                                                         paste(State,
                                                               '<br>', "Total Applicants: ", TotalApps),
                                                         paste(State,
                                                               '<br>', "Total Applicants: ", TotalApps,
                                                               '<br>', "Info Set: ", InfoSet,
                                                               '<br>', "Certified: ", certified,
                                                               '<br>', "Certified-Expired: ", `certified-expired`,
                                                               '<br>', "Denied: ", `denied`,
                                                               '<br>', "Withdrawn: " , `withdrawn`)))
# keep only hover text
VisaDoorMaps <- VisaDoorMaps[,c("Code", "State", "TotalApps", "Decile", "HoverOccupationBroad", "HoverOccupationNarrow", "HoverEconomicSector", "HoverEducation", "HoverVisaStatus")]
# spring cleaning
rm(status_counts, status_info)
# save data
saveRDS(VisaDoorMaps, "/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/Shiny/visadoor-app/data/VisaDoorChMaps.rds")



