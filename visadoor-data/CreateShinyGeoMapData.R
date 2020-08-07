rm(list = ls())
library(data.table)
library(magrittr)
library(zipcode)
library(stringi)

# load data
df <- readRDS("/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/VisaDoor/VisaDoorOutputClean.rds")
StateCodes <- readRDS("/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/VisaDoor/StateCodes.rds")
# drop data without postal code (can't be geocoded)
df <- df[!(is.na(df$`Postal Code`)) & !(is.na(df$State)),]
colnames(df)[which(colnames(df)=="State")] <- "Code"
# merge with state names
df <- merge(df, StateCodes, by = "Code", all.x = TRUE)

#######################
# 
# GEOCODES
#
#######################
data(zipcode)
colnames(zipcode) <- c("Postal Code", "City", "State", "Latitude", "Longitude")
df <- merge(df[,-"City"], zipcode[,c("Postal Code", "City", "Latitude", "Longitude")], by = "Postal Code")
# keep variables of interest
df <- df[,c("ID", "Code", "State", "City", "Decision Date", "Priority Date", "Economic Sector", "Occupation Broad",
            "Occupation Narrow", "Avg. Yearly Wage", "Employer", "Max Education", "Mayor", "Edu Institution",
            "Edu City", "Edu Country", "Edu Year", "Years of Experience", "Latitude", "Longitude")]
# upper case
df[,"Employer":= stri_trans_totitle(`Employer`)]
df[,"Edu Institution":= stri_trans_totitle(`Edu Institution`)] 
df[,"Edu City":= stri_trans_totitle(`Edu City`)] 
df[,"Edu Country":= stri_trans_totitle(`Edu Country`)] 
df[,"Economic Sector":= stri_trans_totitle(`Economic Sector`)] 

# education: none to other
df[`Max Education`=="none", "Max Education"] <- "other" 

# economic sector: unclassified to other economic sector
df[`Economic Sector`=="unclassified", "Economic Sector"] <- "other economic sector"

# text to be displayed
df$hover <- with(df, paste0("ID: ", ID,
                            '<br>', "State: ", State,
                            '<br>', "City: ", City,
                            '<br>', "Employer: ", Employer,
                            '<br>', "Avg. Yearly Wage: ", `Avg. Yearly Wage`,
                            '<br>', "Years of Experience: ", `Years of Experience`))

# order by number amount of non-missing info
missing_count <- apply(df[,c("State", "City", "Employer", "Avg. Yearly Wage", "Years of Experience")], 1, function(x) sum(is.na(x)))
df <- df[order(missing_count)]  
# spring cleaning
rm(zipcode, StateCodes, missing_count)
# save data
saveRDS(df, "/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/Shiny/visadoor-app/data/VisaDoorGeoMaps.rds")



