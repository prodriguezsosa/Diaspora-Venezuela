rm(list = ls())
library(data.table)
library(stringi)

# load data
df <- readRDS("/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/VisaDoor/VisaDoorOutputClean.rds")
# keep relevant variables
df <- df[,c("ID","Employer")]
# upper case
df[,"Employer":= stri_trans_totitle(`Employer`)]
# replace spaces with _
df[,"Employer":= gsub(" ", "_", `Employer`)]
# count frquency
df <- df[,.N, by = `Employer`] %>% set_colnames(c("word", "freq")) %>% data.frame(., stringsAsFactors = FALSE)
# save data
saveRDS(df, "/Users/pedrorodriguez/Dropbox/Venezuela/DiasporaVenezuela/Shiny/visadoor-app/data/VisaDoorEmployerFreq.rds")