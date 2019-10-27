#parsing the zips
install.packages("stringr")
library(readxl)
library(stringr)

#import the data
clinic_list <- read_excel("Documents/Documents/Minerva/Jobs+internships+work/2019/ORPRN_PERC/clinic_list.xlsx")
zips <- read_excel("Documents/Documents/Minerva/Jobs+internships+work/2019/ORPRN_PERC/zips_only.xlsx")

# creates a column for zip only
clinic_list$zip <- str_split_fixed(clinic_list$`City and zip`, ", OR ", 2)[,2]

#make a matches list to the zip to county list
clinic_list$matches <- match(clinic_list$zip, zips$zip, nomatch = NULL , incomparables = NULL)

#add a county column based on zip codes (zip code converter spreadsheet)
counter <- 1
clinic_list$county <- "NA"
for (x in clinic_list$matches){
  if (is.na(x) == FALSE){
    clinic_list$county[counter] <- zips$county[x]
  }
  counter <- counter + 1
}

#remove unnecessary columns
clinic_list$matches <- NULL
clinic_list$zip <- NULL
