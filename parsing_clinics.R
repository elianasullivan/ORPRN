#install packages
install.packages("dplyr")
install.packages("library")
install.packages("stylo")
install.packages("tidyverse")
library(readxl)
library(dplyr)
library(stylo)
library(tidyverse)

# import data
family_med <- read_excel("H:/data/care oregon clinics_family.xlsx")
internal_med <- read_excel("H:/data/care_oregon_clinics_im.xlsx")
orprn_practices <- read_excel("H:/data/orprn_practices.xlsx")


# combine the family medicine and internal medicine clinic lists
care_oregon_clinics <- rbind(family_med, internal_med)

# make clean data frame
n_rows <- nrow(care_oregon_clinics)
clean_data <- data.frame(matrix(0, ncol = 12, nrow = n_rows/8))
colnames(clean_data) <- c("Clinic or Clinician", "Address","City and zip", "Phone number", "Gender","Specialties","Type", "Accepting new?", "Duplicate address?", "orprn_name", "orprn_worked","orprn_projects")

# set iterators
total = 1
col_spot = 1
row_spot = 1

# iterate through the list and put into new data frame
while (total <= n_rows){
  clean_data[row_spot, col_spot] <- care_oregon_clinics[total,]
  total <- total + 1
  col_spot <- col_spot + 1
  if (col_spot == 9) {
    col_spot = 1
    row_spot <- row_spot+ 1
  }
}

# clean it up: remove duplicates and extra words
clean_data <- distinct(clean_data)
clean_data <- as.data.frame(sapply(clean_data,gsub,pattern="Type:",replacement=""))
clean_data <- as.data.frame(sapply(clean_data,gsub,pattern="Gender:",replacement=""))
clean_data <- as.data.frame(sapply(clean_data,gsub,pattern="Specialties:",replacement=""))
clean_data <- as.data.frame(sapply(clean_data,gsub,pattern="Accepting New Members:",replacement=""))

#make a new data frame with no duplicated addresses
no_duplicate_address <- clean_data[!duplicated(clean_data$Address), ]

#adds a column that says TRUE if there's at least one other entry with the exact same address and FALSE if not
clean_data$`Duplicate address?` <- duplicated(clean_data$Address) | duplicated(clean_data$Address, fromLast = TRUE)

#make something to mark if their address matches ours (go through the list of those we have worked with)
orprn_practices$matches <- match(orprn_practices$`Street Address`, clean_data$Address, nomatch = NULL , incomparables = NULL)

#add associated columns of matching addresses to the clean data
clean_data$orprn_name <- "NA"
clean_data$orprn_projects <- "NA"
clean_data$orprn_worked <- "no"
counter <- 1
for (x in orprn_practices$matches){
  if (is.na(x) == FALSE){
    clean_data$orprn_name[x] <- orprn_practices$Practice[counter]
    clean_data$orprn_projects[x] <- orprn_practices$`Participating Project(s)`[counter]
    clean_data$orprn_worked[x] <- "yes"
  }
  counter <- counter + 1
}

#make a column with the index of the first match, delete later after use
clean_data$matches <- match(clean_data$Address, clean_data$Address, nomatch = NULL , incomparables = NULL)

# fill in the info for all matching addresses
y <- 1
while (y <= nrow(clean_data)){
  if ((duplicated(clean_data$Address)[y]) == TRUE){
    clean_data$orprn_name[y] <- clean_data$orprn_name[clean_data$matches[y]]
    clean_data$orprn_worked[y] <- clean_data$orprn_worked[clean_data$matches[y]]
    clean_data$orprn_projects[y] <- clean_data$orprn_projects[clean_data$matches[y]]
  } 
  y <- y+1
}

#make something to mark if their address matches ours, for no duplicate addresses (go through the list of those we have worked with)
orprn_practices$no_dup_matches <- match(orprn_practices$`Street Address`, no_duplicate_address$Address, nomatch = NULL , incomparables = NULL)

#add associated columns of matching addresses to the clean data
no_duplicate_address$orprn_name <- "NA"
no_duplicate_address$orprn_projects <- "NA"
no_duplicate_address$orprn_worked <- "no"
counter <- 1
for (x in orprn_practices$no_dup_matches){
  if (is.na(x) == FALSE){
    no_duplicate_address$orprn_name[x] <- orprn_practices$Practice[counter]
    no_duplicate_address$orprn_projects[x] <- orprn_practices$`Participating Project(s)`[counter]
    no_duplicate_address$orprn_worked[x] <- "yes"
  }
  counter <- counter + 1
}

#the number of unique places ORPRN has worked with before
sum(no_duplicate_address$orprn_worked == "yes")
sum(no_duplicate_address$orprn_worked == "no")

#REMOVE MATCHES FROM CLEAN DATA
clean_data$matches <- NULL

# write the clean data to a spreadsheet
write.csv(clean_data, "CO_Clinics_3.csv")

# write the no address duplicates to a spreadsheet
write.csv(clean_data, "no_duplicate_addresses.csv")

#haven't yet added in all of the ones that didn't match
#would want to check with other lists too before integrating ideally
