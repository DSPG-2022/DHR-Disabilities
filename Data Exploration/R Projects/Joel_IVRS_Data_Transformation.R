library(tidyr)
library(dplyr)
library(stringr)
library(zipcodeR)
library(tibble)

IVRS_data_old <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
IVRS_data_new_2020 <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
IVRS_data_new_2021 <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
IVRS_County_Office <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
county_pop <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)


# Initial split on comma to get City
    IVRS_data_transformed <- IVRS_data_old %>%
                              separate('Client Location', c('Client City', 'Client State Zip Code'), ',')
    
    # Remove any leading and trailing whitespace
    IVRS_data_transformed$'Client City' <- str_trim(IVRS_data_transformed$'Client City')

# Secondary split to get State and Zipcode
    IVRS_data_transformed <- IVRS_data_transformed %>%
                              separate('Client State Zip Code',c('Client State','Client Zip Code'),sep="\\s+(?=\\S*$)")
    
    # Remove any leading and trailing whitespace
    IVRS_data_transformed$'Client State' <- str_trim(IVRS_data_transformed$'Client State')

# Split Zip code into 5 digit format and extra information
    IVRS_data_transformed <- IVRS_data_transformed %>%
                              separate('Client Zip Code',c('Client Zip Code','Client Zip Code Extra Info'),sep=5)
    
    # Remove any leading and trailing whitespace
    IVRS_data_transformed$'Client Zip Code' <- str_trim(IVRS_data_transformed$'Client Zip Code')

# Remove '-' from extra zip code info
    IVRS_data_transformed$'Client Zip Code Extra Info' <- 
      str_replace_all(IVRS_data_transformed$'Client Zip Code Extra Info', "[^[:alnum:]]", "")
    
# Remove any leading and trailing whitespace
    IVRS_data_transformed$'Client Zip Code Extra Info' <- str_trim(IVRS_data_transformed$'Client Zip Code Extra Info')
    
    # remove un-needed columns from new IVRS data
    IVRS_data_new_2020_cleaned <- subset(IVRS_data_new_2020, select = -c(39))
    IVRS_data_new_2021_cleaned <- subset(IVRS_data_new_2021, select = -c(39))
    
    # add needed columns to old and new IVRS data
    ClientZipCodeExtraInfo <- rep(NA, dim(IVRS_data_new_2020_cleaned)[1])
    ClientLocationPoint <- rep(NA, dim(IVRS_data_new_2020_cleaned)[1])
    IVRS_data_new_2020_cleaned <- add_column(IVRS_data_new_2020_cleaned, ClientZipCodeExtraInfo, .after = 41) 
    IVRS_data_new_2020_cleaned <- add_column(IVRS_data_new_2020_cleaned, ClientLocationPoint, .after = 42)
    
    ClientZipCodeExtraInfo <- rep(NA, dim(IVRS_data_new_2021_cleaned)[1])
    ClientLocationPoint <- rep(NA, dim(IVRS_data_new_2021_cleaned)[1])
    IVRS_data_new_2021_cleaned <- add_column(IVRS_data_new_2021_cleaned, ClientZipCodeExtraInfo, .after = 41)
    IVRS_data_new_2021_cleaned <- add_column(IVRS_data_new_2021_cleaned, ClientLocationPoint, .after = 42)
    
    HighSchoolDiplomaDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    HighSchoolEquivalencyDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    SpecialEducationCertificateDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    VocationalTechnicalLicenseDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    VocationalTechnicalCertificateDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    OtherDiplomaDegreeCertificateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    AssociateDegreeDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    BachelorsDegreeDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    MastersDegreeDateDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    GraduateDegreeDuringParticipation<- rep(NA, dim(IVRS_data_transformed)[1])
    NumberofBarrierstoEmployment<- rep(NA, dim(IVRS_data_transformed)[1])
    
    
    temp <- add_column(IVRS_data_transformed, HighSchoolDiplomaDateDuringParticipation, .after = 43) 
    temp <- add_column(temp, HighSchoolEquivalencyDateDuringParticipation, .after = 44) 
    temp <- add_column(temp, SpecialEducationCertificateDateDuringParticipation, .after = 45) 
    temp <- add_column(temp, VocationalTechnicalLicenseDateDuringParticipation, .after = 46) 
    temp <- add_column(temp, VocationalTechnicalCertificateDateDuringParticipation, .after = 47) 
    temp <- add_column(temp, OtherDiplomaDegreeCertificateDuringParticipation, .after = 48) 
    temp <- add_column(temp, AssociateDegreeDateDuringParticipation, .after = 49) 
    temp <- add_column(temp, BachelorsDegreeDateDuringParticipation, .after = 50) 
    temp <- add_column(temp, MastersDegreeDateDuringParticipation, .after = 51) 
    temp <- add_column(temp, GraduateDegreeDuringParticipation, .after = 52) 
    temp <- add_column(temp, NumberofBarrierstoEmployment, .after = 53)
    
    IVRS_data_transformed <- temp
    
    # append data
    colnames(IVRS_data_new_2020_cleaned) <- colnames(IVRS_data_transformed)
    colnames(IVRS_data_new_2021_cleaned) <- colnames(IVRS_data_transformed)
    IVRS_Appended_temp <- rbind(IVRS_data_transformed, IVRS_data_new_2020_cleaned)
    IVRS_Appended <- rbind(IVRS_Appended_temp, IVRS_data_new_2021_cleaned)
    
# Renames Iowa county O'Brien to have ' instead of Oâ???TBrien (Double check wording in the data as these characters consistently incorrectly save)
    # !!IMPORTANT!! File needs to be saved using UTF-8 encoding for the special characters to not disappear. 
    #               This can be changed in File -> Save with encoding -> Choose Encoding -> UTF-8
    IVRS_Appended$'Client County'[IVRS_Appended$'Client County' == 'Oâ???TBrien'] <- "O'Brien"   
    
# Changes state names to abbreviations

    stateNames <- rep(NA, dim(IVRS_Appended)[1])
    
    index <- 0
    
    for(state in IVRS_Appended$'Client State'){
      
      state <- str_trim(state)
      index <- index + 1
      
      if(is.na(state)){
        stateNames[index] <- NA
      }
      else if(state == 'ArKS'){
        stateNames[index] <- 'Arkansas'
      }
      else if(state == 'ARKS'){
        stateNames[index] <- 'Arkansas'
      }
      else if(state == 'District of Columbia'){
        stateNames[index] <- state
      }
      else if(state == 'Puerto Rico'){
        stateNames[index] <- NA
      }
      else if(state == 'West VA'){
        stateNames[index] <- 'West Virginia'
      }
      else if(nchar(state) > 2){
        stateNames[index] <- state
      }
      else {
        stateNames[index] <- state.name[grep(state, state.abb)]
      }
    }

    IVRS_Appended$'Client State' <- stateNames
    
 
# Change wage at application and closure values to correctly calculate wage change
    #
    # Note: Wage change is computed by the formula (Wage (Closure) - Wage (Application) = Wage Change).
    #
    #       NA's in the wage data indicates that the client was unemployed. 
    #       
    #       To make the formula accurate, Wage (Application) NA's  are changed to 0 so as to correctly calculate if there was a wage increase. 
    #       (i.e. client A indicates they are unemployed at the start of application [Wage (Application)], and at the end their wage was $15 an hour [Wage (Closure)]. 
    #       If unemployed was left at NA then the formula would return NA since any NA value in an equation defaults to equaling NA in R. However since
    #       it is being set to 0, this will correctly compute a $15 increase in wage change.)
    #
    #       Also vice versa, any 0's in Wage (Closure) were changed to NA so that the formula would output NA to indicate that the wage change
    #       resulted in unemployment.
    #
    
    # change columns to numeric
    IVRS_Appended$'Hourly Wage (Application)' <- as.integer(IVRS_Appended$'Hourly Wage (Application)')
    IVRS_Appended$'Hourly Wage (Closure)' <- as.integer(IVRS_Appended$'Hourly Wage (Closure)')
    IVRS_Appended$'Monthly Wage (Application)' <- as.integer(IVRS_Appended$'Monthly Wage (Application)')
    IVRS_Appended$'Monthly Wage (Closure)' <- as.integer(IVRS_Appended$'Monthly Wage (Closure)')
    IVRS_Appended$'Annual Wage (Application)' <- as.integer(IVRS_Appended$'Annual Wage (Application)')
    IVRS_Appended$'Annual Wage (Closure)' <- as.integer(IVRS_Appended$'Annual Wage (Closure)')
    IVRS_Appended$'Hourly Wage Change' <- as.integer(IVRS_Appended$'Hourly Wage Change')
    IVRS_Appended$'Monthly Wage Change' <- as.integer(IVRS_Appended$'Monthly Wage Change')
    IVRS_Appended$'Annual Wage Change' <- as.integer(IVRS_Appended$'Annual Wage Change')
    
    # hourly
    IVRS_Appended$'Hourly Wage (Application)'[is.na(IVRS_Appended$'Hourly Wage (Application)')] <- 0
    IVRS_Appended$'Hourly Wage (Closure)'[IVRS_Appended$'Hourly Wage (Closure)' == 0] <- NA
    
    #monthly
    IVRS_Appended$'Monthly Wage (Application)'[is.na(IVRS_Appended$'Monthly Wage (Application)')] <- 0
    IVRS_Appended$'Monthly Wage (Closure)'[IVRS_Appended$'Monthly Wage (Closure)' == 0] <- NA
    
    #annually
    IVRS_Appended$'Annual Wage (Application)'[is.na(IVRS_Appended$'Annual Wage (Application)')] <- 0
    IVRS_Appended$'Annual Wage (Closure)'[IVRS_Appended$'Annual Wage (Closure)' == 0] <- NA
    
# Recalculated wage change fields
    IVRS_Appended$'Hourly Wage Change' <- IVRS_Appended$'Hourly Wage (Closure)' - IVRS_Appended$'Hourly Wage (Application)'
    IVRS_Appended$'Monthly Wage Change' <- IVRS_Appended$'Monthly Wage (Closure)' - IVRS_Appended$'Monthly Wage (Application)'
    IVRS_Appended$'Annual Wage Change' <- IVRS_Appended$'Annual Wage (Closure)' - IVRS_Appended$'Annual Wage (Application)'

# Creates new columns for whether there was a wage increase, no change, decrease, or unemployed
    IVRS_Appended <- mutate(IVRS_Appended, 'Hourly Wage Change Category' = case_when(
      IVRS_Appended$'Hourly Wage Change' < 0 ~ "Decrease",
      IVRS_Appended$'Hourly Wage Change' == 0 ~ "No Change",
      IVRS_Appended$'Hourly Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_Appended$'Hourly Wage Change') ~ "Unemployed"))
    IVRS_Appended <- mutate(IVRS_Appended, 'Monthly Wage Change Category' = case_when(
      IVRS_Appended$'Monthly Wage Change' < 0 ~ "Decrease",
      IVRS_Appended$'Monthly Wage Change' == 0 ~ "No Change",
      IVRS_Appended$'Monthly Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_Appended$'Monthly Wage Change') ~ "Unemployed"))
    IVRS_Appended <- mutate(IVRS_Appended, 'Annual Wage Change Category' = case_when(
      IVRS_Appended$'Annual Wage Change' < 0 ~ "Decrease",
      IVRS_Appended$'Annual Wage Change' == 0 ~ "No Change",
      IVRS_Appended$'Annual Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_Appended$'Annual Wage Change') ~ "Unemployed"))

# remove numbers from office areas
    IVRS_Appended$`Office Area` = substr(IVRS_Appended$`Office Area`,1,nchar(IVRS_Appended$`Office Area`)-6)

    
# write to file
    write.csv(IVRS_Appended, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Cleaned_Closed_Iowa_Vocational_Rehabilitation_Cases.csv", row.names = FALSE)
