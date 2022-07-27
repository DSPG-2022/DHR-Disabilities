library(tidyr)
library(dplyr)
library(stringr)
library(zipcodeR)
library(tibble)

IVRS_data_old <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
IVRS_data_new_2020 <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE,fileEncoding="UTF-8-BOM")
IVRS_data_new_2021 <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
IVRS_County_Office <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)


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

# Append 2021 to 2020 data
    appended <- rbind(IVRS_data_new_2021, IVRS_data_new_2020)
    
# left join data including only new columns
    IVRS_merged <- left_join(IVRS_data_transformed, select(IVRS_data_new_2020, c(2,43:53)), by = c("Case ID" = "CaseID"))
    
# Renames Iowa county O'Brien to have ' instead of Oâ€™Brien (Double check wording in the data as these characters consistently incorrectly save)
    # !!IMPORTANT!! File needs to be saved using UTF-8 encoding for the special characters to not disappear. 
    #               This can be changed in File -> Save with encoding -> Choose Encoding -> UTF-8
    IVRS_merged$'Client County'[IVRS_merged$'Client County' == 'Oâ€™Brien'] <- "O'Brien"   
    
# Changes state names to abbreviations

    stateNames <- rep(NA, dim(IVRS_merged)[1])
    
    index <- 0
    
    for(state in IVRS_merged$'Client State'){
      
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

    IVRS_merged$'Client State' <- stateNames
    
 
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
    IVRS_merged$'Hourly Wage (Application)' <- as.integer(IVRS_merged$'Hourly Wage (Application)')
    IVRS_merged$'Hourly Wage (Closure)' <- as.integer(IVRS_merged$'Hourly Wage (Closure)')
    IVRS_merged$'Monthly Wage (Application)' <- as.integer(IVRS_merged$'Monthly Wage (Application)')
    IVRS_merged$'Monthly Wage (Closure)' <- as.integer(IVRS_merged$'Monthly Wage (Closure)')
    IVRS_merged$'Annual Wage (Application)' <- as.integer(IVRS_merged$'Annual Wage (Application)')
    IVRS_merged$'Annual Wage (Closure)' <- as.integer(IVRS_merged$'Annual Wage (Closure)')
    IVRS_merged$'Hourly Wage Change' <- as.integer(IVRS_merged$'Hourly Wage Change')
    IVRS_merged$'Monthly Wage Change' <- as.integer(IVRS_merged$'Monthly Wage Change')
    IVRS_merged$'Annual Wage Change' <- as.integer(IVRS_merged$'Annual Wage Change')
    
    # hourly
    IVRS_merged$'Hourly Wage (Application)'[is.na(IVRS_merged$'Hourly Wage (Application)')] <- 0
    IVRS_merged$'Hourly Wage (Closure)'[IVRS_merged$'Hourly Wage (Closure)' == 0] <- NA
    
    #monthly
    IVRS_merged$'Monthly Wage (Application)'[is.na(IVRS_merged$'Monthly Wage (Application)')] <- 0
    IVRS_merged$'Monthly Wage (Closure)'[IVRS_merged$'Monthly Wage (Closure)' == 0] <- NA
    
    #annually
    IVRS_merged$'Annual Wage (Application)'[is.na(IVRS_merged$'Annual Wage (Application)')] <- 0
    IVRS_merged$'Annual Wage (Closure)'[IVRS_merged$'Annual Wage (Closure)' == 0] <- NA
    
# Recalculated wage change fields
    IVRS_merged$'Hourly Wage Change' <- IVRS_merged$'Hourly Wage (Closure)' - IVRS_merged$'Hourly Wage (Application)'
    IVRS_merged$'Monthly Wage Change' <- IVRS_merged$'Monthly Wage (Closure)' - IVRS_merged$'Monthly Wage (Application)'
    IVRS_merged$'Annual Wage Change' <- IVRS_merged$'Annual Wage (Closure)' - IVRS_merged$'Annual Wage (Application)'

# Creates new columns for whether there was a wage increase, no change, decrease, or unemployed
    IVRS_merged <- mutate(IVRS_merged, 'Hourly Wage Change Category' = case_when(
      IVRS_merged$'Hourly Wage Change' < 0 ~ "Decrease",
      IVRS_merged$'Hourly Wage Change' == 0 ~ "No Change",
      IVRS_merged$'Hourly Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_merged$'Hourly Wage Change') ~ "Unemployed"))
    IVRS_merged <- mutate(IVRS_merged, 'Monthly Wage Change Category' = case_when(
      IVRS_merged$'Monthly Wage Change' < 0 ~ "Decrease",
      IVRS_merged$'Monthly Wage Change' == 0 ~ "No Change",
      IVRS_merged$'Monthly Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_merged$'Monthly Wage Change') ~ "Unemployed"))
    IVRS_merged <- mutate(IVRS_merged, 'Annual Wage Change Category' = case_when(
      IVRS_merged$'Annual Wage Change' < 0 ~ "Decrease",
      IVRS_merged$'Annual Wage Change' == 0 ~ "No Change",
      IVRS_merged$'Annual Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_merged$'Annual Wage Change') ~ "Unemployed"))

# remove numbers from office areas
    IVRS_merged$`Office Area` = substr(IVRS_merged$`Office Area`,1,nchar(IVRS_merged$`Office Area`)-6)

    
# write to file
    write.csv(IVRS_merged, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/IVRS Data/Cleaned_Closed_Iowa_Vocational_Rehabilitation_Cases_New_Data.csv", row.names = FALSE)
