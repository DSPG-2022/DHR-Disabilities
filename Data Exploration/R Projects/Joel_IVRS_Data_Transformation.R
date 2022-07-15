library(tidyr)
library(dplyr)
library(stringr)
library(zipcodeR)

IVRS_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
IVRS_County_Office <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
county_pop <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)


# Initial split on comma to get City
    IVRS_data_transformed <- IVRS_data %>%
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
    
# Renames Iowa county O'Brien to have ' instead of Oâ€™Brien
    # !!IMPORTANT!! File needs to be saved using UTF-8 encoding for the special characters to not disappear. 
    #               This can be changed in File -> Save with encoding -> Choose Encoding -> UTF-8
    IVRS_data_transformed$'Client County'[IVRS_data_transformed$'Client County' == 'Oâ€™Brien'] <- "O'Brien"   
    
## !!!NOT WORKING!!! ##
# Changes state names to abbreviations

    stateAbbreviations <- rep(NA, dim(IVRS_data_transformed)[1])
    
    index <- 0
    
    for(state in IVRS_data_transformed$'Client State'){
      
      state <- str_trim(state)
      index <- index + 1
      
      if(is.na(state)){
        stateAbbreviations[index] <- NA
      }
      else if(state == 'ArKS'){
        stateAbbreviations[index] <- 'AR'
      }
      else if(state == 'ARKS'){
        stateAbbreviations[index] <- 'AR'
      }
      else if(state == 'District of Columbia'){
        stateAbbreviations[index] <- 'DC'
      }
      else if(state == 'Puerto Rico'){
        stateAbbreviations[index] <- NA
      }
      else if(state == 'West VA'){
        stateAbbreviations[index] <- 'WV'
      }
      else if(nchar(state) > 2){
        stateAbbreviations[index] <- state.abb[grep(state, state.name)]
      }
      else {
        stateAbbreviations[index] <- state
      }
    }

 IVRS_data_transformed$'Client State' <- stateAbbreviations
    
    
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
    
    # hourly
    IVRS_data_transformed$'Hourly Wage (Application)'[is.na(IVRS_data_transformed$'Hourly Wage (Application)')] <- 0
    IVRS_data_transformed$'Hourly Wage (Closure)'[IVRS_data_transformed$'Hourly Wage (Closure)' == 0] <- NA
    
    #monthly
    IVRS_data_transformed$'Monthly Wage (Application)'[is.na(IVRS_data_transformed$'Monthly Wage (Application)')] <- 0
    IVRS_data_transformed$'Monthly Wage (Closure)'[IVRS_data_transformed$'Monthly Wage (Closure)' == 0] <- NA
    
    #annually
    IVRS_data_transformed$'Annual Wage (Application)'[is.na(IVRS_data_transformed$'Annual Wage (Application)')] <- 0
    IVRS_data_transformed$'Annual Wage (Closure)'[IVRS_data_transformed$'Annual Wage (Closure)' == 0] <- NA
    
# Recalculated wage change fields
    IVRS_data_transformed$'Hourly Wage Change' <- IVRS_data_transformed$'Hourly Wage (Closure)' - IVRS_data_transformed$'Hourly Wage (Application)'
    IVRS_data_transformed$'Monthly Wage Change' <- IVRS_data_transformed$'Monthly Wage (Closure)' - IVRS_data_transformed$'Monthly Wage (Application)'
    IVRS_data_transformed$'Annual Wage Change' <- IVRS_data_transformed$'Annual Wage (Closure)' - IVRS_data_transformed$'Annual Wage (Application)'

# Creates new columns for whether there was a wage increase, no change, decrease, or unemployed
    IVRS_data_transformed <- mutate(IVRS_data_transformed, 'Hourly Wage Change Category' = case_when(
                                                                                            IVRS_data_transformed$'Hourly Wage Change' < 0 ~ "Decrease",
                                                                                            IVRS_data_transformed$'Hourly Wage Change' == 0 ~ "No Change",
                                                                                            IVRS_data_transformed$'Hourly Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_data_transformed$'Hourly Wage Change') ~ "Unemployed"))
    IVRS_data_transformed <- mutate(IVRS_data_transformed, 'Monthly Wage Change Category' = case_when(
                                                                                            IVRS_data_transformed$'Monthly Wage Change' < 0 ~ "Decrease",
                                                                                            IVRS_data_transformed$'Monthly Wage Change' == 0 ~ "No Change",
                                                                                            IVRS_data_transformed$'Monthly Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_data_transformed$'Monthly Wage Change') ~ "Unemployed"))
    IVRS_data_transformed <- mutate(IVRS_data_transformed, 'Annual Wage Change Category' = case_when(
                                                                                            IVRS_data_transformed$'Annual Wage Change' < 0 ~ "Decrease",
                                                                                            IVRS_data_transformed$'Annual Wage Change' == 0 ~ "No Change",
                                                                                            IVRS_data_transformed$'Annual Wage Change' > 0 ~ "Increase",
                                                                                            is.na(IVRS_data_transformed$'Annual Wage Change') ~ "Unemployed"))
    
# drop uneeded population columns
    county_pop_transformed <- subset(county_pop, select = -c(9:65,68))

# change population data to long
    county_pop_transformed <- county_pop_transformed %>%
      pivot_longer(
        -c(1:7),
        names_to = "Census",
        values_to = "Population"
      )
    
# split to get Year from Category
    county_pop_transformed <- county_pop_transformed %>%
      separate(Census,c('Census', 'Year'),sep="(?<=\\w)(?=[0-9]{4})")

# remove numbers from office areas
    IVRS_data_transformed$`Office Area` = substr(IVRS_data_transformed$`Office Area`,1,nchar(IVRS_data_transformed$`Office Area`)-6)

    
# write to file
    write.csv(IVRS_data_transformed, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Cleaned_Closed_Iowa_Vocational_Rehabilitation_Cases.csv", row.names = FALSE)
    write.csv(county_pop_transformed, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/2008_2019_county_pop_long.csv", row.names = FALSE)
    
