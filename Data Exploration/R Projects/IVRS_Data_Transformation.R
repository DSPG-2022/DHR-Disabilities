library(tidyr)
library(dplyr)
library(stringr)
library(zipcodeR)
IVRS_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)

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
    
## !!!NOT WORKING!!! ##
# Changes state names to abbreviations
#
#    stateAbbreviations <- c()
#    
#    for(state in IVRS_data_transformed$'Client State'){
#      
#      state <- str_trim(state)
#      
#      if(is.na(state)){
#        stateAbbreviations <- append(stateAbbreviations, NA)
#      }
#      else if(state == 'ArKS'){
#        stateAbbreviations <- append(stateAbbreviations, 'AR')
#      }
#      else if(state == 'ARKS'){
#        stateAbbreviations <- append(stateAbbreviations, 'AR')
#      }
#      else if(state == 'District of Columbia'){
#        stateAbbreviations <- append(stateAbbreviations, 'DC')
#      }
#      else if(state == 'Puerto Rico'){
#        stateAbbreviations <- append(stateAbbreviations, NA)
#      }
#      else if(nchar(state) > 2){
#        stateAbbreviations <- append(stateAbbreviations, state.abb[grep(state, state.name)])
#      }
#      else {
#        stateAbbreviations <- append(stateAbbreviations, state)
#      }
#    }
#
# IVRS_data_transformed$'Client State' <- stateAbbreviations
    
    
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
    
# write to file
    write.csv(IVRS_data_transformed, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Cleaned_Closed_Iowa_Vocational_Rehabilitation_Cases.csv", row.names = FALSE)
