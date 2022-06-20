library(tidyr)
library(dplyr)
library(stringr)

IVRS_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)

# Initial split on comma to get City
    IVRS_data_transformed <- IVRS_data %>%
                              separate('Client Location', c('Client City', 'Client State Zip Code'), ',')

# Secondary split to get State and Zipcode
    IVRS_data_transformed <- IVRS_data_transformed %>%
                              separate('Client State Zip Code',c('Client State','Client Zip Code'),sep="\\s+(?=\\S*$)")

# Split Zip code into 5 digit format and extra information
    IVRS_data_transformed <- IVRS_data_transformed %>%
                              separate('Client Zip Code',c('Client Zip Code','Client Zip Code Extra Info'),sep=5)

# Remove '-' from extra zip code info
    IVRS_data_transformed$'Client Zip Code Extra Info' <- 
      str_replace_all(IVRS_data_transformed$'Client Zip Code Extra Info', "[^[:alnum:]]", "")

# Change wage at application and closure values to NA if 0
    # hourly
    IVRS_data_transformed$'Hourly Wage (Application)'[IVRS_data_transformed$'Hourly Wage (Application)' == 0] <- NA
    IVRS_data_transformed$'Hourly Wage (Closure)'[IVRS_data_transformed$'Hourly Wage (Closure)' == 0] <- NA
    
    #monthly
    IVRS_data_transformed$'Monthly Wage (Application)'[IVRS_data_transformed$'Monthly Wage (Application)' == 0] <- NA
    IVRS_data_transformed$'Monthly Wage (Closure)'[IVRS_data_transformed$'Monthly Wage (Closure)' == 0] <- NA
    
    #annually
    IVRS_data_transformed$'Annual Wage (Application)'[IVRS_data_transformed$'Annual Wage (Application)' == 0] <- NA
    IVRS_data_transformed$'Annual Wage (Closure)'[IVRS_data_transformed$'Annual Wage (Closure)' == 0] <- NA
    
# Recalculated wage change fields
    IVRS_data_transformed$'Hourly Wage Change' <- IVRS_data_transformed$'Hourly Wage (Closure)' - IVRS_data_transformed$'Hourly Wage (Application)'
    IVRS_data_transformed$'Monthly Wage Change' <- IVRS_data_transformed$'Monthly Wage (Closure)' - IVRS_data_transformed$'Monthly Wage (Application)'
    IVRS_data_transformed$'Annual Wage Change' <- IVRS_data_transformed$'Annual Wage (Closure)' - IVRS_data_transformed$'Annual Wage (Application)'

# Creates new columns for whether there was a wage increase or not
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
