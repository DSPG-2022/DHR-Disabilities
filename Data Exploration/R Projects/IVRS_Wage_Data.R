# Load libraries
library(dplyr)

# Get IVRS data (Load in cleaned IVRS data file)
IVRS_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)

# Get relevant fields
IVRS_wage_data <- subset(IVRS_data, select = -c(4,7:10,18,21:25,27:31))

# Reorder columns for easier readability
IVRS_wage_data <- select(IVRS_wage_data, c(2,1,4,3,23:27,5:8,13:14,9,15:16,10:12,17:22,28:30))

# write to file
write.csv(IVRS_wage_data, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/IVRS_Hourly_Wage.csv", row.names = FALSE)
