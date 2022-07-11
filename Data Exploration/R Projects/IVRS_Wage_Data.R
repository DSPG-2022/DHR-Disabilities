# Load libraries
library(dplyr)

# Get IVRS data (Load in cleaned IVRS data file)
IVRS_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)

# Get relevant fields
IVRS_wage_data <- subset(IVRS_data, select = -c(4,7:10,16:18,21:25,27:31,34:35,37:38,45:46))

# Reorder columns for easier readability
IVRS_wage_data <- select(IVRS_wage_data, c(2,1,4,3,17:21,5:7,8:9,11,13,12,10,15:16,22,14))

# write to file
write.csv(IVRS_wage_data, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/IVRS_Hourly_Wage.csv", row.names = FALSE)
