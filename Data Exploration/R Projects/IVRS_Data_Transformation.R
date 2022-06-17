library(tidyr)
library(dplyr)

IVRS_data <- read.csv("C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Closed_Iowa_Vocational_Rehabilitation_Cases.csv", header=TRUE, stringsAsFactors=FALSE)

# Initial split on comma to get City
IVRS_data_transformed <- IVRS_data %>%
                          separate(Client.Location, c('Client.City', 'Client.State.Zip.Code'), ',')

# Secondary split to get State and Zipcode
IVRS_data_transformed <- IVRS_data_transformed %>%
                          separate(Client.State.Zip.Code,c("Client.State","Client.Zip.Code"),sep="\\s+(?=\\S*$)")

# Split Zip code into 5 digit format and extra information


write.csv(IVRS_data_transformed, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Cleaned_Closed_Iowa_Vocational_Rehabilitation_Cases.csv")
