library(tidyr)
library(dplyr)
library(stringr)

IVRS_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)

# Initial split on comma to get City
IVRS_data_transformed <- IVRS_data %>%
                          separate(Client.Location, c('Client.City', 'Client.State.Zip.Code'), ',')

# Secondary split to get State and Zipcode
IVRS_data_transformed <- IVRS_data_transformed %>%
                          separate(Client.State.Zip.Code,c("Client.State","Client.Zip.Code"),sep="\\s+(?=\\S*$)")

# Split Zip code into 5 digit format and extra information
IVRS_data_transformed <- IVRS_data_transformed %>%
                          separate(Client.Zip.Code,c("Client.Zip.Code","Client.Zip.Code.Extra.Info"),sep=5)

# Remove '-' from extra zip code info
IVRS_data_transformed$Client.Zip.Code.Extra.Info <- 
  str_replace_all(IVRS_data_transformed$Client.Zip.Code.Extra.Info, "[^[:alnum:]]", "")


# write to file
write.csv(IVRS_data_transformed, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Closed_Iowa_Vocational_Rehabilitation_Cases.csv", row.names = FALSE)
