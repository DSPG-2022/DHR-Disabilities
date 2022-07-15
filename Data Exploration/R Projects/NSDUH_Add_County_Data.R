library(dplyr)

# read in data
county_regions <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE, fileEncoding="UTF-8-BOM")
nh_21_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
nh_22_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
nh_23_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)

# filter iowa
df1 <- nh_21_data %>% filter(State == "Iowa" & `Substate Region` != "Iowa")
df2 <- nh_22_data %>% filter(State == "Iowa" & `Substate Region` != "Iowa")
df3 <- nh_23_data %>% filter(State == "Iowa" & `Substate Region` != "Iowa")

# join data
df1_new <- merge(x = df1, y = county_regions, by = "Substate Region", all.x = TRUE)
df2_new <- merge(x = df2, y = county_regions, by = "Substate Region", all.x = TRUE)
df3_new <- merge(x = df3, y = county_regions, by = "Substate Region", all.x = TRUE)

# drop duplicate columns
df1_new <- subset(df1_new, select = -c(6:7))
df2_new <- subset(df2_new, select = -c(6:7))
df3_new <- subset(df3_new, select = -c(6:7))

# rename state column
names(df1_new)[names(df1_new) == 'State.x'] <- 'State'
names(df2_new)[names(df2_new) == 'State.x'] <- 'State'
names(df3_new)[names(df3_new) == 'State.x'] <- 'State'

# write to file
write.csv(df1_new, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Cleaned_NSDUHsubstateExcelTab21-2020.csv", row.names = FALSE)
write.csv(df2_new, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Cleaned_NSDUHsubstateExcelTab22-2020.csv", row.names = FALSE)
write.csv(df3_new, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/Cleaned_NSDUHsubstateExcelTab23-2020.csv", row.names = FALSE)
