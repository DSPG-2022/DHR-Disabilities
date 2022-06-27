# Credit: This code has been copied and modified from Xingray Xue's Xingrui_ACS_EMPLY.Rmd file

library(tidycensus)
library(tidyverse)

# need these variables for 1 year
acs1 <- load_variables(2008, "acs1")
View(acs1)

DisabledType <- get_acs(geography = "county",
                              variables = "B18101_001",
                              year = 2008,
                              state = "IA",
                              survey = "acs1")

DisabledType <- get_acs(geography = "county",
                        variables = "B18101_001",
                        year = 2010,
                        state = "IA",
                        survey = "acs1")
B18101_001

DisabledHearing <- c("5 to 17 years" = c("B18102_007", "B18102_026"), "18 to 34 years" = c("B18102_010", "B18102_029"), "35 to 64 years" = c("B18102_013", "B18102_032"))
DisabledVision <- c("5 to 17 years" = c("B18103_007", "B18103_026"), "18 to 34 years" = c("B18103_010", "B18103_029"), "35 to 64 years" = c("B18103_007", "B18103_032"))
DisabledCognitive <- c("5 to 17 years" = c("B18104_004", "B18104_020"), "18 to 34 years" = c("B18104_007", "B18104_023"), "35 to 64 years" = c("B18104_010", "B18104_026"))
DisabledAmbulatory <- c("5 to 17 years" = c("B18105_004", "B18105_020"), "18 to 34 years" = c("B18105_007", "B18105_023"), "35 to 64 years" = c("B18105_010", "B18105_026"))
DisabledSelfCare <- c("5 to 17 years" = c("B18106_004", "B18106_020"), "18 to 34 years" = c("B18106_007", "B18106_023"), "35 to 64 years" = c("B18106_010", "B18106_026"))
DisabledIndependentLiving <- c("5 to 17 years" = c("B18107_004", "B18107_020"), "18 to 34 years" = c("B18107_007", "B18107_023"), "35 to 64 years" = c("B18107_010", "B18107_026"))

DisabledType <- get_decennial(geography = "county",
                        variables = c(Hearing = DisabledHearing, Vision = DisabledVision, Cognitive = DisabledCognitive, "Self-care" = DisabledSelfCare, "Independent living" = DisabledIndependentLiving),
                        year = 2008,
                        state = "IA",
                        sumfile = "sf3")

DisabledType$variable <- gsub("(years).*", "\\1", DisabledType$variable)

DisabledType <- DisabledType %>%
  group_by(NAME, variable) %>%
  summarise(estimate = sum(estimate),
            moe = sum(moe))

# Population of Iowans 5-64 years of age with disabilities (Disaggregate by difficulty type)

DisabledType_pivot1 <- DisabledType %>% separate(variable, into = c("Type", "Age"), sep = "\\.") %>%
  pivot_wider(names_from = Age, values_from = c(estimate, moe))

DisabledType_sum1 <- aggregate(estimate ~ NAME, data = DisabledType, sum) %>%
  slice(rep(1:n(), each = 5))

DisabledType_pivot1 <- DisabledType_pivot1 %>%
  ungroup() %>%
  mutate(Percentage = rowSums(DisabledType_pivot1[3:5] / DisabledType_sum1$estimate) * 100)

names(DisabledType_pivot1) <- gsub("estimate_", "", names(DisabledType_pivot1))
DisabledType_pivot1$NAME <- gsub(" County, Iowa", "", DisabledType_pivot1$NAME)
names(DisabledType_pivot1)[names(DisabledType_pivot1) == "NAME"] <- "County"

# write.csv(DisabledType_pivot1, "DisabledType1.csv")