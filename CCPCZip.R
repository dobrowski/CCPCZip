

# https://www.elneedsassessment.org/NeedsAssessment.aspx?reportType=schoolage
# 
# Login: CCCCA2324
# Password: CCcca2324+




#### Libraries -------

library(tidyverse)
library(here)
library(vroom)
library(readxl)


# Send from Laurie who got it from Department of Social Services Website (I have attached the excel spreadsheet for all California Large Licensed Homes):
# https://www.ccld.dss.ca.gov/carefacilitysearch/DownloadData

large.homes <- vroom(here("data","ChildCareCenters01282024.csv"), .name_repair = ~ janitor::make_clean_names(., case = "snake")) %>%
    filter(str_detect(county_name, "MONTEREY"),
           facility_status != "CLOSED")

# Early Learning Needs Assessment Tool (ELNAT) website: 
# https://www.elneedsassessment.org/
# Custom variables, select all 

#full <- "ELNeedsAssessment_20240202191043.xlsx" # "A4:HV40" 
needed <- "ELNeedsAssessment_20240205123445.xlsx"
preschool <- "ELNeedsAssessment_20240227192524.xlsx"

# el.needs.full  <- read_xlsx(here("data", full), range = "A4:HV40", sheet = "working")  %>%
#     pivot_longer(`children (one-year est from ACS) - 3 year olds`:`children in CDE-administered programs in licensed exempt settings - 3, 4, & 5 year olds`) %>%
#   # pivot_longer(`Number of children (five-year estimates from American Community Survey) - 3 year olds` :`Percent unmet need-based on income-eligibility and need for care - 3, 4, & 5 year olds`,
#   #              values_transform = list(value = as.numeric) ) %>%
#     separate_wider_delim(name, " - ", names = c("variable", "age")) %>%
#     pivot_wider(names_from = age, values_from = value)


# Check if includes only 3 and 4 year olds .  Move 5 year old to school age 

el.needs <- read_xlsx(here("data", preschool), range = "A4:Q40", sheet = "working")  %>%
    mutate(across(ends_with("olds"), ~ as.numeric(.x) )) %>%
    select(Zip, allocation, contains("3 & 4")) %>%
    mutate(
        unmet.perc.1 = if_else(`Percent unmet need-based on income-eligibility and need for care - 3 & 4 year olds` > .4, TRUE, FALSE),
        unmet.num.1 = if_else(`Number unmet need-based on income-eligibility and need for care - 3 & 4 year olds` > 150, TRUE, FALSE),
        unmet.perc.2 = if_else(`Percent unmet need-based on income-eligibility and need for care - 3 & 4 year olds` > .25, TRUE, FALSE),
        unmet.num.2 = if_else(`Number unmet need-based on income-eligibility and need for care - 3 & 4 year olds` > 75, TRUE, FALSE),
        unmet.perc.3 = if_else(`Percent unmet need-based on income-eligibility and need for care - 3 & 4 year olds` > .25, TRUE, FALSE),
        unmet.num.3 = if_else(`Number unmet need-based on income-eligibility and need for care - 3 & 4 year olds` > 50, TRUE, FALSE),
        priority1 = if_else(unmet.perc.1 == TRUE & unmet.num.1 == TRUE, TRUE, FALSE ),
        priority2 = if_else(unmet.perc.2 == TRUE & unmet.num.2 == TRUE & priority1 == FALSE, TRUE, FALSE ),
        priority3 = if_else(unmet.perc.3 == TRUE & unmet.num.3 == TRUE & priority1 == FALSE & priority2 == FALSE, TRUE, FALSE ),
        priority.preschool = case_when(priority1 == TRUE ~ "Priority 1",
                                       priority2 == TRUE ~ "Priority 2",
                                       priority3 == TRUE ~ "Priority 3",
                                       TRUE ~ "Not a priority"
        )
        
    )
    

# CSPP and CCTR Infant/Toddler Program

# Priority 1: A zip code qualifies as Priority 1 when there are 40% or more of eligible children underserved, and there are more than 150 children underserved.
# Priority 2: A zip code qualifies as Priority 2 when there are 25% or more of eligible children underserved, and there are more than 75 children underserved.
# Priority 3:
#     Option 1: A zip code qualifies as Priority 3 when there are 25% or more of eligible children underserved, and there are more than 50 children underserved.
# Option 2: All other zip codes in the county.
# Option 3: No other zip codes in the county.


infant <- "ELNeedsAssessment_20240216145728.xlsx"



el.infant <- read_xlsx(here("data", infant), range = "A6:V42")  %>%
    mutate(across(ends_with("months"), ~ as.numeric(.x) )) %>%
    select(Zip, allocation, contains("&")) %>%
    mutate(
        unmet.perc.1 = if_else(`Percent unmet need-based on income-eligibility and need for care - 0-11,12-23,& 24-35 months` > .4, TRUE, FALSE),
        unmet.num.1 = if_else(`Number unmet need-based on income-eligibility and need for care - 0-11,12-23,& 24-35 months` > 150, TRUE, FALSE),
        unmet.perc.2 = if_else(`Percent unmet need-based on income-eligibility and need for care - 0-11,12-23,& 24-35 months` > .25, TRUE, FALSE),
        unmet.num.2 = if_else(`Number unmet need-based on income-eligibility and need for care - 0-11,12-23,& 24-35 months` > 75, TRUE, FALSE),
        unmet.perc.3 = if_else(`Percent unmet need-based on income-eligibility and need for care - 0-11,12-23,& 24-35 months` > .25, TRUE, FALSE),
        unmet.num.3 = if_else(`Number unmet need-based on income-eligibility and need for care - 0-11,12-23,& 24-35 months` > 50, TRUE, FALSE),
        priority1 = if_else(unmet.perc.1 == TRUE & unmet.num.1 == TRUE, TRUE, FALSE ),
        priority2 = if_else(unmet.perc.2 == TRUE & unmet.num.2 == TRUE & priority1 == FALSE, TRUE, FALSE ),
        priority3 = if_else(unmet.perc.3 == TRUE & unmet.num.3 == TRUE & priority1 == FALSE & priority2 == FALSE, TRUE, FALSE ),
        priority.infant = case_when(priority1 == TRUE ~ "Priority 1",
                                       priority2 == TRUE ~ "Priority 2",
                                       priority3 == TRUE ~ "Priority 3",
                                       TRUE ~ "Not a priority"
        )
    )





# CCTR School Aged Program

# Priority 1: A zip code qualifies as Priority 1 when there are 40% or more of eligible children underserved, and there are more than 200 eligible children underserved.
# Priority 2: A zip code qualifies as Priority 2 when there are 25% or more of eligible children underserved, and there are more than 100 eligible children underserved.
# Priority 3:
#     Option 1: A zip code qualifies as Priority 3 when there are 25% or more of eligible children underserved, and there are more than 50 eligible children underserved.
# Option 2: All other zip codes in the county.
# Option 3: No other zip codes in the county.



five.year.olds <- read_xlsx(here("data", needed), range = "A4:Q40", sheet = "working")  %>%
    mutate(across(ends_with("olds"), ~ as.numeric(.x) )) %>%
    select(Zip, contains("- 5")) 


school.age <- "ELNeedsAssessment_20240216150113.xlsx"


el.school <- read_xlsx(here("data", school.age), range = "A4:W40")  %>%
    mutate(across(ends_with("olds"), ~ as.numeric(.x) )) %>%
    left_join(five.year.olds) %>%
    rowwise() %>%
   # select(Zip, allocation, contains("&")) %>%
    mutate(sub.spaces = sum(
        `Number of children in CalWORKs Stage 2 programs - 6-12 year olds year olds`,
        `Number of children in CalWORKs Stage 3 programs - 6-12 year olds year olds`,
        `Number of children enrolled in General Child Care, center-based child care (CCTR) - 6-12 year olds year olds`,
        `Number of children enrolled in Family Child Care Home Education Networks (CFCC) - 6-12 year olds year olds`,
        `Number of children enrolled in Migrant Child Care, center-based child care (CMIG) - 6-12 year olds year olds`,
        `Number of children enrolled in Severely Handicapped Program (CHAN) - 6-12 year olds year olds`,
        `Number of children in Alternative Payment Programs (CAPP) - 6-12 year olds year olds`,
        `Number of children in CDE-administered programs in licensed exempt settings - 6-12 year olds year olds`,
        `Number of children in CDE-administered programs in licensed family child care homes - 6-12 year olds year olds`,
        `Number of children in CDE-administered programs in licensed, center-based settings - 6-12 year olds year olds`,
        `Total enrollment in publicly subsidized programs (for income-eligible and qualifying need for care unmet need report) - 5 year olds`
    ),
    not.served = `Number of children living in households earning under 85% state median income (SMI) (ACS5yr) - 6-12 year olds year olds` + `Number of children eligible for subsidized child care (income and qualifying need) (five-year estimates, from American Community Survey) - 5 year olds` - sub.spaces,
    perc.not.served = not.served/(`Number of children living in households earning under 85% state median income (SMI) (ACS5yr) - 6-12 year olds year olds`+ `Number of children eligible for subsidized child care (income and qualifying need) (five-year estimates, from American Community Survey) - 5 year olds`),

        unmet.perc.1 = if_else(perc.not.served > .4, TRUE, FALSE),
        unmet.num.1 = if_else(not.served > 200, TRUE, FALSE),
        unmet.perc.2 = if_else(perc.not.served > .25, TRUE, FALSE),
        unmet.num.2 = if_else(not.served > 100, TRUE, FALSE),
        unmet.perc.3 = if_else(perc.not.served > .25, TRUE, FALSE),
        unmet.num.3 = if_else(not.served > 50, TRUE, FALSE),
        priority1 = if_else(unmet.perc.1 == TRUE & unmet.num.1 == TRUE, TRUE, FALSE ),
        priority2 = if_else(unmet.perc.2 == TRUE & unmet.num.2 == TRUE & priority1 == FALSE, TRUE, FALSE ),
        priority3 = if_else(unmet.perc.3 == TRUE & unmet.num.3 == TRUE & priority1 == FALSE & priority2 == FALSE, TRUE, FALSE ),
    priority.school = case_when(priority1 == TRUE ~ "Priority 1",
                                priority2 == TRUE ~ "Priority 2",
                                priority3 == TRUE ~ "Priority 3",
                                TRUE ~ "Not a priority"
    )
    )





summary <- el.infant %>%
    select(Zip, priority.infant) %>%
    left_join(el.needs) %>% 
    select(Zip, priority.infant, priority.preschool) %>%
    left_join(el.school) %>% 
    select(Zip, priority.infant, priority.preschool, priority.school)


write.csv(summary, "Priority Summary.csv")
