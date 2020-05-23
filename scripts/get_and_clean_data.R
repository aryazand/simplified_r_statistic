# ****************
# In this script we get and clean data on the number SARS-CoV-2 cases
# we save the resulting data as in ./data/case_data.csv
# ****************
library(tidyverse)
library(zoo)
# ****************
# Load Data
# ****************
column_specs =
cols(
date = col_date(format = "%Y%m%d"),
state = col_character(),
positive = col_double(),
negative = col_double(),
pending = col_double(),
hospitalizedCurrently = col_double(),
hospitalizedCumulative = col_double(),
inIcuCurrently = col_double(),
inIcuCumulative = col_double(),
onVentilatorCurrently = col_double(),
onVentilatorCumulative = col_double(),
recovered = col_double(),
dataQualityGrade = col_character(),
lastUpdateEt = col_datetime(format = "%m/%d/%Y %H:%M"),
hash = col_character(),
dateChecked = col_skip(),
death = col_double(),
hospitalized = col_double(),
total = col_double(),
totalTestResults = col_double(),
posNeg = col_double(),
fips = col_character(),
deathIncrease = col_double(),
hospitalizedIncrease = col_double(),
negativeIncrease = col_double(),
positiveIncrease = col_double(),
totalTestResultsIncrease = col_double()
)
US_countydata <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", col_types = "Dcccnn")
US_statedata <- read_csv("https://covidtracking.com/api/v1/states/daily.csv", col_types = column_specs)
countrydata <- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv", col_types = "Dcnnnn")
# *******************************
#       Standardize Data
# *******************************
# Create a unique label for each region in each dataset
US_countydata <- US_countydata %>%
unite(col = region, county, state, sep=", ") %>%
dplyr::select(date, region, regionID = fips, total_cases = cases) %>%
mutate(region_type = "county", regionID_type = "fips")
US_statedata <- US_statedata %>%
dplyr::select(date, region = state, total_cases = positive, new_cases = positiveIncrease, regionID = fips) %>%
mutate(region_type = "state", regionID_type = "fips")
countrydata <- countrydata %>%
dplyr::select(date, region = location, new_cases, total_cases) %>%
mutate(region_type = "nation", regionID = NA, regionID_type = NA)
#Replace abberviated names with full names in US State data
fips_reference = tigris::fips_codes %>% select(1:3) %>% distinct()
US_statedata = left_join(US_statedata, fips_reference, by=c("regionID" = "state_code")) %>%
mutate(region = state_name) %>%
select(-c(state, state_name))
# Calculate new cases for NYT data
calculate_newcases_func = function(data) {
data <- data %>%
group_by(region, regionID, region_type, regionID_type) %>%
arrange(date) %>%
mutate(new_cases = c(0, diff(total_cases))) %>%
ungroup()
return(data)
}
US_countydata <- calculate_newcases_func(US_countydata)
#Bind all data into one table
DATA <- bind_rows(list(US_countydata, US_statedata, countrydata))
# ***********************
#       Clean Data
# ***********************
# -------------------------------------------------------
# Remoeve Unknown Regions
# -------------------------------------------------------
DATA = DATA %>% filter(!(grepl("Unknown", region)))
# -------------------------------------------------------
# If a region starts with NAs for its total cases, remove those starting days
# -------------------------------------------------------
DATA2 <- DATA %>%
filter(!is.na(total_cases)) %>%
group_by(region, regionID, region_type, regionID_type) %>%
summarise(reference_date = min(date))
DATA <- full_join(DATA, DATA2) %>%
filter(date >= reference_date) %>%
select(-reference_date)
# -------------------------------------------------------
# 2. Recalculate new cases
# -------------------------------------------------------
DATA <- DATA %>%
group_by(region, regionID, region_type, regionID_type) %>%
arrange(date) %>%
mutate(new_cases = c(0, diff(total_cases)))
# -------------------------------------------------------
# Remove regions that have less than 14 days of record
# -------------------------------------------------------
DATA <- DATA %>%
group_by(region, regionID, region_type, regionID_type) %>%
filter(n() > 14)
# -------------------------------------------------------
# Fix days with negative increase in cumulative cases
# -------------------------------------------------------
#   When this is occurs, I'm assuming any previous days with more total_cases is incorrect
#   Therefore, we will filter out days that that have more total_cases than a future day
# -------------------------------------------------------
# Filter  out days that that have more total_cases than a future day
# -------------------------------------------------------
DATA = DATA %>%
group_by(region, region_type, regionID, regionID_type) %>%
arrange(date)
DATES_TO_INTERPROLATE = filter(DATA, F) %>% ungroup()
DATA = DATA %>%
# identify days with negative number of "new cases"
mutate(negative_increase = new_cases < 0) %>%
mutate(has_negative_newcases = any(negative_increase, na.rm=T))
while(any(DATA$new_cases < 0)) {
DATA = split(DATA, DATA$has_negative_newcases)
DATA[["TRUE"]] = DATA[["TRUE"]] %>%
# identify when the latest date in which new_cases < 0
mutate(reference_date = which.max(negative_increase)) %>%
# identify days before reference_date that have more total_cases than reference_date
mutate(rows_to_filter = (((date < date[reference_date]) & (total_cases > total_cases[reference_date]))))
# add days you will filter out to DATES_TO_INTERPROLATE
DATES_TO_INTERPROLATE = DATA[["TRUE"]] %>% filter(rows_to_filter == T) %>% bind_rows(DATES_TO_INTERPROLATE, .)
DATA[["TRUE"]] = DATA[["TRUE"]] %>%
# filter out days
filter(rows_to_filter == F) %>%
# recalculate new_cases
mutate(new_cases = c(0, diff(total_cases)))
DATA = bind_rows(DATA)
DATA = DATA %>%
# identify days with negative number of "new cases"
mutate(negative_increase = new_cases < 0) %>%
mutate(has_negative_newcases = any(negative_increase))
}
# Remove that are no longer necessary
DATA = DATA %>%
select(-c(negative_increase, has_negative_newcases, reference_date, rows_to_filter)) %>%
ungroup()
# -------------------------------------------------------
# Interprolate total_cases for dates that were removed
# -------------------------------------------------------
DATES_TO_INTERPROLATE = DATES_TO_INTERPROLATE %>%
select(region, region_type, regionID, regionID_type, date) %>%
mutate(INTERPROLATED = T)
DATA = DATA %>% mutate(INTERPROLATED = F)
DATA = bind_rows(DATA, DATES_TO_INTERPROLATE)
DATA = DATA %>% select(region, region_type, regionID, regionID_type, date, total_cases, new_cases, INTERPROLATED)
DATA = DATA %>%
group_by(region, region_type, regionID, regionID_type) %>%
arrange(date) %>%
mutate(total_cases = zoo::na.approx(total_cases, na.rm=F)) %>%
mutate(new_cases = c(0, diff(total_cases)))
# -------------------------------------------------------
# Replace NA values in new_cases with 0
# -------------------------------------------------------
DATA$new_cases = replace(DATA$new_cases, which(is.na(DATA$new_cases)), 0)
# *****************
# Save DATA
# *****************
write_csv(DATA, "case_data.csv")
