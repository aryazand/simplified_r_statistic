#####
# This script simply downloads data from our data sources
#####

download.file(url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", 
              destfile = "./data/NYT_us-counties.csv")

download.file(url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", 
              destfile = "./data/NYT_us-states.csv")

download.file(url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv", 
              destfile = "./data/NYT_us.csv")
