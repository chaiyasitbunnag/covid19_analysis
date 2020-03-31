library(ggplot2)
library(dplyr)
library(bigrquery)
library(httr)
library(ggmap)

##### importing data #####

project_id = "covid19-analysis-272811"

## confirmed cases data
bq.get_confirmed_cases_data = "select * from `covid19_data.confirmed_cases`"

confirmed = query_exec(bq.get_confirmed_cases_data,
                       project = project_id,
                       use_legacy_sql = FALSE,
                       max_pages = Inf)

bq.get_deaths_data = "select * from `covid19_data.deaths`"

## deaths data
deaths = query_exec(bq.get_deaths_data,
                    project = project_id,
                    use_legacy_sql = FALSE,
                    max_pages = Inf)

## recovered data
bq.get_recovered_cases_data = "select * from `covid19_data.confirmed_cases`"

confirmed = query_exec(bq.get_confirmed_cases_data,
                       project = project_id,
                       use_legacy_sql = FALSE,
                       max_pages = Inf)

## summary data
bq.get_summary_data = "
select
   province_state
   ,country_region
   ,date
   ,latitude
   ,longitude
   ,confirmed
   ,deaths
   ,active
   ,recovered
   ,fips
   ,admin2
   ,combined_key
from `covid19_data.summary`"

summary_data = query_exec(bq.get_summary_data,
                    project = project_id,
                    use_legacy_sql = FALSE,
                    max_pages = Inf)

##### end importing data #####

##### connecting google map api #####
api_key <- "AIzaSyCgAe7o8wIde89PUJpgB-KcfMrBhNcngh4"
country_list <- 

ggmap::register_google(key = "AIzaSyCgAe7o8wIde89PUJpgB-KcfMrBhNcngh4")


google_map = ggmap(get_googlemap(center = c(lon=5,lat=0),zoom = 1, scale = 2,
                         maptype ='roadmap',
                         color = 'color'))
google_map
google_map + geom_point(aes(x=138.2529,y=36.20482))

distinct(summary_data,date) %>% arrange(date)

## test geo api
url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=Japan&key=",api_key)
res <- GET(url,encode = "json")
geo_info <- rjson::fromJSON(file = url)
geo_info$results[[1]]$geometry$location$lng
