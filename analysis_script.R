library(ggplot2)
library(dplyr)
library(bigrquery)
library(httr)
library(ggmap)

##### importing data from bigquery #####
## 1. connect to public dataset: https://console.cloud.google.com/marketplace/details/bigquery-public-datasets/covid19-dataset-list
## 2. go to https://console.cloud.google.com/bigquery
## 3. create a project (your owned project)
## 4. go to hamburger icon > apis and services > credentials > enable bigquery data transfer api
## 5. copy dataset (covid19_jhu_csse) from bigquery-public-data to your owedn project (you cannot directly connect directly to publid data project from r)
## 6. select your target project name and name the datasert name then done

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
## enable geo api in gcp first
api_key <- "AIzaSyCgAe7o8wIde89PUJpgB-KcfMrBhNcngh4"
ggmap::register_google(key = "AIzaSyCgAe7o8wIde89PUJpgB-KcfMrBhNcngh4")

## test plotting map
google_map = ggmap(get_googlemap(center = c(lon=5,lat=0),zoom = 1, scale = 2,
                         maptype ='roadmap',
                         color = 'color'))

google_map + geom_point(aes(x=138.2529,y=36.20482))

## check date (data up to when?)
distinct(summary_data,date) %>% arrange(date)

## test geo api
url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=Japan&key=",api_key)
res <- GET(url,encode = "json")
geo_info <- rjson::fromJSON(file = url)
geo_info$results[[1]]$geometry$location$lng

## loop countries to get coordinates
country_list <- distinct(summary_data, country_region) %>% filter(country_region != "Others")
