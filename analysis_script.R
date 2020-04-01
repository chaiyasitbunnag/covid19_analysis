library(ggplot2)
library(jsonlite)
library(plotly)
library(dplyr)
library(tidyr)
library(bigrquery)
library(httr)
library(ggmap)
library(lubridate)
library(RColorBrewer)

setwd("C:/git_r_project/covid19_analysis")
##### importing data from bigquery #####
## 1. connect to public dataset: https://console.cloud.google.com/marketplace/details/bigquery-public-datasets/covid19-dataset-list
## 2. go to https://console.cloud.google.com/bigquery
## 3. create a project (your owned project)
## 4. go to hamburger icon > apis and services > credentials > enable bigquery data transfer api
## 5. copy dataset (covid19_jhu_csse) from bigquery-public-data to your owedn project (you cannot directly connect directly to publid data project from r)
## 6. select your target project name and name the datasert name then done

project_id = "covid19-analysis-272811"

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

## test geo api in case the data doen't provide lat long
url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=Japan&key=",api_key)
res <- GET(url,encode = "json")
geo_info <- rjson::fromJSON(file = url)
geo_info$results[[1]]$geometry$location$lat
geo_info$results[[1]]$geometry$location$lng
## loop if too many countries' lat lon missing

summary2 <- 
summary_data %>%
   mutate(country_region = trimws(country_region)) %>% 
   group_by(country_region) %>%
   fill(latitude, .direction = "up") %>% 
   fill(longitude, .direction = "up") %>% 
   fill(latitude, .direction = "down") %>% 
   fill(longitude, .direction = "down")

## days recordred not equal
summary2 %>% group_by(country_region,date) %>% tally() %>% arrange(-n)
summary2 %>% filter(country_region=="Thailand"&date==as.Date("2020-03-29"))
summary2 %>% filter(country_region=="US"&date==as.Date("2020-03-29"))
us <- summary2 %>% filter(country_region=="US")
us_china <- summary2 %>% filter(country_region=="US"|country_region=="China"|country_region=="Mainland China")
us_china$country_region[us_china$country_region=="Mainland China"] <- "China"
## test us
format(Sys.Date(), format = "%Y-%b-%d")
Sys.setlocale("LC_TIME", "English")

g <- 
   us_china %>% 
   group_by(country_region,date) %>% 
   summarise(total_confirmed=sum(confirmed)) %>%
   mutate(new = total_confirmed - lag(total_confirmed,n=1,default = first(total_confirmed))) %>% 
   arrange(country_region,date) %>% 
   mutate(new_lag5 = new - lag(new,n=5,default = first(total_confirmed))) %>% 
   ungroup()

   ggplot(g,aes(x=date,y=log2(new_lag5)))+
   geom_point(aes(col = country_region))+
   geom_smooth(aes(group = country_region,col=country_region))+
   theme(legend.position = "top")
   
   ggplot(g,aes(x=date,y=new_lag5))+
   geom_point(aes(col = country_region))+
   geom_smooth(aes(group = country_region,col=country_region,se=F))+
   theme(legend.position = "top")
   
lag5_nonlog<- 
   ggplot(g,aes(x=date,y=new_lag5))+
   geom_point(aes(col = country_region))+
   geom_smooth(aes(group = country_region,col=country_region),se=F)+
   theme_minimal()+
   theme(legend.position = "top",
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank())
set.seed(999)
lag5_log<- 
   ggplot(g,aes(x=date,y=log10(new_lag5)))+
   geom_point(aes(col = country_region),alpha=0.3,size=2)+
   geom_smooth(aes(group = country_region,col=country_region),se=F)+
   scale_color_manual(values = sample(colors(distinct = T),2))+
   theme_minimal()+
   theme(legend.position = "top",
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank())
   
lag5_nonlog
lag5_log


ggplotly(g2) %>% hide_legend()

us_china_lag3 <- 
   us_china %>% 
   group_by(country_region,date) %>% 
   summarise(total_confirmed=sum(confirmed)) %>%
   mutate(new = total_confirmed - lag(total_confirmed,n=1,default = first(total_confirmed))) %>%
   group_by(country_region,date) %>% 
   mutate(new_lag3 = new - lag(new,n=3,default = first(new))) %>% 
   arrange(country_region,date)
   
 
   ggplot(us_china_lag3,aes(x=date,y=new))+
   geom_point(aes(col = country_region))+
   geom_smooth(aes(group = country_region,col=country_region))+
   theme(legend.position = "top")

g <-
   us %>%
   group_by(country_region,date) %>% summarise(total_confirmed=sum(confirmed)) %>% arrange(date) %>% 
   ggplot(aes(x = date, y = total_confirmed))+geom_line()

ggplotly(g)

