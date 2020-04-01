library(ggplot2)
library(jsonlite)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(bigrquery)
library(httr)
library(ggmap)
library(lubridate)
library(RColorBrewer)
library(easypackages)

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

max(summary_data$date)

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

summary2$country_region[summary2$country_region=="Mainland China"] <- "China"
summary2$country_region[str_detect(summary2$country_region,"Bahamas")] <- "Bahamas"
summary2$country_region[str_detect(summary2$country_region,"Hong Kong")] <- "Hong Kong"
summary2$country_region[str_detect(summary2$country_region,"Gambia")] <- "Gambia"
summary2$country_region[str_detect(summary2$country_region,"Czech")] <- "Czech"
summary2$country_region[str_detect(summary2$country_region,"Korea")] <- "South Korea"
summary2$country_region[str_detect(summary2$country_region,"Congo")] <- "Congo"
#summary2$country_region[str_detect(summary2$country_region,"Ireland")] <- "Ireland"

summary2 <- 
summary2 %>% 
   group_by(country_region,date) %>% 
   summarise(confirmed = max(confirmed))

summary2 %>% 
   group_by(country_region) %>% 
   summarise(total_cases=sum(confirmed)) %>% 
   View()


summary2 %>% filter(str_detect(country_region,"Repub")) %>% View()

## days recordred not equal
summary2 %>% group_by(country_region,date) %>% tally() %>% arrange(-n)
summary2 %>% distinct(country_region,date) %>% group_by(country_region) %>% tally() %>% arrange(-n)

summary2 %>% filter(country_region=="Thailand"&date==as.Date("2020-03-29"))
summary2 %>% filter(country_region=="US"&date==as.Date("2020-03-29"))
us <- summary2 %>% filter(country_region=="US")
us_china <- summary2 %>% filter(country_region=="US"|country_region=="China")
us_china$country_region[us_china$country_region=="Mainland China"] <- "China"
## test us
format(Sys.Date(), format = "%Y-%b-%d")
Sys.setlocale("LC_TIME", "English")

##### plot china and us #####
lags_data <- 
   us_china %>% 
   #group_by(country_region,date) %>% 
   #summarise(total_confirmed=sum(confirmed)) %>%
   mutate(new = confirmed - lag(confirmed,n=1,default = first(confirmed))) %>% 
   arrange(country_region,date) %>% 
   mutate(new_lag5 = new - lag(new,n=5,default = first(new)),
          new_lag7 = new - lag(new,n=7,default = first(new)),
          new_lag14 = new - lag(new,n=14,default = first(new))) %>% 
   
   ungroup()

plotlag <- function(df,lag_col,lag) {
   
   set.seed(999)
   ggplot(df,aes(x=date,y=lag_col))+
      geom_jitter(aes(col = country_region),alpha=0.3)+
      geom_smooth(aes(group = country_region,col=country_region),se=F)+
      scale_color_manual(values = sample(colors(distinct = T),2))+
      theme_minimal()+
      theme(legend.position = "top",
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(0.5,1,1,1,"cm"),
            line = element_line(linetype = "dashed"))+
      labs(subtitle = paste0("new cases found compared to new cases ",lag,"-day prior"))
}


output_lag5<-plotlag(df=lags_data,lag_col=lags_data$new_lag5,lag=5)
output_lag7<-plotlag(df=lags_data,lag_col=lags_data$new_lag7,lag=7)
output_lag14<-plotlag(df=lags_data,lag_col=lags_data$new_lag14,lag=14)


print(output_lag5)
print(output_lag7)
print(output_lag14)



set.seed(999)
g<-us_china %>% 
   mutate(new = confirmed - lag(confirmed, n=1,default = first(confirmed))) %>% 
   filter(new > 0)

new_log_b5<- 
   ggplot(g,aes(x=date,y=log(new,base=5)))+
   geom_jitter(aes(col = country_region),alpha=0.3,size=2)+
   geom_smooth(aes(group = country_region,col=country_region),se=F,method = "lm")+
   scale_color_manual(values = sample(colors(distinct = T),2))+
   theme_minimal()+
   theme(legend.position = "top",
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         plot.margin = margin(0.5,1,0.5,1,"cm"),
         line = element_line(linetype = "dashed"),
         )
   
print(new_log_b5)


## find growth metric
speed <- 
g %>% mutate(speed_rate = (total_confirmed / lag(total_confirmed,n=15,default = first(total_confirmed))) %>% round(2))
speed %>% group_by(country_region) %>% summarise(mean_speed = mean(speed_rate,na.rm = T))

set.seed(999)
ggplot(speed,aes(x=date,y=speed_rate))+
   geom_point(aes(col = country_region),alpha=0.3,size=2)+
   geom_smooth(aes(group = country_region,col=country_region),se=F)+
   scale_color_manual(values = sample(colors(distinct = T),2))+
   theme_minimal()+
   theme(legend.position = "top",
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         plot.margin = margin(0.5,1,0.5,1,"cm"))

## speed mean all ##
speed_rank14 <- 
summary2 %>%
   filter(confirmed!=0&country_region %in% n30$country_region) %>%
   mutate(new = confirmed - lag(confirmed,n=1,default = first(confirmed))) %>%
   filter(new>0) %>% 
   arrange(country_region,date) %>%
   filter(country_region %in% n14$country_region) %>% 
   mutate(speed_rate = (new / lag(new,n=14)) %>% round(2)) %>%
   group_by(country_region) %>% summarise(mean_speed = mean(speed_rate,na.rm = T) %>% round(1)) %>%
   mutate(rank_handle = as.numeric(factor(rank(mean_speed)))) %>% 
   ungroup() %>%
   arrange(-rank_handle)
   
n30 <-    
   summary2 %>%
      filter(confirmed!=0) %>%
      group_by(country_region) %>% 
      summarise(n = n()) %>% 
      filter(n >=30)

n14 <- 
summary2 %>%
   filter(confirmed!=0&country_region %in% n30$country_region) %>%
   mutate(new = confirmed - lag(confirmed,n=1,default = first(confirmed))) %>%
   filter(new>0) %>% 
   count(country_region) %>% 
   filter(n>=15) #n+1
