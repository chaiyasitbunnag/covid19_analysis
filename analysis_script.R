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
library(ggrepel)

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
summary2$country_region[summary2$country_region=="UK"] <- "United Kingdom"
summary2$country_region[summary2$country_region=="Taiwan*"] <- "Taiwan"
summary2<-summary2 %>% filter(country_region != "Others")
#summary2$country_region[str_detect(summary2$country_region,"Ireland")] <- "Ireland"

summary2 <- 
summary2 %>% 
   group_by(country_region,date) %>% 
   summarise(confirmed = max(confirmed))



##### plot china and us #####
format(Sys.Date(), format = "%Y-%b-%d")
Sys.setlocale("LC_TIME", "English")

us_china <- summary2 %>% filter(country_region == "US"|country_region == "China")

lags_data <- 
   us_china %>%
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
summary2 %>%
   filter(confirmed>100) %>% 
   mutate(speed_rate = (confirmed / lag(confirmed,n=14,default = first(confirmed))) %>% round(2))

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
   filter(confirmed>0&country_region %in% n14$country_region) %>%
   mutate(speed_rate = (confirmed / lag(confirmed,n=14)) %>% round(2)) %>%
   group_by(country_region) %>% 
   summarise(mean_speed = sum(speed_rate,na.rm = T)) %>%
   mutate(mean_order = as.numeric(factor(rank(mean_speed))))%>% 
   ungroup() %>%
   arrange(-mean_order)

speed_rank14$country_region[speed_rank14$country_region=="Taiwan*"] <- "Taiwan"

set.seed(999)
speed_rank14 %>%
   mutate(country_region = reorder(country_region,-mean_order)) %>% 
   ggplot(.,aes(x=country_region,y=mean_order,label = paste0(country_region,": ",round(mean_speed/100,2)),col = country_region)) +
   geom_point()+
   geom_text_repel(size=3,hjust=-0.2)+
   guides(colour=guide_colorbar(label=F))+
   scale_color_manual(values = sample(colors(distinct = T),length(unique(speed_rank14$country_region))))+
   scale_y_continuous(breaks = seq(1,max(speed_rank14$mean_order),1))+
   coord_flip()+
   theme_minimal()+
   theme(axis.text.x = element_blank(),
         axis.title = element_blank(),
         axis.text.y = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         line = element_blank())+
   labs(subtitle = "14-day average speed (total cases) from low (left) to high (right)")

n14 <- 
   summary2 %>%
   filter(confirmed>=50) %>%
   #mutate(new = confirmed - lag(confirmed,n=1,default = first(confirmed))) %>%
   #filter(new>0) %>% 
   #arrange(country_region,date) %>%
   mutate(speed_rate = (confirmed / lag(confirmed,n=14)) %>% round(2)) %>% 
   count(country_region) %>%
   filter(n>=15)

n14 %>% 

suspect_report = c("China","Russia","Iran","Indonesia","Saudi Arabia","Eypt")
# source https://www.bloomberg.com/news/articles/2020-04-01/china-concealed-extent-of-virus-outbreak-u-s-intelligence-says
# https://www.bloomberg.com/news/articles/2020-03-24/hyatt-to-furlough-u-s-corporate-employees-with-hotels-shuttered

