# Loading Packages --------------------------------------------------------
library(shiny)
library(tidyverse)
library(lubridate)
library(geofacet)
library(scales)
library(zoo)
library(ggtext) 
library(ggthemes)
library(shinythemes)
library(plotly)
library(sf)
windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))
state <- "Florida"

# Loading in Data ---------------------------------------------------------
#Prereq data
State_Names <- read_rds("data/State_Names.RDS")
county_pop_clean <- read_rds("data/county_pop_clean.RDS")
state_pop_clean <- read_rds("data/state_pop_clean.RDS")

#US Data
US_Data_Raw <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
US_Data <- left_join(US_Data_Raw,State_Names, by=c("state"))
US_Data <- left_join(US_Data,state_pop_clean,by="state")
US_Data <- left_join(US_Data,county_pop_clean, by=c("state","county")) %>%
  rename("County"=county,"State"=state, "Date"=date, "Cases"=cases,"Deaths"=deaths)
US_Data <- US_Data %>% filter(State==state,County!="Unknown", !is.na(fips))

#Maps
county_map <- read_sf("data/cb_2018_us_county_5m/cb_2018_us_county_5m.shp") %>% 
  rename("County"=NAME,"fips"=GEOID) %>% 
  filter(fips %in% US_Data$fips)

state_map <- read_sf("data/cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
  rename("State"=NAME) %>% filter(State==state)

# Making a Vector of Dates ------------------------------------------------
latest_date <- last(US_Data$Date)
number_of_days <- ymd(latest_date) - ymd("2020-01-20")

dates <- vector(mode = "character", length = number_of_days)
for (i in 1:number_of_days) {
  dates[i] <- as.character(ymd(latest_date) - i)
}

dates <- as_tibble(dates)

# Cleaning/Prepping Data --------------------------------------------------
US_Cases_Grouped <- US_Data %>%
  arrange(Date) %>%
  group_by(Date, State, County) %>%
  summarise(Cases=sum(Cases),
            Deaths=sum(Deaths),
            abb=last(abb),
            Population=last(State_Population),
            fips=last(fips)) %>% 
  ungroup() %>%
  mutate(Cases_Per_100k=(Cases/Population)*100000,
         Deaths_Per_100k=(Deaths/Population)*100000) %>% 
  group_by(State, County) %>%
  mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
         New_Cases_Avg=rollmean(New_Cases,k = 7,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
  ungroup() %>%
  mutate(New_Cases_Per_100k=(New_Cases/Population)*100000,
         New_Cases_Per_100k_Avg=(New_Cases_Avg/Population)*100000,
         New_Cases=ifelse(New_Cases<0,NA,New_Cases),
         New_Cases_Per_100k=ifelse(New_Cases_Per_100k<0,NA,New_Cases_Per_100k))


# Combining with Map Data -------------------------------------------------
Mappable_Data <-left_join(US_Cases_Grouped,county_map, by=c("fips"))

Mappable_Data <- st_as_sf(Mappable_Data) %>% filter(!is.na(Date)) %>% filter(Date=="2020-06-15",
                                                                             State!="Alaska")

state_plot <- ggplot(Mappable_Data) + 
    geom_sf(data=state_map, fill="grey50") +
    geom_sf(aes(fill=New_Cases_Per_100k), color=NA) +
    geom_sf(data=county_map,fill=NA, color="grey70") +  
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_viridis_c(option = "plasma", labels = comma) +
    labs(y=NULL,
         x=NULL,
         fill="New Cases Per\n100,000 Residents",
         title="New Cases By County on {frame_time}",
         caption = "Plot: @jakepscott2020 | Data: New York Times",
         subtitle = "New cases counted as 7 day rolling average of new cases per thousand residents") +
    theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = rel(1.2)),
          plot.subtitle = element_text(face = "plain", size = rel(.8), color = "grey70"),
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70"),
          legend.title = element_text(face="bold",size = rel(.8)),
          legend.text = element_text(size=rel(.7)),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title.position = "plot")
ggplotly(state_plot)
