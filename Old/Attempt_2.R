# Loading Libraries--------------------------------------------------------
library(shiny)
library(tidyverse)
library(lubridate)
library(geofacet)
library(scales)
library(zoo)
library(ggtext) 
library(ggthemes)
library(shinythemes)
library(gganimate)
library(sf)
library(tidycensus)
state <- "New York"
county <- "Madison"
rollmean <- 7
# Loading Raw Data --------------------------------------------------------
US_Data <- read_rds("data/US_Data.RDS") %>% 
  filter(County!="Unknown", !is.na(fips))

# Getting Dates -----------------------------------------------------------
latest_date <- last(US_Data$Date)
number_of_days <- ymd(latest_date) - ymd("2020-03-25")

dates <- vector(mode = "character", length = number_of_days)
for (i in 1:number_of_days) {
  dates[i] <- as.character(ymd(latest_date) - i)
}
dates <- as_tibble(dates)


# Prepping Data -----------------------------------------------------------
US_Grouped <- US_Data %>%
  arrange(State, County,Date) %>% 
  mutate(cases_per_100k=(Cases/County_Population)*100000,
         deaths_per_100k=(Deaths/County_Population)*100000) %>% 
  group_by(State, County) %>%
  mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
         New_Cases_Avg=rollmean(New_Cases,k = rollmean,fill = NA, align = "right"),
         New_Deaths=Deaths-lag(Deaths), ##New cases is today's cases minus yesterdays
         New_Deaths_Avg=rollmean(Deaths,k = rollmean,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
  ungroup() %>%
  mutate(New_Cases_Per_100k=(New_Cases/County_Population)*100000,
         New_Deaths_Per_100k=(New_Deaths/County_Population)*100000,
         New_Cases_Per_100k_Avg=(New_Cases_Avg/County_Population)*100000,
         New_Deaths_Per_100k_Avg=(New_Deaths_Avg/County_Population)*100000) 
  
##Getting whether new cases are increasing or decreasing
Increasing_or_Decreasing <- 
  US_Grouped %>% 
  select(County,State,Date,New_Cases_Avg, New_Deaths_Avg) %>% 
  group_by(State,County) %>%
  mutate(change_in_new_cases=New_Cases_Avg-lag(New_Cases_Avg),
         change_in_new_deaths=New_Deaths_Avg-lag(New_Deaths_Avg)) %>% 
  ungroup() %>%
  mutate(Cases_Up_or_Down=case_when(change_in_new_cases>0~"Increasing",
                              change_in_new_cases<0~"Decreasing",
                              change_in_new_cases==0~"Steady"),
         Deaths_Up_or_Down=case_when(change_in_new_deaths>0~"Increasing",
                                    change_in_new_deaths<0~"Decreasing",
                                    change_in_new_deaths==0~"Steady")) %>% 
  select(Date,State,County,change_in_new_cases,Cases_Up_or_Down, Deaths_Up_or_Down)

US_Grouped <- left_join(US_Grouped,Increasing_or_Decreasing, by=c("State","County","Date")) %>% 
  mutate(label_new_cases=paste("There were ",round(New_Cases,0), " new cases reported in ",
                               State, " on ", as.character(month(Date, label = T,abbr = F)),
                               " ", day(Date), ", ", year(Date),sep=""),
         label_new_cases_per_100k=paste("There were ",round(New_Cases_Per_100k,0), " new cases per 100,000 residents reported in ",
                                           State, " on ", as.character(month(Date, label = T,abbr = F)),
                                           " ", day(Date), ", ", year(Date),sep=""),
         label_cases=paste("There were ", round(Cases,0), " cumulative cases in ",
                           State, " as of ", as.character(month(Date, label = T,abbr = F)),
                           " ", day(Date), ", ", year(Date),sep=""),
         label_cases_per_100k=paste("There were ", round(cases_per_100k,0), " cumulative cases per 100,000 residents in ",
                                       State," as of ", as.character(month(Date, label = T,abbr = F)),
                                       " ", day(Date), ", ", year(Date), ",", sep=""),
         label_new_deaths=paste("There were ", round(New_Deaths,0), " new deaths reported in ",
                                State, " on ", as.character(month(Date, label = T,abbr = F)),
                                " ", day(Date), ", ", year(Date), ",", sep=""),
         label_new_deaths_per_100k=paste("There were ", round(New_Deaths_Per_100k,0), " new deaths per 100,000 residents reported in ",
                                            State, " on ", as.character(month(Date, label = T,abbr = F)),
                                            " ", day(Date), ", ", year(Date), ",", sep=""),
         label_deaths=paste("There were ", round(Deaths,0), " cumulative deaths in ",
                            State," as of ", as.character(month(Date, label = T,abbr = F)),
                            " ", day(Date), ", ", year(Date), sep=""),
         label_deaths_per_100k=paste("There were ", round(deaths_per_100k,0), " cumulative deaths per 100,000 residents in ",
                                        State," as of ", as.character(month(Date, label = T,abbr = F)),
                                        " ", day(Date), ", ", year(Date), sep=""))

US_Grouped <- US_Grouped %>% filter(State==state,County==county)

ggplot(US_Grouped, aes(x=Date,y=New_Cases_Per_100k)) +
  geom_col(aes(text=label_new_cases_per_100k,fill=Cases_Up_or_Down,color=Cases_Up_or_Down), alpha=.7) +
  geom_line(aes(y=New_Cases_Avg),lwd=1) +
  scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), 
               guide = guide_axis(check.overlap = T)) +
  scale_y_continuous(expand = c(0,0),label = comma) +
  scale_fill_manual(values = c("#91cf60","grey70","red"), 
                    breaks = c("Decreasing", "Steady","Increasing")) +
  scale_color_manual(values = c("#91cf60","grey70","red"), 
                     breaks = c("Decreasing", "Steady","Increasing")) +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title=paste("The Rolling Average of New Cases is", last(US_Grouped$Cases_Up_or_Down), 
                   "in", county, sep=" "),
       subtitle = "7 day rolling average of new cases",
       caption = "Plot: @jakepscott2020 | Data: New York Times") +
  theme_bw(base_family = "Source Sans Pro",base_size = 16) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.position = "none",
        axis.text.x = element_text(size=rel(.8)),
        axis.text.y = element_text(size=rel(.8)),
        plot.title.position = "plot")
