# County_Coronavirus_Statistics
This is old code for a Shiny app, built in R, that allows users to explore the evolution of Coronavirus in counties across the country. This code is *outdated*, and this county-level function has been subsumed by the [Coronavirus_Shiny_App](https://github.com/jakepscott/Coronavirus_Shiny_App). I would not recommend using this code, but I have kept it here in case I ever need to look back.

## Getting Started
To run this on your computer, you should be able to just run the County_Level_Function.R  or app.R files. The Cleaning_Data.R file only needs to be run if you do not have the data folder in the working directory.

### Prerequisites

To run the code you just need R and the following packages installed and loaded:

```
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
```

## Author

* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16)
