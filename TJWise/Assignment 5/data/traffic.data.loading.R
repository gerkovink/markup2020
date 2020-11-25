## Package Loading 
library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(shiny.semantic)


## Data Loading
## Data downloaded from: https://www.kaggle.com/sobhanmoosavi/us-accidents
pre.traffic.dat <- read.csv("data/US_Accidents_June20.csv")

## Data Sampling
traffic.dat <- sample_n(pre.traffic.dat, size = 10000)

## Data Manipulation 
traffic.red.dat <- traffic.dat %>% 
  select(c(ID, Severity, Start_Time, End_Time, Start_Lat, Start_Lng, Street, 
           Side, City, County, State, Zipcode, Country, Weather_Timestamp, 
           Temperature.F., Wind_Chill.F., Humidity..., Pressure.in., 
           Visibility.mi., Wind_Direction, Wind_Speed.mph., Precipitation.in.,
           Weather_Condition, Amenity:Astronomical_Twilight))

traffic.colnames <- colnames(traffic.red.dat)

  ## Convert Chr to Fcts
  traffic.red.dat[,c(2, 8, 9, 10, 11, 13, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 
                   34, 35, 36, 37, 38, 39, 40)] <- lapply(
  traffic.red.dat[,c(2, 8, 9, 10, 11, 13, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
                     33, 34, 35, 36, 37, 38, 39, 40)], as.factor)
  
  ## Rename to Specify Lat & Lng Columns
    colnames(traffic.red.dat)[5] <- "latitude"
    colnames(traffic.red.dat)[6] <- "longitude"

  ## Convert Severity to Colour Indicators 
      traffic.red.dat$sev.color[traffic.red.dat$Severity == "1"] <- "Blue"
      traffic.red.dat$sev.color[traffic.red.dat$Severity == "2"] <- "Green"
      traffic.red.dat$sev.color[traffic.red.dat$Severity == "3"] <- "Orange"
      traffic.red.dat$sev.color[traffic.red.dat$Severity == "4"] <- "Red"
      
  ## Convert Civil_Twilight to Colour Indicators
      traffic.red.dat$twil.color[traffic.red.dat$Civil_Twilight == ""] <- "White"
      traffic.red.dat$twil.color[traffic.red.dat$Civil_Twilight == "Day"] <- "Yellow"
      traffic.red.dat$twil.color[traffic.red.dat$Civil_Twilight == "Night"] <- "Black"
      
  ## Convert Factor to Logical
      levels(traffic.red.dat$Amenity) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Bump) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Crossing) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Give_Way) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Junction) <- c(FALSE, TRUE)
      levels(traffic.red.dat$No_Exit) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Railway) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Roundabout) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Station) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Stop) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Traffic_Calming) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Traffic_Signal) <- c(FALSE, TRUE)
      levels(traffic.red.dat$Turning_Loop) <- c(FALSE, TRUE)
      
      traffic.red.dat$Amenity <- as.logical(traffic.red.dat$Amenity)
      traffic.red.dat$Bump <- as.logical(traffic.red.dat$Bump)
      traffic.red.dat$Crossing <- as.logical(traffic.red.dat$Crossing)
      traffic.red.dat$Give_Way <- as.logical(traffic.red.dat$Give_Way)
      traffic.red.dat$Junction <- as.logical(traffic.red.dat$Junction)
      traffic.red.dat$No_Exit <- as.logical(traffic.red.dat$No_Exit)
      traffic.red.dat$Railway <- as.logical(traffic.red.dat$Railway)
      traffic.red.dat$Roundabout <- as.logical(traffic.red.dat$Roundabout)
      traffic.red.dat$Station <- as.logical(traffic.red.dat$Station)
      traffic.red.dat$Stop <- as.logical(traffic.red.dat$Stop)
      traffic.red.dat$Traffic_Calming <- as.logical(traffic.red.dat$Traffic_Calming)
      traffic.red.dat$Traffic_Signal <- as.logical(traffic.red.dat$Traffic_Signal)
      traffic.red.dat$Turning_Loop <- as.logical(traffic.red.dat$Turning_Loop)
      
  ## Convert Start_Time from Chr to Time 
      traffic.red.dat$Start_Time <- as.Date(traffic.red.dat$Start_Time)
      traffic.red.dat$date <- format(traffic.red.dat$Start_Time, "%d-%B-%Y")
      traffic.red.dat$year <- format(traffic.red.dat$Start_Time, "%Y")
      
      
  ## Merge to Form Popup Labels 
      traffic.red.dat$popuplabel <- paste(sep = " ", 
                                          traffic.red.dat$Street, "<br/>", 
                                          traffic.red.dat$City, "<br/>",
                                          traffic.red.dat$County, "<br/>",
                                          "Date: ", traffic.red.dat$date)
      
      
  ## Convert Weather to New Variable 
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Blowing Dust"] <- "Dust" 
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Blowing Dust / Windy"] <- "Dust"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Blowing Snow / Windy"] <- "Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Clear"] <- "Clear"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Cloudy"] <- "Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Cloudy / Windy"] <- "Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Drizzle"] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Drizzle / Windy"] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Drizzle and Fog"] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Fair"] <- "Clear"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Fair / Windy"] <- "Clear"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Fog"] <- "Fog"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Fog / Windy"] <- "Fog"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Freezing Rain"] <- "Freezing Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Funnel Cloud"] <- "Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Haze"] <- "Mist"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Haze / Windy"] <- "Mist"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Blowing Snow"] <- "Heavy Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Rain"] <- "Heavy Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Rain Showers"] <- "Heavy Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Sleet"] <- "Freezing Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Snow / Windy"] <- "Heavy Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Snow with Thunder"] <- "Heavy Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy T-Storm"] <- "Heavy Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Thunderstorms and Rain"] <- "Heavy Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Thunderstorms and Snow" ] <- "Heavy Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Heavy Thunderstorms with Small Hail" ] <- "Heavy Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Ice Pellets" ] <- "Hail"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Drizzle" ] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Drizzle / Windy" ] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Freezing Rain" ] <- "Freezing Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Freezing Rain / Windy" ] <- "Freezing Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Haze" ] <- "Mist"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Rain" ] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Rain / Windy" ] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Rain Shower" ] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Rain with Thunder" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Sleet" ] <- "Freezing Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Snow" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Snow and Sleet" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Snow and Sleet / Windy" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Snow Grains" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Snow Shower" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Snow Showers" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Snow with Thunder"] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Thunderstorms and Rain" ] <- "Light Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Light Thunderstorms and Snow" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Low Drifting Snow" ] <- "Light Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Mist" ] <- "Mist"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Mostly Cloudy" ] <- "Very Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Mostly Cloudy / Windy" ] <- "Very Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Other" ] <- "Other"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Overcast" ] <- "Very Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Partial Fog" ] <- "Fog"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Partly Cloudy" ] <- "Lightly Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Partly Cloudy / Windy" ] <- "Lightly Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Patches of Fog" ] <- "Fog"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Patches of Fog / Windy" ] <- "Fog"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Rain" ] <- "Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Rain Shower" ] <- "Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Rain Showers" ] <- "Rain"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Sand" ] <- "Dust"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Sand / Dust Whirlwinds" ] <- "Dust"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Scattered Clouds" ] <- "Lightly Cloudy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Small Hail" ] <- "Light Hail"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Smoke" ] <- "Smoke"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Smoke / Windy" ] <- "Smoke"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Snow" ] <- "Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Snow / Windy" ] <- "Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Snow and Sleet" ] <- "Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Snow and Sleet / Windy" ] <- "Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Squalls" ] <- "Windy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Squalls / Windy" ] <- "Windy"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "T-Storm" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "T-Storm / Windy" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Thunder" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Thunder / Windy" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Thunderstorms in the Vicinity" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Thunderstorm" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Thunderstorms and Rain" ] <- "Thunderstorm"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Volcanic Ash" ] <- "Ash"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Widespread Dust" ] <- "Dust"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "Wintry Mix / Windy" ] <- "Heavy Snow"
  traffic.red.dat$weather_cat[traffic.red.dat$Weather_Condition == "" ] <- "Unknown"
  traffic.red.dat$weather_cat[is.na(traffic.red.dat$weather_cat)] <- "Other"
  traffic.red.dat$weather_cat <- as.factor(traffic.red.dat$weather_cat)
  
  
  location.dat  <- traffic.red.dat 
