library(rgdal)
library(leaflet)
library(coronavirus)
update_dataset()

my_spdf <- readOGR( 
  dsn= paste0(here::here("data_clean", "world_shape_file")), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

from_countries <- c("USA", "UK", "Iran", "South Korea")
to_countries <- c("United States", "United Kingdom", "Iran (Islamic Republic of)", "Korea, Republic of")
a <- country %>%
  mutate(Country = plyr::mapvalues(Country, from = from_countries,
                to = to_countries))

data("coronavirus")
summary_df <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  rename("NAME" = country)

my_spdf@data <- left_join(my_spdf@data, a %>%
            rename(NAME = Country),
          by = "NAME") %>%
  left_join(., summary_df %>%
              mutate(total_cases = total_cases/1000),
            by = "NAME") %>%
  mutate(NAME = as.factor(NAME))

my_spdf@data$Number[ which(my_spdf@data$Number == 0)] = NA
my_spdf@data$total_cases[ which(is.na(my_spdf@data$total_cases))] = 0


my_bins <- c(0, 1, 5, 10, 20, 30, 40, 50)
mypalette <- colorBin(palette="YlOrBr", domain=my_spdf@data$Number, na.color="transparent", bins=my_bins)

mytext <- paste(
  "Country: ", my_spdf@data$NAME,"<br/>", 
  "Number of studies included: ", my_spdf@data$Number, "<br/>", 
  "Number of COVID-19 reported cases (thousands): ", round(my_spdf@data$total_cases, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

html_map <- leaflet(my_spdf) %>% 
  addTiles()  %>% 
  setView(lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(Number), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~Number, opacity=0.9, title = "Number of studies included", position = "bottomright" )
