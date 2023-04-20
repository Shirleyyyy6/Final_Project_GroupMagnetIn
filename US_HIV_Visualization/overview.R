library(dplyr)
library(ggplot2)
library(plotly)
library(ggmap)
library(sf)
library(tidyverse)
library(readr)
library(showtext)
library(viridis)
library(tidyverse)
library(gt)

mortality <- read_csv("HIV deaths.csv")
overview <- read_csv("HIV diagnoses.csv")

overview$Geography <- tolower(overview$Geography)
mortality$Geography <-tolower(mortality$Geography)

# Get the geographic data for the United States at the state level
us_states <- map_data("state")

overview_map <- 
  full_join(us_states, overview, by = c("region" = "Geography")) %>%
  group_by(Year,region) %>%
  mutate(text_label = str_c(region, "\nCases: ",Cases,
                            "\nRate(per 100k): ", `Rate per 100000`))

mortality_map <-
  full_join(us_states, mortality, by = c("region" = "Geography")) %>%
  group_by(Year,region) %>%
  mutate(text_label = str_c(region, "\nCases: ",Cases,
                            "\nRate(per 100k): ", `Rate per 100000`))

overview_map$`Rate per 100000` <- as.numeric(overview_map$`Rate per 100000`)
mortality_map$`Rate per 100000` <- as.numeric(mortality_map$`Rate per 100000`)

showtext_auto()
font_add_google("Montserrat", family = "my_font")


# Plot the map
create_overview <- function(data,year){
overview.plot <- 
  data %>%
  filter(Year==year) %>%
  ggplot( aes(x = long, y = lat, group = region, fill = `Rate per 100000`)) +
  geom_polygon(color = "white",aes(text = text_label)) +
  # scale_fill_viridis_d(`Rate per 100000`) +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = 'none') +
  theme(title = element_text(hjust = 0.5)) +
  theme(text = element_text(family = "my_font"))
ggplotly(overview.plot,tooltip = "text")
}
create_overview(overview_map,2018)



overview_barplot <- function(data,year){
  overview.bar <- data %>%
    filter(Year==year) %>%
    select(region,`Rate per 100000`) %>%
    unique() %>%
    mutate(text_label=str_c("State: ",region,
                            "\nRate per 100k: ", `Rate per 100000`
      
    )) %>%
    ggplot(aes(x=reorder(region,desc(`Rate per 100000`)),
               y=`Rate per 100000`,
               fill=region, text=text_label)) +
    geom_bar(stat="identity") + 
    xlab("State") + 
    # theme_classic() +
    theme_minimal()+
    theme(legend.position = 'none') +
    theme(axis.text.x = element_text(angle=45)) + 
    theme(text = element_text(family = "my_font"))
  ggplotly(overview.bar,tooltip="text")
}
overview_barplot(overview_map,2018)

overview_table <- function(data,year){
  data %>%
    filter(Year==year) %>%
    select(region,`Rate per 100000`) %>%
    unique() %>%
    arrange(desc(`Rate per 100000`)) %>%
    gt()
}
overview_table(overview_map,2018)
