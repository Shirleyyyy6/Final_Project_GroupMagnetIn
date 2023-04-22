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
library(extrafont)

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
font_add_google('Abril Fatface', family ='my_font_title')

# Plot the map
create_overview <- function(data,year){
overview.plot <- 
  data %>%
  filter(Year==year) %>%
  ggplot( aes(x = long, y = lat, group = region, fill = `Rate per 100000`)) +
  geom_polygon(color = "white",aes(text = text_label)) +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(legend.position = 'none') +
  theme(title = element_text(hjust = 0.5)) +
  theme(text = element_text(family = "my_font")) +
  scale_fill_viridis(alpha=0.8,option = 'G') +
  xlab("longitude") + ylab("latitude")
ggplotly(overview.plot,tooltip = "text")
}
create_overview(overview_map,2008)
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
    theme(text = element_text(family = "my_font")) +
    scale_fill_viridis_d(alpha=0.8,option="G")
  ggplotly(overview.bar,tooltip="text")
}
overview_barplot(overview_map,2008)
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




overview_title <- function(data,year){
  text <- ifelse(startsWith(deparse(substitute(data)), "overview"),
                 "morbidity", "mortality")
  out <- paste("US HIV Map in",as.character(year),"\n(by" , text, ")")
  text_df <- data.frame(x = 0.5, y = 0.5, label = out)

  ggplot(text_df, aes(x=x,y=y,label = label)) +
    geom_text(size = 20, family = 'my_font_title') +
    theme_void()
}
overview_title(overview_map,2018)

