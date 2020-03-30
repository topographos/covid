devtools::install_github("gadenbuie/ggpomological")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(rvest)
library(sf)
library(stringr)
library(tmap)
library(gganimate)
library(hrbrthemes)
devtools::install_github("hrbrmstr/hrbrthemes")

# get todays dates and times

now = now()

date= today()

date_char = as.character.Date(date)

wday = wday(date, label = TRUE)

new_col_name = paste0(wday, "_", date)

# scrape data from the website into a table

url = xml2::read_html("https://www.gov.scot/coronavirus-covid-19/")

selector_name<-"table"

scotland_total = html_nodes(x = url, css = "li") %>% 
  html_text(trim = TRUE) %>% 
  unlist() 



cases_latest <- html_node(x = url, css = selector_name) %>%
  html_table(trim = TRUE) %>%
  mutate(cases = `Positive cases`) %>%
  rename(HBName = `Health board`,
         !!date_char := `Positive cases`) %>%
  mutate(HBName = trimws(as.character(HBName))) %>%
  mutate(HBName = replace(HBName, str_detect(HBName, "Ayrshire"), "Ayrshire and Arran"))

# load  healthboards dataset

hb_shp = st_read("data/SG_NHS_HealthBoards_2019.shp") %>% 
  select(HBName) %>% 
  mutate(HBName = trimws(as.character(HBName)))

# join covid table to health board geofraphies

hb_cases = left_join(hb_shp, cases_latest, by = "HBName")
  #replace(is.na(.), 0)
  
#make a map
legend_title = paste0("Data from ", date_char)
map_hb = tm_shape(hb_cases) +
  tm_polygons( col = "cases", 
               style = "jenks", 
               palette = "Reds",
               title = legend_title,
               colorNA = "white",
               textNA = "no cases"
               ) +
  tm_bubbles(col = "black",
             size = "cases",
             shape = 19) +
  tm_layout(frame = FALSE,
            title = "Positive cases of COVID-19",
            ) +
  tm_credits("Source:https://www.gov.scot/coronavirus-covid-19/",
             position=c("left", "bottom")
             )

map_cases = tm_shape(hb_cases) +
  tm_borders() +
  tm_bubbles(size = "cases",
             style="fixed", breaks=c(0, seq(0, 6, by=2), Inf),
            # palette = "Reds",
             title.size = "Number of cases",
             title.col = "",
             shape = 19) +
  tm_layout(frame = FALSE,
            title = "Positive cases of COVID-19",
  ) +
  tm_credits("Source:https://www.gov.scot/coronavirus-covid-19/",
             position=c("left", "bottom")
  )
?tm_bubbles()
tmap_mode("plot")
map_hb


#interctive map
tmap_mode("view")
map_hb

# update master table

scotland_cases = read_csv("data/scotland_cases.csv") %>% 
  left_join(cases_latest, by = "HBName") %>% 
  select(-cases)


write_csv(scotland_cases, "data/scotland_cases.csv")

# pivot longer
covid_longer = covid_master %>%
pivot_longer(-HBName, names_to = "date", values_to = "cases_count")  %>%
mutate(date = as.Date(date))

covid_longer %>%
ggplot(aes(x=date, y=cases_count, color = HBName))+
 geom_line() +
 scale_x_date(date_breaks = '1 day', date_labels = '%m %d') +
 ?scale_color_pomological()
 

# create master table - only once

scotland_totals = read_csv("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-totals-scotland.csv")


scotland_totals = scotland_totals %>% 
mutate(new_cases = (ConfirmedCases - lag(ConfirmedCases)))  %>%
mutate(pct_change = new_cases / lag(ConfirmedCases) * 100) %>%
mutate(pct_check = (ConfirmedCases/lag(ConfirmedCases) - 1) * 100)

scotland_totals %>% filter(new_cases >0 ) %>% 
  ggplot()+
  geom_line(aes(x=Date, y=new_cases), color='#ba0000') +
  ggtitle("Daily Confirmed Cases")
  labs(y = "Cases") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_date(date_breaks = '1 week', date_labels = '%m %d') +
  scale_linetype('')

#plot 
covid = ggplot(scotland_totals)+
  geom_line(aes(x=Date, y=Deaths), color='black') +
  geom_point(aes(x=Date, y=Deaths), color='black')+
  geom_line(aes(x=Date, y=ConfirmedCases), color='#ba0000') +
  geom_point(aes(x=Date, y=ConfirmedCases), color='#ba0000')+
  labs(y = "Cases") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_date(date_breaks = '1 week', date_labels = '%m %d') +
  scale_linetype('')

covid_anim = covid + transition_reveal(Date)

anim_save("covid_anim_plot.gif", covid_anim)

scotland_cases = read_csv("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv") %>% 
  filter(Country == "Scotland") %>% 
  select(Area, Date, TotalCases) %>% 
  rename(HBName = Area)

scotland_cases_wide = scotland_cases %>% 
  pivot_wider(names_from = Date, values_from = TotalCases)

write_csv(scotland_cases_wide, "data/scotland_cases.csv")


