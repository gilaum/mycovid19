---
title: "myc19"
output: html_document
---


```{r setup, include=FALSE}
library(flexdashboard)
library(tidyverse)
library(scales)
library(lubridate)
library(gridExtra)
library(openintro)
#library(plotly)
#library(DT)
```

```{r}
knitr::opts_chunk$set(fig.width=12, fig.height=8) 
```


```{r}
###############################################################
###############################################################
# Definition and Scripts 

# Turn off scientific notation
options(scipen=999)

# Define colors
cb.blue <- "#0072B2"
cb.orange <- "#E69F00"
cb.purple <- "#CC79A7"
cb.red <- "#D55E00"
cb.black <- "#000000"

cb.six <- c("#000000",  
            cb.blue, 
            cb.orange, cb.purple,
            "#56B4E9", "#009E73" )
cb.seven <- c("#009E73", cb.blue, "#000000",
              "#999999", 
              cb.orange, cb.purple, cb.red) #"red")

cb.8 <- c("#009E73", cb.blue, "#000000",
              "#999999", 
              cb.orange, cb.purple, "red", "maroon")


clr.ww.conf = cb.orange
clr.us.conf = cb.blue
clr.ww.dead = 'darkred'
clr.us.dead = cb.purple

###############################################################
###############################################################
# hard code in 1 million so I don't fat finger it later
mill <- 1000000

###############################################################
###############################################################

# Population Statistics
michpop <- 10000000
ohpop <- 11700000
txpop <- 28700000
wipop <- 5800000
capop <- 39600000
nypop <- 19500000
wapop <- 7500000
flpop <- 21500000

## worldwide pop
italypop <- 61000000
frpop <- 65000000
gerpop <- 84000000
chpop <- 1439000000
uspop <- 327000000
skpop <- 51000000
spainpop <- 47000000

###############################################################
###############################################################

# American War Deaths
#  From Wikipedia

civil.war <- 750000
WW2 <- 405399
WW1 <- 116516
nam <- 58209
korean.war <- 54246
iraq.war <- 4576
afg <- 2216

###############################################################
###############################################################

# Per 1 million population graph
#     a = df
#     type = conf, act, death, recovered
#     region = one of the following:
#             State for US plot; or Country.Reg for worldwide plot
#     typex = conft, act, death, recovered

graph.my.data.per1mm <- function(a, type, region, typex) {
  ggplot(a, aes(x = date, y = type, na.rm = TRUE,
               color = region,
               group = region
    )) +
    geom_point(size = 3) + 
    geom_line(size = 2) +
    scale_colour_manual(values = cb.seven) +
    scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
    scale_y_continuous(labels = comma) +
    #xlab("Date") +
    ylab("per 1 Million Population") +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = NULL)) +
    theme_bw() +
    theme(legend.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 13, angle = 35),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
    labs(title = paste("Per 1 Million Population, COVID-19",
                  typex,
                  "Cases")) +
    theme(plot.title = element_text(size = 16))
}
  

###############################################################
###############################################################  
               
# Raw data graph
#     a = df
#     type = conf, act, death, recovered
#     region = one of the following:
#             State for US plot; or Country.Reg for worldwide plot
#     typex = conft, act, death, recovered

graph.my.data.raw <- function(a, type, region, typex) {
  ggplot(a, aes(x = date, y = type, na.rm = TRUE,
                color = region,
                group = region
  )) +
    geom_point(size = 3) + 
    geom_line(size = 2) +
    scale_colour_manual(values = cb.seven) +
    scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
    scale_y_continuous(labels = comma) +
    xlab("") +
    ylab("Total") +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = NULL)) +
    theme_bw() +
    theme(legend.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 13, angle = 35),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
    labs(title = paste("Total Number of COVID-19",
                       typex,
                       "Cases")) +
    theme(plot.title = element_text(size = 16))
}


###############################################################
###############################################################  

# Raw data graph
#     a = df
#     type = conf, act, death, recovered
#     region = one of the following:
#             State for US plot; or Country.Reg for worldwide plot
#     typex = conft, act, death, recovered

graph.bigpic.us.raw <- function(a, type, typex) {
  a %>% 
    group_by(date) %>% 
    summarize(tot.conf = sum(us.conf),
              tot.deaths = sum(us.death),
              tot.rec = sum(us.rec),
              tot.active = sum(us.act)) %>% 
  ggplot(aes(x = date, y = type, na.rm = TRUE#,
                #color = region,
                #group = region
  )) +
    geom_point(size = 3) + 
    geom_line(size = 2) +
    #scale_colour_manual(values = cb.seven) +
    scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
    scale_y_continuous(labels = comma) +
    xlab("") +
    ylab("Total") +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = NULL)) +
    theme_bw() +
    theme(legend.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 13, angle = 35),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
    labs(title = paste("Total Number of COVID-19",
                       typex,
                       "Cases")) +
    theme(plot.title = element_text(size = 16))
}

###############################################################
###############################################################  
###############################################################
###############################################################  

# To insert valueBox in Shiny App
  
valueBox2 <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-center"),
                      div(style = ("font-size: 40px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}

###############################################################
###############################################################  

```

```{r}
# Prep the Data

# Establish cutoff time of day
gmt.date2 <- as.Date(now("UTC"))
use.this.date <- gmt.date2 - 1
use.this.date <- as.Date(use.this.date, origin = "1970-01-01")

# Get the dates we want to scrape the correct data
dates <- seq(as.Date("2020-01-22"), use.this.date, by  = 1)

# Format the dates in the format of the html used by Johns Hopkins
dates2 <- format(dates, "%m-%d-%Y")

# Scrape the data
mytest <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/', dates2, ".csv")

# Get data into a data frame
trythis <- lapply(mytest, read.csv)

cvdata <- do.call(bind_rows, trythis)

# copy the df
cvdata2 <- cvdata

# There have been various column names used by Johns Hopkins. Next step is to get them common
#  For province/state, country/region, Last Update, Lat, Long
cvdata2 <- cvdata2 %>% 
  mutate(PS = ifelse(is.na(Province_State), ?..Province.State, 
                      Province_State)) %>% 
  mutate(PS2 = ifelse(is.na(PS), Province.State, PS)) %>% 
  mutate(CR = ifelse(is.na(Country_Region), Country.Region, 
                     Country_Region)) %>% 
  mutate(LU = ifelse(is.na(Last_Update), Last.Update, 
                     Last_Update)) %>% 
  mutate(lats = ifelse(is.na(Latitude), Lat, 
                     Latitude),
       longs = ifelse(is.na(Longitude), Long_,
                      Longitude))

# copy the df
cvdata3 <- cvdata2

# Now only keep the columns we want to keep
cvdata3 <- cvdata3 %>% 
  select(PS2, CR, Admin2, LU, Confirmed, Deaths, Recovered, lats, longs) 

# Rename most columns
cvdata3 <- cvdata3%>% 
  dplyr::rename(Prov.State = PS2,
         Country.Reg = CR,
         City = Admin2,
         Date = LU,
         Lat = lats,
         Long = longs) 

# copy the df
cvdata4 <- cvdata3

# Add new columns for Active cases
cvdata4 <- cvdata4 %>% 
  mutate(Active = Confirmed - Deaths - Recovered) 

# Separate info in Date column so we have a column with only date info
cvdata4 <- cvdata4 %>% 
  separate(Date, c("date", "other"), sep = "([\\ \\T])") %>% 
  select(-other)

cvdata4x <- cvdata4


# Dates from Johns Hopkins are in a variety of formats and are not consistently
#   formatted from one day to the next.
#    This will get the dates to be in consistent format

cvdata4x <- cvdata4x %>% 
  mutate(date2 = date,
         date3 = date) %>% 
  separate(date, c("date", "other2", "other3"), sep = "([\\-])") %>%  # winner winner
  separate(date, c("blah1", "blah2", "blah3"), sep = "([\\/])") %>%  # winner winner
  mutate(newmonth = ifelse(blah1 == "2020", other2, blah1)) %>% 
  mutate(newmonth2 = ifelse(is.na(newmonth), blah1, newmonth)) %>% 
  #mutate(newmonth2 = ifelse(newmonth == other2, newmonth, blah1)) %>% 
  #mutate(newday = ifelse(blah2 == "2020", other3, blah2)) %>% 
  mutate(newday = ifelse(is.na(other3), blah2, other3)) %>% 
  #mutate(newday2 = ifelse(is.na(newday), blah2, newday)) %>% 
  select(date2, newmonth2, newday, other2, other3, blah1, blah2, blah3, everything())

# Convert newmonth2 and newday to numeric
cvdata4x$newmonth2 <- as.numeric(as.character(cvdata4x$newmonth2))
cvdata4x$newday <- as.numeric(as.character(cvdata4x$newday))

# First get month and day to be two digits each
#   Then create new date column
cvdata4x <- cvdata4x %>% 
  mutate(blah11 = sprintf("%02d", newmonth2)) %>% 
  mutate(blah12 = sprintf("%02d", newday)) %>% 
  mutate(newdate99 = paste("2020",
                           blah11,
                           blah12,
                           sep = "-")) %>% 
  select(newdate99, blah11, newmonth2, blah12, newday, everything())

# copy df
cvdata4y <- cvdata4x

# Keep appropriate columns
cvdata4y <- cvdata4y %>% 
  mutate(date = newdate99) %>% 
  select(Prov.State, Country.Reg, City, date, Confirmed, Deaths,
         Recovered, Active, Lat, Long)

#copy the df
cvdata5 <- cvdata4y

# copy the df
cvdata6 <- cvdata5

# South Korea data is dirty for March 13. This will fix
cvdata6 <- cvdata6 %>% 
  mutate(date = replace(date, 
                        Confirmed == 7979 & Deaths == 66 & Recovered == 510 & Country.Reg == "Korea, South",
                        "2020-03-13")) 

# change format of date to be Date instead of Character
cvdata6 <- cvdata6 %>% 
  mutate(newdate = as.Date(date, origin = "1970-01-01")) %>% 
  mutate(date = newdate) %>% 
  select(-newdate) 


# In Prov.State column, Johns Hopkins had originally included (city, state abbrev). They have since
#  changed the formatting. We need to get the formats to be common
cvdata6.us <- cvdata6 %>% 
  filter(Country.Reg == "US") %>% 
  mutate(PS2 = Prov.State) %>% 
  separate(Prov.State, c("blah", "blah2"), sep = "([\\,])") %>% 
  mutate(blah2.5 = trimws(blah2, "l")) %>% 
  mutate(blah4 = abbr2state(blah2.5)) %>% 
  mutate(PS3 = ifelse(is.na(blah4), PS2, blah4),
         State = PS3) %>% 
  select(State, Country.Reg, City, date, Confirmed, Deaths, Recovered, Active, 
         Lat, Long)

# Find most recent date in data set
maxdate <- max(cvdata6$date)

# Set up US data frame by state
bystate <- cvdata6.us %>% 
  filter(Country.Reg == "US") %>% 
  group_by(State, date) %>% 
  summarize(us.conf = sum(Confirmed),
            us.death = sum(Deaths),
            us.rec = sum(Recovered),
            us.act = sum(Active)) %>% 
  arrange(desc(us.conf), .by_group = TRUE) %>% 
  ungroup()

curr.us.counts <- bystate %>% 
  filter(date == maxdate) %>% 
  summarize(tot.conf = sum(us.conf),
            tot.deaths = sum(us.death),
            tot.rec = sum(us.rec),
            tot.active = sum(us.act))

#curr.us.counts

curr.us.death.rate <- round(((curr.us.counts$tot.deaths/curr.us.counts$tot.conf) * 100), 2)

# Johns Hopkins originally set up "South Korea" as a nation. They then changed
#  the nomenclature to "Korea, South". Need to get this to be common
cvdata6 <- cvdata6 %>% 
  mutate(c2 = ifelse(Country.Reg == "Korea, South", "South Korea", Country.Reg)) %>%
  select(-Country.Reg) %>% 
  mutate(Country.Reg = c2) %>% 
  select(-c2) %>% 
  select(Country.Reg, everything()) 

# Combine Mainland China and China
cvdata6 <- cvdata6 %>% 
  mutate(cr2 = ifelse(Country.Reg == "China" | Country.Reg == "Mainland China", "China",
                      Country.Reg)) %>% 
  mutate(Country.Reg = cr2) %>% 
  select(-cr2) %>% 
  select(Country.Reg, everything()) 

# Set up worldwide data frame by country
bycountry <- cvdata6 %>% 
  group_by(Country.Reg, date) %>% 
  summarize(ww.conf = sum(Confirmed),
            ww.death = sum(Deaths),
            ww.rec = sum(Recovered),
            ww.act = sum(Active)) %>% 
  arrange(desc(ww.conf), .by_group = TRUE) %>% 
  ungroup() 

curr.ww.counts <- bycountry %>% 
  filter(date == maxdate) %>% 
  summarize(tot.conf = sum(ww.conf),
            tot.deaths = sum(ww.death),
            tot.rec = sum(ww.rec),
            tot.active = sum(ww.act))

#curr.ww.counts

curr.ww.death.rate <- round(((curr.ww.counts$tot.deaths/curr.ww.counts$tot.conf) * 100), 2)

# worldwide data EXCLUDING USA
ww.not.us <- cvdata6 %>% 
  filter(Country.Reg != "US") %>% 
  group_by(date) %>% 
  summarize(ww.conf = sum(Confirmed),
            ww.death = sum(Deaths),
            ww.rec = sum(Recovered),
            ww.act = sum(Active)) %>% 
  mutate(region = "rest.of.world") %>% 
  gather(type, cases, ww.conf:ww.death) 

# USA only data
us.only <- cvdata6 %>% 
  filter(Country.Reg == "US") %>% 
  group_by(date) %>% 
  summarize(usa.conf = sum(Confirmed),
            usa.death = sum(Deaths),
            usa.rec = sum(Recovered),
            usa.act = sum(Active)) %>% 
  mutate(region = "USA") %>% 
  gather(type, cases, usa.conf:usa.death)

# Merge worldwide NOT USA, and USA-only
mynewdf <- bind_rows(ww.not.us, us.only)

# Overall Confirmed Cases
overall.confirm.graph <- mynewdf %>% 
  filter(type == "ww.conf" | type == "usa.conf") %>% 
  ggplot(aes(x = date, y = cases, na.rm = TRUE,
           color = region,
           group = region
)) +
  geom_point(size = 3) + 
  geom_line(size = 2) +
  scale_colour_manual(values = c(cb.blue, cb.red)) +
  scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
  scale_y_continuous(labels = comma) +
  xlab("") +
  ylab("Total") +
  theme(panel.grid.minor = element_blank()) +
  guides(color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 13, angle = 30),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  labs(title = "Worldwide Total Number of COVID-19 Confirmed Cases") +
  theme(plot.title = element_text(size = 16))

# Death Graph
overall.death.graph <- mynewdf %>% 
  filter(type == "ww.death" | type == "usa.death") %>% 
  ggplot(aes(x = date, y = cases, na.rm = TRUE,
             color = region,
             group = region
  )) +
  geom_point(size = 3) + 
  geom_line(size = 2) +
  scale_colour_manual(values = c(cb.blue, cb.red)) +
  scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
  scale_y_continuous(labels = comma) +
  xlab("") +
  ylab("Total") +
  geom_hline(aes(yintercept = nam), color = cb.black, linetype = "dashed") +
  geom_text(aes(x=as.Date("2020-02-10"), nam, 
                label = "U.S. Deaths Vietnam", vjust = -1), size = 3,
            color = cb.black) +
  geom_hline(aes(yintercept = WW1), color = cb.black, linetype = "dashed") +
  geom_text(aes(x=as.Date("2020-02-10"), WW1, 
                label = "U.S. Deaths World War 1", vjust = -1), size = 3,
            color = cb.black) +
  #geom_hline(aes(yintercept = WW2), color = cb.black, linetype = "dashed") +
  #geom_text(aes(x=as.Date("2020-02-10"), WW2, 
  #              label = "U.S. Deaths World War 2", vjust = -1), size = 3,
  #          color = cb.black) +
  geom_hline(aes(yintercept = korean.war), color = cb.black, linetype = "dashed") +
  geom_text(aes(x=as.Date("2020-02-10"), korean.war, 
                label = "U.S. Deaths Korean War", vjust = 2), size = 3,
            color = cb.black) +
  geom_hline(aes(yintercept = (iraq.war + afg)), color = cb.black, linetype = "dashed") +
  geom_text(aes(x=as.Date("2020-02-10"), iraq.war + afg, 
                label = "U.S. Deaths Iraq and Afghanistan", vjust = -1), size = 3,
            color = cb.black) +
  theme(panel.grid.minor = element_blank()) +
  guides(color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 13, angle = 35),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  labs(title = "Worldwide Total Number of COVID-19 Deaths") +
  theme(plot.title = element_text(size = 16))


```

```{r}
# Selected Countries
# |||||
# VVVVV

# Italy data very dirty
# This is attempt to clean
bycountry <- bycountry %>% 
  mutate(ww.conf = replace(ww.conf, date == "2020-03-11" & Country.Reg == "Italy", 12462),
         ww.conf = replace(ww.conf, date == "2020-03-14" & Country.Reg == "Italy", 21157),
         ww.rec = replace(ww.rec, date == "2020-03-11" & Country.Reg == "Italy", 1045),
         ww.rec = replace(ww.rec, date == "2020-03-14" & Country.Reg == "Italy", 1966),
         ww.death = replace(ww.death, date == "2020-03-11" & Country.Reg == "Italy", 827),
         ww.death = replace(ww.death, date == "2020-03-14" & Country.Reg == "Italy", 1441)) %>%
  mutate(ww.act2 = ww.conf - ww.death - ww.rec) %>% 
  mutate(ww.act = ww.act2) %>% 
  select(-ww.act2) 
                        

# Selected Countries
mycountry <- bycountry %>% 
  filter(Country.Reg == "Italy" | Country.Reg ==  "France" | Country.Reg == "Germany" |
           Country.Reg == "China" | Country.Reg == "US" | Country.Reg == "South Korea" |
           Country.Reg == "Spain") 



# Add population figures, and per capita and per 1mm info, into the data frame
mycountry <- mycountry %>% 
  mutate(pop = ifelse(Country.Reg == "Italy", italypop,
                      ifelse(Country.Reg == "France", frpop,
                             ifelse(Country.Reg == "Germany", gerpop,
                                    ifelse(Country.Reg == "China", chpop,
                                           ifelse(Country.Reg == "US", uspop,
                                                  ifelse(Country.Reg == "South Korea", skpop,
                                                         spainpop))))))) %>% 
  mutate(ww.per.cap.conf = ww.conf/pop,
         ww.conf.per1mm = ww.per.cap.conf * mill,
         ww.per.cap.death = ww.death/pop,
         ww.deaths.per1mm = ww.per.cap.death * mill,
         ww.per.cap.act = ww.act/pop,
         ww.active.per1mm = ww.per.cap.act * mill,
         ww.per.cap.rec = ww.rec/pop,
         ww.recov.per1mm = ww.per.cap.rec * mill)

```

```{r}
#################
################
# US States
# |||||
# VVVVV

# Selected States
mystates <- bystate %>% 
  filter(State == "Michigan" | State ==  "Ohio" | State == "Texas" |
           State == "Wisconsin" | State == "California" | #State == "New York" |
           State == "Washington" | State == "Florida") 

# Add population figures, and per capita and per 1mm info, into the data frame
mystates <- mystates %>% 
  mutate(pop = ifelse(State == "Michigan", michpop,
                      ifelse(State == "Ohio", ohpop,
                             ifelse(State == "Texas", txpop,
                                    ifelse(State == "Wisconsin", wipop,
                                           ifelse(State == "California", capop,
                                                  ifelse(State == "Florida", flpop,
                                                  #ifelse(State == "New York", nypop,
                                                         wapop))))))) %>% 
  mutate(per.cap.conf = us.conf/pop,
         conf.per1mm = per.cap.conf * mill,
         per.cap.death = us.death/pop,
         deaths.per1mm = per.cap.death * mill,
         per.cap.act = us.act/pop,
         active.per1mm = per.cap.act * mill,
         per.cap.rec = us.rec/pop,
         recov.per1mm = per.cap.rec * mill)

```


Overview
==============

Row
-----------------------
### Worldwide Confirmed  

```{r , echo = FALSE}
#```{r vbox_world_confirmed, echo = FALSE}
ww_confirmed2 <- format(curr.ww.counts$tot.conf, big.mark = ",")
valueBox(value = ww_confirmed2, icon = 'fa-user-md', color = clr.ww.conf)

```


### Worldwide Deaths  
```{r vbox_world_deaths, echo = FALSE}
ww_death2 <- format(curr.ww.counts$tot.deaths, big.mark = ",")
ww_pct_death = round((curr.ww.counts$tot.deaths / curr.ww.counts$tot.conf) * 100, 1)
valueBox(paste(ww_death2, 
         " (", ww_pct_death, "%)"),icon = 'fa-dizzy', color = clr.ww.dead)

```


### U.S. Confirmed  
```{r , echo = FALSE}
us_confirmed2 <- format(curr.us.counts$tot.conf, big.mark = ",")
valueBox(us_confirmed2, icon = 'fa-user-md', color = clr.us.conf)

```


### U.S. Deaths  
```{r , echo = FALSE}
us_death2 <- format(curr.us.counts$tot.deaths, big.mark = ",")
us_pct_death = round((curr.us.counts$tot.deaths / curr.us.counts$tot.conf) * 100, 1)
valueBox(paste(us_death2,
               " (", us_pct_death, "%)"),icon = 'fa-dizzy', color = clr.us.dead)

```

Row 
-----------------------

### Worldwide Confirmed Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
#```{r, fig.width=16}#, fig.height=8}
print(overall.confirm.graph)

```

Row 
-----------------------

### Worldwide Deaths Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
#```{r, fig.width=16, fig.height=8}
plot(overall.death.graph)

```


Worldwide Totals
==============

Row
-----------------------
### Worldwide Confirmed  


```{r , echo = FALSE}
valueBox(ww_confirmed2, icon = 'fa-user-md', color = clr.ww.conf)

```


### Worldwide Deaths  

```{r , echo = FALSE}
#valueBox(curr.ww.counts$tot.deaths, icon = 'fa-dizzy', color = clr.ww.dead)
valueBox(paste(ww_death2, 
         " (", ww_pct_death, "%)"),icon = 'fa-dizzy', color = clr.ww.dead)

```


### U.S. Confirmed  

```{r , echo = FALSE}
valueBox(us_confirmed2, icon = 'fa-user-md', color = clr.us.conf)

```


### U.S. Deaths    
```{r , echo = FALSE}
valueBox(paste(us_death2,
               " (", us_pct_death, "%)"),icon = 'fa-dizzy', color = clr.us.dead)


```



Row  
-----------------------  

### Select Countries Confirmed Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.conf.ww.raw <- graph.my.data.raw(mycountry, 
                                       mycountry$ww.conf, 
                                       mycountry$Country.Reg,
                                       "Confirmed")


plot(graph.conf.ww.raw)

```



Row  
-----------------------  

### Select Countries Deaths Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.death.ww.raw <- graph.my.data.raw(mycountry, 
                                       mycountry$ww.death, 
                                       mycountry$Country.Reg,
                                       "Death")
```

```{r, echo = FALSE}
plot(graph.death.ww.raw)
```


Worldwide Per Million
==============

Row
-----------------------
### Worldwide Confirmed  


```{r , echo = FALSE}
valueBox(ww_confirmed2, icon = 'fa-user-md', color = clr.ww.conf)

```


### Worldwide Deaths  

```{r , echo = FALSE}
#valueBox(curr.ww.counts$tot.deaths, icon = 'fa-dizzy', color = clr.ww.dead)
valueBox(paste(ww_death2, 
         " (", ww_pct_death, "%)"),icon = 'fa-dizzy', color = clr.ww.dead)

```


### U.S. Confirmed  

```{r , echo = FALSE}
valueBox(us_confirmed2, icon = 'fa-user-md', color = clr.us.conf)

```


### U.S. Deaths    
```{r , echo = FALSE}
valueBox(paste(us_death2,
               " (", us_pct_death, "%)"),icon = 'fa-dizzy', color = clr.us.dead)


```



Row  
-----------------------  

### Select Countries Confirmed Per 1 Million Population Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.conf.ww.mil <- graph.my.data.per1mm(mycountry, 
                                          mycountry$ww.conf.per1mm, 
                                          mycountry$Country.Reg,
                                          "Confirmed")

graph.conf.ww.mil

```



Row  
-----------------------  

### Select Countries Deaths Per 1 Million Population Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.death.ww.mil <- graph.my.data.per1mm(mycountry, 
                                          mycountry$ww.deaths.per1mm, 
                                          mycountry$Country.Reg,
                                          "Death")

graph.death.ww.mil

```


U.S. Totals
==============

Row
-----------------------
### Worldwide Confirmed  


```{r , echo = FALSE}
valueBox(ww_confirmed2, icon = 'fa-user-md', color = clr.ww.conf)

```


### Worldwide Deaths  

```{r , echo = FALSE}
#valueBox(curr.ww.counts$tot.deaths, icon = 'fa-dizzy', color = clr.ww.dead)
valueBox(paste(ww_death2, 
         " (", ww_pct_death, "%)"),icon = 'fa-dizzy', color = clr.ww.dead)

```


### U.S. Confirmed  

```{r , echo = FALSE}
valueBox(us_confirmed2, icon = 'fa-user-md', color = clr.us.conf)

```


### U.S. Deaths    
```{r , echo = FALSE}
valueBox(paste(us_death2,
               " (", us_pct_death, "%)"),icon = 'fa-dizzy', color = clr.us.dead)


```



Row  
-----------------------  

### Select U.S. States Confirmed Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.conf.us.raw <- graph.my.data.raw(mystates, 
                                          mystates$us.conf, 
                                          mystates$State,
                                          "Confirmed")

plot(graph.conf.us.raw)

```



Row  
-----------------------  

### Select U.S. States Deaths Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.death.us.raw <- graph.my.data.raw(mystates, 
                                       mystates$us.death, 
                                       mystates$State,
                                       "Death")

plot(graph.death.us.raw)
```

U.S. Per Million
==============

Row
-----------------------
### Worldwide Confirmed  


```{r , echo = FALSE}
valueBox(ww_confirmed2, icon = 'fa-user-md', color = clr.ww.conf)

```


### Worldwide Deaths  

```{r , echo = FALSE}
#valueBox(curr.ww.counts$tot.deaths, icon = 'fa-dizzy', color = clr.ww.dead)
valueBox(paste(ww_death2, 
         " (", ww_pct_death, "%)"),icon = 'fa-dizzy', color = clr.ww.dead)

```


### U.S. Confirmed  

```{r , echo = FALSE}
valueBox(us_confirmed2, icon = 'fa-user-md', color = clr.us.conf)

```


### U.S. Deaths    
```{r , echo = FALSE}
valueBox(paste(us_death2,
               " (", us_pct_death, "%)"),icon = 'fa-dizzy', color = clr.us.dead)


```



Row  
-----------------------  

### Select U.S. States Confirmed Per 1 Million Population Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.conf.us.mil <- graph.my.data.per1mm(mystates, 
              mystates$conf.per1mm, 
              mystates$State,
              "Confirmed")

graph.conf.us.mil

```


Row  
-----------------------  

### Select U.S. States Deaths Per 1 Million Population Graph  

```{r, echo = FALSE, warning = FALSE, message=FALSE}
graph.death.us.mil <- graph.my.data.per1mm(mystates, 
                                      mystates$deaths.per1mm, 
                                      mystates$State,
                                      "Death")

graph.death.us.mil

```


Notes
==============

Row
-----------------------
 Data come from Johns Hopkins (JH):  
 
 
   *  Updated daily at approximately 7:59pm eastern time.  
   
   *  JH pulls its data from a variety of sources, including but not limited to WHO, China CDC, US CDC, European Centre for Disease Prevention and Control, Government of Canada, etc.  
   
   *  Click here [https://github.com/CSSEGISandData/COVID-19] for direct links to data source.  
   
   *  Data are not always consistent. There are several instances of flaws in the data. JH has done an excellent job of managing the flaws, but they do continue to exist.  
   
   *  I can be reached via twitter @TomAlig333

