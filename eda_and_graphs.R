#### LIBRARIES & LOGIN ####
library(DBI)
library(RMySQL)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(readxl)
library(cowplot)
library(sf)
library(ggsflabel)
library(ggnewscale)
library(ggrepel)

db_connection <-
  dbConnect(
    MySQL(),
    dbname = "DB_NAME",
    host = "DB_HOSTNAME",
    user = Sys.getenv('MYSQL_USER'),
    password = Sys.getenv('MYSQL_PWORD'))

names <- read_xlsx("tract_names.xlsx")

# INFLATION ####
get_inflation_cf <- function(to_year, symbol = "CPIAUCSL", source = "FRED"){
  
  require(quantmod)
  to_year <- as.character(to_year)
  
  # RCPIURS is also an option which retroactively updates inflation prior to 2000 with advances in methodology
  # Census recommends this one
  # see: https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm
  # and: https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch10.pdf
  # I am using this one for now which is the most up to date methodology
  ts <- getSymbols("CPIAUCSL", src='FRED', auto.assign = FALSE) #Consumer Price Index for All Urban Consumers: All Items
  
  # getSymbols returns monthly inflation index
  # we want yearly for our applications, can change later
  avg.cpi <- apply.yearly(ts, mean)
  
  # Conversion factor for all years based on desired year
  cf <- as.numeric(avg.cpi[to_year]) / avg.cpi
  data.frame("cf" = cf, "year" = as.numeric(substr(row.names(data.frame(cf)), 1, 4)))
  
}

inflation_2021 <- get_inflation_cf(2021)

# FONTS ####
library(showtext) 
library(sysfonts)
font_add(family="poppins", regular="Poppins-Regular.ttf")
font_add(family="oswald", regular="Oswald-Regular.ttf")
font_add(family="poppins bold", regular="Poppins-Bold.ttf")

showtext_auto()

# EXPLORATION ####
##### Income Change #####
#Percent change of income
earnings.og <- dbGetQuery(db_connection, "SELECT * FROM earnings WHERE county_fips = '129';") %>% 
  left_join(inflation_2021, by = "year") %>% 
  mutate(medn_earnings = medn_earnings*CPIAUCSL)

#adding "all" sex for fulltime employees as average of M & F
fulltime <- earnings.og %>% 
  filter(emp_type == "fulltime") %>% 
  group_by(tract_fips, year, race) %>% 
  mutate(fulltime_all_sexes_average = (medn_earnings + lag(medn_earnings))/2) %>% 
  select(tract_fips, year, race, emp_type, fulltime_all_sexes_average) %>% 
  rename(medn_earnings = fulltime_all_sexes_average) %>% 
  filter(!is.na(medn_earnings)) %>% 
  mutate(sex = "all")

earnings <- full_join(fulltime, earnings.og) %>% 
  select(tract_fips, race, sex, year, emp_type, medn_earnings, medn_earnings_moe)

##### Property Taxes ####
property_taxes <- read_xlsx("D:/RStudio/CFC/property_taxes.xlsx") %>% 
  left_join(inflation_2021, by = "year") %>%
  select(state_fips, county_fips, tract_fips,year,total_medn,total_medn_moe,CPIAUCSL) %>% 
  mutate(total_medn = total_medn*CPIAUCSL)


#difference in property taxes since 2010 to 2020
taxes.y <- property_taxes %>% 
  filter(year == 2010,
         county_fips == "129") %>% 
  rename(total_medn_2010 = total_medn) %>%
  select(state_fips, county_fips, tract_fips, total_medn_2010)

taxes <- property_taxes %>% 
  filter(between(year, 2010, 2020),
         county_fips == "129")

taxes.y <- full_join(taxes.y, taxes)

taxes.y <- taxes.y %>%   
  arrange(tract_fips, year) %>%
  mutate(diff_total_medn = total_medn - total_medn_2010) %>% 
  mutate(total_taxes_perc_change = ((total_medn - total_medn_2010)/ total_medn_2010)*100) %>%   select(tract_fips, year, total_medn, diff_total_medn, total_taxes_perc_change)

county <- taxes.y %>% 
  filter(tract_fips == "000000",
         year == 2019) %>% 
  select(tract_fips, total_taxes_perc_change)

#define 10 highest tax increases
ten.highest.taxes <- taxes.y %>% 
  filter(year == 2019) %>% 
  arrange(desc(total_taxes_perc_change)) %>% 
  select(tract_fips, total_taxes_perc_change) %>% 
  head(10)

ten.highest.taxes <- bind_rows(ten.highest.taxes, county)

##### Household Income ####
household_income <- dbGetQuery(db_connection, "SELECT * FROM household_income WHERE county_fips = '129';") %>% 
  left_join(inflation_2021, by = "year") %>% 
  mutate(median_household_income = median_household_income*CPIAUCSL)

###### Taxes percent of Hh income ####
taxes.hh <- full_join(taxes, household_income)

taxes.hh <- taxes.hh %>% #used later in Poverty graph
  group_by(tract_fips, year) %>% 
  select(tract_fips, year, race, total_medn, total_medn_moe, median_household_income, median_household_income_moe) %>% 
  mutate(tot_tax_hh_perc = (total_medn / median_household_income))


##### Housing Problems ####
housing_problems <- dbGetQuery(db_connection, "SELECT * FROM housing_problems WHERE county_fips = '129';")

###### Cost-burden Graph #####
ten_costburden <- left_join(ten.highest.taxes, housing_problems)

ten_costburden %>%
  filter(year == 2019) %>% 
  select(tract_fips, high_cost_pct_hhold) %>% 
  arrange(desc(high_cost_pct_hhold))

ten_costburden <- ten_costburden %>% left_join(names %>% 
                select(tract_fips, location) %>% 
                rename(nickname = location))

ten_costburden %>% #Nick, graph #4
  filter(year == 2019,
         tract_fips!="000000") %>% 
  ggplot(aes(reorder(nickname, high_cost_pct_hhold), high_cost_pct_hhold, 
             fill = factor(ifelse(high_cost_pct_hhold > .356, "#EF946C",
                                         ifelse(high_cost_pct_hhold < .356, "6A3D9A", "")))))+
    geom_bar(stat="identity", position = position_dodge(0.9), color = "black", alpha = .6)+ 
    scale_fill_manual(name = "area", values=c("#EF946C", "#6A3D9A"))+
    scale_y_continuous(limits = c(0, .6), expand = c(0, 0))+
    geom_hline(yintercept=.356, alpha = .2, size =1, linetype = "dashed")+
    #geom_text(aes(label = "County Average", y= .356, x = 1, hjust = -.1), size=4, family = "poppins")+ #looks weird as hell so I'll label later
    geom_text(aes(label=(paste0(high_cost_pct_hhold*100,"%"))),
              hjust = 1.1,
              size = 4.2,
              inherit.aes = TRUE)+
    theme(axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          text=element_text(size=16,
                            family="poppins"),
          legend.position = "none",
          plot.title = element_text(
            family="oswald",
            hjust = 13,
            size = 16
            )
          )+
    labs(title = "% of Housing Cost-Burdened in Neighborhood in 2019", x="",y="")+
    coord_flip()

ten_costburden %>%
  filter(year == 2010) %>% 
  ggplot(aes(tract_fips, high_cost_pct_hhold, fill = factor(ifelse(tract_fips=="000000", "Highlighted", "Normal"))))+
  geom_bar(stat="identity", position = position_dodge(0.9))+ 
  scale_fill_manual(name = "area", values=c("dark blue","light blue"))+
  geom_hline(yintercept=.356)+
  geom_text(aes(label=(paste0(high_cost_pct_hhold*100,"%"))),
            hjust = .5,
            vjust=-.5,
            size = 3.5,
            inherit.aes = TRUE)+
  theme(axis.text.x=element_text(angle=40, vjust=.5),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())+
  labs(title = "Percent of those Housing Cost-Burden in 2010", x="",y="")

##### Poverty Graph ####
poverty <- dbGetQuery(db_connection, "SELECT * FROM poverty_fpl WHERE county_fips = '129';") %>%  
  mutate(perc_below_200 = lt200fpl_ct/total_ct)

poverty.hh <- full_join(poverty, taxes.hh) %>% 
  select(tract_fips, year, total_ct, lt200fpl_ct, perc_below_200, race, total_medn, median_household_income, tot_tax_hh_perc) %>% 
  left_join(taxes.y) %>% 
  left_join(names %>%
              select(tract_fips, location) %>%
              rename(nickname = location))

#define the top tracts I want
disparity <- poverty.hh %>%
  filter(perc_below_200 > 0.30,
         year == 2019,
         race %in% c("whitenonhisp", "black", "hispanic")) %>%
  arrange(desc(total_taxes_perc_change)) %>% 
  distinct(tract_fips, year) %>% 
  head(6)

disparity.2 <- poverty.hh %>%
  filter(perc_below_200 > 0.30,
         year == 2017,
         race %in% c("whitenonhisp", "black", "hispanic")) %>%
  arrange(desc(total_taxes_perc_change)) %>% 
  distinct(tract_fips, year) %>% 
  head(1)

disparity <- bind_rows(disparity, disparity.2)

#i want the county too
disparity <- disparity %>% 
  add_row(tract_fips = "000000",
          year = 2019)

#bring back rest of the info I want
disparity <- disparity %>% left_join(poverty.hh %>% 
                                       filter(race %in% c("whitenonhisp", "black", "hispanic")))

#let's add some SSS comparisons
sss <- read_xlsx("D:/RStudio/CFC/SSS/NC2020_SSS.xlsx", sheet = "NHC_2") %>%
  rename(median_household_income = "Annual Self-Sufficiency Wage")

sss <- left_join(sss, inflation_2021, by = "year") %>% 
  mutate(median_household_income = median_household_income*CPIAUCSL) %>% 
  select(nickname, median_household_income)

sss <- taxes.hh %>% 
  filter(tract_fips == "000000",
         race =="all",
         year ==2019) %>% 
  select(tract_fips, year, total_medn) %>% 
  bind_cols(sss) %>% 
  mutate(tract_fips = "SSS_000000",
         race = "SSS") %>% 
  mutate(tot_tax_hh_perc = (total_medn / median_household_income)) %>%
  rename(family_type = nickname) %>%
  mutate(nickname = "New Hanover County")

disparity <- bind_rows(disparity, sss)

#make sure I got the correct ones
disparity %>% 
  distinct(tract_fips)

###### Graph #####

##update this one
disparity %>%
  filter(tot_tax_hh_perc < .3) %>% 
  ggplot(aes(x=median_household_income,
             y=tot_tax_hh_perc,
             color = race,
             shape = nickname)
  )+
  geom_point(alpha= .7, size = 4.2, stroke = 1.1)+
  scale_x_continuous(labels=scales::dollar_format(),
                     breaks = seq(20000, 80000, by = 15000))+
  scale_y_continuous(labels=scales::percent_format(),
                     #limits = c(.01, .32),
                     limits = c(.019, .09),
                     breaks = seq(.02, .35, by = .03)
                       )+
  scale_shape_manual(values = c(20, 0:7),
                     name = NULL,
                     breaks = c("New Hanover County",
                                "S. of Downtown, Tract 112",
                                "E. of Downtown, Tract 102",
                                "Echo Farms/Silver Lake",
                                "Downtown",
                                "Alandale",
                                "E. Murrayville/W. Bayshore",
                                "N. of Greenfield"
                                ),
                     labels = c("New Hanover County Medians",
                                "S. of Downtown, Tract 112",
                                "E. of Downtown, Tract 102",
                                "Echo Farms/Silver Lake",
                                "Downtown",
                                "Alandale",
                                "E. Murrayville/W. Bayshore",
                                "N. of Greenfield"
                     )
                     )+
  scale_color_manual(values=c("#EF946C","#76AE87","black","#6A3D9A")
                     )+
  geom_label_repel(data=disparity[disparity$race=="SSS",],
            aes(label=
            "SSS for 2 Adults, 1 Preschooler, \n& 1 Teenager in NHC.\n\nNeeded income: $58,958.84\nMedian property taxes: $1,863.34"),
            size = 3.8,
            nudge_x = 65500,
            nudge_y = .016,
            hjust = 0,
          alpha =.5,
          family="poppins")+
  theme(text=element_text(size=16,
                          family="poppins"),
        axis.title=element_text(size=12),
        plot.title = element_text(
          family="oswald",
          hjust = -.01,
          size = 18),
        #plot.margin = margin(1,1,1,1, "cm"),
        legend.position="top",
        legend.justification='left',
        legend.background=element_rect(fill='transparent'),
        legend.key=element_rect(fill='transparent'),
       # legend.margin=margin(t=.04, l=.2, r=.02, b=.02, unit='cm'),
        #legend.box.margin=margin(10,10,10,10), #may need for reasons
       # legend.box="horizontal",
        legend.text = element_text(size=12))+
  guides(shape=guide_legend(
    ncol=2),
    color="none")+
  labs(title="Property Taxes as % of Household Income in 2019",
       x="Household Income",
       y="% Income Spent on Property Taxes")

###### Smaller Graph #####

#aggregate
downtown <- as_tibble_col(c("011000","011200","010200","010900"), column_name = "tract_fips") %>% 
  left_join(poverty.hh) %>% 
  filter(race %in% c("whitenonhisp", "black", "hispanic"))

downtown.1 <- downtown %>%
  filter(year == 2019,
         tract_fips != "011000")

downtown <- downtown %>% 
  filter(year == 2017,
         tract_fips == "011000") %>% 
  full_join(downtown.1) %>% 
  group_by(race) %>% 
  mutate(median_household_income = mean(median_household_income),
         tot_tax_hh_perc = mean(tot_tax_hh_perc),
         nickname = "Downtown")

downtown <- downtown %>% head(3)

ogden <- as_tibble_col(c("011705","011607","011608"), column_name = "tract_fips") %>% 
  left_join(poverty.hh) %>% 
  filter(race %in% c("whitenonhisp", "black", "hispanic"),
         year == 2019) %>% 
  group_by(race) %>% 
  mutate(median_household_income = mean(median_household_income),
         tot_tax_hh_perc = mean(tot_tax_hh_perc),
         nickname = "Murrayville/Ogden"
  )

ogden <- ogden %>% head(3)

farms <- as_tibble_col(c("012101","012104"), column_name = "tract_fips") %>% 
  left_join(poverty.hh) %>% 
  filter(race %in% c("whitenonhisp", "black", "hispanic"),
         year == 2019) %>% 
  group_by(race) %>% 
  mutate(median_household_income = mean(median_household_income),
         tot_tax_hh_perc = mean(tot_tax_hh_perc),
         nickname = "Echo Farms/Silver Lake")

farms <- farms %>% head(3)

county <- disparity %>% 
  filter(tract_fips == "000000")

zone_disparity <- bind_rows(downtown, ogden, farms,county, sss)

shape = c(15,17,20, 18)

zone_disparity %>%
  ggplot(aes(x=median_household_income,
             y=tot_tax_hh_perc,
             color = race,
             shape = nickname)
  )+
  geom_point(alpha= .7, size = 4.2, stroke = 1.1)+
  scale_x_continuous(labels=scales::dollar_format(),
                    breaks = seq(20000, 80000, by = 15000),
                    limits = c(20000, 85000)
    )+
  scale_y_continuous(labels=scales::percent_format(),
                     limits = c(.019, .085),
                     breaks = seq(.02, .35, by = .03)
  )+
  scale_color_manual(values=c("#EF946C","#76AE87","black","#6A3D9A")
  )+
  scale_shape_manual(values=c(15,17,18,20)
)+
  geom_label_repel(data=disparity[disparity$race=="SSS",],
                   aes(label=
                         "SSS for 2 Adults, 1 Preschooler, \n& 1 Teenager in NHC.\n\nNeeded income: $58,958.84\nMedian property taxes: $1,863.34"),
                   size = 3.8,
                   nudge_x = 500,
                   nudge_y = .016,
                   hjust = 0,
                   alpha =.5,
                   family="poppins")+
  theme(text=element_text(size=16,
                          family="poppins"),
        axis.title=element_text(size=12),
        plot.title = element_text(
          family="oswald",
          hjust = -.01,
          size = 18),
        legend.position="top",
        legend.justification='left',
        legend.background=element_rect(fill='transparent'),
        legend.key=element_rect(fill='transparent'),
        legend.text = element_text(size=12),
        legend.title= element_blank())+
  guides(shape=guide_legend(
    ncol=2),
    color="none")+
  labs(title="Property Taxes as % of Household Income",
       x="Household Income",
       y="% Income Spent on Property Taxes")



# Percent Change Tables ####
#Creating year over year percent change with baseline of 2010
#EARNINGS
earnings.y <- earnings.og %>% 
  filter(year == 2010,
         sex == "all",
         emp_type == "all",
         race %in% c("all", "whitenonhisp", "black", "hispanic")) %>% 
  rename(medn_earnings_2010 = medn_earnings) %>% 
  select(tract_fips, race, sex, emp_type, medn_earnings_2010)

earnings2 <- earnings.og %>% 
  filter(between(year, 2010, 2020),
         sex == "all",
         emp_type == "all",
         race %in% c("all", "whitenonhisp", "black", "hispanic"))
  
earnings.y <- full_join(earnings.y, earnings2)

earnings.y <- earnings.y %>% 
   arrange(tract_fips, race, year) %>% 
  mutate(earnings_diff = medn_earnings - medn_earnings_2010) %>% 
  mutate(earnings_perc_change = (((medn_earnings - medn_earnings_2010)/(medn_earnings_2010))*100))

#put it all together
percents <- full_join(earnings.y, taxes.y)

# GRAPHS ####
##### Tax / Earnings Change Data ####
#I want to recreate the graph HfH Wake Co. did for NHC

#I need a table with tract 110's 2017 data
graph <- percents %>%
  filter(race == "all",
         year == 2019,
         sex == "all",
         emp_type == "all") %>% 
  select(tract_fips, earnings_perc_change, total_taxes_perc_change, diff_total_medn, medn_earnings) %>% 
  mutate(taxes_grew_more = ifelse(earnings_perc_change < total_taxes_perc_change, "True", "False")) %>% 
  filter(taxes_grew_more == "True")

graph2 <- percents %>%
  filter(race == "all",
         year == 2017,
         tract_fips == "011000",
         sex == "all",
         emp_type == "all") %>% 
  select(tract_fips, earnings_perc_change, total_taxes_perc_change, diff_total_medn, medn_earnings) %>% 
  mutate(taxes_grew_more = ifelse(earnings_perc_change < total_taxes_perc_change, "True", "False")) %>% 
  filter(taxes_grew_more == "True")

graph <- full_join(graph, graph2)
names <- read_xlsx("tract_names.xlsx")
graph <- graph %>% left_join(names %>% 
                               select(tract_fips, location) %>% 
                               rename(nickname = location)) %>% 
  arrange(desc(total_taxes_perc_change)) %>% 
  head(10)

graph.1 <- graph %>% 
  pivot_longer(2:3, values_to = "value", names_to = "factor") %>% 
  full_join(graph)

graph.a <- graph.1 %>% 
  filter(factor=="total_taxes_perc_change")
graph.b <- graph.1 %>% 
  filter(factor=="earnings_perc_change")

##### Graph ----
graph.1 %>% 
  ggplot(aes(x = reorder(nickname, total_taxes_perc_change), y=value, fill = value))+
  geom_bar(data= graph.a, stat = "identity", color = "black",  width = .5, position = position_nudge(x = .25), aes(x = reorder(nickname, total_taxes_perc_change), y=value, fill = value)) +
  scale_fill_gradient(low="#cfbcda", high="#6A3D9A")+
  new_scale_fill()+
  geom_bar(data= graph.b, stat = "identity", color = "black", width = .5, position = position_nudge(x = -.25), aes(x = reorder(nickname, total_taxes_perc_change), y=value, fill = value)) +
  scale_fill_gradient(low="#cde4cb", high="#76AE87")+
  coord_flip()+
  theme(axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(size=14),
        panel.grid=element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        text=element_text(size=16,  family="poppins"))+
    geom_label(aes(family = "poppins",
                   label=ifelse(tract_fips=="011000" & factor=="total_taxes_perc_change",
                                paste0(round(value,0),"%", "*"),
                                paste0(round(value,0),"%")),
                   group = value),
               position = position_dodge(width = .85),
               hjust = ifelse(graph.1$value < 0, 1.05, -.05),
               vjust = .54,
               size = 4.5,
               fill ="white"
             )+
    geom_text(aes(family = "poppins bold",
                  label=ifelse(factor=="total_taxes_perc_change",paste0("$",round(diff_total_medn,0)), "")),
              position = position_dodge(width = 1),
              hjust = 1.11,
              vjust = -1.08,
              size = 4.5,
              alpha = .5
              )+
  labs(x = NULL ,y = "Percent Changes, 2010 to 2019")+
  scale_y_continuous(limits = c(-45, 111), expand = c(0, 0))

##### Smaller ----
#let's divide this ten into three areas: downtown, Greenfield Lake/Echo Farms/Silver Lake, and Murrayville/Ogden
downtown <- as_tibble_col(c("011000","011200","010200","010900"), column_name = "tract_fips") %>% 
  left_join(graph) %>% 
  mutate(avg_earnings_change = mean(earnings_perc_change),
         avg_taxes_change = mean(total_taxes_perc_change),
         avg_amt_tax_change = mean(diff_total_medn),
         avg_median_earnings = mean(medn_earnings),
         nickname = "Downtown"
  ) %>% 
  select(7:11) %>% 
  head(1)

ogden <- as_tibble_col(c("011705","011607","011608"), column_name = "tract_fips") %>% 
  left_join(graph) %>% 
  mutate(avg_earnings_change = mean(earnings_perc_change),
         avg_taxes_change = mean(total_taxes_perc_change),
         avg_amt_tax_change = mean(diff_total_medn),
         avg_median_earnings = mean(medn_earnings),
         nickname = "Murrayville/Ogden"
  ) %>% 
  select(7:11) %>% 
  head(1)

farms <- as_tibble_col(c("012101","012104"), column_name = "tract_fips") %>% 
  left_join(graph) %>% 
  mutate(avg_earnings_change = mean(earnings_perc_change),
         avg_taxes_change = mean(total_taxes_perc_change),
         avg_amt_tax_change = mean(diff_total_medn),
         avg_median_earnings = mean(medn_earnings),
         nickname = "Echo Farms/Silver Lake"
  ) %>% 
  select(7:11) %>% 
  head(1)

zones <- bind_rows(farms, downtown, ogden) 

zones <- zones %>%  
  pivot_longer(2:5, values_to = "value", names_to = "factor") %>% 
  full_join(zones)


###### Smaller graphs ----

g.earnings <- zones %>% 
    filter(factor=="avg_earnings_change")
g.taxes <- zones %>%
    filter(factor=="avg_taxes_change")
  
zones %>% 
    ggplot(aes(x = reorder(nickname, avg_taxes_change), y=value, fill = value))+
    geom_bar(data= g.taxes, stat = "identity", color = "black",  width = .45, position = position_nudge(x = .225),
             aes(x = reorder(nickname, avg_taxes_change), y=value, fill = value)) +
    scale_fill_gradient(low="#cfbcda", high="#6A3D9A",limits = c(-10,90))+
    new_scale_fill()+
    geom_bar(data= g.earnings, stat = "identity", color = "black", width = .45, position = position_nudge(x = -.225), aes(x = reorder(nickname, avg_taxes_change), y=value, fill = value)) +
    scale_fill_gradient(low="#cde4cb", high="#76AE87",limits = c(-10,90))+
    coord_flip()+
    theme(axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_text(size=14),
          panel.grid=element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          text=element_text(size=16,  family="poppins"))+
  geom_label(data= g.taxes,
             aes(x = reorder(nickname, avg_taxes_change),
                 y=value,
                 label = case_when(factor=="avg_taxes_change"|factor=="avg_earnings_change" ~ paste0(round(value,0),"%")),
                 group = "taxes",
                 family = "poppins"),
             hjust = ifelse(g.taxes$value < 0, 1.1, -.07),
             vjust = -.8,
             size = 4.25,
             fill ="white"
  )+
  geom_label(data= g.earnings,
             aes(x = reorder(nickname, avg_taxes_change),
                 y=value,
                 label = case_when(factor=="avg_taxes_change"|factor=="avg_earnings_change" ~ paste0(round(value,0),"%")),
                 group = "earnings",
                 family = "poppins"),
             hjust = ifelse(g.earnings$value < 0, 1.05, -.07),
              vjust = 1.8,
              size = 4.25,
             fill ="white"
  )+
  labs(x = NULL ,y = "Percent Changes, 2010 to 2019")+
    scale_y_continuous(limits = c(-10, 60), expand = c(0, 0))

#### Map ####

###### Data ####
tract_sf <- read_sf('NC-2010-tract-sf/gz_2010_37_140_00_500k.shp') %>%
  mutate(GEOID = str_extract(GEO_ID, "[0-9]{11}$"))

map <- percents %>%
  filter(tract_fips != "011000",
         race == "all",
         year == 2019,
         sex == "all",
         emp_type == "all") %>% 
  select(tract_fips, earnings_perc_change, total_taxes_perc_change, diff_total_medn, medn_earnings, earnings_diff, total_medn) %>% 
  mutate(taxes_grew_more = ifelse(earnings_perc_change < total_taxes_perc_change, "True", "False"))

map2 <- percents %>%
  filter(race == "all",
         year == 2017,
         tract_fips == "011000",
         sex == "all",
         emp_type == "all") %>% 
  select(tract_fips, earnings_perc_change, total_taxes_perc_change, diff_total_medn, medn_earnings, earnings_diff,total_medn) %>% 
  mutate(taxes_grew_more = ifelse(earnings_perc_change < total_taxes_perc_change, "True", "False"))

map <- full_join(map, map2)

map <- tract_sf %>%
  rename(state_fips = STATE, county_fips = COUNTY, tract_fips = TRACT) %>%
  filter(state_fips == "37",
         county_fips == "129") %>%
  left_join(
    map
  )

###### Tax Change Map ####

nhc <- map %>% 
  ggplot()+
  geom_sf(aes(fill = total_taxes_perc_change))+
  scale_fill_gradient(low = "#EEEAF0",  high= "#6A3D9A")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none")

ggsave("nhc.png", nhc)

# Create a new variable 'fill_var' that will be used for fill aesthetics
map$fill_var <- ifelse(map$tract_fips %in% c("011000","011200","010200","010900","012101","012104","011705","011607","011608"), map$total_taxes_perc_change, NA)

map %>% 
  summarise(min = min(total_taxes_perc_change),
            max = max(total_taxes_perc_change))

# Use the new variable 'fill_var' for fill aesthetics
zones <- ggplot()+
  geom_sf(data = map, aes(fill = fill_var, size = 2))+
  scale_fill_gradient(low = "#EEEAF0", high = "#6A3D9A", limits = c(-30.124284,122.13952))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


ggsave("nhc_zones.png", zones)
