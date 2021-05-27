# making a "copy" of a data.table (new_table <- old_table) and setting keys on new_table
# resets keys on old_table. To get an actual copy of data.table use new_table <- copy(old_table)

library(tidyverse)
library(janitor)
library(readxl)
library(magrittr) # for %<>% operator in function get_one_sheet 
library(data.table)
library(broom)

# data sources----

#EPI: https://sedac.ciesin.columbia.edu/data/collection/epi/sets/browse
#Gini: https://dataverse.harvard.edu/file.xhtml?fileId=4149926&version=5.0
#GDP: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
#trade: https://wits.worldbank.org/datadownload.aspx?lang=en
#top and bottom: https://wid.world/data/
#government: https://info.worldbank.org/governance/wgi/
#hours: https://ourworldindata.org/working-hours

# functions ----

get_one_sheet <- function(pth, sht, skp){#epi data in multi-sheet excel files
  mydf <- read_excel(path=pth, sheet=sht, skip=skp)%>%
     clean_names()%>%
     select(contains("iso"),
            contains("country"),
            contains("epi"),
            -contains("epi_regions"),
            -contains("change"),
            -contains("rnk")
            )%>%
    mutate(year=as.numeric(gsub(".*?([0-9]+).*", "\\1", pth)))%>% #extracts year from path
    na_if("..")
  colnames(mydf) <- c("iso", "country", "epi", "year")
  mydf$epi %<>% as.numeric
  return(mydf)
}

top_5_partners <- function(df){
  df %>% filter(indicator %in% c("Partner share(%)-Top 5 Import Partner"))%>%
    select(partner, indicator, starts_with("x"))%>%
    pivot_longer(cols=starts_with("x"), names_to="year", values_to="share_imports")%>%
    select(partner, year, share_imports)%>%
    mutate(year=as.numeric(str_sub(year, start=2)))%>%
    na.omit()
}

count_partners <- function(df){
  df %>% 
    group_by(year)%>%
    summarize(n_partners=n())
}

trade_partner_epis <- function(df, yr){#get isos from wits df, attach epis to trade partners
  x <- left_join(df, wits, by=c("partner"="country"))%>%
    rename(partner_iso=iso)%>%
    mutate(year=yr)
  y <- left_join(x, epi_data, by=c("partner_iso"="iso", "year"="year"))%>%
   select(partner, year, share_imports, partner_iso, epi)%>%
   na.omit()
   return(y)
}

import_epi <- function(df){#top 5 proportions, use proportions to create weighted average. 
  df <- df%>%
    mutate(prop=share_imports/(sum(share_imports)))%>%
    summarize(import_epi=sum(prop*epi))%>%
    pull()  
}

extract_num <- function(df, yr, ind, pro_cat){# extracts a cell from a data frame
  year <- paste0("x", yr)
  df %>% filter(indicator==ind & product_categories==pro_cat)%>%
    select({{ year }})%>%
    pull()
}
safer_extract_num <- possibly(extract_num, otherwise = NA)# return NA instead of stopping for error.

tidy_indicator <- function(df){# tidy the indicator_series dataframes
  df%>%
    clean_names()%>%
    select(-code)%>%
    pivot_longer(cols=contains("ind"), names_to = "indicator_year", values_to="value")%>%
    separate(indicator_year, into=c("indicator", "year"), sep="_ind_")
}

# data processing ----

epi_files <- tibble(paths=paste0("raw_data/epi/",
                                 list.files(path="raw_data/epi", pattern=".xls*")),
                    sheet=c(3, 2, 4, 4, 3, 3, 4, 5),
                    skip=c(0, 0, 1, 0, 0, 0, 0, 0)) #info for reading epi excel files.

# epi data ----

epi_data <- epi_files%>%
  mutate(data=pmap(list(pth=paths, sht=sheet, skp=skip), get_one_sheet))%>%
  select(data)%>%
  unnest(cols=data)%>%
  mutate(year=as.numeric(year))%>% #note that year comes from file name (not in sheet)
  as.data.table()%>%
  setkeyv(c("iso", "year"))

# trade data ----

wits <- read_csv("raw_data/wits.csv")%>% #mapping from country name to iso for trade data.
  clean_names()%>%
  select(country_name, country_iso3)%>%
  rename(country=country_name,
         iso=country_iso3)%>%  
  as.data.table()%>%
  setkeyv(c("country"))

trade_files <- list.files("raw_data/trade")%>%# paths to each country's trade data (.csv files)
  as_tibble()%>%
  mutate(value=paste0("raw_data/trade/", value))

trade_nested <- trade_files %>%
  mutate(data=map(value, read_csv),
         value=gsub(".*en_(.+)_All.*", "\\1", value),# extracts iso between en_ and _All in file name
         data=map(data, clean_names),
         partners=map(data, top_5_partners))%>% 
  unnest(cols=partners)%>%
  rename(iso=value)%>%
  group_by(iso, year)%>%
  mutate(n_partners=n())%>%
  filter(n_partners==5)%>% #only retain country/years where 5 trade partners listed.
  as.data.table()%>%
  setkeyv(c("iso", "year"))

# data for Canada import weight ---- 

canada <- trade_nested%>%filter(iso=="CAN" & year==2018)%>%select(data)%>%pull()
 canada <- canada[[1]]%>%
   filter(indicator=="GDP (current US$ Mil)"|
          indicator=="Imports (in US$ Mil)"|
          indicator=="Exports (in US$ Mil)")%>%
   select(indicator, x2018)%>%
   rename(`Value in 2018`=x2018)

# join trade and epi data ---- 
 
epi_data <- trade_nested[epi_data, roll = "nearest"]%>%
  na.omit()%>%
  rename(trade_data=data)%>%
  group_by(iso, trade_data, year, n_partners, epi)%>%
  nest()%>%
  rename(partners=data)%>%
  mutate(partners=map2(partners, year, trade_partner_epis),
         import_epi=map_dbl(partners, import_epi)
         )%>%
  filter(import_epi>0)%>%
  mutate(imports=pmap_dbl(list(df=trade_data,
                               yr=year,
                               ind="Imports (in US$ Mil)",
                               pro_cat="All Products"), safer_extract_num),
         gdp=pmap_dbl(list(df=trade_data,
                           yr=year, ind="GDP (current US$ Mil)",
                           pro_cat="..."), safer_extract_num),
         exports=pmap_dbl(list(df=trade_data,
                               yr=year,
                               ind="Exports (in US$ Mil)",
                               pro_cat="All Products"), safer_extract_num),
         weighted_epi=(imports/(gdp-exports+imports))*import_epi+((gdp-exports)/(gdp-exports+imports))*epi)%>%
  ungroup()%>%
  select(iso, year, epi, partners, weighted_epi)%>%
  left_join(wits)%>%
  as.data.table()%>%
  setkeyv(c("country", "year"))

# canada's top 5 trade partners ----
 
canada_partners <- epi_data%>%
  filter(iso=="CAN"& year==2018)%>%
  select(partners)%>%
  unnest(cols=partners)%>%
  select(partner, year, share_imports, epi)
  
# gini data ----  
 
gini_data <- read_csv("raw_data/swiid/swiid9_0_summary.csv")%>%
  select(country, year, gini_disp)%>%
  as.data.table()%>%
  setkeyv(c("country", "year"))

# gini that overlaps epi ----
  
gini_for_plot <- gini_data%>%
  filter(year>2006)%>%
  group_by(country)%>%
  mutate(gini_sd=sd(gini_disp))
 
# join gini data to epi, trade data----
 
epi_data <- gini_data[epi_data, roll = "nearest"]%>%
  select(-partners)%>%
  setkeyv(c("iso", "year"))

# gdp data---- 

gdp_file <- "raw_data/world_bank/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2252129.csv" 
  
gdp_data <- read_csv(gdp_file, skip=4)%>%
  clean_names()%>%
  select(country_name, country_code, starts_with("x"))%>%
  pivot_longer(cols=starts_with("x"), names_to="year", values_to="gdp_per_cap")%>%
  mutate(year=as.numeric(str_sub(year, start=2)))%>%
  filter(!is.na(gdp_per_cap))%>%
  mutate(log_gdp_per_cap=log10(gdp_per_cap))%>%
  rename(iso=country_code)%>% 
  select(-gdp_per_cap)%>%
  as.data.table()%>%
  setkeyv(c("iso", "year"))

# join gdp data to epi, trade, gini data. 
 
epi_data <- gdp_data[epi_data, roll = "nearest"]%>%
  setkeyv(c("iso", "year"))

# government effectiveness data.

gov_data <- read_csv("raw_data/government_effective/gov_effect.csv")%>%
  clean_names()%>%
  select(country_iso3, indicator, subindicator_type, starts_with("x"))%>%
  filter(subindicator_type=="Estimate")%>%
  select(-subindicator_type)%>%
  pivot_longer(cols=starts_with("x"), names_to="year", values_to="value")%>%
  mutate(year=as.numeric(str_sub(year, start=2)))%>%
  rename(iso=country_iso3)%>%
  pivot_wider(id_cols = c("iso", "year"), names_from=indicator, values_from=value)%>%
  clean_names()%>%
  as.data.table()%>%
  setkeyv(c("iso", "year"))

# join government data to...

epi_data <- gov_data[epi_data, roll = "nearest"]%>%
  setkeyv(c("iso", "year"))

# share of income bottom 50% top 10%.----

tails <-read_delim("raw_data/wid/WID_Data_Metadata/WID_Data_21052021-151421.csv", 
                 ";", escape_double = FALSE, col_names = FALSE, 
                 trim_ws = TRUE, skip = 1)%>%
  select(-X2)
colnames(tails) <- c("country", "who", "year", "share")

tails <- tails%>%
  mutate(who=case_when(who=="p90p100"~"top_ten_share",
                       who=="p0p50"~"bottom_fifty_share"))%>%
  pivot_wider(names_from=who, values_from=share, values_fn = list)%>%
  inner_join(wits)%>%
  unnest(cols=c(top_ten_share, bottom_fifty_share))%>%
  select(-country)%>%
  as.data.table()%>%
  setkeyv(c("iso", "year"))

# join income share to ...

epi_data <- tails[epi_data, roll = "nearest"]%>%
  setkeyv(c("iso", "year"))

# log(pop) with income less than $5.50 per day----
  
five_fifty <- read_csv("raw_data/our_world_in_data/distribution-of-population-poverty-thresholds.csv")%>%
  clean_names()%>%
  select(-entity, -starts_with("above"), -starts_with("x5_50"))%>%
  pivot_longer(cols=c(-code, -year), names_to="name", values_to="value")%>%
  group_by(code, year)%>%
  summarize(log_num_poverty=log10(sum(value)+1))%>%
  rename(iso=code)%>%
  filter(!is.na(iso))%>%
  as.data.table()%>%
  setkeyv(c("iso", "year"))

# join poverty data to ...----

epi_data <- five_fifty[epi_data, roll = "nearest"]

# epi indicator series----

# file paths and three letter identifiers

series_files <- list.files("raw_data/epi/indicators/2020-epi-indicators-time-series-na/")%>%
  as_tibble()%>%
  filter(! value %in% c("Admin_Country_EEZ.csv",
                        "WWT_sources_reduced.csv",
                        "GearType_EEZ.csv",
                        "MSW_types.csv",
                        "TPA_biomes.csv"))%>%
  mutate(tla=str_sub(value, end=3),
    path=paste0("raw_data/epi/indicators/2020-epi-indicators-time-series-na/", value))%>%
  select(-value)

# mapping from tla to short names.

series_names <- read_excel("raw_data/epi/2020-epi.xlsx", sheet=8)%>%
  clean_names()%>%
  select(variable, abbreviation)%>%
  rename(tla=abbreviation, series=variable)

# read in the csv files, unnest to long format, calcuate yearly differences.

indicator_series <- series_files%>%
  inner_join(series_names)%>%
  mutate(data=map(path, read_csv),
         data=map(data, tidy_indicator))%>%
  unnest(cols=data)%>%
  select(-tla, -path)%>%
  group_by(iso, indicator)%>%
  mutate(diff=c(NA, diff(value)))%>%
  filter(year>2004)

# data for rat race animation----

working_hours <- read_csv("raw_data/our_world_in_data/annual-working-hours-per-worker.csv")%>%
  clean_names()%>%
  select(-code)%>%
  rename(country=entity, hours=average_annual_working_hours_per_worker)%>%
  as.data.table()%>%
  setkeyv(c("country", "year"))

# join working hours and inequality data----

rat_race <- gini_data[working_hours, roll="nearest"]%>%
  setkeyv(c("country", "year"))

gdp_data%>%
  setkeyv(c("country_name", "year"))
rat_race <- gdp_data[rat_race,  roll="nearest"]

# only keep needed objects----

rm(list = setdiff(ls(), c("gini_for_plot",
                          "rat_race",
                          "epi_data",
                          "indicator_series",
                          "canada",
                          "canada_partners"))) 

# save objects as binary file.----

save.image(file='RData_files/process.RData')
