library("tidyverse")
library("GGally")
library("plotly")
library("here")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library("cowplot")
library("biscale")
library("sf")

here::i_am("analysis.R")
# functions----

formatter <- function(string){
  string%>%
    str_replace_all("_", " ")%>%
    str_to_title()
}

biscale_choro <- function(){
  data <- bi_class(ave_data, x = log_gdp_per_cap, y = epi, style = "quantile", dim = 3)
  map <- ggplot(mapping=aes(label=country)) +
    geom_sf(data = data, mapping = aes(geometry=geometry,fill = bi_class), size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    bi_theme()
  legend <- bi_legend(pal = "DkBlue",
                      dim = 3,
                      xlab = "Higher log(GDP/capita)",
                      ylab = "Higher EPI score",
                      size = 12)
  finalPlot <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0, 0, 0.3, 0.3)
  return(finalPlot)
}



# load the processed data----

load(here("RData_files","process.RData"))

ave_data <- epi_data%>%
  group_by(iso,country,country_name)%>%
  summarise(across(.cols=everything(),mean,na.rm=TRUE),
            nobs=n())

world <- ne_countries(scale = "medium", returnclass = "sf")%>%
  select(iso_a3,geounit,geometry)

ave_data <- left_join(ave_data,world,by=c("iso"="iso_a3"))%>%
  ungroup()
rownames(ave_data) <- ave_data$country

epi_vs_year <- ggplot(epi_data,aes(year,epi,group=country,colour=log_gdp_per_cap))+
  geom_line()+
  scale_colour_viridis_c()

wealth_vs_epi <- biscale_choro()
 

mydf <- epi_data%>%
  mutate(year=factor(year))
rownames(mydf) <- paste(mydf$country, mydf$year, sep=":")

# control variables pairs plots----

(controls_pairs <- ave_data%>%
   select(control_of_corruption,
         rule_of_law,
         political_stability_no_violence,
         voice_and_accountability,
         government_effectiveness,
         regulatory_quality,
         log_gdp_per_cap)%>%
  ggpairs(lower = list(continuous = wrap(ggally_points, size=.5, alpha = .5)), 
          labeller = as_labeller(formatter))+
    labs(title="Different measures of basically the same thing.",
         caption="Source: https://info.worldbank.org/governance/wgi/ and https://data.worldbank.org/indicator/NY.GDP.PCAP.CD"))

# inequity variables pairs plots----

(inequality_pairs <- ave_data%>%
  select(gini_disp, log_num_poverty, top_ten_share, bottom_fifty_share)%>%
  ggpairs(lower = list(continuous = wrap(ggally_points, size=.5, alpha = .5)), 
          labeller = as_labeller(formatter))+
    labs(title="Different measures of subtly different things.",
         caption="Source: https://wid.world/data/ and https://dataverse.harvard.edu/file.xhtml?fileId=4149926&version=5.0"))

# EPI vs. import weighted EPI----

(epi_vs_wepi <- ave_data%>%
  ggplot()+
  geom_abline(slope=1, intercept=0, col="white", lwd=2)+  
  scale_colour_viridis_c()+  
  geom_point(aes(epi, weighted_epi, colour=log_gdp_per_cap, label=country))+
  labs(x="Environmental Performance Index",
       y="Import weighted EPI",
       caption="source:https://sedac.ciesin.columbia.edu/data/collection/epi/sets/browse and https://wits.worldbank.org/datadownload.aspx?lang=en"))

(epi_vs_wepi <- ggplotly(epi_vs_wepi, tooltip = c("label"))%>%
   config(displayModeBar = F))

# regressions----

dep_vars <- c("epi~", "weighted_epi~")
controls <- "control_of_corruption+rule_of_law+political_stability_no_violence+voice_and_accountability+government_effectiveness+regulatory_quality+log_gdp_per_cap"
inequality <- c("+gini_disp", "+log_num_poverty", "+top_ten_share", "+bottom_fifty_share")
regressions <- crossing(dep_vars, controls, inequality)%>%
  mutate(call=paste0(dep_vars, controls, inequality),
         mod=map(.x=call, .f=lm, data=ave_data),
         rob_mod=map(mod,lmtest::coeftest,vcov=sandwich::vcovHC)
         )

# 32 indicator's time series----

(yearly_indicator_diff <- ggplot(indicator_series, aes(year, change, group=iso))+
    theme(text = element_text(size = 8))+
    geom_line(alpha=.05)+
    facet_wrap(vars(series), scales="free_y")+
    labs(title="Some performance indicators constant over time???",
         caption="source: https://sedac.ciesin.columbia.edu/downloads/data/epi/epi-environmental-performance-index-2020/",
         y="annual rate of change in indicator")+ 
    theme(axis.text.x = element_text(angle=45)))

# inequality vs. working hours (rat race)----

(rr <- ggplot(filter(rat_race, year>1979), aes(gini_disp, hours, frame=year, label=country_name))+
  geom_text()+
  geom_smooth(method="lm")+
  labs(x="Gini coefficient for disposable income",
       y="Average annual hours of work"))

# gini variation----

(gini_plot <- ggplot(epi_data , aes(year, gini_disp, group=country, colour=log_gdp_per_cap, label=country))+
  scale_color_viridis_c()+
  geom_line()+
  labs(y="Gini coefficient for disposable income",
       colour="log(GDP/capita)"))


kuznet <-ggplot(ave_data,aes(log_gdp_per_cap,epi,label=iso))+
  geom_text()+
  geom_smooth(se=FALSE)+
  labs(title="Does not look like inverted Environmental Kuznet curve.",
    x="Log (GDP/capita)",
       y="Environmental Performance Index")


#save all objects----

save.image(file=here("RData_files","analysis.RData"))
