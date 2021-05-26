library(tidyverse)
library(GGally)
library(plotly)

load("RData_files/process.RData")

mydf <- epi_data%>%
  mutate(year=factor(year))
rownames(mydf) <- paste(mydf$country,mydf$year,sep=":")

formatter <- function(string){
  string%>%
    str_replace_all("_"," ")%>%
    str_to_title()
}

(controls_pairs <- mydf%>%
  select(control_of_corruption,rule_of_law,political_stability_no_violence,voice_and_accountability,government_effectiveness,regulatory_quality,log_gdp_per_cap)%>%
  ggpairs(lower = list(continuous = wrap(ggally_points, size=.25, alpha = .25)),labeller = as_labeller(formatter))+
    labs(title="Different measures of basically the same thing.",
         caption="Source: https://info.worldbank.org/governance/wgi/ and https://data.worldbank.org/indicator/NY.GDP.PCAP.CD"))

(inequality_pairs <- mydf%>%
  select(gini_disp,log_num_poverty,top_ten_share,bottom_fifty_share)%>%
  ggpairs(lower = list(continuous = wrap(ggally_points, size=.25, alpha = .25)), labeller = as_labeller(formatter))+
    labs(title="Different measures of subtly different things.",caption="Source: https://wid.world/data/ and https://dataverse.harvard.edu/file.xhtml?fileId=4149926&version=5.0 "))

epi_vs_wepi <- mydf%>%
    filter(year!="2020")%>%
  ggplot()+
  geom_abline(slope=1,intercept=0,col="white",lwd=2)+  
  scale_colour_viridis_c()+  
  geom_point(aes(epi,weighted_epi,colour=log_gdp_per_cap,label=country),alpha=.25)+
  labs(x="Environmental Performance Index",
       y="Import weighted EPI",
       caption="source:https://sedac.ciesin.columbia.edu/data/collection/epi/sets/browse and https://wits.worldbank.org/datadownload.aspx?lang=en")+
    facet_wrap(vars(year))

(epi_vs_wepi <- ggplotly(epi_vs_wepi,tooltip = c("label"))%>%
   config(displayModeBar = F))

dep_vars <- c("epi~","weighted_epi~")
controls <- "year+control_of_corruption+rule_of_law+political_stability_no_violence+voice_and_accountability+government_effectiveness+regulatory_quality+log_gdp_per_cap"
inequality <- c("+gini_disp","+log_num_poverty","+top_ten_share","+bottom_fifty_share")

regressions <- crossing(dep_vars,controls,inequality)%>%
  mutate(call=paste0(dep_vars,controls,inequality),
         mod=map(call,lm,mydf))

(yearly_indicator_diff <- ggplot(indicator_series,aes(year,diff,group=iso))+
    theme(text = element_text(size = 8))+
    geom_line(alpha=.05)+
    facet_wrap(vars(series),scales="free_y")+
    labs(title="Some indicators constant over time?",
         caption="source: https://sedac.ciesin.columbia.edu/downloads/data/epi/epi-environmental-performance-index-2020/2020-epi-indicators-time-series-na-csv.zip",
         y="annual change in indicator")+ 
    theme(axis.text.x = element_text(angle=45)))

(rr <- ggplot(filter(rat_race,year>1979),aes(gini_disp,hours,frame=year,label=country_name))+
  geom_text()+
  geom_smooth(method="lm")+
  labs(x="Gini coefficient for disposable income",
       y="Average annual hours of work"))



gini_plot <- ggplot(gini_for_plot,aes(year,gini_disp,group=country,colour=gini_sd,label=country))+
  scale_color_viridis_c()+
  geom_line(alpha=.5)+
  labs(y="Gini coefficient for disposable income")
gini_plot <- ggplotly(gini_plot)





save.image(file='RData_files/analysis.RData')
