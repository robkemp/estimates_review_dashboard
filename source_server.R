library(shiny)
library(tidyr)
library(grid)
library(scales)
library(codemog)
library(dplyr)
library(ggplot2)
load("/opt/shiny-server/samples/sample-apps/codemog_data/county_est.rdata")
load("/opt/shiny-server/samples/sample-apps/codemog_data/county_hist.rdata")
load("/opt/shiny-server/samples/sample-apps/codemog_data/county_forecast.rdata")
county_forecast=county_forecast
c=read.csv("county_estimates_current.csv", stringsAsFactors = FALSE)%>%
  gather(var, value, -county:-region )%>%
  separate(var, into=c("variable", "year"))%>%
  spread(variable, value)%>%
  mutate(cv_pop=as.numeric(Population))%>%
  select(county,countyfips, year, cv_pop)%>%
  filter(countyfips!=0)

pv=read.csv("county_estimates_previous.csv", stringsAsFactors = FALSE)%>%
  gather(var, value,  -county:-region)%>%
  separate(var, into=c("variable", "year"))%>%
  spread(variable, value)%>%
  mutate(pv_pop=as.numeric(Population))%>%
  select(county,countyfips, year, pv_pop)%>%
  filter(countyfips!=0)
hist=county_hist%>%
  filter(year<=2010, year>=2005)%>%
  select(county, countyfips, year, totalPopulation)
ts=c%>%
  rename(totalPopulation=cv_pop)%>%
  bind_rows(hist)%>%
  left_join(pv, by=c("countyfips", "year"))%>%
  arrange(countyfips,year)%>%
  group_by(countyfips)%>%
  mutate(change=totalPopulation-lag(totalPopulation),
         percent_change=round(100*(change/totalPopulation),2),
         revision=totalPopulation-pv_pop)
gd=hist%>%
  bind_rows(c)%>%
  bind_rows(pv)
comp=read.csv("county_estimates_current.csv", stringsAsFactors = FALSE)%>%
  gather(var, value, -county:-region )%>%
  separate(var, into=c("variable", "year"))%>%
  spread(variable, value)%>%
  mutate(NetMig=as.numeric(NetMig),
         Births=as.numeric(Births),
         Deaths=as.numeric(Deaths),
         NatInc=Births-Deaths,
         GQ_Change=as.numeric(GQ))%>%
  select(-Population)%>%
  filter(countyfips!=0)

pv_comp=read.csv("county_estimates_previous.csv", stringsAsFactors = FALSE)%>%
  gather(var, value, -county:-region )%>%
  separate(var, into=c("variable", "year"))%>%
  spread(variable, value)%>%
  mutate(pNetMig=as.numeric(NetMig),
         pBirths=as.numeric(Births),
         pDeaths=as.numeric(Deaths),
         pNatInc=Births-Deaths,
         pGQ=as.numeric(GQ))%>%
  select(-Births:-Population)%>%
  filter(countyfips!=0)
ts_comp=left_join(comp,pv_comp, by=c("county", "countyfips", "region", "year"))%>%
  mutate(NetMig_Revise=NetMig-pNetMig,
         GQ_Revise=GQ-pGQ,
         NatInc_Revise=NatInc-pNatInc,
         Births_Revise=Births-pBirths,
         Deaths_Revise=Deaths-pDeaths)

c_names=county_est%>%
  filter(year==2010)%>%
  select(countyfips, county)

totalPop=function(data,fips){
  fips=as.numeric(fips)
  
  d=filter(data, countyfips==fips)
  
  p=ggplot(d,aes(x=as.factor(year), y=as.integer(totalPopulation),group=countyfips, label="Historical"))+
    geom_line(color=rgb(31,74,126,max=255), size=1.75)+
    labs(x="Year", y="Population", title="County Population")+
    scale_y_continuous(label=comma)+
    
    theme_codemog(base_size=12)+
    theme(axis.text.x=element_text(angle=90))+
    geom_point(aes(x=as.factor(year), y=pv_pop, label="Current"), color=rgb(0,149,58,max=255), size=2.5)+
    geom_point(aes(x=as.factor(year), y=cv_pop, label="Previous"), color=rgb(191,32,38,max=255), size=2.5)
  return(p)
}
changePop=function(data,fips){
  fips=as.numeric(fips)
  
  d=filter(data, countyfips==fips)
  
  p=ggplot(d,aes(x=as.factor(year), y=as.numeric(percent_change),group=countyfips))+
    geom_bar(stat="identity",fill=rgb(31,74,126,max=255))+
    labs(x="Year", y="Population", title="County Population Change (%)")+
    #scale_y_continuous(label=comma)+
    theme_codemog(base_size=12)+
    theme(axis.text.x=element_text(angle=90))
  return(p)
}
