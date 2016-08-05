
library(shiny)
library(tidyr)
library(grid)
library(scales)
library(codemog)
library(dplyr)
library(ggplot2)

#source("source_server.r")



load("county_est.rdata")
load("county_hist.rdata")
load("county_forecast.rdata")
load("muni_win_est.rdata")
load("muni_win_hist.rdata")
load("muni_hist.rdata")
load("muni_est.rdata")


county_forecast=county_forecast
c=read.csv("county_estimates_current.csv", stringsAsFactors = FALSE)%>%
  gather(var, value, -county:-region )%>%
  separate(var, into=c("variable", "year"))%>%
  spread(variable, value)%>%
  mutate(cv_pop=as.numeric(Population))%>%
  select(county,countyfips, year, cv_pop)#%>%
  #filter(countyfips!=0)
mc=read.csv("muni_estimates_current.csv", stringsAsFactors = FALSE)%>%
  #filter(MuniTotalFlag==1)%>%
  gather(var, value, -countyfips:-municipality)%>%
  separate(var, into=c("variable", "year"))%>%
  filter(year!="2010c")%>%
  spread(variable, value)%>%
  mutate(cv_pop=as.numeric(totalpopulation))%>%
  select(municipality,countyfips,placefips, year, cv_pop)%>%
  filter(placefips!=0)
mch=read.csv("muni_estimates_current.csv", stringsAsFactors = FALSE)%>%
  #filter(MuniTotalFlag==1)%>%
  gather(var, value, -countyfips:-municipality)%>%
  separate(var, into=c("variable", "year"))%>%
  filter(year!="2010c")%>%
  spread(variable, value)%>%
  mutate(cv_housing=as.numeric(totalHousingUnits))%>%
  select(municipality,countyfips,placefips, year, cv_housing)%>%
  filter(placefips!=0)
multi=read.csv("muni_estimates_current.csv", stringsAsFactors = FALSE)%>%
  filter(MuniTotalFlag==1)%>%
  select(placefips, multi=MultiCountyPlaceFlag)
pv=read.csv("county_estimates_previous.csv", stringsAsFactors = FALSE)%>%
  gather(var, value,  -county:-region)%>%
  separate(var, into=c("variable", "year"))%>%
  spread(variable, value)%>%
  mutate(pv_pop=as.numeric(Population))%>%
  select(county,countyfips, year, pv_pop)#%>%
  #filter(countyfips!=0)
mpv=read.csv("muni_estimates_previous.csv", stringsAsFactors = FALSE)%>%
  #filter(MuniTotalFlag==1)%>%
  gather(var, value, -countyfips:-municipality)%>%
  separate(var, into=c("variable", "year"))%>%
  filter(year!="2010c")%>%
  spread(variable, value)%>%
  mutate(pv_pop=as.numeric(totalpopulation))%>%
  select(municipality,placefips,countyfips, year, pv_pop)%>%
  filter(placefips!=0)
mpvh=read.csv("muni_estimates_previous.csv", stringsAsFactors = FALSE)%>%
  #filter(MuniTotalFlag==1)%>%
  gather(var, value, -countyfips:-municipality)%>%
  separate(var, into=c("variable", "year"))%>%
  filter(year!="2010c")%>%
  spread(variable, value)%>%
  mutate(pv_housing=as.numeric(totalHousingUnits))%>%
  select(municipality,placefips,countyfips, year, pv_housing)%>%
  filter(placefips!=0)
sdc=read.csv("district_estimates_current.csv", stringsAsFactors = FALSE)%>%
  gather(var, value_cv, -LG_ID:-District)
# %>%
#   mutate(var=paste0(var,"v14"))
sdpv=read.csv("district_estimates_previous.csv", stringsAsFactors = FALSE)%>%
  gather(var, value_pv, -LG_ID:-District)
# %>%
#   mutate(var_pv=paste0(var_pv,"v13"))
sd=inner_join(sdc,sdpv)%>%
  mutate(revision=round(round(value_cv,0)-round(value_pv,0), 0),
         value_pv=round(value_pv,0),
         value_cv=round(value_cv,0))
hist=county_hist%>%
  filter(year<=2010, year>=2005)%>%
  select(county, countyfips, year, totalPopulation)
m_hist_one=muni_win_hist%>%
  filter(year<=2010, year>=2005)%>%
  select(municipality,countyfips, placefips, year, totalPopulation)%>%
  mutate(countyfips=as.integer(countyfips),
         placefips=as.integer(placefips))
m_hist_multi=muni_hist%>%
  filter(year<=2010, year>=2005)%>%
  select(municipality, placefips, year, totalPopulation)%>%
  mutate(countyfips=as.integer(999),
         placefips=as.integer(placefips))%>%
  bind_rows(m_hist_one)
# m_hist=list(m=m_hist_multi, s=m_hist_one)

ts=c%>%
  rename(totalPopulation=cv_pop)%>%
  bind_rows(hist)%>%
  left_join(pv, by=c("countyfips", "year"))%>%
  arrange(countyfips,year)%>%
  group_by(countyfips)%>%
  mutate(change=totalPopulation-lag(totalPopulation),
         percent_change=round(100*(change/totalPopulation),2),
         revision=totalPopulation-pv_pop)
mts=mc%>%
  rename(totalPopulation=cv_pop)%>%
  bind_rows(m_hist_one)%>%
  left_join(mpv )%>%
  arrange(placefips,year)%>%
  group_by(countyfips, placefips)%>%
  mutate(
#     change=totalPopulation-lag(totalPopulation),
#          percent_change=round(100*(change/totalPopulation),2),
         revision=totalPopulation-pv_pop)

mtsh=mch%>%
  rename(totalHousingUnits=cv_housing)%>%
  left_join(mpvh)%>%
  arrange(placefips,year)%>%
  group_by(countyfips, placefips)%>%
  mutate(
    #     change=totalPopulation-lag(totalPopulation),
    #          percent_change=round(100*(change/totalPopulation),2),
    revision=totalHousingUnits-pv_housing)
by=c("placefips", "year")
gd=hist%>%
  bind_rows(c)%>%
  bind_rows(pv)
gdh=mch%>%
  left_join(mpvh)

mgd=m_hist_one%>%
  bind_rows(mc)%>%
  bind_rows(mpv)
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
  mutate(GQChng=GQ,
         NetMig_Revise=NetMig-pNetMig,
         GQ_Revise=GQChng-pGQ,
         NatInc_Revise=NatInc-pNatInc,
         Births_Revise=Births-pBirths,
         Deaths_Revise=Deaths-pDeaths)%>%
  select(-GQ)

c_names=county_est%>%
  filter(year==2010)%>%
  select(countyfips, county)
m_names=muni_est%>%
  filter(year==2010)%>%
  select(placefips, municipality)

sd_pop=function(data,lgid){
  # lgid=as.numeric(lgid)
  
  d=filter(data, LG_ID==lgid)%>%
    separate(var, into=c("var", "year"), sep="_")
  
  p=ggplot(d, aes(x=year, y=value_cv, group=var))+
    geom_line(aes(color=var, linetype=var),size=1)+
    theme_codemog()+
    labs(x="Year", y="Value", title="Current Vintage Variable Change")
  return(p)
  
}

totalPop=function(data,fips){
  fips=as.numeric(fips)
  
  d=filter(data, countyfips==fips)
  
  p=ggplot(d,aes(x=as.factor(year), y=as.integer(totalPopulation),group=countyfips))+
    geom_line(color=rgb(31,74,126,max=255), size=1.75)+
    labs(x="Year", y="Population", title="County Population")+
    scale_y_continuous(label=comma)+
    
    theme_codemog(base_size=12)+
    theme(axis.text.x=element_text(angle=90))+
    geom_point(aes(x=as.factor(year), y=pv_pop, label="Current"), color=rgb(0,149,58,max=255), size=2.5)+
    geom_point(aes(x=as.factor(year), y=cv_pop, label="Previous"), color=rgb(191,32,38,max=255), size=2.5)
  return(p)
}

mtotalPop=function(data,placefips){
  fips=as.numeric(placefips)
  
  d=filter(data, placefips==fips)
  
  p=ggplot(d,aes(x=as.factor(year), y=as.integer(totalPopulation),group=countyfips))+
             geom_line(color=rgb(31,74,126,max=255), size=1.75)+
             labs(x="Year", y="Population", title="Municipal Population")+
             scale_y_continuous(label=comma)+
             
             theme_codemog(base_size=12)+
             theme(axis.text.x=element_text(angle=90))+
             geom_point(aes(x=as.factor(year), y=pv_pop, label="Previous"), color=rgb(0,149,58,max=255), size=2.5)+
             geom_point(aes(x=as.factor(year), y=cv_pop, label="Current"), color=rgb(191,32,38,max=255), size=2.5)
           return(p)
}
mtotalHousing=function(data,placefips){
  fips=as.numeric(placefips)
  
  d=filter(data, placefips==fips)
  
  p=ggplot(d,aes(group=countyfips))+
    geom_point(aes(x=as.factor(year), y=pv_housing, label="Previous"), color=rgb(0,149,58,max=255), size=2.5)+
    geom_point(aes(x=as.factor(year), y=cv_housing, label="Current"), color=rgb(191,32,38,max=255), size=2.5)+
    labs(x="Year", y="Population", title="Municipal Total Housing")+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=12)+
    theme(axis.text.x=element_text(angle=90))

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

shinyServer(function(input, output) {
  cntyfips=reactive({c_names%>%filter(county==input$cnty)%>%select(countyfips)})
  plcfips=reactive({m_names%>%filter(municipality==input$muni)%>%select(placefips)})
  lgid=reactive({sd%>%filter(District==input$dist)%>%select(LG_ID)%>%mutate(LG_ID=as.numeric(LG_ID))})
  mtotalPlot_input=reactive({mtotalPop(data=mgd, placefips=plcfips())})
  output$mtotalPlot=renderPlot({mtotalPlot_input()})
  mtotalHousing_input=reactive({mtotalHousing(data=gdh, placefips=plcfips())})
  output$housingPlot=renderPlot({mtotalHousing_input()})
  totalPlot_input=reactive({totalPop(data=gd, fips=cntyfips())})
  output$totalPlot=renderPlot({totalPlot_input()})
  changePlot_input=reactive({changePop(data=ts, fips=cntyfips())})
  output$changePlot=renderPlot({changePlot_input()})
  control_in=reactive({county_ts_chart(cntyfips())})
  output$control=renderPlot({control_in()})
  ts_in=reactive({ts%>%filter(countyfips==as.numeric(cntyfips()))%>%select(year,totalPopulation, change, percent_change, revision)})
  output$popTable=renderDataTable({ts_in()})
  mts_in=reactive({mts%>%filter(placefips==as.numeric(plcfips()))%>%select(year,totalPopulation, revision)})
  output$mpopTable=renderDataTable({mts_in()})
  mtsh_in=reactive({mtsh%>%filter(placefips==as.numeric(plcfips()))%>%select(year,totalHousingUnits, revision)})
  output$housingTable=renderDataTable({mtsh_in()})
  sd_in=reactive({sd%>%filter(LG_ID==as.numeric(unique(lgid())))%>%select(var,Current2014=value_cv, Previous2014=value_pv, revision)})
  output$distTable=renderDataTable({sd_in()})
  # sd_p=reactive({sd_pop(sdc, lgid())})
  # output$distPlot=renderPlot({sd_p()})
  comp_in=reactive({filter(ts_comp, countyfips==as.numeric(cntyfips()))})
  output$compTable=renderDataTable({comp_in()%>%select(year,NetMig, NetMig_Revise, GQChng, GQ_Revise,Births, Deaths, NatInc, NatInc_Revise )})
  output$popTableRank=renderDataTable({ts%>%select(county.x,year, totalPopulation, change, percent_change, revision)})
})
