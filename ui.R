library(shinydashboard)
library(shinythemes)
library(codemog)
library(dplyr)
load("county_est.rdata")
load("muni_est.rdata")
load("muni_hist.rdata")
load("muni_win_est.rdata")
load("muni_win_hist.rdata")

c_names=county_est%>%
  filter(year==2010)%>%
  select(countyfips, county)
m_names=muni_est%>%
  filter(year==2010)%>%
  select(placefips, municipality)
sd_names=read.csv("district_estimates_current.csv", stringsAsFactors = FALSE)%>%
  select(LG_ID, Areaname)

dashboardPage(
  dashboardHeader(title= "Estimate Review"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("County", tabName = "county", icon = icon("signal", lib = "glyphicon")),
      menuItem("City", tabName = "city", icon = icon("signal", lib = "glyphicon")),
      menuItem("District", tabName = "district", icon = icon("signal", lib = "glyphicon"))
#       menuItem("Rankings", tabName = "rank", icon = icon("signal", lib = "glyphicon"))
    )
  ),
  dashboardBody(
  tabItems(
    tabItem(tabName = "county",
            
            fluidRow(box(selectInput("cnty","County:",
                                 choices=c_names$county)),
            valueBox(
              "Pre-Release Drafts, Don't Distribute", "Release Status", icon = icon("eye-open", lib = "glyphicon"),
              color = "red", width=6))
              ,
            fluidRow(
              box(plotOutput("totalPlot", height = 250), 
                  footer="Key: Blue=Historical, Red=Current Vintage, Green=Previous Vintage"),
              box(plotOutput("changePlot", height = 250))),
            fluidRow(
              box(
                title = "Population",
                dataTableOutput("popTable")
                ),
                box(
                  title = "Component Table",
                  dataTableOutput("compTable")
              )
              )
              ),
    tabItem(tabName = "city",
            
            fluidRow(box(selectInput("muni","Municipality:",
                                     choices=m_names$municipality)),
                     valueBox(
                       "OLD DATA, DON'T USE", "Release Status", icon = icon("eye-open", lib = "glyphicon"),
                       color = "red", width=6)),
            fluidRow(
              box(plotOutput("mtotalPlot", height = 250), 
                  footer="Key: Blue=Historical, Red=Current Vintage, Green=Previous Vintage"),
              box(plotOutput("housingPlot", height = 250))),
            fluidRow(
              box(
                title = "Population Table",
                dataTableOutput("mpopTable")
              ),
              box(
                title = "Housing Table",
                dataTableOutput("housingTable")
              )
            )
            ),
    tabItem(tabName = "district",
            
            fluidRow(box(selectInput("dist","Special District:",
                                     choices=unique(sd_names$Areaname))),
                     valueBox(
                       "OLD DATA, DON'T USE", "Release Status", icon = icon("eye-open", lib = "glyphicon"),
                       color = "red", width=6)),
              box(
                title = "Revision Table",
                dataTableOutput("distTable")
              )
            )
#     tabItem(tabName = "rank",
#             
#             fluidRow(valueBox(
#                        "Internal Only", "Release Status", icon = icon("eye-open", lib = "glyphicon"),
#                        color = "red", width=6)),
#             fluidRow(
#               box(dataTableOutput("popTableRank")))
#     )
    )
  )
    
)
