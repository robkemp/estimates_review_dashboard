#### Load required packages ####
library(shinydashboard)
library(shinythemes)
library(codemog) # Need to get this one from https://github.com/robkemp/codemog ...see the README for Install Instructions
library(dplyr)

## This creates a list of names with the associated fips code to use in the select menus for counties, municipalities and special districts
c_names=county_est%>%
  filter(year==2010)%>%
  select(countyfips, county)
m_names=muni_est%>%
  filter(year==2010)%>%
  select(placefips, municipality)
sd_names=read.csv("district_estimates_current.csv", stringsAsFactors = FALSE)%>%
  select(LG_ID, Areaname)

## Unlike a normal Shiny app, because I'm using the dashboard templating, the dashboardPage call builds the
## user interface. 
dashboardPage(
  dashboardHeader(title= "Estimate Review"), #Titles the page
  dashboardSidebar( #I use this to build the content int the sidebar of the app,, which is tab choices for now
    sidebarMenu(
      menuItem("County", tabName = "county", icon = icon("signal", lib = "glyphicon")),
      menuItem("City", tabName = "city", icon = icon("signal", lib = "glyphicon")),
      menuItem("District", tabName = "district", icon = icon("signal", lib = "glyphicon"))
    )
  ),
  dashboardBody( #This call starts building the body of the user interface
  tabItems( 
    # This call creates a set of items called by the tabs I made in the side bar and populates them
    # I'll comment the firt one to give an idea of how that works.
    tabItem(tabName = "county", # this name corresponds to the tab named above in the sidebar
            # fluidRow creates a row that you can put things into, you don't have to define rows,
            # but it structures the interface with more control.
              # the box() call creates a box that will be formatted on the page to put content in
            fluidRow(box(selectInput("cnty","County:", # this creates a dropdown menu of geographies and labels it "County:"
                                                       # and gives the output an object name to use as an index "cnty"
                                 choices=c_names$county)),# this specifies the choice values to use.
            valueBox( #valueBox lets you create a region fo teh dashboard that is a specific color with a specific message
              "Pre-Release Finals, Can Distribute", "Release Status", icon = icon("eye-open", lib = "glyphicon"),
              color = "yellow", width=6))
              ,
            fluidRow( # I created a second fluidRow here to force this content onto a second ro regardless of browser window size.
              box(plotOutput("totalPlot", height = 250), # Creates a box and puts a plot called totalPlot in it that is generated in the "server.r" scripts
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
                       "Pre-Release Finals, Can Distribute", "Release Status", icon = icon("eye-open", lib = "glyphicon"),
                       color = "yellow", width=6)),
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
                       "Internal Only", "Release Status", icon = icon("eye-open", lib = "glyphicon"),
                       color = "red", width=6)),
              box(
                title = "Revision Table",
                dataTableOutput("distTable")
              )
            )
    )
  )
    
)
