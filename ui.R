
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(jsonlite)
library(httr)
library(data.table)
library(tidyverse)
library(shiny)
library(leaflet)
library(htmltools)
library(plotly)
library(stringr)
library(shinyBS)
library(shinydashboard)
library(plyr)

shinyUI(
  dashboardPage(
    dashboardHeader(title =textOutput("title"),titleWidth = 350),
    dashboardSidebar(width=300,
      bsTooltip(id = "siteid", title = "type or paste site number", 
                placement = "bottom", trigger = "hover"),
      textInput("siteid", "Enter Lake ID Number", "27011700"),
      #textInput("siteid", "Enter Lake ID Number", "11030500"),
      tags$head(tags$script(src = "message.js")),
      actionButton("do", "Get Data!"),
      checkboxInput("gear", "Only trap and gill nets", TRUE),
      sidebarMenu(
        menuItem("Map", tabName = "map"),
        menuItem("List of Lakes",tabName = "alllakes"),
        menuItem("All Data", tabName = "stationlist"),
        menuItem("Biomass/CPUE", tabName = "alldata"),
        menuItem("CPUE Plot", tabName = "byyear"),
        menuItem("Biomass Plot", tabName = "biomassplot")
        #menuItem("Plot_AllData", tabName = "plotalldata"),
        #menuItem("Summary", tabName = "summary")
      )
    ),
    dashboardBody( 
      tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 20;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: black;
        }
        '))),
      tags$script(HTML('
                       $(document).ready(function() {
                       $("header").find("nav").append(\'<span class="myClass"> | Fish Data Scraper App | developed by Steve Hummel  </span>\');
                       })
                       ')),
      tabItems(
        tabItem(tabName = "map",
                box(
                  width = "100%",
                  height = "700px", status = "info", solidHeader = TRUE,
                  title = "Minnesota Lakes > 300 acres",
                  leafletOutput("mymap",width="100%",height="700px"))
        ),
        tabItem(tabName = "alllakes",
                dataTableOutput("alllakes")
        ),
        tabItem(tabName = "alldata",
              
                dataTableOutput("text1")
        ),
        tabItem(tabName = "byyear",
                plotlyOutput("cpueplot")
       ),
       tabItem(tabName = "biomassplot",
               plotlyOutput("biomassplot")
       ),
        tabItem(tabName = "stationlist",
               dataTableOutput("text3")
   
                  ))
)
)
)
#)

    