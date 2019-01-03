
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
library(dplyr)
library(reshape2)
library(DT)

abv=read.csv("abv.csv",h=T)


read_lake_survey <- function(orig_url_or_id) {
  
  orig_url_or_id <- orig_url_or_id[1]
  
  if (grepl("^htt", orig_url_or_id)) {
    tmp <- httr::parse_url(orig_url_or_id)
    if (!is.null(tmp$query$downum)) {
      orig_url_or_id <- tmp$query$downum
    } else {
      stop("Invalid URL specified", call.=FALSE)
    }
  }
  
 httr::GET(
   url = "http://maps2.dnr.state.mn.us/cgi-bin/lakefinder/detail.cgi",
    query = list(
      type = "lake_survey",
      callback = "",
      id = orig_url_or_id,
      `_` = as.numeric(Sys.time())
    )
  ) -> res
  
  httr::stop_for_status(res)
  
  out <- httr::content(res, as="text", encoding="UTF-8")
  out <- jsonlite::fromJSON(out, flatten=TRUE)
  out
  
} 


shinyServer(function(input, output, session) {

 
  
  SiteID <- renderText({
  
    shiny::validate(
    need(input$siteid  != "", 'You need to enter a Site ID'))
    
    input$siteid  
  
    })
 

#globals <- reactiveValues()

  
  
    
O=eventReactive(input$do,{
    
    dat=read_lake_survey(SiteID())
    surveys <- dat$result$surveys
    tmp2 <- map2(surveys$fishCatchSummaries, surveys$surveyDate, ~{ .x$survey_date <- .y ; .x })
    tmp2 <- map2(tmp2, surveys$surveyType, ~{ .x$survey_type <- .y ; .x })
    tmp2 <- map2(tmp2, surveys$surveySubType, ~{ .x$survey_subtype <- .y ; .x })
    tmp2 <- map2_df(tmp2, surveys$surveyID, ~{ .$survey_id <- .y ; .x })
    tmp2[tmp2 == "N/A" ] <- NA 
    tmp2 <- as_tibble(tmp2)
    final_data <- data.frame(type_convert(tmp2))
    class(as.data.frame(final_data))
    tmp3=transform(final_data,Biomass=final_data$averageWeight*final_data$totalCatch,Class= abv$Coding[match(tmp2$species, abv$Abv)],
                   FullName=abv$Common[match(tmp2$species, abv$Abv)])
   
    tmp3
    
    
    })   

 
Data3 = reactive({

  if (input$gear==TRUE) {
 O()[O()$gear == "Standard gill nets" | O()$gear == "Standard trap nets", ]
} else {
  O()
}

})
  




output$text3 <- DT::renderDataTable({ 

  shiny::validate(
    need(input$siteid  != "", 'You need to enter a Site ID'),
    need(input$do, 'Click "Get Data"!'))
  
  
  datatable(Data3(), caption = paste("Lake Name:",dataset5()),
            extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
            options = list(
              # dom = 't',
              # deferRender = TRUE,
              searching = TRUE,
              #autoWidth = TRUE,
              # scrollCollapse = TRUE,
              rownames = FALSE,
              scroller = TRUE,
              scrollX = TRUE,
              scrollY = "500px",
              #fixedHeader = TRUE,
              class = 'cell-border stripe'

            )
  )
 

})

Z <- reactive({ 

#Data3()[, "Biomass"] <- as.numeric(as.character( Data3()[, "Biomass"]))
#Data3()[, "CPUE"]<- as.numeric(as.character( Data3()[, "CPUE"] )) 
  
x=aggregate(cbind(Biomass, CPUE) ~ survey_date + Class, data = Data3(), sum, na.rm = TRUE)

  
#Data3()$Biomass <- as.numeric( as.character( Data3()$Biomass ) )  
#Data3()$CPUE <- as.numeric( as.character( Data3()$CPUE ) )  
#df_melt <- melt(Data3(), id = c("survey_date","Class"))
#dcast(df_melt, survey_date + Class ~ variable, sum)  
  
  
#dd=data.table(B,stringsAsFactors = FALSE)
#dd=data.table(Data3())
#options(datatable.optimize=1)
#ddd=dd[,sum(CPUE), by=FullName]
#names(ddd)=c("Name","CPUE")
#bbb=dd[, sum(Biomass), by=FullName]
#names(bbb)=c("Name","Biomass")
#vv=dd[ ,CPUE:=as.numeric(sum(CPUE)),Biomass:=as.numeric(sum(Biomass)),by=list(Class,survey_date)]  
#dtnew <- dd[, Biomass2:=as.numeric(Biomass),CPUE2:=as.numeric(CPUE),Class2:=as.factor(Class),Date2=as.factor(survey_date)]
#dd[, sum(Biomass),by=list(survey_date,Class)]

})

output$text1 <- renderDataTable({ 

  datatable(Z(),caption = paste("Lake Name:",dataset5()),
            extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
            options = list(
              # dom = 't',
              # deferRender = TRUE,
              searching = TRUE,
              #autoWidth = TRUE,
              # scrollCollapse = TRUE,
              rownames = FALSE,
              scroller = TRUE,
              scrollX = TRUE,
              scrollY = "500px",
              fixedHeader = TRUE,
              class = 'cell-border stripe'

            )
  )
  
  
  })

MAP=reactive ({
read.csv("lakes.csv",h=T) 
})
output$alllakes <- renderDataTable({ 

  datatable(MAP(),caption = "Search for lake by name or id. Not all lakes have fish survey data")

  
})
     

 
MAP2=reactive ({
lakes=read.csv("lakes.csv",h=T) 
lakes[lakes$Acres > 300, ]  
})

dataset5 <- renderText({
as.character(MAP()[MAP()$Lake_ID == SiteID(),"Name"])
})
output$title=renderText ({

  shiny::validate(
    need(input$do, 'Click "Get Data"!'))
 
   paste("Lake Name:",dataset5())

  })





 

output$cpueplot <- renderPlotly({ 

plot1=ggplot(data=Z(),aes(x=factor(survey_date),y=CPUE,fill=Class))+
    geom_bar(stat="identity",position="stack")+
    scale_x_discrete()+
  theme(axis.text.x = element_text(color="black", size=9, angle=0))+labs(x="Date",title=paste0(dataset5()," Lake"," Catch Per Unit Effort"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,500)) 

ggplotly(plot1)
})


output$biomassplot <- renderPlotly({ 
  
  plot2=ggplot(data=Z(),aes(x=factor(survey_date),y=Biomass,fill=Class))+
    geom_bar(stat="identity",position="stack")+
    scale_x_discrete()+
 theme(axis.text.x = element_text(color="black", size=9, angle=0))+labs(x="Date",title=paste0(dataset5()," Lake"," Biomass"))+
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,1000)) + 
    labs(fill = "Class")
  
  ggplotly(plot2)

  })

output$mymap <- renderLeaflet({
  
  icons <- awesomeIcons(
    icon = 'ios-waterdrop',
    iconColor = 'black',
    library = 'ion'
  )
  leaflet(MAP2()) %>% addProviderTiles("Esri.WorldImagery") %>%
    #setView(-124.007251, 45.586442,-117.102349 , 48.001199, zoom=4)
    #fitBounds( -124.007251, 46.586442,-117.102349 , 46.001199)%>%
    addMarkers( ~MAP2()$Long, ~MAP2()$Lat,icon=icons,label = ~as.character(MAP2()$Name),
                       popup = paste("No:",MAP2()$Lake_ID,"<br>", "Name:", MAP2()$Name, "<br>","Acreage",MAP2()$Acres))
  
})


})


