################################################ IMPORTANT ########################################################
### Please run this app using browser such as Google Chrome and EDGE because it connects to external resources ####


### Name : Robert
### Student ID: 28243447


######################################################################################
################################# Import Libraries ###################################
######################################################################################
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(leaflet)
library(shinyWidgets)
library(visNetwork)
library(sunburstR)
library(sf)
library(geojsonio)
library(highcharter)
library(magick)
library(cowplot)
library(DT)
library(collapsibleTree)
library(shinyjs)
library(reshape)
library(htmltools)

######################################################################################
################################### Loading Data #####################################
######################################################################################
data <- read.csv('unicorn.csv')
colnames(data)
investment <- read.csv('investments.csv')
founder <- read.csv('founder.csv')
data_investment <- data %>% inner_join(investment, by="Id")
data$Id <- as.numeric(as.character(data$Id))
founder$Id <- as.numeric(as.character(founder$Id))
data_founder <- data %>% inner_join(founder, by="Id") 


######################################################################################
###################################### Server ########################################
######################################################################################
server <- function(input, output, session) {
  twitterTimeline <- function(href, ...) {
    tagList(
      tags$a(class = "twitter-timeline", href = href, ...),
      tags$script("twttr.widgets.load()")
    )
  }
  
  #################################### Sheet 1 ######################################
  ###################################################################################
  
  ############################### Sidebar Filter ##############################
  ########## Slider for Year of Becoming Unicorn Companies ##########
  output$year_slider <- renderUI({
    sliderInput("range", "Unicorn Year",
                min = min(data$Joined.Year), 
                max = max(data$Joined.Year),
                value = c(min(data$Joined.Year),max(data$Joined.Year)), 
                step = 1)
  })
  
  ########## Dropdown for Country ##########
  output$country <- renderUI({
    pickerInput("Country","Country", 
                choices=as.character(unique(data$Country[data$Joined.Year >= input$range[1] | data$Joined.Year <= input$range[2]])), 
                selected =as.character(unique(data$Country[data$Joined.Year >= input$range[1] & data$Joined.Year <= input$range[2]])), 
                options = list('actions-box' = TRUE),
                multiple = T)
  })
  
  ########## Dropdown for Business Category ##########
  output$business <- renderUI({
    pickerInput("Business","Business", 
                choices=as.character(unique(data$Single_Category[data$Country %in% input$Country])), 
                selected =  as.character(unique(data$Single_Category[data$Country %in% input$Country])), 
                options = list('actions-box' = TRUE),
                multiple = T)
  })
  
  ########## Dropdown for Company ##########
  output$company <- renderUI({
    pickerInput("Company","Unicorn", 
                choices=as.character(unique(data$Company[data$Country %in% input$Country & data$Single_Category %in% input$Business])), 
                selected = as.character(unique(data$Company[data$Country %in% input$Country & data$Single_Category %in% input$Business])), 
                options = list('actions-box' = TRUE),
                multiple = T)
  })
  
  
  ############################### User Guide ##############################
  observeEvent(input$guide, {
    if(input$tabs=="unicorn"){
      showModal(modalDialog(
        title = "User Guide",
        HTML("<h2>Welcome!!!</h2>
          This tab provides information about the details of an unicorn <br>
          <h4>---- Interaction ---</h4>
          - <b>Total Founders </b>, <b>Valuation</b>, and <b>Duration</b> info boxes will be updated based on the filters
            in the sidebar, the selection on the <b>Map</b> and the selection on the <b>Similar Unicorns</b><br>
          - <b>Map</b> will be updated based on the selection on the filters in the sidebar and the selection on the <b>Similar Unicorns</b><br>
          - <b>Similar Unicorns</b> will be updated based on the selection on the <b>Map</b> and the selection on the filters in the sidebar if 
            and only if there is only 1 company is selected <br>
          - <b>Twitter Timeline</b> will be updated based on the selection on the <b>Map</b>, the selection on the filters in the sidebar if 
            and only if there is only 1 company is selected and the selection on the <b>Similar Unicorns</b><br>
          - <b>Details</b> will be updated based on the selection on the <b>Map</b>, the selection on the filters in the sidebar if and only 
            if there is only 1 company is selected, and the selection on the <b>Similar Unicorns</b><br>
          <h4>---- Features ----</h4>
          <b>Sidebar :</b> Selection <br>
          <b>Map :</b> Zoom, Hover and Click <br>
          <b>Similar Unicorns :</b> Click <br>
          <h4>Note: Some unicorn might not have Twitter account</h4>
          <h3>Click Outside this Box to Exit</h3>"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else if (input$tabs == "charts"){
      showModal(modalDialog(
        title = "User Guide",
        HTML("<h2>Welcome!!!</h2>
          This tab provides information about the investment details of an unicorn <br>
             <h4>---- Interaction ---</h4>
             - <b>Total Funding </b>, <b>Biggest Funding</b>, and <b>Top Investor</b> info box will be updated based on the filters
             in the sidebar<br>
             - <b>Investment Network</b> will be updated based on the selection on the filters in the Sidebar</b><br>
             - <b>Investment Details</b> will be updated based on the selection on the investor or unicorn node of <b>Investment Network</b><br>
             - <b>Investment Destination</b> will be updated based on the selection on the investor node of <b>Investment Network</b><br>
             - <b>Business Category</b> will be updated based on the selection on the investor node of <b>Investment Network</b><br>
             - <b>Investment Source</b> will be updated based on the selection on the unicorn node of <b>Investment Network</b><br>
             - <b>Investor</b> will be updated based on the selection on the unicorn node of <b>Investment Network</b><br>
             - <b>Additional :</b> <b>Investment Destination</b> and <b>Business Category</b> boxes will only appear if the investor node is selected, meanwhile,
               <b>Investment Source</b> and <b>Investor</b> boxes will only appear if the unicorn node is selected
             <h4>---- Features ----</h4>
             <b>Sidebar :</b> Selection <br>
             <b>Investment Network :</b> Zoom, Drag, Hover and Click on Nodes and Links<br>
             <b>Investment Details :</b> Search <br>
             <b>Investment Destination :</b> Zoom, Hover, Expand, Collapse <br>
             <b>Business Category :</b> Hover <br>
             <b>Investment Source :</b> Hover <br>
             <b>Investors :</b> Hover <br>
             <h4>Note: When you want to switch between Filter Start from unicorn and company, please click on the white area inside the Investment Network before do the switching</h4>
             <h4>Note: For some investment 'Not Published' means the details about the investor is hidden by the unicorn</h4>
             <h3>Click Outside this Box to Exit</h3>"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else if (input$tabs == "unicorn_facts"){
      showModal(modalDialog(
        title = "User Guide",
        HTML("<h2>Welcome!!!</h2>
          This tab provides information about the unicorn statistics <br>
             <h4>---- Interaction ---</h4>
             The related interaction is divided by the background color. Each section has its own focus and the combination of all sections become a single narration
             <h4>---- Features ----</h4>
             <b>The information of the features is provided in the footer of each section in red color font<br>
             <h3>Click Outside this Box to Exit</h3>"),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  ################################# Main Body ################################
  ########## Map for Showing the Distribution of Unicorn Companies ##########
  output$map <- renderLeaflet({
    leaflet(data = data[data$Company %in% r$mapcompany,]) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude,
                 icon = makeIcon(~Url_Profile, iconWidth = 25, iconHeight = 25), 
                 clusterOptions = markerClusterOptions(), 
                 popup = ~paste("<b>Name : </b>", Company, "</br>","<b>Location : </b>",Country), 
                 layerId = ~Company)
  })
  
  ### Reactive Value to Control the User Action
  r <- reactiveValues()
  observe( {
    r$mapcompany <- as.vector(data$Company[data$Company %in% input$Company])
    
    if(length(input$Company)==1){
      r$condition <- TRUE
      r$activecompany <- as.vector(input$Company)
      r$url <- as.vector(data$Url_Profile[((data$Single_Category %in% input$Business | data$Country %in% input$Country | data$Region == data$Region[data$Country == input$Country]) 
                                           & (data$Company != input$Company)) ][1:6])
      
      r$company <- as.vector(data$Company[((data$Single_Category %in% input$Business | data$Country %in% input$Country | data$Region == data$Region[data$Country == input$Country]) 
                                           & (data$Company != input$Company))][1:6])
    }
    else{
      r$condition <- FALSE
    }
  })
  observeEvent(input$map_marker_click, {
    r$mapcompany <- as.vector(data$Company[data$Company %in% input$Company])
    if(length(input$map_marker_click$id)==1){
      r$condition <- TRUE
      r$activecompany <- as.vector(input$map_marker_click$id)
      r$url <- as.vector(data$Url_Profile[(data$Single_Category == data$Single_Category[data$Company == input$map_marker_click$id] |
                                             data$Country == data$Country[data$Company == input$map_marker_click$id] |
                                             data$Region == data$Region[data$Company == input$map_marker_click$id]) &
                                            (data$Company != input$map_marker_click$id)][1:6])
      r$company <- as.vector(data$Company[(data$Single_Category == data$Single_Category[data$Company == input$map_marker_click$id] |
                                             data$Country == data$Country[data$Company == input$map_marker_click$id]|
                                             data$Region == data$Region[data$Company == input$map_marker_click$id]) &
                                            (data$Company != input$map_marker_click$id)][1:6])
      
    }
  })
  
  ########## Image for Similar Unicorn Box ##########
  ### Similar 1
  ## Observe Event
  observeEvent(input$myclick1, {
    r$activecompany <- as.vector(r$company[1])
    r$mapcompany <- as.vector(r$company[1])
    r$url <- as.vector(data$Url_Profile[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[1]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[1]]) |
                                           data$Region %in% (data$Region[data$Company==r$company[1]])) &
                                          (data$Company != r$company[1])][1:6])
    r$company <- as.vector(data$Company[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[1]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[1]])|
                                           data$Region %in% (data$Region[data$Company==r$company[1]])) &
                                          (data$Company != r$company[1])][1:6])
  })
  
  ## For Drawing Image
  output$similarunicorn1 <- renderPlot({
    if(r$condition){
      ggdraw() + draw_image(r$url[1])
    }
  }, height = 100, width = 100)
  
  ## For Printing the Corresponding Text of the Image
  output$similarunicorn1_text <- renderUI({
    if(r$condition){
      HTML(paste("<h5 style='text-align:center'>",r$company[1],"</h5>"))
    }
  })
  
  
  ### Similar 2
  ## Observe Event
  observeEvent(input$myclick2, {
    r$activecompany <- as.vector(r$company[2])
    r$mapcompany <- as.vector(r$company[2])
    r$url <- as.vector(data$Url_Profile[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[2]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[2]]) |
                                           data$Region %in% (data$Region[data$Company==r$company[2]])) &
                                          (data$Company != r$company[2])][1:6])
    r$company <- as.vector(data$Company[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[2]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[2]])|
                                           data$Region %in% (data$Region[data$Company==r$company[2]])) &
                                          (data$Company != r$company[2])][1:6])
  })
  
  ## For Drawing Image
  output$similarunicorn2 <- renderPlot({
    if(r$condition){
      ggdraw() +  draw_image(r$url[2])
      
    }
  }, height = 100, width = 100)
  
  ## For Printing the Corresponding Text of the Image
  output$similarunicorn2_text <- renderUI({
    if(r$condition){
      HTML(paste("<h5 style='text-align:center'>",r$company[2],"</h5>"))
    }
  })
  
  ### Similar 3
  ## Observe Event
  observeEvent(input$myclick3, {
    r$activecompany <- as.vector(r$company[3])
    r$mapcompany <- as.vector(r$company[3])
    r$url <- as.vector(data$Url_Profile[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[3]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[3]]) |
                                           data$Region %in% (data$Region[data$Company==r$company[3]])) &
                                          (data$Company != r$company[3])][1:6])
    r$company <- as.vector(data$Company[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[3]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[3]])|
                                           data$Region %in% (data$Region[data$Company==r$company[3]])) &
                                          (data$Company != r$company[3])][1:6])
  })
  
  ## For Drawing Image
  output$similarunicorn3 <- renderPlot({
    if(r$condition){
      ggdraw() +  draw_image(r$url[3])
      
    }
  }, height = 100, width = 100)
  
  ## For Printing the Corresponding Text of the Image
  output$similarunicorn3_text <- renderUI({
    if(r$condition){
      HTML(paste("<h5 style='text-align:center'>",r$company[3],"</h5>"))
    }
  })
  
  ### Similar 4
  ## Observe Event
  observeEvent(input$myclick4, {
    r$activecompany <- as.vector(r$company[4])
    r$mapcompany <- as.vector(r$company[4])
    r$url <- as.vector(data$Url_Profile[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[4]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[4]]) |
                                           data$Region %in% (data$Region[data$Company==r$company[4]])) &
                                          (data$Company != r$company[4])][1:6])
    r$company <- as.vector(data$Company[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[4]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[4]])|
                                           data$Region %in% (data$Region[data$Company==r$company[4]])) &
                                          (data$Company != r$company[4])][1:6])
  })
  
  ## For Drawing Image
  output$similarunicorn4 <- renderPlot({
    if(r$condition){
      ggdraw() +  draw_image(r$url[4])
      
    }
  }, height = 100, width = 100)
  
  ## For Printing the Corresponding Text of the Image
  output$similarunicorn4_text <- renderUI({
    if(r$condition){
      HTML(paste("<h5 style='text-align:center'>",r$company[4],"</h5>"))
    }
  })
  
  ### Similar 5
  ## Observe Event
  observeEvent(input$myclick5, {
    r$activecompany <- as.vector(r$company[5])
    r$mapcompany <- as.vector(r$company[5])
    r$url <- as.vector(data$Url_Profile[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[5]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[5]]) |
                                           data$Region %in% (data$Region[data$Company==r$company[5]])) &
                                          (data$Company != r$company[5])][1:6])
    r$company <- as.vector(data$Company[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[5]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[5]])|
                                           data$Region %in% (data$Region[data$Company==r$company[5]])) &
                                          (data$Company != r$company[5])][1:6])
  })
  
  ## For Drawing Image
  output$similarunicorn5 <- renderPlot({
    if(r$condition){
      ggdraw() +  draw_image(r$url[5])
      
    }
  }, height = 100, width = 100)
  
  ## For Printing the Corresponding Text of the Image
  output$similarunicorn5_text <- renderUI({
    if(r$condition){
      HTML(paste("<h5 style='text-align:center'>",r$company[5],"</h5>"))
    }
  })
  
  ### Similar 6
  ## Observe Event
  observeEvent(input$myclick6, {
    r$activecompany <- as.vector(r$company[6])
    r$mapcompany <- as.vector(r$company[6])
    r$url <- as.vector(data$Url_Profile[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[6]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[6]]) |
                                           data$Region %in% (data$Region[data$Company==r$company[6]])) &
                                          (data$Company != r$company[6])][1:6])
    r$company <- as.vector(data$Company[(data$Single_Category %in% (data$Single_Category[data$Company==r$company[6]]) | 
                                           data$Country %in% (data$Country[data$Company==r$company[6]])|
                                           data$Region %in% (data$Region[data$Company==r$company[6]])) &
                                          (data$Company != r$company[6])][1:6])
  })
  
  ## For Drawing Image
  output$similarunicorn6 <- renderPlot({
    if(r$condition){
      ggdraw() +  draw_image(r$url[6])
      
    }
  }, height = 100, width = 100)
  
  ## For Printing the Corresponding Text of the Image
  output$similarunicorn6_text <- renderUI({
    if(r$condition){
      HTML(paste("<h5 style='text-align:center'>",r$company[6],"</h5>"))
    }
  })
  
  ########## Twitter Timeline ##########
  output$mytimeline <- renderUI({
    if(r$condition){
      twitterTimeline(data$Twitter[data$Company == r$activecompany])
    }
  })
  
  ########## Details of the Unicorn ##########
  output$details <- renderUI({
    if(r$condition){
      HTML(paste("<h5 style='text-align:justify'>",
                 "<b style='color:#4C93CA'>","Description :","</b>", data$Long_Desc[data$Company == r$activecompany],"<br>",
                 "<b style='color:#4C93CA'>","Founded Year :","</b>", data$Found_Year[data$Company == r$activecompany],"<br>",
                 "<b style='color:#4C93CA'>","Founder :","</b>", paste(as.vector(founder$name[founder$Id == data$Id[data$Company == r$activecompany]]), collapse = ", "), "<br>",
                 "<b style='color:#4C93CA'>","Business :","</b>", data$Single_Category[data$Company == r$activecompany], "<br>",
                 "<b style='color:#4C93CA'>","Country :","</b>", data$Country[data$Company == r$activecompany],"</h5>"))
    }
  })
  ############################### Header Boxes ##############################
  ########## Box 1 ##########
  output$durationbox1 <- renderInfoBox({
    value <-if(r$condition){ 
      length(data_founder$name[data_founder$Company %in% r$activecompany])
    }
    infoBox("Total Founders", paste(value, "Founder(s)"), icon = icon("user"), color = "purple", fill = TRUE)
  })
  
  ########## Box 2 ##########
  output$durationbox2 <- renderInfoBox({
    value <-if(r$condition){
      data$Valuation[data$Company %in% r$activecompany]
    }
    infoBox("Valuation", paste("US$",value, "M"), icon = icon("dollar-sign"), color = "purple", fill = TRUE)
  })
  
  ########## Box 3 ##########
  output$durationbox3 <- renderInfoBox({
    value <-if(r$condition){
      data$Duration[data$Company %in% r$activecompany]
    }
    infoBox("Duration", paste(round(as.numeric(value)), "Month(s)"), icon = icon("hourglass-end"), color = "purple", fill = TRUE)
  })
  
  ########## Box 4 ##########
  output$durationbox4 <- renderInfoBox({
    value <- if (length(r$mapcompany)>0){ 
      temp <- data %>% arrange(desc(Found_Year)) %>% top_n(1) 
      value <- temp$Company
    }
    infoBox("Newest Unicorn", paste(value), icon = icon("lightbulb"), color = "purple", fill = TRUE)
  })
  
  
  
  
  
  #################################### Sheet 2 ######################################
  ###################################################################################
  
  ############################### Sidebar Filter ##############################
  ### Dropdown for Investor Based on Company Filter
  output$CompInv <- renderUI({
    pickerInput("CompInv","Investor", 
                choices=as.character(unique(data_investment$Investor[data_investment$Company %in% input$Company2])), 
                options = list('actions-box' = TRUE),
                multiple = T)
  })
  
  ### Dropdown for Company Based on Investor Filter
  output$InvComp <- renderUI({
    pickerInput("InvComp","Unicorn", 
                choices=as.character(unique(data_investment$Company[data_investment$Investor %in% input$Investor2])), 
                options = list('actions-box' = TRUE),
                multiple = T)
  })
  shinyjs::showElement(id= "option1")
  shinyjs::hideElement(id= "option2")
  
  ################################# Main Body ################################
  ########## Network Visualisation for Company and Investor ##########
  output$network <- renderVisNetwork({
    if((length(input$Company2)!=0 & length(input$CompInv)!=0 & input$col == "Unicorn") |
       (length(input$Investor2)!=0 & length(input$InvComp)!=0)& input$col == "Investor"){
      
      data_investment <- data_investment[((data_investment$Company %in% input$Company2) & 
                                            (data_investment$Investor %in% input$CompInv) & 
                                            input$col == "Unicorn") |
                                           ((data_investment$Investor %in% input$Investor2) & 
                                              (data_investment$Company %in% input$InvComp) & 
                                              input$col == "Investor"),] %>%
        select(Company, Valuation,date, amount, type, Investor)
      
      links <- data_investment %>% select(Investor, Company,amount, date, type)
      colnames(links) <- c("from","to","weight","date","type")
      
      temp1 <- data_investment %>% 
        group_by(Company) %>% 
        summarise(amount = sum(amount))
      temp1 <- cbind(temp1,1, "Unicorn")
      colnames(temp1) <- c("id", "audience.size","media.type","type.label")
      temp2 <- data_investment %>% 
        group_by(Investor) %>% 
        summarise(amount = sum(amount))
      temp2 <- cbind(temp2,2,"Investor")
      colnames(temp2) <- c("id", "audience.size","media.type","type.label")
      nodes <- rbind(temp1,temp2)
      nodes$media <- nodes$id
      nodes <- nodes[,c(1,5, 3, 4,2)]
      nodes <- nodes[nodes$id %in% links$to | nodes$id %in% links$from,]
      
      nodes$shape  <- "dot"  
      nodes$shadow <- TRUE # Nodes will drop shadow
      nodes$title  <- paste("Name : ", nodes$media, "<br>",
                            "Amount : US$",round(nodes$audience.size/1000000), "M") # Text on click
      nodes$label  <- nodes$media # Node label
      nodes$size   <- log(nodes$audience.size, base=2) # Node size
      nodes$borderWidth <- 2 # Node border width
      nodes$color.background <- c("magenta", "greenyellow")[nodes$media.type]
      nodes$color.border <- "red"
      nodes$color.highlight.background <- "yellow"
      nodes$color.highlight.border <- "red"
      
      links$width <- log(links$weight,8) # line width
      links$color <- "skyblue"    # line color
      links$title <- paste("From :",links$from,"<br>",
                           "To : ",links$to,"<br>", 
                           "Date : " ,links$date,"<br>",
                           "Type : ", links$type,"<br>",
                           "Amount : US$",round(links$weight/1000000), "M")
      
      nodes$group <- nodes$type.label 
      visnet3 <- visNetwork(nodes, links)
      visnet3 <- visGroups(visnet3, 
                           groupname = "Unicorn", 
                           shape = "dot",
                           color = list(background = "magenta", 
                                        border="red"))
      visnet3 <- visGroups(visnet3, 
                           groupname = "Investor", 
                           shape = "dot",       
                           color = list(background = "greenyellow", 
                                        border="red"))
      visnet3 %>% visEvents(click = "function(nodes){
                            Shiny.onInputChange('got_network_current_node_id', nodes);}") %>% 
        visLegend(main="Group", 
                  position="right", 
                  ncol=1, 
                  width = 0.1)
    }
  })
  
  ########## Observe the Click Event on Node of the Network ##########
  observeEvent(input$got_network_current_node_id, {
    if (is.null(input$got_network_current_node_id)) {
      NULL
    } else {
      if (length(input$got_network_current_node_id$node) == 0) {
        NULL
      }else{
        if(input$got_network_current_node_id$nodes[[1]] %in% data_investment$Investor){
          shinyjs::showElement(id= "option1")
          shinyjs::hideElement(id= "option2")
        }
        else if (input$got_network_current_node_id$nodes[[1]] %in% data_investment$Company){
          shinyjs::hideElement(id= "option1")
          shinyjs::showElement(id= "option2")
          
        }
      }
    }
  })
  
  ########## Data Table for Details ########## 
  output$transport_table <- DT::renderDataTable({
    if (is.null(input$got_network_current_node_id)) {
      NULL
    } else {
      if (length(input$got_network_current_node_id$node) == 0) {
        NULL
      } else {
        data_investment[(data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$InvComp & input$col == "Investor") | 
                          (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$Investor2 & input$col == "Investor") |
                          (data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$Company2 & input$col == "Unicorn") | 
                          (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$CompInv & input$col == "Unicorn"),] %>%
          select(Company, Investor, date, type, amount)
      }
    }
  }, rownames = FALSE,
  options = list(pageLength = 5))
  
  ########## Stacked Bar for Showing Investment on Business over Years ########## 
  output$stackchart1 <- renderHighchart({
    if (is.null(input$got_network_current_node_id)) {
      NULL
    } else {
      if (length(input$got_network_current_node_id$node) == 0) {
        NULL
      } else if (input$got_network_current_node_id$nodes[[1]] %in% data_investment$Investor) {
        df <- data_investment[(data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$InvComp & input$col == "Investor") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$Investor2 & input$col == "Investor") |
                                (data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$Company2 & input$col == "Unicorn") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$CompInv & input$col == "Unicorn"),] %>% 
          select(Single_Category, Investment.Year, amount) %>% 
          group_by(Single_Category, Investment.Year) %>% 
          summarise(Investment = sum(amount))
        
        df %>% hchart(type = "column",
                      hcaes(x = Investment.Year,
                            y = Investment,
                            group = Single_Category))%>%
          hc_plotOptions(column = list(stacking = "stack")) %>%
          hc_yAxis(title = list(text = 'Investment Amount [US$]'))
      }
    }
  })
  
  ########## Stacked Bar for Showing Investment from Investors over Years ########## 
  output$stackchart2 <- renderHighchart({
    if (is.null(input$got_network_current_node_id)) {
      NULL
    } else {
      if (length(input$got_network_current_node_id$node) == 0) {
        NULL
      } else if (input$got_network_current_node_id$nodes[[1]] %in% data_investment$Company) {
        df <- data_investment[(data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$InvComp & input$col == "Investor") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$Investor2 & input$col == "Investor") |
                                (data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$Company2 & input$col == "Unicorn") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$CompInv & input$col == "Unicorn"),] %>% 
          select(Investor, Investment.Year, amount) %>% 
          group_by(Investor, Investment.Year) %>% 
          summarise(Investment = sum(amount))
        
        df %>% hchart(type = "column",
                      hcaes(x = Investment.Year,
                            y = Investment,
                            group = Investor))%>%
          hc_plotOptions(column = list(stacking = "stack")) %>%
          hc_xAxis(title = list(text = 'Investment Year')) %>%
          hc_yAxis(title = list(text = 'Investment Amount [US$]'))
      }
    }
    
  })
  
  ########## Pie Chart for showing the proportion of the investment based on the investor country ##########
  output$piechart <- renderHighchart({
    if (is.null(input$got_network_current_node_id)) {
      NULL
    } else {
      if (length(input$got_network_current_node_id$node) == 0) {
        NULL
      } else if (input$got_network_current_node_id$nodes[[1]] %in% data_investment$Company) {
        df <- data_investment[(data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$InvComp & input$col == "Investor") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$Investor2 & input$col == "Investor") |
                                (data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$Company2 & input$col == "Unicorn") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$CompInv & input$col == "Unicorn"),] %>% 
          select(Investor.Location, amount) %>% 
          group_by(Investor.Location) %>% 
          summarise(Investment = sum(amount))
        
        highchart() %>% 
          hc_chart(type = "pie") %>% 
          hc_add_series_labels_values(labels = df$Investor.Location, 
                                      values = df$Investment)%>%
          hc_tooltip(crosshairs = TRUE, 
                     borderWidth = 5, 
                     sort = TRUE, 
                     shared = TRUE, 
                     table = TRUE,
                     pointFormat = paste(' : <b>{point.percentage:.1f}%</b>')
          )
      }
    }
  })
  
  ########## Collapsible Tree for showing the destination country of the investment ##########
  output$collapsibleTree <- renderCollapsibleTree({
    if (is.null(input$got_network_current_node_id)) {
      NULL
    } else {
      if (length(input$got_network_current_node_id$node) == 0) {
        NULL
      } else if (input$got_network_current_node_id$nodes[[1]] %in% data_investment$Investor) {
        
        df <- data_investment[(data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$InvComp & input$col == "Investor") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$Investor2 & input$col == "Investor") |
                                (data_investment$Investor == input$got_network_current_node_id$nodes[[1]] & data_investment$Company %in% input$Company2 & input$col == "Unicorn") | 
                                (data_investment$Company == input$got_network_current_node_id$nodes[[1]] & data_investment$Investor %in% input$CompInv & input$col == "Unicorn"),] %>%
          select(Country, Investor.Location, Company, amount) %>% 
          group_by(Country, Investor.Location, Company) %>%
          summarise(amount = sum(amount))
        
        collapsibleTree(
          df,
          hierarchy = c("Country","Company"),
          root = as.vector(unique(df$Investor.Location)),
          width = 800,
          tooltip = TRUE,
          nodeSize = "amount",
          zoomable = TRUE
        )
        
      }
    }
  })
  
  ############################### Header Boxes ##############################
  ########## Box 1 ##########
  output$durationbox12  <- renderInfoBox({
    if ((length(input$Company2)!=0 & length(input$CompInv)!=0 & input$col == "Unicorn") |
        (length(input$Investor2)!=0 & length(input$InvComp)!=0 & input$col == "Investor")){
      value <- sum(data_investment$amount[(input$col == "Unicorn" & data_investment$Company %in% input$Company2 & data_investment$Investor %in% input$CompInv)|
                                            (input$col == "Investor" & data_investment$Investor %in% input$Investor2 & data_investment$Company %in% input$InvComp)])
      infoBox("Total Funding", 
              paste("US$",round(value/1000000), "M"), 
              icon = icon("dollar-sign"), 
              color = "purple", 
              fill = TRUE)
    }
    else
    {
      infoBox("Total Funding", 
              paste("US$", "M"), 
              icon = icon("dollar-sign"), 
              color = "purple", fill = TRUE)
    }
  })
  
  ########## Box 2 ##########
  output$durationbox22  <- renderInfoBox({
    if ((length(input$Company2)!=0 & length(input$CompInv)!=0 & input$col == "Unicorn") |
        (length(input$Investor2)!=0 & length(input$InvComp)!=0 & input$col == "Investor")){
      df <- data_investment[(input$col == "Unicorn" & data_investment$Company %in% input$Company2 & data_investment$Investor %in% input$CompInv)|
                              (input$col == "Investor" & data_investment$Investor %in% input$Investor2 & data_investment$Company %in% input$InvComp),]
      value <- df %>% group_by(Investor) %>% summarise(Total = sum(amount)) %>% arrange(Total) %>% top_n(1)
      infoBox("Biggest Funding", 
              paste("US$",round(value$Total/1000000), "M"), 
              icon = icon("money"), 
              color = "purple", 
              fill = TRUE)
    }
    else
    {
      infoBox("Biggest Funding", 
              paste("US$","M"), 
              icon = icon("money"), 
              color = "purple", 
              fill = TRUE)
    }
  })
  
  ########## Box 3 ##########
  output$durationbox32  <- renderInfoBox({
    if ((length(input$Company2)!=0 & length(input$CompInv)!=0 & input$col == "Unicorn") |
        (length(input$Investor2)!=0 & length(input$InvComp)!=0 & input$col == "Investor")){
      df <- data_investment[(input$col == "Unicorn" & data_investment$Company %in% input$Company2 & data_investment$Investor %in% input$CompInv)|
                              (input$col == "Investor" & data_investment$Investor %in% input$Investor2 & data_investment$Company %in% input$InvComp),]
      value <- df %>% 
        group_by(Investor) %>% 
        summarise(Total = sum(amount)) %>% 
        arrange(Total) %>% 
        top_n(1)
      infoBox("Top Investor", 
              paste(value$Investor), 
              icon = icon("male"), 
              color = "purple", 
              fill = TRUE)
    }
    else
    {
      infoBox("Top Investor", 
              paste("-"), 
              icon = icon("male"), 
              color = "purple", 
              fill = TRUE)
    }
  })
  
  ########## Box 4 ##########
  output$durationbox42  <- renderInfoBox({
    value <- data_investment %>% select(Company, Investor, date, Newest) %>% arrange(desc(Newest))
    infoBox("Latest Funding", 
            paste("From ",value$Investor,"to ", value$Company, "on ", value$date), 
            icon = icon("arrow-right"), 
            color = "purple", fill = TRUE)
  })
  
  
  #################################### Sheet 3 ######################################
  ###################################################################################
  
  ################################# Main Body ################################
  ########## World Map for showing the distribution of Unicorn Companies ##########
  output$worldmap <- renderHighchart({
    data <- data %>% select(Country,Valuation) %>% group_by(Country) %>% summarise(Quantity = n())
    world_as_sf <- st_read("data/world-shape-files/ne_50m_admin_0_countries.shp")
    world_as_geojsn <- geojson_list(world_as_sf)
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', event.point.Country);}")
    legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
    
    highchart(type = "map") %>%
      hc_add_series_map(map = world_as_geojsn,
                        df = data,
                        color = "#d35400",
                        value = "Quantity",
                        joinBy = c("name_long","Country"),
                        name = "Total Unicorn")%>%
      hc_colorAxis(minColor = "#a7e4e8", maxColor = "#14767d") %>%
      hc_plotOptions(series = list(stacking = FALSE, events = list(click = canvasClickFunction, legendItemClick = legendClickFunction)))
  })
  
  shinyjs::showElement(id= "barcharttext")
  shinyjs::hideElement(id= "barchartviz")
  observeEvent(input$canvasClicked, {
    if(is.null(input$canvasClicked)){
      NULL
    } else
    {
      shinyjs::showElement(id= "barchartviz")
      shinyjs::hideElement(id= "barcharttext")
    }
  })
  output$barcharttext <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:20px;
                   font-weight: 500; line-height: 1.1; 
               color: #d35400; text-align: center;'>","</br>","</br>","</br>","</br>","</br>","</br>","</br>","Click on The Map to See Me....","</br>","</br>","</br>","</br>","</br>","</br>","</br>","</br>","</br>","</h1>"))
  })
    
  ########## Bar Chart Shows the number of Unicorn Companies Over Years ##########
  output$barchart <- renderHighchart({
    if(length(input$canvasClicked)==0){
      NULL
    }else{
      data <- data %>% 
        select(Country, Joined.Year)
      colnames(data) <- c("Country","Year")
      data <- data[data$Country==input$canvasClicked,]
      data <- data %>% 
        group_by(Year) %>% 
        summarise(Country = n())
      data %>% hchart(type = "column",
                      hcaes(x = Year,
                            y = Country),
                      name = "Total Unicorn")%>%
        hc_yAxis(title = list(text = 'Total Unicorns'))
    }
  })
  
  ########## Line Chart shows the number of unicorn companies over years ##########
  output$spline2 <- renderHighchart({
    data <- data %>% select(Country,Single_Category, Joined.Year)
    colnames(data) <- c("Country","Single_Category","Year")
    data <- data %>% group_by(Year) %>% summarise(Country = n())
    data %>% hchart(type = "spline",
                    hcaes(x = Year,
                          y = Country),
                    name = "Total Unicorns")%>%
      hc_yAxis(title = list(text = 'Total Unicorns'))
  })
  
  ########## Map Animation shows the growth in term of Valuation ##########
  output$mapanimation <- renderLeaflet({
    pal <- colorFactor(
      palette = 'Dark2',
      domain = data$Country
    )
    tbl_1 <- tibble(Joined.Year = min(data$Joined.Year):max(data$Joined.Year))
    tbl_2 <- tibble(Country = unique(data$Country), Country.Latitude = unique(data$Country.Latitude), Country.Longitude = unique(data$Country.Longitude))
    tbl_1$fake <- 1
    tbl_2$fake <- 1
    my_cross_join <- full_join(tbl_1, tbl_2, by = "fake") %>%
      select(-fake)
    
    data <- data %>% select(Valuation, Country.Latitude, Country.Longitude, Joined.Year, Country) %>% group_by(Country, Joined.Year, Country.Latitude, Country.Longitude) %>% summarise(Valuation = sum(Valuation))
    data <- left_join(my_cross_join, data[,c("Joined.Year","Country","Valuation")], by = c("Joined.Year","Country"))
    data[is.na(data)] <- 0
    data$summ <- ave(data$Valuation,data$Country,FUN=cumsum)
    
    leaflet(data = data[data$Joined.Year==input$year,]) %>% addTiles() %>% 
      addCircleMarkers(lng = ~Country.Longitude, 
                       lat = ~Country.Latitude, 
                       radius = ~log(summ,2), 
                       fillColor = ~pal(Country),
                       stroke=FALSE,
                       fillOpacity =0.8,
                       popup = ~paste("<b>Country : </b>", Country, "</br>","<b>Total Valuation : </b>", "US$",summ,"M")
      )
  })
  
  ########## Line Chart shows the number of unicorn companies over years by Business Category ##########
  output$linechart <- renderHighchart({
    data <- data %>% select(Single_Category, Joined.Year)
    data <- cbind(data, 1)
    colnames(data) <- c("Single_Category","Joined.Year","Quantity")
    data <- data %>% group_by(Single_Category, Joined.Year) %>% summarise(Quantity = sum(Quantity))
    
    hchart(data, "spline", hcaes(x = Joined.Year, y = Quantity, group = Single_Category)) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = 'Year')) %>%
      hc_yAxis(title = list(text = 'Total Unicorns'))
  })
  
  ########## Sun Chart show the proportion of all features ##########
  output$sun <- renderSund2b({
    data <- data %>% select(Joined.Year, Country, Single_Category)
    colnames(data) <- c("Year","Country","Single_Category")
    data <- data %>% group_by(Year, Country, Single_Category) %>% summarise(Quantity = n())
    data$Single_Category <- gsub('-', ' ', data$Single_Category)
    data$V1 <- paste(data$Year, "-", data$Country, "-", data$Single_Category)
    data$V2 <- data$Quantity
    data <- data[,c("V1","V2")]
    sund2b(data)
  })
  
  ########## Pie Chart shows the proportion of total founders ##########
  output$founder1 <- renderHighchart({
    data$Id <- as.numeric(as.character(data$Id))
    founder$Id <- as.numeric(as.character(founder$Id))
    data_founder <- data %>% inner_join(founder, by="Id") %>% select(Company,name) %>%
      group_by(Company) %>% summarise(Total_Founder = n()) %>% group_by(Total_Founder) %>% summarise(Total_Company = n())
    
    highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_add_series_labels_values(labels = data_founder$Total_Founder, values = data_founder$Total_Company)%>%    
      hc_tooltip(crosshairs = TRUE, borderWidth = 5, sort = TRUE, shared = TRUE, table = TRUE,
                 pointFormat = paste(' founder(s) <b> : {point.percentage:.1f}%</b>')
      )
    
  })
  
  ########## Pie Chart shows the proportion of founders' gender ##########
  output$founder2 <- renderHighchart({
    founder <- founder %>% group_by(gender) %>% summarise(Total_Founder = n())
    highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_add_series_labels_values(labels = founder$gender, values = founder$Total_Founder)%>%    
      hc_tooltip(crosshairs = TRUE, borderWidth = 5, sort = TRUE, shared = TRUE, table = TRUE,
                 pointFormat = paste('<b> : {point.percentage:.1f}%</b>')
      )
  })
  
  ########## Stacked Bar Chart shows the proportion of founders' gender by Business Category ##########
  output$founder3 <- renderHighchart({
    data$Id <- as.numeric(as.character(data$Id))
    founder$Id <- as.numeric(as.character(founder$Id))
    data_founder <- data %>% inner_join(founder, by="Id") %>% select(Single_Category,gender) %>%
      group_by(Single_Category, gender) %>% summarise(Total = n())
    data_founder <- cast(data_founder, Single_Category ~ gender)
    data_founder[is.na(data_founder)] <- 0
    categories_column <- "Single_Category"
    measure_columns <- c("Male","Female")
    generated_chart <- highchart() %>%
      hc_xAxis(categories = data_founder[, categories_column],
               title = categories_column)
    
    invisible(lapply(measure_columns, function(column) {
      generated_chart <<-
        hc_add_series(hc = generated_chart, name = column,
                      data = data_founder[, column])
    }))
    
    generated_chart <- generated_chart %>%
      hc_chart(type = "bar") %>%
      hc_plotOptions(series = list(stacking = "percent")) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(shared = TRUE)
    generated_chart
  })
  
  ########## Stacked Bar Chart shows the proportion of founders' gender by Country ##########
  output$founder4 <- renderHighchart({
    data$Id <- as.numeric(as.character(data$Id))
    founder$Id <- as.numeric(as.character(founder$Id))
    data_founder <- data %>% inner_join(founder, by="Id") %>% select(Country,gender) %>%
      group_by(Country, gender) %>% summarise(Total = n())
    
    data_founder <- cast(data_founder, Country ~ gender)
    data_founder[is.na(data_founder)] <- 0
    categories_column <- "Country"
    measure_columns <- c("Male","Female")
    generated_chart <- highchart() %>%
      hc_xAxis(categories = data_founder[, categories_column],
               title = categories_column)
    
    invisible(lapply(measure_columns, function(column) {
      generated_chart <<-
        hc_add_series(hc = generated_chart, name = column,
                      data = data_founder[, column])
    }))
    
    generated_chart <- generated_chart %>%
      hc_chart(type = "bar") %>%
      hc_plotOptions(series = list(stacking = "percent")) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(shared = TRUE)
    generated_chart
  })
  
  
  ########## Line Graph show the total investment over the year ##########
  output$lineinvestment1 <- renderHighchart({
    data_investment <- data_investment %>% 
      select(Investment.Year,amount) %>% 
      group_by(Investment.Year) %>% 
      summarise(Investment = sum(amount/1000000))
    data_investment
    data_investment %>% hchart(type = "spline",
                               hcaes(x = Investment.Year,
                                     y = round(Investment)),
                               name = "Total Investment") %>%
      hc_xAxis(title = list(text = 'Year')) %>%
      hc_yAxis(title = list(text = 'Total Investment [US$ million]'))
  })
  
  ########## Line Graph show the total investment over the year by business category ##########
  output$lineinvestment2 <- renderHighchart({
    data_investment <- data_investment %>% 
      select(Investment.Year,Single_Category, amount) %>% 
      group_by(Investment.Year, Single_Category) %>% 
      summarise(Investment = sum(amount/1000000))
    data_investment
    data_investment %>% hchart(type = "spline",
                               hcaes(x = Investment.Year,
                                     y = round(Investment),
                                     group = Single_Category)) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_legend(align = "right", verticalAlign = "top",
                layout = "vertical") %>%
      hc_xAxis(title = list(text = 'Year')) %>%
      hc_yAxis(title = list(text = 'Total Investment [US$ million]'))
  })
  
  
  ########## Network Graph shows the investment network over countries ##########
  output$countrynetwork <- renderVisNetwork({
    if(is.null(input$Company3) & is.null(input$Investor3)){
      NULL
    } else
    if((length(input$Company3)!=0 & input$col2=="Unicorn")|(length(input$Investor3)!=0 & input$col2=="Investor")){
      
      data_investment <- data_investment[(data_investment$Country %in% input$Company3 & input$col2 == "Unicorn")|
                                           (data_investment$Investor.Location %in% input$Investor3 & input$col2 == "Investor"), ] 
      
      data_investment <- data_investment[data_investment$Investor.Location!="Not Published",]
      data_investment$Investor.Location <- paste(data_investment$Investor.Location, "(Investor)", sep="")
      links <- data_investment %>% select(Investor.Location, Country,amount) %>% group_by(Investor.Location, Country) %>% summarise(amount = sum(amount))
      colnames(links) <- c("from","to","weight")
      
      temp1 <- data_investment %>% group_by(Country) %>% summarise(amount = sum(amount))
      temp1 <- cbind(temp1,1, "Unicorn")
      colnames(temp1) <- c("id", "audience.size","media.type","type.label")
      temp2 <- data_investment %>% group_by(Investor.Location) %>% summarise(amount = sum(amount))
      temp2 <- cbind(temp2,2,"Investor")
      colnames(temp2) <- c("id", "audience.size","media.type","type.label")
      nodes <- rbind(temp1,temp2)
      nodes$media <- nodes$id
      
      nodes <- nodes[,c(1,5, 3, 4,2)]
      nodes <- nodes[nodes$id %in% links$to | nodes$id %in% links$from,]
      
      nodes$shape  <- "dot"  
      nodes$shadow <- TRUE # Nodes will drop shadow
      nodes$title  <- paste("Name : ", nodes$media, "<br>",
                            "Amount : US$",round(nodes$audience.size/1000000), "M") # Text on click
      nodes$label  <- nodes$media # Node label
      nodes$size   <- log(nodes$audience.size, base=2) # Node size
      nodes$borderWidth <- 2 # Node border width
      nodes$color.background <- c("magenta", "greenyellow")[nodes$media.type]
      nodes$color.border <- "red"
      nodes$color.highlight.background <- "yellow"
      nodes$color.highlight.border <- "red"
      
      links$width <- log(links$weight,10) # line width
      links$color <- "#d35400"    # line color
      links$arrows <- "to"
      links$title <- paste("From :",links$from,"<br>",
                           "To : ",links$to,"<br>", 
                           "Amount : US$",round(links$weight/1000000), "M")
      nodes$group <- nodes$type.label 
      visnet3 <- visNetwork(nodes, links)
      visnet3 <- visGroups(visnet3, groupname = "Unicorn", shape = "dot",
                           color = list(background = "magenta", border="red"))
      visnet3 <- visGroups(visnet3, groupname = "Investor", shape = "dot",       
                           color = list(background = "greenyellow", border="red"))
      visnet3 %>% visEvents(click = "function(nodes){
                              Shiny.onInputChange('got_network_current_node_id', nodes);
      }") %>% visLegend(main="Group", position="right", ncol=1, width = 0.1)
    }
  })
  
  
  ########## Network Graph shows the investment network over company and investor ##########
  output$companynetwork <- renderVisNetwork({
    if(is.null(input$Company4) & is.null(input$Investor4)){
      NULL
    } else
    if((length(input$Company4)!=0 & input$col3=="Unicorn")|(length(input$Investor4)!=0 & input$col3=="Investor")){
      data_investment <- data_investment[((data_investment$Company %in% input$Company4) & (input$col3 == "Unicorn"))|
                                           ((data_investment$Investor %in% input$Investor4) & (input$col3 == "Investor")), ] 
      data_investment <- data_investment[data_investment$Investor!="Not Published",]
      links <- data_investment %>% select(Investor, Company,amount) %>% group_by(Investor, Company) %>% summarise(amount = sum(amount))
      colnames(links) <- c("from","to","weight")
      
      temp1 <- data_investment %>% 
        group_by(Company) %>% 
        summarise(amount = sum(amount))
      temp1 <- cbind(temp1,1, "Unicorn")
      colnames(temp1) <- c("id", "audience.size","media.type","type.label")
      temp2 <- data_investment %>% 
        group_by(Investor) %>% 
        summarise(amount = sum(amount))
      temp2 <- cbind(temp2,2,"Investor")
      colnames(temp2) <- c("id", "audience.size","media.type","type.label")
      nodes <- rbind(temp1,temp2)
      nodes$media <- nodes$id
      nodes <- nodes[,c(1,5, 3, 4,2)]
      nodes <- nodes[nodes$id %in% links$to | nodes$id %in% links$from,]
      
      nodes$shape  <- "dot"  
      nodes$shadow <- TRUE # Nodes will drop shadow
      nodes$title  <- paste("Name : ", nodes$media, "<br>",
                            "Amount : US$",round(nodes$audience.size/1000000), "M") # Text on click
      nodes$label  <- nodes$media # Node label
      nodes$size   <- log(nodes$audience.size, base=2) # Node size
      nodes$borderWidth <- 2 # Node border width
      nodes$color.background <- c("#A3C4BD", "#18FBEC")[nodes$media.type]
      nodes$color.border <- "red"
      nodes$color.highlight.background <- "yellow"
      nodes$color.highlight.border <- "red"
      
      links$width <- log(links$weight,10) # line width
      links$color <- "#00BBBB"    # line color
      links$arrows <- "to"
      links$title <- paste("From :",links$from,"<br>",
                           "To : ",links$to,"<br>", 
                           "Amount : US$",round(links$weight/1000000), "M")
      
      nodes$group <- nodes$type.label 
      visnet3 <- visNetwork(nodes, links)
      visnet3 <- visGroups(visnet3, 
                           groupname = "Unicorn", 
                           shape = "dot",
                           color = list(background = "#A3C4BD", 
                                        border="red"))
      visnet3 <- visGroups(visnet3, 
                           groupname = "Investor", 
                           shape = "dot",       
                           color = list(background = "#18FBEC", 
                                        border="red"))
      visnet3 %>% visEvents(click = "function(nodes){
                              Shiny.onInputChange('got_network_current_node_id', nodes);}") %>% 
        visLegend(main="Group", 
                  position="right", 
                  ncol=1, 
                  width = 0.1) 
    }
  })
  
  ########## Bar Chart shows The Top 10 Country by Unicorn Quantity ##########
  output$top1 <- renderHighchart({
    data <- data %>% group_by(Country) %>% summarise(Quantity = n()) %>% arrange(desc(Quantity))%>% top_n(10)
    data %>% hchart(type = "column",
                    hcaes(x = Country,
                          y = Quantity),
                    name = "Unicorn Quantity")%>%
      hc_yAxis(title = list(text = 'Quantity')) %>%
      hc_title(text = "The Top 10 Country by Unicorn Quantity")
  })
  
  ########## Bar Chart shows The Top 10 Most Valuable Unicorn ##########
  output$top2 <- renderHighchart({
    data <- data %>% arrange(desc(Valuation)) %>% select(Company, Valuation) %>% top_n(10)
    data %>% hchart(type = "column",
                    hcaes(x = Company,
                          y = Valuation),
                    name = "Valuation")%>%
      hc_xAxis(title = list(text = 'Unicorn'))%>%
      hc_yAxis(title = list(text = 'Valuation [US$ million]'))%>%
      hc_title(text = "The Top 10 Most Valuable Unicorn")
  })
  
  ########## Bar Chart shows The Top 10 Fastest Unicorn ##########
  output$top3 <- renderHighchart({
    data <- data %>% arrange(Duration)%>% select(Company, Duration) %>% top_n(-10)
    data %>% hchart(type = "column",
                    hcaes(x = Company,
                          y = Duration),
                    name = "Duration")%>%
      hc_xAxis(title = list(text = 'Unicorn'))%>%
      hc_yAxis(title = list(text = 'Duration [Month]')) %>%
      hc_title(text = "The Top 10 Fastest Unicorn")
  })
  
  ########## Bar Chart shows The Top 10 Biggest Investor ##########
  output$top4 <- renderHighchart({
    data_investment <- data_investment %>% group_by(Investor) %>% summarise(Investment = sum(amount)) %>% arrange(desc(Investment)) %>% top_n(11) %>% top_n(-10)
    data_investment %>% hchart(type = "column",
                               hcaes(x = Investor,
                                     y = Investment),
                               name = "Investment")%>%
      hc_yAxis(title = list(text = 'Investment [US$]')) %>%
      hc_title(text = "The Top 10 Biggest Investor")
  })
  
  ########## Display the Main Title ##########
  output$sheet3main <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:60px;
                   padding-bottom : 30px;text-decoration: underline;
                   font-weight: 500; line-height: 1.1; 
                   color: #00BBBB; text-align: center;'>","The World Unicorns","</h1>"))
  })
  
  ########## Display the Title for Section 1 ##########
  output$sheet3title1 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                   padding-left : 30px;
                   font-weight: 1000; line-height: 1.1; 
                   color: #d35400; text-align: left;'>","The Distribution","</h1>"))
  })
  
  ########## Display the Title for Section 2 ##########
  output$sheet3title2 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                   font-weight: 1000; line-height: 1.1; 
                   color: #d35400; text-align: center;'>","The Unicorn Curve","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 3 ##########
  output$sheet3title3 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                   padding-left : 30px;
                   font-weight: 1000; line-height: 1.1; 
                   color: #d35400; text-align: left;'>","The Valuation Growth","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 4 ##########
  output$sheet3title4 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                   font-weight: 1000; line-height: 1.1; 
                   color: #d35400; text-align: center;'>","The Business Curve","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 5 ##########
  output$sheet3title5 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                   padding-left : 30px;
               font-weight: 1000; line-height: 1.1; 
               color: #d35400; text-align: left;'>","The Proportion","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 6 ##########
  output$sheet3title6 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                   font-weight: 1000; line-height: 1.1; 
                   color: #d35400; text-align: center;'>","The Founders","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 7 ##########
  output$sheet3title7 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                   font-weight: 1000; line-height: 1.1; 
                   color: #d35400; text-align: center;'>","The Genders Proportion","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 8 ##########
  output$sheet3title8 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                 font-weight: 1000; line-height: 1.1; 
                 color: #d35400; text-align: center;'>","The Investment Curves","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 9 ##########
  output$sheet3title9 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                 font-weight: 1000; line-height: 1.1; 
                 color: #d35400; text-align: center;'>","The Country Investment Network","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 10 ##########
  output$sheet3title10 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                 font-weight: 1000; line-height: 1.1; 
                 color: #d35400; text-align: center;'>","The Company Investment Network","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 11 ##########
  output$sheet3title11 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                 font-weight: 1000; line-height: 1.1; 
                 color: #d35400; text-align: center;'>","The Top of The Top","</br>","</h1>"))
  })
  
  ########## Display the Title for Section 12 ##########
  output$sheet3title12 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;font-size:50px;
                 font-weight: 1000; line-height: 1.1; 
                 color: #d35400; text-align: center;'>","The Quote","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 1 ##########
  output$sheet3info1 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                                   font-weight: 200; line-height: 1.4; 
                                   color: #00BBBB; text-align: center;'>","The Domination of United States and China","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 2 ##########
  output$sheet3info2 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: center;'>","</br>","</br>","</br>","</br>","Is It a Bubble?","</br>","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 3 ##########
  output$sheet3info3 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: center;'>","</br>","Started by United States and China, Followed by Other Countries","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 4 ##########
  output$sheet3info4 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: center;'>","</br>","</br>","</br>","</br>","</br>","From E-Commerce to Artificial Intelligence","</br>","</br>","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 5 ##########
  output$sheet3info5 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: center;'>","</br>","</br>","</br>","</br>","</br>","</br>","Circle of Everything","</br>","</br>","</br>","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 6 ##########
  output$sheet3info6_1 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: left;'>","</br>","Two is Better","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 6 ##########
  output$sheet3info6_2 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: right;'>","Dominated by Male","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 7 ##########
  output$sheet3info7_1 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: left;'>","</br>","</br>","Gender by Country","</br>","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 7 ##########
  output$sheet3info7_2 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
                 padding-bottom : 10px;
                 font-weight: 200; line-height: 1.4; 
                 color: #00BBBB; text-align: right;'>","Gender by Business","</br>","</br>","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 8 ##########
  output$sheet3info8_1 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
               padding-bottom : 10px;
               font-weight: 200; line-height: 1.4; 
               color: #00BBBB; text-align: left;'>","</br>","</br>","Will 2019 Follow the Trend?","</br>","</br>","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 8 ##########
  output$sheet3info8_2 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
               padding-bottom : 10px;
               font-weight: 200; line-height: 1.4; 
               color: #00BBBB; text-align: right;'>","</br>","</br>","Apps, Apps, and Apps","</br>","</br>","</h1>"))
  })
  
  ########## Display the Info for Section 11 ##########
  output$sheet3info12_1 <- renderUI({
    HTML(paste("<h1 style='font-family: fantasy;
               padding-bottom : 10px;
               font-weight: 200; line-height: 1.4; 
               color: #00BBBB; text-align: right;'>","</br>","Always deliver more than expected.","</h1>"))
  })
  
  ########## Display the Info for Section 11 ##########
  output$sheet3info12_2 <- renderUI({
    HTML(paste("<h3 style='font-family: fantasy;
               padding-bottom : 10px;
               font-weight: 200; line-height: 1.4; 
               color: red; text-align: right;'>","–Larry Page, Co-Founder, Google","</h3>"))
  })
  
  ########## Display the Hint for Section 1 ##########
  output$sheet3hint1 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
                 padding-right : 30px;
                 font-weight: 500; line-height: 1.1; 
                 color: red; text-align: right;'>","HINT : Hover and Click On the Map","</h5>"))
  })
  
  ########## Display the Hint for Section 2 ##########
  output$sheet3hint2 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
                 padding-right : 30px;
                 font-weight: 500; line-height: 1.1; 
                 color: red; text-align: right;'>","HINT : Hover on The Line","</h5>"))
  })
  
  ########## Display the Hint for Section 3 ##########
  output$sheet3hint3 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
                 padding-right : 30px;
                 font-weight: 500; line-height: 1.1; 
                 color: red; text-align: right;'>","HINT : Play to See the Growth and Hover the Circle","</h5>"))
  })
  
  ########## Display the Hint for Section 4 ##########
  output$sheet3hint4 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
                 padding-right : 30px;
                 font-weight: 500; line-height: 1.1; 
                 color: red; text-align: right;'>","HINT : Hover on The Line and Deselect the Legend for Fewer Line","</h5>"))
  })
  
  ########## Display the Hint for Section 5 ##########
  output$sheet3hint5 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
                 padding-right : 30px;
                 font-weight: 500; line-height: 1.1; 
                 color: red; text-align: right;'>","HINT : Hover and Click on The Circle","</h5>"))
  })
  
  ########## Display the Hint for Section 6 ##########
  output$sheet3hint6 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
                 padding-right : 30px;
                 font-weight: 500; line-height: 1.1; 
                 color: red; text-align: right;'>","HINT : Hover on The Pies","</h5>"))
  })
  
  ########## Display the Hint for Section 7 ##########
  output$sheet3hint7 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
                 padding-right : 30px;
                 font-weight: 500; line-height: 1.1; 
                 color: red; text-align: right;'>","HINT : Hover on The Stacked Bars","</h5>"))
  })
  
  ########## Display the Hint for Section 8 ##########
  output$sheet3hint8 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
               padding-right : 30px;
               font-weight: 500; line-height: 1.1; 
               color: red; text-align: right;'>","HINT : Hover on The Line and Deselect the Legend for Fewer Line","</h5>"))
  })
  
  ########## Display the Hint for Section 19##########
  output$sheet3hint9 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
               padding-right : 30px;
               font-weight: 500; line-height: 1.1; 
               color: red; text-align: left;'>","HINT : Filter on the Dropdown or Hover, Click or Drag on the Nodes and Links","</h5>"))
  })
  
  ########## Display the Hint for Section 10 ##########
  output$sheet3hint10 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
               padding-right : 30px;
               font-weight: 500; line-height: 1.1; 
               color: red; text-align: left;'>","HINT : Filter on the Dropdown or Hover, Click or Drag on the Nodes and Links","</h5>"))
  })
  
  ########## Display the Hint for Section 11 ##########
  output$sheet3hint11 <- renderUI({
    HTML(paste("<h5 style='font-family: fantasy;
               padding-left : 30px;
               font-weight: 500; line-height: 1.1; 
               color: red; text-align: left;'>","HINT : Hover on The Bars","</h5>"))
  })
  
  ########## Slider for Map Animation ##########
  output$unicornslider <- renderUI({
    sliderInput("year", "Year of Unicorn",min(data$Joined.Year), 
                max(data$Joined.Year),
                value = max(data$Joined.Year),
                step=1,
                animate=T)
  })
  
  ########## Radio Button for Country Investment Network ##########
  output$sheet3radio1 <- renderUI({
    prettyRadioButtons("col2","Filter Based On",
                       choices = c("Unicorn", "Investor"),
                       selected = "Unicorn")
  })
  
  ########## Radio Button for Company Investment Network ##########
  output$sheet3radio2 <- renderUI({
    prettyRadioButtons("col3",HTML('<FONT color="#00BBBB">Filter Based on'),
                       choices = c("Unicorn", "Investor"),
                       selected = "Unicorn")
  })
}


######################################################################################
######################################## UI ##########################################
######################################################################################
ui <- dashboardPagePlus(skin = "blue",
                        sidebar_fullCollapse = TRUE,
                        dashboardHeader(title = "Unicorn Analysis",
                                        tags$li(class = "dropdown", actionLink("guide", "User Guide"))
                        ),
                        
                        ###### Dashboard Sidebar ######
                        dashboardSidebar(
                          sidebarMenu(
                            ### Sidebar for Tab Unicorn ###
                            id = "tabs",
                            menuItem("Unicorn", tabName = "unicorn", icon = icon("dashboard")),
                            conditionalPanel(
                              condition = "input.tabs == 'unicorn'",
                              uiOutput("year_slider"),
                              uiOutput("country"),
                              uiOutput("business"),
                              uiOutput("company")
                            ),
                            ### Sidebar for Tab Investment ###
                            menuItem("Investment", tabName = "charts", icon = icon("bar-chart-o")),
                            conditionalPanel(
                              condition = "input.tabs == 'charts'",
                              radioButtons("col","Filter Start From",
                                           choices = c("Unicorn", "Investor"),
                                           selected = "Unicorn"),
                              conditionalPanel(
                                condition = "input.col == 'Unicorn'", 
                                pickerInput("Company2","Unicorn", 
                                            choices=as.character(unique(data_investment$Company)), 
                                            options = list('actions-box' = TRUE),
                                            multiple = T),
                                uiOutput("CompInv")
                              ),
                              conditionalPanel(
                                condition = "input.col == 'Investor'", 
                                pickerInput("Investor2","Investor", 
                                            choices=as.character(unique(data_investment$Investor)), 
                                            options = list('actions-box' = TRUE),
                                            multiple = T),
                                uiOutput("InvComp")
                              )
                              
                            ),
                            ### Sidebar for Tab Unicorn Stats ###
                            menuItem("Unicorn Stats", tabName = "unicorn_facts", icon = icon("bar-chart-o"))
                          )
                        ),
                        
                        ###### Dashboard Body ######
                        dashboardBody(
                          tabItems(
                            ###### Dashboard Body for Tab Unicorn ######
                            tabItem(tabName = "unicorn",
                                    ### Header Boxes ###
                                    fixedRow(
                                      infoBoxOutput(width = 3, "durationbox1"),
                                      infoBoxOutput(width = 3, "durationbox2"),
                                      infoBoxOutput(width = 3, "durationbox3"),
                                      infoBoxOutput(width = 3, "durationbox4")
                                    ),
                                    ### Body ###
                                    fixedRow(
                                      column(6,
                                             fluidRow(
                                               box(width = 12,
                                                   title = "Map", 
                                                   status = "primary", 
                                                   solidHeader = TRUE,
                                                   collapsible = TRUE,
                                                   leafletOutput("map")
                                               )
                                               ,
                                               box(width = 12, 
                                                   style = 'overflow-y:hidden;height:150px',
                                                   title = "Similar Unicorns", 
                                                   status = "primary", 
                                                   solidHeader = TRUE,
                                                   collapsible = TRUE,
                                                   fluidRow(
                                                     column(2, 
                                                            div(
                                                              style = 'height:100px;',
                                                              plotOutput("similarunicorn1", click=clickOpts(id="myclick1"))
                                                            ),
                                                            htmlOutput("similarunicorn1_text")
                                                     ),
                                                     column(2, 
                                                            div(
                                                              style = 'height:100px;',
                                                              plotOutput("similarunicorn2", click=clickOpts(id="myclick2")
                                                              )
                                                            ),
                                                            htmlOutput("similarunicorn2_text")
                                                     ),
                                                     column(2, 
                                                            div(
                                                              style = 'height:100px;',
                                                              plotOutput("similarunicorn3", click=clickOpts(id="myclick3")
                                                              )
                                                            ),
                                                            htmlOutput("similarunicorn3_text")
                                                     ),
                                                     column(2, 
                                                            div(
                                                              style = 'height:100px;',
                                                              plotOutput("similarunicorn4", click=clickOpts(id="myclick4")
                                                              )
                                                            ),
                                                            htmlOutput("similarunicorn4_text")
                                                     ),
                                                     column(2, 
                                                            div(
                                                              style = 'height:100px;',
                                                              plotOutput("similarunicorn5", click=clickOpts(id="myclick5")
                                                              )
                                                            ),
                                                            htmlOutput("similarunicorn5_text")
                                                     ),
                                                     column(2, 
                                                            div(
                                                              style = 'height:100px;',
                                                              plotOutput("similarunicorn6", click=clickOpts(id="myclick6")
                                                              )
                                                            ),
                                                            htmlOutput("similarunicorn6_text")
                                                     )
                                                   )
                                               )
                                             )
                                      ),
                                      column(6,
                                             fluidRow(
                                               box(width = 12,
                                                   title = "Twitter Timeline", 
                                                   status = "primary", 
                                                   solidHeader = TRUE,
                                                   collapsible = TRUE,
                                                   tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                                                   div(
                                                     style = 'overflow-y:scroll;height:280px;',
                                                     uiOutput("mytimeline")
                                                   )
                                               ),
                                               box(width = 12,
                                                   title = "Details", 
                                                   status = "primary", 
                                                   solidHeader = TRUE,
                                                   collapsible = TRUE,
                                                   div(
                                                     style = 'overflow-y:scroll;height:250px;',
                                                     uiOutput("details")
                                                   )
                                               )
                                               
                                             )
                                      )
                                      
                                    )
                            ),
                            ###### Dashboard Body for Tab Investment ######
                            tabItem(tabName = "charts",
                                    ### Header Boxes ###
                                    fixedRow(
                                      infoBoxOutput(width = 3, "durationbox12"),
                                      infoBoxOutput(width = 3, "durationbox22"),
                                      infoBoxOutput(width = 3, "durationbox32"),
                                      infoBoxOutput(width = 3, "durationbox42")
                                    ),
                                    ### Body ###
                                    fixedRow(
                                      column(6,
                                             fluidRow(
                                               box(width = 12,
                                                   title = "Investment Network", 
                                                   status = "primary", 
                                                   solidHeader = TRUE,
                                                   collapsible = TRUE,
                                                   visNetworkOutput("network")
                                               ),
                                               box(width = 12,
                                                   title = "Investment Details", 
                                                   status = "primary", 
                                                   solidHeader = TRUE,
                                                   collapsible = TRUE,
                                                   DT::dataTableOutput("transport_table")
                                               )
                                             )),
                                      column(6,
                                             useShinyjs(),
                                             fluidRow(id = 'option1',
                                                      box(width = 12,
                                                          title = "Investment Destination", 
                                                          status = "primary", 
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          collapsibleTreeOutput("collapsibleTree")
                                                      ),
                                                      box(width = 12,
                                                          title = "Business Category", 
                                                          status = "primary", 
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          highchartOutput("stackchart1")
                                                      )
                                             ),
                                             fluidRow(id = 'option2',
                                                      box(width = 12,
                                                          title = "Investment Source", 
                                                          status = "primary", 
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          highchartOutput("piechart")
                                                      ),
                                                      box(width = 12,
                                                          title = "Investors", 
                                                          status = "primary", 
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          highchartOutput("stackchart2")
                                                      )
                                             )
                                      )
                                      
                                      
                                    )
                            ),
                            ###### Dashboard Body for Tab Unicorn Stats ######
                            tabItem(tabName = "unicorn_facts",
                                    fixedRow(
                                      htmlOutput("sheet3main")
                                    ),
                                    #### Section 1 ####
                                    fixedRow(
                                      htmlOutput("sheet3title1")
                                    ),
                                    fixedRow(
                                      column(6,
                                             highchartOutput("worldmap", height = 600)
                                      ),
                                      column(6,
                                             useShinyjs(),
                                             fluidRow(id = 'barcharttext',
                                                      uiOutput("barcharttext")),
                                             fluidRow(id = 'barchartviz',
                                               highchartOutput("barchart", height = 400)
                                             ),
                                             fluidRow(
                                               htmlOutput("sheet3info1"),
                                               htmlOutput("sheet3hint1")
                                             )
                                             
                                      )
                                    ),
                                    #### Section 2 ####
                                    fluidRow(style = "background-color:#ffffff;",
                                             column(8,
                                                    htmlOutput("sheet3title2"),
                                                    highchartOutput("spline2")
                                             ),
                                             column(4,
                                                    htmlOutput("sheet3info2"),
                                                    htmlOutput("sheet3hint2")
                                             )
                                    ),
                                    #### Section 3 ####
                                    fluidRow(style = "background-color:#AAD3DF;",
                                             fixedRow(
                                               htmlOutput("sheet3title3")
                                             ),
                                             fixedRow(
                                               column(8,
                                                      leafletOutput("mapanimation")
                                               ),
                                               column(4,
                                                      uiOutput("unicornslider"),
                                                      htmlOutput("sheet3info3"),
                                                      htmlOutput("sheet3hint3")
                                               )
                                             )
                                    ),
                                    #### Section 4 ####
                                    fluidRow(style = "background-color:#ffffff",
                                             column(8,
                                                    htmlOutput("sheet3title4"),
                                                    highchartOutput("linechart", height = 600)
                                             ),
                                             column(4,
                                                    htmlOutput("sheet3info4"),
                                                    htmlOutput("sheet3hint4")
                                             )
                                    ),
                                    #### Section 5 ####
                                    fluidRow(style = "background-color:#AAD3DF;",
                                             column(6,
                                                    htmlOutput("sheet3title5"), 
                                                    sund2bOutput("sun", height = 600)
                                             ),
                                             column(6,
                                                    htmlOutput("sheet3info5"),
                                                    htmlOutput("sheet3hint5")
                                             )
                                    ),
                                    #### Section 6 ####
                                    fluidRow(style = "background-color:#ffffff",
                                             column(12,
                                                    htmlOutput("sheet3title6")
                                             )
                                    ),
                                    fluidRow(style = "background-color:#ffffff;",
                                             column(4,
                                                    highchartOutput("founder1")
                                             ),
                                             column(4,
                                                    htmlOutput("sheet3info6_1"),
                                                    htmlOutput("sheet3info6_2"),
                                                    htmlOutput("sheet3hint6")
                                             ),
                                             column(4,
                                                    highchartOutput("founder2")
                                             )
                                    ),
                                    #### Section 7 ####
                                    fluidRow(style = "background-color:#AAD3DF",
                                             column(12,
                                                    htmlOutput("sheet3title7")
                                             )
                                    ),
                                    fluidRow(style = "background-color:#AAD3DF;",
                                             column(4,
                                                    highchartOutput("founder4", height = 700)
                                             ),
                                             column(4,
                                                    htmlOutput("sheet3info7_1"),
                                                    htmlOutput("sheet3info7_2"),
                                                    htmlOutput("sheet3hint7")
                                             ),
                                             column(4,
                                                    highchartOutput("founder3", height = 700)
                                             )
                                    ),
                                    #### Section 8 ####
                                    fluidRow(style = "background-color:#ffffff",
                                             column(12,
                                                    htmlOutput("sheet3title8")
                                             )
                                    ),
                                    fluidRow(style = "background-color:#ffffff;",
                                             column(8,
                                                    highchartOutput("lineinvestment1")
                                             ),
                                             column(4,
                                                    htmlOutput("sheet3info8_1")
                                             )
                                    ),
                                    fluidRow(style = "background-color:#ffffff;",
                                             column(2,
                                                    htmlOutput("sheet3info8_2"),
                                                    htmlOutput("sheet3hint8")
                                             ),
                                             column(10,
                                                    highchartOutput("lineinvestment2")
                                             )
                                    ),
                                    #### Section 9 ####
                                    fluidRow(style = "background-color:#AAD3DF",
                                             column(12,
                                                    htmlOutput("sheet3title9")
                                             )
                                    ),
                                    fluidRow(style = "background-color:#AAD3DF;",
                                             column(4,
                                                    uiOutput("sheet3radio1"),
                                                    conditionalPanel(
                                                      condition = "input.col2 == 'Unicorn'", 
                                                      pickerInput("Company3","Country", 
                                                                  choices=as.character(unique(data_investment$Country[data_investment$Investor.Location!="Not Published"])), 
                                                                  # selected = as.character(unique(data_investment$Country[data_investment$Investor.Location!="Not Published"])),
                                                                  options = list('actions-box' = TRUE),
                                                                  multiple = T)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.col2 == 'Investor'", 
                                                      pickerInput("Investor3","Investor.Location", 
                                                                  choices=as.character(unique(data_investment$Investor.Location[data_investment$Investor.Location!="Not Published"])), 
                                                                  # selected = as.character(unique(data_investment$Investor.Location[data_investment$Investor.Location!="Not Published"])),
                                                                  options = list('actions-box' = TRUE),
                                                                  multiple = T)
                                                    ),
                                                    htmlOutput("sheet3hint9")
                                             ),
                                             column(8,
                                                    visNetworkOutput("countrynetwork", height = 800)
                                             )),
                                    
                                    #### Section 10 ####
                                    fluidRow(style = "background-color:#ffffff",
                                             column(12,
                                                    htmlOutput("sheet3title10")
                                                    
                                             )
                                    ),
                                    fluidRow(style = "background-color:#ffffff;",
                                             column(4,
                                                    uiOutput("sheet3radio2"),
                                                    conditionalPanel(
                                                      condition = "input.col3 == 'Unicorn'", 
                                                      pickerInput("Company4","Country", 
                                                                  choices=as.character(unique(data_investment$Company[data_investment$Investor!="Not Published"])), 
                                                                  # selected = "Bukalapak", 
                                                                  options = list('actions-box' = TRUE),
                                                                  multiple = T)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.col3 == 'Investor'", 
                                                      pickerInput("Investor4","Investor", 
                                                                  choices=as.character(unique(data_investment$Investor[data_investment$Investor!="Not Published"])), 
                                                                  # selected = "Temasek Holdings", 
                                                                  options = list('actions-box' = TRUE),
                                                                  multiple = T)
                                                    ),
                                                    htmlOutput("sheet3hint10")
                                             ),
                                             column(8,
                                                    visNetworkOutput("companynetwork", height = 800)
                                             )),
                                    #### Section 11 ####
                                    fluidRow(style = "background-color:#AAD3DF;",htmlOutput("sheet3title11")),
                                    fluidRow(style = "background-color:#AAD3DF;",
                                      column(3, highchartOutput("top1")),
                                      column(3, highchartOutput("top2")),
                                      column(3, highchartOutput("top3")),
                                      column(3, highchartOutput("top4"))
                                    ),
                                    fluidRow(style = "background-color:#AAD3DF;",
                                             htmlOutput("sheet3hint11")),
                                    #### Section 12 ####
                                    fluidRow(
                                      htmlOutput("sheet3title12"),
                                      htmlOutput("sheet3info12_1"),
                                      htmlOutput("sheet3info12_2")
                                    )
                            )
                          )
                        ),
                        ###### Dashboard Footer ######
                        dashboardFooter(
                          left_text = actionButton("twitter_share",
                                                   label = "Robert", 
                                                   icon = icon("linkedin"), 
                                                   style = 'padding:4px; font-size:80%', 
                                                   onclick = sprintf("window.open('%s')", "https://www.linkedin.com/in/robert1995/")),
                          right_text = "Monash University, Melbourne, 2019"
                        )
)
############ Run App ##################
shinyApp(ui, server)


