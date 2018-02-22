#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(ggthemes)
library(rlang)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(dplyr)

#setwd("/users/ben/modelPredictions/wbGrowth/")

#load('P_ForMike_V2.RData')
#load('pForMike_V1.RData')

load('westBrookPredictions.RData')
load('westBrookPred2.RData') 

 p2 <- p2 %>% rename(predGr = predGrowTH , Species = Speies) %>% mutate(age = ifelse(isYOY == '0' , 'Young of the Year' , 'Adult'))


pFinal$riverN<-factor(pFinal$riverN, levels=c('West Brook' , 'Open Small' , 'Open Large' , 'Isolated Small'))
pFinal$speciesName <- factor(pFinal$speciesName , levels = c("Brook Trout" , "Brown Trout" , "Atlantic Salmon"))
pFinal$seasonN <- factor(pFinal$seasonN , levels = c("Spring" , "Summer" , "Fall" , "Winter"))
pFinal$ageN <- factor(pFinal$ageN , levels = c("Young of the Year" , "Adult"))


p2$River<-factor(p2$River, levels=c('West Brook' , 'Open Small' , 'Open Large' , 'Isolated Small'))
p2$Species <- factor(p2$Species , levels = c("Brook Trout" , "Brown Trout" , "Atlantic Salmon"))
p2$Season <- factor( p2$Season , levels = c("Spring" , "Summer" , "Fall" , "Winter"))
p2$age <- factor( p2$age , levels = c("Young of the Year" , "Adult"))



shapes <- c(21 ,23 , 24 , 22)


#p <- read.csv("westBrookPredictionsSpeciesBKTandBNT.csv" , header = TRUE) 

theme_Publication <- function(base_size=14, base_family="Times") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size= unit(1 , "cm"),
            legend.text = element_text(size = 12),
            legend.margin = unit(0.2, "cm"),
            legend.title = element_text(face="bold" , size = 14),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


#p <- p %>% filter(iter < 3000)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  p3 <- reactive({ p2 %>% mutate(dataTypee = ifelse(dataType == 'predGr' , 'Mean Growth mm/d', 
                                             ifelse(dataType == 'predGrSigma' , 'Standard Deviation' ,
                                             ifelse(dataType == 'predCV' , 'CV' , 'Hi')))) %>%
                          filter(dataTypee == input$dataType1 ) 
                          
      })
  
  p <- reactive({ pFinal %>%  mutate(dataTypee = ifelse(dataType == 'predGr' , 'Mean Growth mm/d', 
                                                           ifelse(dataType == 'predGrSigma' , 'Standard Deviation' ,
                                                                  ifelse(dataType == 'predCV' , 'CV' , 'Hi')))) %>%
                   filter(dataTypee == input$dataType2 ) %>%
      rename(predGr = predGrowTH)  })  
  
  
  
  
  #### Comparison Code      
  
  #plotPred <- function(d, varsToPlot, isYOYGG, speciesGG ) {
  #all = c('temp','flow','count' )
  #notPlot <- NA
  #notPlot[1] <- all[!(all %in% varsToPlot)][1]
  #notPlot[2] <- all[!(all %in% varsToPlot)][2]
  #d %>% filter(isYOY == isYOYGG, eval(as.name(notPlot[1])) == 0, eval(as.name(notPlot[2])) == 0 ) %>%
  #distinct(eval(as.name(varsToPlot[1])), iter, isYOY, river, season, .keep_all = TRUE) }
  
  # flowTwo$streamName<-factor(flowTwo$streamName, levels=c('West Brook' , 'Open Small' , 'Open Large' , 'Isolated Small'))     
  # countTwo$streamName<-factor(countTwo$streamName, levels=c('West Brook' , 'Open Small' , 'Open Large' , 'Isolated Small'))     
  # lengthTwo$streamName<-factor(lengthTwo$streamName, levels=c('West Brook' , 'Open Small' , 'Open Large' , 'Isolated Small'))     
  
  
  
  data <- reactive({ p3() %>% filter(river %in% input$riverA , 
                                     season %in% input$seasoNA , 
                                     #isYOY %in% input$ageA , 
                                     Species %in% input$specieSA , 
                                     iter < input$slider2A) })
  
  
 
  output$plot1A <- renderPlot({  
    
    iterdataT <- input$dataType1
    
    
    data() %>% filter( type == 'temp' ) %>% 
          ggplot( aes(y = predGr , x = temp , shape = River)  ) +
          geom_line(aes(group = interaction(iter , Season , River , Species , isYOY) , colour = Species)  , alpha = 0.05 ) +
          geom_line(aes( y = predGrMean , x = temp , linetype = Season ,  group = interaction( Species , Season  , River, isYOY) , colour = Species) , size = 0.5) + 
          geom_point( aes( y = predGrMean , x = temp , group = interaction(Season , Species , River, isYOY)  , shape = River , fill = Species )  , size = 4 ) + 
          scale_shape_manual(values= shapes) +
          scale_colour_Publication()+ scale_fill_Publication() + 
          theme_Publication() + ylim(-0.3 , 1.1) + 
          guides(fill = FALSE) +
         #theme(legend.key = element_rect(colour = "black")) +
          labs(y = iterdataT , x = "Temp") + facet_grid(.~age)})
  
  output$plot2A <- renderPlot({  
    
    iterdataT <- input$dataType1
    
    
    data() %>% filter( type == 'flow' ) %>% 
      ggplot( aes(y = predGr , x = flow , shape = River)  ) +
      geom_line(aes(group = interaction(iter , Season , River , Species , isYOY) , colour = Species)  , alpha = 0.05 ) +
      geom_line(aes( y = predGrMean , x = flow , linetype = Season ,  group = interaction( Species , Season  , River, isYOY) , colour = Species) , size = 0.5) + 
      geom_point( aes( y = predGrMean , x = flow , group = interaction(Season , Species , River, isYOY)  , shape = River , fill = Species )  , size = 4 ) + 
      scale_shape_manual(values= shapes) +
      scale_colour_Publication()+ scale_fill_Publication() + 
      theme_Publication() + ylim(-0.3 , 1.1) + 
      guides(fill = FALSE) +
      #theme(legend.key = element_rect(colour = "black")) +
      labs(y = iterdataT , x = "Stream Flow")+ facet_grid(.~age)})
  
  output$plot3A <- renderPlot({  
    
    iterdataT <- input$dataType1
    
    
    data() %>% filter( type == 'count' ) %>% 
      ggplot( aes(y = predGr , x = count , shape = River)  ) +
      geom_line(aes(group = interaction(iter , Season , River , Species , isYOY) , colour = Species)  , alpha = 0.05 ) +
      geom_line(aes( y = predGrMean , x = count , linetype = Season ,  group = interaction( Species , Season  , River, isYOY) , colour = Species) , size = 0.5) + 
      geom_point( aes( y = predGrMean , x = count , group = interaction(Season , Species , River, isYOY)  , shape = River , fill = Species )  , size = 4 ) + 
      scale_shape_manual(values= shapes) +
      scale_colour_Publication()+ scale_fill_Publication() + 
      theme_Publication() + ylim(-0.3 , 1.1) + 
      guides(fill = FALSE) +
      #theme(legend.key = element_rect(colour = "black")) +
      labs(y = iterdataT , x = "Estimated Abundance")+ facet_grid(.~age)})
  
  output$plot4A <- renderPlot({  
    
    iterdataT <- input$dataType1
    
    
    data() %>% filter( type == 'length' ) %>% 
      ggplot( aes(y = predGr , x = len , shape = River)  ) +
      geom_line(aes(group = interaction(iter , Season , River , Species , isYOY) , colour = Species)  , alpha = 0.05 ) +
      geom_line(aes( y = predGrMean , x = len , linetype = Season ,  group = interaction( Species , Season  , River, isYOY) , colour = Species) , size = 0.5) + 
      geom_point( aes( y = predGrMean , x = len , group = interaction(Season , Species , River, isYOY)  , shape = River , fill = Species )  , size = 4 ) + 
      scale_shape_manual(values= shapes) +
      scale_colour_Publication()+ scale_fill_Publication() + 
      theme_Publication() + ylim(-0.3 , 1.1) + 
      guides(fill = FALSE) +
      #theme(legend.key = element_rect(colour = "black")) +
      labs(y = iterdataT , x = "Length")+ facet_grid(.~age)})
  
  
  #### Comparison Code        
  
  input_axis <- reactive({ sym(input$aXis) })
  #p2 <-  p %>% filter(len == '0')
  d1 <- reactive({ p() %>% select(count , flow , temp , len , iter , isYOY , species , speciesName ,  season , seasonN , river , predGr , meanPrGrowth , riverN , ageN ) %>% 
      mutate(speciesName = as.character(speciesName)) %>%
      filter(iter < input$slider2B)})
  
  
  mainFilter <- reactive({ 
    
    
    
    
    ### Age      
    if( input$facet1 == "Age" & input$facet2 == "Season"){ 
      
      e <- d1()%>% filter(isYOY %in% input$ageAgeSeason ,
                          species %in% input$speciesAgeSeason ,
                          river %in% input$riverAgeSeason, 
                          season %in% input$seasonAgeSeason) } 
    
    if( input$facet1 == "Age" & input$facet2 == "River"){
      
      e <- d1()%>% filter(isYOY %in% input$ageAgeRiver ,
                          species %in% input$speciesAgeRiver ,
                          river %in% input$riverAgeRiver , 
                          season %in% input$seasonAgeRiver) } 
    
    if( input$facet1 == "Age" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(isYOY %in% input$ageAgeSpecies ,
                          species %in% input$speciesAgeSpecies ,
                          river %in% input$riverAgeSpecies , 
                          season %in% input$seasonAgeSpecies) } 
    
    
    ### River 
    if( input$facet1 == "River" & input$facet2 == "Season"){ 
      
      e <- d1()%>% filter(isYOY %in% input$ageRiverSeason ,
                          species %in% input$speciesRiverSeason ,
                          river %in% input$riverRiverSeason, 
                          season %in% input$seasonRiverSeason) } 
    
    if( input$facet1 == "River" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(isYOY %in% input$ageRiverAge ,
                          species %in% input$speciesRiverAge ,
                          river %in% input$riverRiverAge , 
                          season %in% input$seasonRiverAge) } 
    
    if( input$facet1 == "River" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(isYOY %in% input$ageRiverSpecies ,
                          species %in% input$speciesRiverSpecies ,
                          river %in% input$riverRiverSpecies , 
                          season %in% input$seasonRiverSpecies) } 
    
    
    
    ### Season 
    if( input$facet1 == "Season" & input$facet2 == "River"){ 
      
      e <- d1()%>% filter(isYOY %in% input$ageSeasonRiver ,
                          species %in% input$speciesSeasonRiver ,
                          river %in% input$riverSeasonRiver, 
                          season %in% input$seasonSeasonRiver) } 
    
    if( input$facet1 == "Season" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(isYOY %in% input$ageSeasonAge ,
                          species %in% input$speciesSeasonAge ,
                          river %in% input$riverSeasonAge , 
                          season %in% input$seasonSeasonAge) } 
    
    if( input$facet1 == "Season" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(isYOY %in% input$ageSeasonSpecies ,
                          species %in% input$speciesSeasonSpecies ,
                          river %in% input$riverSeasonSpecies , 
                          season %in% input$seasonSeasonSpecies) } 
    
    ### Species 
    if( input$facet1 == "Species" & input$facet2 == "River"){ 
      
      e <- d1()%>% filter(isYOY %in% input$ageSpeciesRiver ,
                          species %in% input$speciesSpeciesRiver ,
                          river %in% input$riverSpeciesRiver, 
                          season %in% input$seasonSpeciesRiver) } 
    
    if( input$facet1 == "Species" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(isYOY %in% input$ageSpeciesAge ,
                          species %in% input$speciesSpeciesAge ,
                          river %in% input$riverSpeciesAge , 
                          season %in% input$seasonSpeciesAge) } 
    
    if( input$facet1 == "Species" & input$facet2 == "Season"){
      
      e <- d1()%>% filter(isYOY %in% input$ageSpeciesSeason ,
                          species %in% input$speciesSpeciesSeason  ,
                          river %in% input$riverSpeciesSeason  , 
                          season %in% input$seasonSpeciesSeason ) } 
    
    
    ### NoneOne
    if( input$facet1 == "None" & input$facet2 == "River"){ 
      
      e <- d1()%>% filter(isYOY %in% input$ageNoneRiver ,
                          species %in% input$speciesNoneRiver ,
                          river %in% input$riverNoneRiver, 
                          season %in% input$seasonNoneRiver) } 
    
    if( input$facet1 == "None" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(isYOY %in% input$ageNoneAge ,
                          species %in% input$speciesNoneAge ,
                          river %in% input$riverNoneAge , 
                          season %in% input$seasonNoneAge) } 
    
    if( input$facet1 == "None" & input$facet2 == "Season"){
      
      e <- d1()%>% filter(isYOY %in% input$ageNoneSeason ,
                          species %in% input$speciesNoneSeason  ,
                          river %in% input$riverNoneSeason  , 
                          season %in% input$seasonNoneSeason ) } 
    
    if( input$facet1 == "None" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(isYOY %in% input$ageNoneSpecies ,
                          species %in% input$speciesNoneSpecies  ,
                          river %in% input$riverNoneSpecies  , 
                          season %in% input$seasonNoneSpecies ) } 
    
    
    ### NoneTwo
    if( input$facet1 == "River" & input$facet2 == "None"){ 
      
      e <- d1()%>% filter(isYOY %in% input$ageRiverNone ,
                          species %in% input$speciesRiverNone ,
                          river %in% input$riverRiverNone, 
                          season %in% input$seasonRiverNone) } 
    
    if( input$facet1 == "Age" & input$facet2 == "None"){
      
      e <- d1()%>% filter(isYOY %in% input$ageAgeNone ,
                          species %in% input$speciesAgeNone ,
                          river %in% input$riverAgeNone , 
                          season %in% input$seasonAgeNone) } 
    
    if( input$facet1 == "Season" & input$facet2 == "None"){
      
      e <- d1()%>% filter(isYOY %in% input$ageSeasonNone ,
                          species %in% input$speciesSeasonNone  ,
                          river %in% input$riverSeasonNone  , 
                          season %in% input$seasonSeasonNone ) } 
    
    if( input$facet1 == "Species" & input$facet2 == "None"){
      
      e <- d1()%>% filter(isYOY %in% input$ageSpeciesNone ,
                          species %in% input$speciesSpeciesNone  ,
                          river %in% input$riverSpeciesNone  , 
                          season %in% input$seasonSpeciesNone ) } 
    
    if( input$facet1 == "None" & input$facet2 == "None"){
      
      e <- d1()%>% filter(isYOY %in% input$ageNoneNone ,
                          species %in% input$speciesNoneNone  ,
                          river %in% input$riverNoneNone  , 
                          season %in% input$seasonNoneNone ) } 
    
    
    e
    
    # iter < input$slider2B) }   
  })
  
  
  
  
  
  tempLengthCount <- reactive({mainFilter() %>% filter(len == input$changeInTempLengthOne ,
                                                       count == input$changeInTempCountOne )})
  
  tempLengthFlow <- reactive({mainFilter() %>% filter(len == input$changeInTempLengthTwo ,
                                                      flow == input$changeInTempFlowOne )})
  
  tempCountFlow <- reactive({mainFilter() %>% filter(count == input$changeInTempCountTwo ,
                                                     flow == input$changeInTempFlowTwo )})
  
  flowLengthTemp <- reactive({mainFilter() %>% filter(len == input$changeInFlowLengthOne ,
                                                      temp == input$changeInFlowTempOne )})
  
  flowLengthCount <- reactive({mainFilter() %>% filter(len == input$changeInFlowLengthTwo ,
                                                       count == input$changeInFlowCountOne )})
  
  flowTempCount <- reactive({mainFilter() %>% filter(temp == input$changeInFlowTempTwo ,
                                                     count == input$changeInFlowCountTwo )})
  
  countLengthTemp <- reactive({mainFilter() %>% filter(len == input$changeInCountLengthOne ,
                                                       temp == input$changeInCountTempOne )})
  
  countLengthFlow <- reactive({mainFilter() %>% filter(len == input$changeInCountLengthTwo ,
                                                       flow == input$changeInCountFlowOne )})
  
  countTempFlow <- reactive({mainFilter() %>% filter(temp == input$changeInCountTempTwo ,
                                                     flow == input$changeInCountFlowTwo )})
  
  lengthCountTemp <- reactive({mainFilter() %>% filter(count == input$changeInLengthCountOne ,
                                                       temp == input$changeInLengthTempOne )})
  
  lengthCountFlow <- reactive({mainFilter() %>% filter(count == input$changeInLengthCountTwo ,
                                                       flow == input$changeInLengthFlowOne )})
  
  lengthTempFlow  <- reactive({mainFilter() %>% filter(temp == input$changeInLengthTempTwo ,
                                                       flow == input$changeInLengthFlowTwo )})
  
  
  
  
  
  
  
  d5 <- reactive({ d1() %>% filter(predGr > 100)})
  
  #d4 <-  reactive({ d1()%>% filter(isYOY == 0 , 
  #                                flow == 0, 
  #                                count == 0,
  #                                river %in% input$riverB, 
  #                                season == 1) %>%
  #                         mutate(type = "normal")})
  
  #d3 <- reactive({ bind_rows( d2() , d4()) })
  # iter < input$slider2B) })
  
  #c <- reactive({p %>% select(count , flow , temp , iter , isYOY , season , river , predGr) %>%
  #    filter(flow == input$changeInFlow, 
  #           count == input$changeInCount  , 
  #           isYOY == 0 , 
  #           river == "west brook" , 
  #           season %in% input$seasoN ,
  #           iter < input$slider2) })
  
  # vis1 <- reactive({ 
  #    x_axis <- sym(input$aXis)
  #    x <- d2() %>% ggvis(x= ~sym(input$aXis) , y= ~predGr) %>% 
  #     group_by(iter, river , season , flow) %>%
  #    layer_lines( stroke =~ flow ) %>%
  #    set_options( width = 800, height = 800) })
  #layer_lines(data = d2() , x =~temp , y =~predGr)})
  #  vis1 %>% bind_shiny("plot1B")
  
  output$plot1B <- renderPlot({  
    x_axis <- input$aXis
    iterdataT <- input$dataType2
    #facet_1 <- sym( input$facet1 )
    #facet_2 <- sym( input$facet2 )
    #s_axis <- input$
    iterColor <- input$color 
    gg <- d5() %>% ggplot(aes_string( x = x_axis , y = "predGr" ))  
    
    if( input$aXis == "temp" & input$color == "flow"){
      gg <- gg + geom_line(data = tempLengthCount() , aes( group=interaction(  iter , river , season , flow, species , isYOY  ) , colour = flow) , alpha = 0.3) +
        geom_line(data = tempLengthCount() , aes( y = meanPrGrowth , x = temp , group = flow ) , size = 1) +
        geom_point(data = tempLengthCount() , aes( y = meanPrGrowth , x = temp , group = flow  , fill = flow), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Temperature" , y = iterdataT , colour = "Stream Flow") +
        guides(fill = FALSE)}
    
    if(input$aXis == "temp" & input$color == "count" ){
      gg <- gg + geom_line(data = tempLengthFlow() , aes( group=interaction(  iter , river , season , count, species , isYOY ) , colour = count), alpha = 0.3) +
        geom_line(data = tempLengthFlow() , aes( y = meanPrGrowth , x = temp , group = count ) , size = 1) +
        geom_point(data = tempLengthFlow() , aes( y = meanPrGrowth , x = temp , group = count  , fill = count), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Temperature" , y = iterdataT , colour = "Estimated Abundance") +
        guides(fill = FALSE)} 
    
    if(input$aXis == "temp" & input$color == "len" ){
      gg <- gg + geom_line(data = tempCountFlow() , aes( group=interaction(  iter , river , season ,species , isYOY , len) , colour = len), alpha = 0.3) +
        geom_line(data = tempCountFlow() , aes( y = meanPrGrowth , x = temp , group = len ) , size = 1) +
        geom_point(data = tempCountFlow() , aes( y = meanPrGrowth , x = temp , group = len  , fill = len), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Temperature" , y = iterdataT , colour = "Fish Length") +
        guides(fill = FALSE) } 
    
    
    if( input$aXis == "flow" & input$color == "count"){
      gg <- gg + geom_line(data = flowLengthTemp() , aes( group=interaction(   iter , river , season ,species , isYOY , count) , colour = count), alpha = 0.3) +
        geom_line(data = flowLengthTemp() , aes( y = meanPrGrowth , x = flow , group = count ) , size = 1) +
        geom_point(data = flowLengthTemp() , aes( y = meanPrGrowth , x = flow , group = count  , fill = count), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Flow" , y = iterdataT , colour = "Estimated Abundance")+
        guides(fill = FALSE) }
    
    if(input$aXis == "flow" & input$color == "temp" ){
      gg <- gg + geom_line(data = flowLengthCount() , aes( group=interaction(  iter , river , season , species , isYOY , temp) , colour = temp), alpha = 0.3) +
        geom_line(data = flowLengthCount() , aes( y = meanPrGrowth , x = flow , group = temp ) , size = 1) +
        geom_point(data = flowLengthCount() , aes( y = meanPrGrowth , x = flow , group = temp  , fill = temp), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Flow" , y = iterdataT , colour = "Stream Temperature")+
        guides(fill = FALSE) }
    
    if(input$aXis == "flow" & input$color == "len" ){
      gg <- gg + geom_line(data = flowTempCount() , aes( group=interaction(  iter , river , season ,  species , isYOY , len) , colour = len), alpha = 0.3) +
        geom_line(data = flowTempCount() , aes( y = meanPrGrowth , x = flow , group = len ) , size = 1) +
        geom_point(data = flowTempCount() , aes( y = meanPrGrowth , x = flow , group = len  , fill = len), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Flow" , y = iterdataT , colour = "Fish Length")+
        guides(fill = FALSE) }
    
    
    if( input$aXis == "count" & input$color == "flow"){
      gg <- gg + geom_line(data = countLengthTemp() , aes( group=interaction( iter , river , season , flow, species , isYOY ) , colour = flow), alpha = 0.3) +
        geom_line(data = countLengthTemp() , aes( y = meanPrGrowth , x = count , group = flow ) , size = 1) +
        geom_point(data = countLengthTemp() , aes( y = meanPrGrowth , x = count , group = flow  , fill = flow), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Estimated Abundance" , y = iterdataT , colour = "Stream Flow")+
        guides(fill = FALSE) }
    
    if(input$aXis == "count" & input$color == "temp" ){
      gg <- gg + geom_line(data = countLengthFlow() , aes( group=interaction(   iter , river , season , temp, species , isYOY ) , colour = temp), alpha = 0.3) +
        geom_line(data = countLengthFlow() , aes( y = meanPrGrowth , x = count , group = temp ) , size = 1) +
        geom_point(data = countLengthFlow() , aes( y = meanPrGrowth , x = count , group = temp  , fill = temp), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Estimated Abundance" , y = iterdataT , colour = "Stream Temperature")+
        guides(fill = FALSE) }
    
    if(input$aXis == "count" & input$color == "len" ){
      gg <- gg + geom_line(data = countTempFlow() , aes( group=interaction(  iter , river , season , species , isYOY , len) , colour = len), alpha = 0.3) +
        geom_line(data = countTempFlow() , aes( y = meanPrGrowth , x = count , group = len ) , size = 1) +
        geom_point(data = countTempFlow() , aes( y = meanPrGrowth , x = count , group = len  , fill = len), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Estimated Abundance" , y = iterdataT , colour = "Fish Length")+
        guides(fill = FALSE) }
    
    
    if( input$aXis == "len" & input$color == "flow"){
      gg <- gg + geom_line(data = lengthCountTemp() , aes( group=interaction( iter , river , season , flow, species , isYOY ) , colour = flow), alpha = 0.3) +
        geom_line(data = lengthCountTemp() , aes( y = meanPrGrowth , x = len , group = flow ) , size = 1) +
        geom_point(data = lengthCountTemp() , aes( y = meanPrGrowth , x = len , group = flow  , fill = flow), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Fish Length" , y = iterdataT , colour = "Stream Flow")+
        guides(fill = FALSE) }
    
    if(input$aXis == "len" & input$color == "temp" ){
      gg <- gg + geom_line(data = lengthCountFlow() , aes( group=interaction(  iter , river , season ,  species , isYOY , temp) , colour = temp), alpha = 0.3) +
        geom_line(data = lengthCountFlow() , aes( y = meanPrGrowth , x = len , group = temp ) , size = 1) +
        geom_point(data = lengthCountFlow() , aes( y = meanPrGrowth , x = len , group = temp  , fill = temp), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Fish Length" , y = iterdataT , colour = "Stream Temperature")+
        guides(fill = FALSE) }
    
    if(input$aXis == "len" & input$color == "count" ){
      gg <- gg + geom_line(data = lengthTempFlow() , aes( group=interaction(  iter , river , season , count , species , isYOY) , colour = count), alpha = 0.3) +
        geom_line(data = lengthTempFlow() , aes( y = meanPrGrowth , x = len , group = count ) , size = 1) +
        geom_point(data = lengthTempFlow() , aes( y = meanPrGrowth , x = len , group = count  , fill = count), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Fish Length" , y = iterdataT , colour = "Estimated Abundance")+
        guides(fill = FALSE) }
    
    
    
    
    gg <- gg  + 
      #ylim(-0.3 , 1.5) +
      xlim(-1.5 , 1.5) 
    theme(
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) + theme_bw()  
    
    
    
    if(input$facet1 == "Age" & input$facet2 == "River"){
      gg <- gg + facet_grid(riverN~ageN)
    }
    
    if(input$facet1 == "Age" & input$facet2 == "Season"){
      gg <- gg + facet_grid(seasonN~ageN)
    }
    
    if(input$facet1 == "Age" & input$facet2 == "Species"){
      gg <- gg + facet_grid(speciesName~ageN)
    }
    
    if(input$facet1 == "River" & input$facet2 == "Age"){
      gg <- gg + facet_grid(ageN~riverN)
    }
    
    if(input$facet1 == "River" & input$facet2 == "Season"){
      gg <- gg + facet_grid(seasonN.x~riverN)
    }
    
    if(input$facet1 == "River" & input$facet2 == "Species"){
      gg <- gg + facet_grid(speciesName~riverN)
    }
    
    
    if(input$facet1 == "Season" & input$facet2 == "Age"){
      gg <- gg + facet_grid(ageN~seasonN)
    }
    
    if(input$facet1 == "Season" & input$facet2 == "River"){
      gg <- gg + facet_grid(riverN~seasonN)
    }
    
    if(input$facet1 == "Season" & input$facet2 == "Species"){
      gg <- gg + facet_grid(speciesName~seasonN)
    }
    
    if(input$facet1 == "Species" & input$facet2 == "Age"){
      gg <- gg + facet_grid(ageN~speciesName)
    }
    
    if(input$facet1 == "Species" & input$facet2 == "River"){
      gg <- gg + facet_grid(riverN~speciesName)
    }
    
    if(input$facet1 == "Species" & input$facet2 == "Season"){
      gg <- gg + facet_grid(seasonN.x~speciesName)
    }
    
    
    
    
    
    if(input$facet1 == "Age" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~ageN)
    }
    
    if(input$facet1 == "None" & input$facet2 == "Age"){
      gg <- gg + facet_grid(ageN~.)
    }
    
    
    if(input$facet1 == "River" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~riverN)
    }
    
    if(input$facet1 == "None" & input$facet2 == "River"){
      gg <- gg + facet_grid(riverN~.)
    }
    
    
    
    if(input$facet1 == "Season" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~seasonN)
    }
    
    if(input$facet1 == "None" & input$facet2 == "Season"){
      gg <- gg + facet_grid(seasonN~.)
    }
    
    
    
    if(input$facet1 == "Species" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~speciesName)
    }
    
    if(input$facet1 == "None" & input$facet2 == "Species"){
      gg <- gg + facet_grid(speciesName~.)
    }
    
    
    if(input$facet1 == "None" & input$facet2 == "None"){
      gg <- gg 
    }
    
    gg <- gg + theme_Publication()
      
      #theme(strip.text.x = element_text(size=16 , face = "bold"),
       #               strip.text.y = element_text(size=16, face="bold")) +  
      #theme(axis.title.x = element_text(color="black", size=20, face="bold"),
       #     axis.title.y = element_text(color="black", size=20, face="bold"), 
        ##    axis.text.x = element_text(face="bold",  size=14),
        #    axis.text.y = element_text(face="bold" , size =14))
    
    
    gg
    
  } )
  
})