#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  postCode <- eventReactive(input$go, {
    toupper(gsub(" ", "", input$postcode, fixed = TRUE))
  })
    
    query_metadata <- "SELECT *
      WHERE {
        <http://statistics.gov.scot/data/house-sales-prices> ?predicate ?object
      }"
  
  house_prices_metadata <- SPARQL(endpoint, query_metadata)[["results"]]
  
  output$description <- renderText({
    house_prices_description <- house_prices_metadata %>%
      filter(predicate == "<http://purl.org/dc/terms/description>") %>%
      select(object) %>%
      `[`(1, 1)
  })
  
  output$dateIssued <- renderText({
    house_prices_issued <- house_prices_metadata %>%
      filter(predicate == "<http://purl.org/dc/terms/issued>") %>%
      select(object) %>%
      `[`(1, 1) %>%
      as.numeric() %>%
      lubridate::as_datetime(tz="Europe/London") %>%
      as.character()
  })
  
  output$dateModified <- renderText({
    house_prices_modified <- house_prices_metadata %>%
      filter(predicate == "<http://purl.org/dc/terms/modified>") %>%
      select(object) %>%
      `[`(1, 1) %>%
      as.numeric() %>%
      lubridate::as_datetime(tz="Europe/London") %>%
      as.character()
  })
  
  output$nextUpdateDue <- renderText({
    house_prices_nextUpdateDue <- house_prices_metadata %>%
      filter(predicate == "<http://publishmydata.com/def/dataset#nextUpdateDue>") %>%
      select(object) %>%
      `[`(1, 1)
  })
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    library(ggplot2)
    library(SPARQL)
    library(dplyr)
    
    endpoint <- "http://statistics.gov.scot/sparql"
    
    query <- paste0("PREFIX dcat: <http://www.w3.org/ns/dcat#>
    PREFIX dcterms: <http://purl.org/dc/terms/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX qb: <http://purl.org/linked-data/cube#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX void: <http://rdfs.org/ns/void#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    
    SELECT ?refAreaCollection_label ?refArea_label ?year_label ?median
    WHERE {
    ?s qb:dataSet <http://statistics.gov.scot/data/house-sales-prices> ;
    qb:measureType <http://statistics.gov.scot/def/measure-properties/median> ;
    <http://statistics.gov.scot/def/measure-properties/median> ?median ;
    <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?refArea ;
    <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?year .
    ?refArea <http://publishmydata.com/def/ontology/foi/memberOf> ?refAreaCollection;
    <http://publishmydata.com/def/ontology/foi/displayName> ?refArea_label .
    ?year rdfs:label ?year_label .
    ?refAreaCollection rdfs:label ?refAreaCollection_label
    
    {VALUES ?refAreaCollection {<http://statistics.gov.scot/def/foi/collection/countries> <http://statistics.gov.scot/def/foi/collection/council-areas>}}
    
    UNION
    
                    {<http://statistics.gov.scot/id/postcodeunit/",
                    postCode(),
"> <http://statistics.gov.scot/def/postcode/dataZone2011> ?refArea }
    
    }
    
    ORDER BY ?refAreaCollection_label ?refArea_label ?year_label")

    house_prices_result <- SPARQL(endpoint,query)
    
    house_prices <- house_prices_result[["results"]] %>%
      mutate(year_label = as.integer(year_label))
    
    house_prices_councils <- house_prices %>%
      filter(refAreaCollection_label == "Council Areas")
    
    house_prices_councils_top <- house_prices_councils %>%
      filter(year_label == 2017) %>%
      top_n(n = 1, wt = median)
    
    house_prices_councils_bottom <- house_prices_councils %>%
      filter(year_label == 2017) %>%
      top_n(n = 1, wt = -median)
      
    house_prices_dz <- house_prices %>%
      filter(refAreaCollection_label == "2011 Data Zones")
    
    house_prices_scotland <- house_prices %>%
      filter(refAreaCollection_label == "Countries")
    
    geom_text_data <- rbind(
      house_prices_scotland[house_prices_scotland[["year_label"]] == 2017,],
      house_prices_dz[house_prices_dz[["year_label"]] == 2017,],
      house_prices_councils_top,
      house_prices_councils_bottom
    )
    
    ggplot(data = house_prices_councils, mapping = aes(x = year_label,
                                                       y = median,
                                                       group = refArea_label)) +
      geom_line(colour = "grey") +
      geom_line(data = house_prices_dz, size = 1.7) +
      geom_line(data = house_prices_scotland) +
      geom_text(data = geom_text_data,
                mapping = aes(label = refArea_label), nudge_x = 0.5,
                hjust = "left", size = 5) +
      scale_x_continuous(breaks = c(1993, 2017), limits = c(1993, 2022)) +
      scale_y_continuous(labels = scales::comma) +
      theme(
        # Declutter
        panel.background = ggplot2::element_blank(),
        legend.position = "none",
        axis.title = ggplot2::element_blank(),
        axis.ticks = element_blank(),
        
        # Text
        text = element_text(family = "Segoe UI", size = 16))
      
  })
  
})
