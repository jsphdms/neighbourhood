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
    
    query <- "PREFIX dcat: <http://www.w3.org/ns/dcat#>
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
    
    {<http://statistics.gov.scot/id/postcodeunit/AB101AB> <http://statistics.gov.scot/def/postcode/dataZone2011> ?refArea }
    
    }
    
    ORDER BY ?refAreaCollection_label ?refArea_label ?year_label"

    house_prices_result <- SPARQL(endpoint,query)
    
    house_prices <- house_prices_result[["results"]] %>%
      mutate(year_label = as.integer(year_label))
    
    house_prices_councils <- house_prices %>%
      filter(refAreaCollection_label == "Council Areas")
    
    ggplot(data = house_prices_councils, mapping = aes(x = year_label, y = median, colour = refArea_label)) +
      geom_line()
    
  })
  
})
