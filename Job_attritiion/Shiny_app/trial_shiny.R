library(ggplot2)
library(dplyr)

#### UI side here ####
ui <- fluidPage(
  
  checkboxGroupInput('year', 'Choose a year', choices = c(2017,2018),
                     selected = c('2017','2018')),
  checkboxGroupInput('taxon', 'Choose a taxon', 
                     choices = c('Acyrthosiphon sp.','Aphis craccivora',
                                 'Therioaphis maculata','Unknown'),
                     selected = c('Acyrthosiphon sp.','Aphis craccivora',
                                  'Therioaphis maculata','Unknown')),
  plotOutput('bars'),
  plotOutput('bars2')
  
)

#### Server side here ####
server <- function(input, output) {
  
  #### Subset of my data for reprex ####
  data <- structure(list(year = c(2017L, 2017L, 2018L, 2017L, 2018L, 2018L, 
                                  2018L, 2018L, 2017L, 2017L, 2018L, 2018L, 2017L, 2017L, 2017L, 
                                  2018L, 2017L, 2018L, 2018L, 2017L, 2018L, 2018L, 2018L, 2018L, 
                                  2018L, 2018L, 2018L, 2018L, 2017L, 2017L, 2018L, 2018L, 2018L, 
                                  2017L, 2017L, 2017L, 2018L, 2017L, 2018L, 2018L), 
                         site = c(122L, 109L, 148L, 121L, 150L, 138L, 153L, 153L, 107L, 113L, 136L, 146L, 
                                  114L, 107L, 113L, 148L, 106L, 146L, 151L, 120L, 139L, 138L, 144L, 
                                  142L, 144L, 143L, 153L, 138L, 124L, 107L, 138L, 144L, 150L, 121L, 
                                  108L, 107L, 135L, 113L, 146L, 147L), 
                         plant = c(20L, 26L, 27L, 2L, 20L, 17L, 27L, 12L, 10L, 4L, 2L, 25L, 17L, 16L, 9L, 26L, 
                                   14L, 18L, 26L, 1L, 29L, 4L, 27L, 16L, 23L, 17L, 16L, 1L, 19L, 
                                   19L, 20L, 8L, 13L, 18L, 30L, 17L, 21L, 3L, 23L, 24L), 
                         taxon = structure(c(3L, 3L, 1L, 1L, 3L, 2L, 2L, 3L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 3L, 1L, 
                                             1L, 3L, 3L, 1L, 2L, 1L, 3L, 1L, 2L, 4L, 2L, 2L, 1L, 1L, 1L, 1L, 
                                             2L, 1L, 1L, 1L, 1L, 1L, 1L), 
                                           .Label = c("Acyrthosiphon sp.", "Aphis craccivora", 
                                                      "Therioaphis maculata", "Unknown"), class = "factor"), 
                         count = c(2L, 1L, 1L, 1L, 1L, 3L, 4L, 1L, 7L, 1L, 2L, 1L, 
                                   4L, 11L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 8L, 1L, 3L, 4L, 1L, 
                                   1L, 158L, 1L, 1L, 2L, 5L, 3L, 1L, 1L, 3L, 1L, 2L, 2L, 1L), 
                         long = c(-105.561694, -120.290645, -106.085614, -111.79121, 
                                  -119.372752, -111.478175, -112.763, -112.763, -111.623191, 
                                  -119.995186, -119.89852, -119.768389, -118.78077, -111.623191, 
                                  -119.995186, -106.085614, -111.791825, -119.768389, -111.013924, 
                                  -120.478646, -111.477622, -111.478175, -117.134722, -117.802018, 
                                  -117.134722, -117.741105, -112.763, -111.478175, -110.937281, 
                                  -111.623191, -111.478175, -117.134722, -119.372752, -111.79121, 
                                  -122.485552, -111.623191, -115.02496, -119.995186, -119.768389, 
                                  -111.415287), lat = c(41.634862, 41.98567, 38.722843, 40.579933, 
                                                        38.753147, 41.009581, 47.92386, 47.92386, 41.609366, 39.51379, 
                                                        39.507393, 39.466116, 39.45819, 41.609366, 39.51379, 38.722843, 
                                                        41.728298, 39.466116, 43.17306, 40.288588, 40.529525, 41.009581, 
                                                        39.220556, 41.569559, 39.220556, 40.979555, 47.92386, 41.009581, 
                                                        42.005173, 41.609366, 41.009581, 39.220556, 38.753147, 40.579933, 
                                                        41.765685, 41.609366, 38.87951, 39.51379, 39.466116, 40.492409
                                  )), row.names = c(221L, 109L, 639L, 211L, 693L, 415L, 781L, 
                                                    769L, 60L, 127L, 352L, 585L, 148L, 63L, 130L, 638L, 43L, 577L, 
                                                    746L, 198L, 441L, 399L, 537L, 479L, 534L, 498L, 771L, 397L, 231L, 
                                                    67L, 418L, 513L, 685L, 217L, 99L, 66L, 342L, 126L, 581L, 610L
                                  ), class = "data.frame")
  
  rdata <- reactive({
    subset(data, year %in% input$year & taxon %in% input$taxon) %>%
      arrange(site, taxon)
  })
  
  #### Here's the broken part #####
  ## This one (using fill) doesn't work.
  output$bars <- renderPlot({
    p <- ggplot(data=rdata()) +
      geom_col(aes(x = site, y = count,
                   fill = taxon, stat = 'identity')) + 
      coord_flip()
    print(p)
    
  })
  
  ## This one (using color) does work. ???
  output$bars2 <- renderPlot({
    p <- ggplot(data=rdata()) +
      geom_bar(aes(x = rdata()[,'site'], y = rdata()[,'count']),
               color = rdata()[,'taxon'], stat = 'identity') + 
      coord_flip()
    print(p)
    
  })
  
  
}

shinyApp(ui = ui, server = server)