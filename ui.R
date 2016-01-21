library(shiny)
library('stringi')
library('dplyr')


shinyUI(navbarPage("Recipe network",
                   tabPanel("Recipe",                      
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("reca1", "Vertex color:", 
                                              choices=c('region','category')),
                                sliderInput("nd1", "Number of Degree:", 
                                            min = 1,max = 10,value = 3),
                                sliderInput("plotsize1", "Plot Size:",
                                            min = 500, max = 8000, value = 700),
                                sliderInput("labelsize1", "Label Size:",
                                            min = 1, max = 6, value = 1),
                                sliderInput("vertexsize1", "Vertex Size:",
                                            min = 6, max = 15, value = 6)
                                ),
                              mainPanel(
                                h1("Recipe-By-Recipe Network"),
                                h2(verbatimTextOutput("legend1")),
                                plotOutput("recPlot")
                              )
                            )
                   ),
                   tabPanel("Ingredient",                      
                            sidebarLayout(
                              sidebarPanel(
                                
                                sliderInput("nd2", "Number of Degree:", 
                                            min = 7,max = 20,value = 13),
                                sliderInput("plotsize2", "Plot Size:",
                                            min = 500, max = 8000, value = 700),
                                sliderInput("labelsize2", "Label Size:",
                                            min = 2, max = 8, value = 2.5),
                                sliderInput("edwid2", "Edge Width:",
                                            min = 0, max = 1, value = 0.5),
                                sliderInput("arrsize2", "Arrow Size:",
                                            min = 2, max = 7, value = 3)
                              ),
                              mainPanel(
                                h1("Ingredient-By-Ingredient Network"),
                                plotOutput("ingPlot")
                              )
                            )
                   ),
                   
                   tabPanel("Twomode",                      
                            sidebarLayout(
                              sidebarPanel(
                                
                                
                                selectInput("reca3", "Vertex color:", 
                                            choices=c('region','category')),
                                selectInput("inoupr3", "Method to pick ingredient", 
                                            choices=c('Indegree'='in','Outdegree'='ou','PageRank'='pr')),
                                sliderInput("plotsize3", "Plot Size:",
                                            min = 500, max = 8000, value = 700),
                                sliderInput("labelsize3", "Label Size:",
                                            min = 1, max = 6, value = 1),
                                sliderInput("invesi3", "Ingredient Vertex Size:",
                                            min = 10, max = 30, value = 10),
                                sliderInput("revesi3", "Recipe Vertex Size:",
                                            min = 10, max = 30, value = 10)
                              ),
                              mainPanel(
                                h1("Ingredient-By-Recipe Network"),
                                h2(verbatimTextOutput("legend3")),
                                plotOutput("ingrecPlot")
                              )
                            )
                   )
                   
                   
                   
))

