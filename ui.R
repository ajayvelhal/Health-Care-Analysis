library(shiny)
library(dplyr)
#library(reshape2)
library(tidyverse)
#library(shinythemes)
library(tidyr)
# Define UI for application that draws a histogram
library(shinydashboard)
library(ggplot2)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(plotly)
library(factoextra)
library(NbClust)
library(cluster)
library(shinydashboardPlus)
# Define UI for application that draws a histogram
ui <-dashboardPage(skin = "green",
                    dashboardHeader(title = span(tagList(icon("file-medical-alt"),"HealthCare Analysis"))),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Upload Data File ", tabName = "dashboard", icon = icon("fas fa-upload")),
                        menuItem("Home Page", tabName = "widgets", icon = icon("fas fa-home")),
                        menuItem("Diseases", tabName = "disease", icon = icon("fas fa-chart-bar"),
                                 menuSubItem("Adult",tabName = "Adult",icon = icon("fas fa-user")),
                                 menuSubItem("Child",tabName = "Child",icon = icon("fas fa-child"))),
                        menuItem("Aggregation", tabName = "widgets2", icon = icon("fas fa-network-wired"),
                                 menuSubItem("Admitted",tabName = "Admitted",icon = icon("fas fa-user-injured")),
                                 menuSubItem("Death",tabName = "Death",icon = icon("angle-double-right"))
                      ),
                        menuItem("Linear Regression", tabName = "widgets3", icon = icon("fas fa-chart-bar")),
                        menuItem("Admission Year Vise", tabName = "widgets4", icon = icon("fas fa-chart-pie")),
                        menuItem("Death Year Vise", tabName = "widgets5", icon = icon("fas fa-chart-pie")),
                        menuItem("Month Trend", tabName = "widgets6", icon = icon("fas fa-chart-line"),
                                 menuSubItem("Male",tabName = "Male",icon = icon("fas fa-male")),
                                 menuSubItem("Female",tabName = "Female",icon = icon("fas fa-female")),
                                 menuSubItem("Male Child",tabName = "Male_Child",icon = icon("fas fa-child")),
                                 menuSubItem("Female Child",tabName = "Female_Child",icon = icon("fas fa-child"))),
                      menuItem("Average Daily Patients", tabName = "widgets8", icon = icon("fas fa-chart-pie")),
                      menuItem("Clustering", tabName = "widgets7", icon = icon("far fa-object-group"))
                      
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "dashboard",
                          
                            # box(fileInput("file_da","Upload A CSV File",accept = c("text/csv","text/comma-separated-values","text/plain",".csv")),
                            #     title = span(tagList(icon("fas fa-upload"), "Browse Here For A File")),
                            #     plotOutput('da',height = "600px"),height = "150px"
                            # 
                            # )   
                        #    fluidRow(column(
                        #     width = 6,
                        #     box(
                        #       status = "primary", solidHeader = TRUE,
                        #       width = 12,
                        #       title = span(tagList(icon("fas fa-window-restore"), "Browse Here For The File"))
                        # ,
                        # fluidRow(
                        #   column(width = 3,
                        #           tags$div(HTML('<i class="fas fa-upload"></i>'))
                        #   ),
                        #   column(width = 12,
                        #          fileInput("file_da","Upload A CSV File",accept = c("text/csv","text/comma-separated-values","text/plain",".csv"))
                        # )
                        # 
                        # )
                        #     )
                        # 
                        # ))
                        
                        fluidRow(
                          column(
                            width=12,
                            gradientBox(
                              title = "Browse Here For A File",
                              icon = "fas fa-window-restore",
                              gradientColor = "green",
                              #height = '100px',
                              boxToolSize = 'sm',
                              footer = fileInput("file_da","Upload A CSV File",accept = c("text/csv","text/comma-separated-values","text/plain",".csv"))
                            ))  
                          ),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                width = 12,
                                plotOutput('da',height = "400px")  
                              )
                            )
                          )
                        ),
                        tabItem(
                          tabName = "widgets",
                          selectInput('month',"Select Month",""),
                          selectInput('year1',"Select Year",""),
                          # fluidRow(
                          #   infoBox("Sales",1000,icon = icon("warning"))
                          # ),
                          plotOutput("plot1")
                          ),
                        tabItem(
                          tabName = "Adult",
                          fluidRow(
                            box(
                              selectInput('disease',"Select Disease","")
                              )
                            ),
                          fluidRow(box(selectInput('year2',"Select Year",""))),
                          tabBox(width = 10,selected = "Male",
                            tabPanel(title="Male",
                                     fluidRow(
                                        column(
                                          width=12,
                                          plotOutput("view_male",height = '300px'))
                                        )
                                     ),
                            tabPanel("Female",width=10,
                                     fluidRow(
                                       column(
                                         width=12,
                                         plotOutput("view_female",height = '300px')
                                         )
                                       )
                                   )
                            )
                          ),
                            
                        
                        tabItem(
                          tabName = "Child",
                          tabBox(
                            tabPanel("Male Child",fluidRow(column(width=12,plotOutput("view_male_child")))),
                            tabPanel("Female Child",fluidRow(column(width=12,plotOutput("view_female_child"))))
                          ),
                          plotOutput("plot2")
                        ),
                        tabItem(
                          tabName = "widgets2"
                          #selectInput('cat',"Select Category",""),
                          #selectInput('year1',"Select Year",""),
                          
                        ),
                        tabItem(
                          tabName = "Admitted",
                          selectInput('cat','Select Category',""),
                          plotOutput("aggr1",height = '600px')
                        ),
                        tabItem(
                          tabName = "Death",
                          selectInput('cat2','Select Category',""),
                          plotOutput("aggr2",height = '600px')
                        ),
                        
                        tabItem(
                          tabName = "widgets3",
                          #radioButtons("rd1",label = "Year",choices = list("2017"="2017","2018"="2018"),selected = "2017"),
                          #selectInput('month',"Select Month",""),
                          
                          
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'green',
                                selectInput('diss',"Select Disease","")
                              ),
                              box(
                                background = 'green',
                                selectInput('yearrr',"Select Year","")
                              )
                            )
                          ),
                        
                          fluidRow(
                            column(
                              width=12,
                              plotOutput("lgr",width = '1085px')
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'green',
                                h3('Summary'),
                                verbatimTextOutput('txt')
                              ),
                              box(
                                background = 'green',
                                h3('Prediction'),
                                collapsible = TRUE,
                                  verbatimTextOutput('txt1')
                              )
                            )
                          )
                          #selectInput('year1',"Select Year",""),
                          
                        ),
                        tabItem(
                          tabName = "widgets4",
                            #selectInput('month',"Select Month",""),
                          selectInput('year3',"Select Year",""),
                          plotlyOutput("adm",height = '500px')
                        ),
                        tabItem(
                          tabName = "widgets5",
                          #selectInput('month',"Select Month",""),
                          selectInput('year4',"Select Year",""),
                          plotlyOutput("dea",height = '500px')
                        ),
                        
                        tabItem(
                          tabName = "widgets6"
                        ),
                        tabItem(
                          tabName = "Male",
                          #radioButtons("rd2",label = "Year",choices = list("2017"="2017","2018"="2018"),selected = "2017"),
                          selectInput('dis','Select Disease',""),
                          plotlyOutput("year_male",height = '600px')
                        ),
                        tabItem(
                          tabName = "Female",
                          selectInput('dis1','Select Disease',""),
                          plotlyOutput("year_female",height = '600px')
                        ),
                        tabItem(
                          tabName = "Male_Child",
                          selectInput('dis2','Select Disease',""),
                          plotlyOutput("year_male_child",height = '600px')
                        ),
                        tabItem(
                          tabName = "Female_Child",
                          selectInput('dis3','Select Disease',""),
                          plotlyOutput("year_female_child",height = '600px')
                        ),
                        tabItem(
                          tabName = "widgets7",
                          #selectInput('dis3','Select Disease',""),
                          plotOutput("scat",height = '600px')
                        ),
                        tabItem(
                          tabName = "widgets8",
                          #selectInput('month',"Select Month",""),
                          selectInput('year9',"Select Year",""),
                          plotlyOutput("day",height = '500px')
                        )
                        )
                      )
                      
                    )

