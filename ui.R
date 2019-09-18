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
library(scales)
library(data.table)
library(DT)
library(knitr)
# Define UI for application that draws a histogram
ui <-dashboardPage(skin = "purple",
                    dashboardHeader(title = span(tagList(icon("file-medical-alt"),"HealthCare Analysis"))),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Upload Data File ", tabName = "dashboard", icon = icon("fas fa-upload")),
                        menuItem("Home Page", tabName = "widgets", icon = icon("fas fa-home")),
                        menuItem("Admitted Patients", tabName = "Adult", icon = icon("fas fa-chart-bar")),
                        menuItem("Average Patients", tabName = "widgets2", icon = icon("signal"),
                                 menuSubItem("Admitted",tabName = "Admitted",icon = icon("syringe")),
                                 menuSubItem("Death",tabName = "Death",icon = icon("heartbeat"))
                      ),
                      menuItem("Season-Wise Admission", tabName = "season", icon = icon("cloud")),
                        menuItem("Linear Regression", tabName = "widgets3", icon = icon("fas fa-chart-bar")),
                        menuItem("Yearly Admissions", tabName = "widgets4", icon = icon("fas fa-chart-pie")),
                        menuItem("Yearly Deaths", tabName = "widgets5", icon = icon("fas fa-chart-pie")),
                        menuItem("Monthly Trend", tabName = "widgets6", icon = icon("fas fa-chart-line"),
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
                            fluidRow(
                          column(
                            width=12,
                            gradientBox(
                              title = "Browse Here For A File",
                              icon = "fas fa-window-restore",
                              gradientColor = "purple",
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
                          fluidRow(
                            box(
                              background = 'black',
                              title = "Total Admitted Patients (2017-18)",
                              width = 12,
                              valueBoxOutput("total_patients_till_date_male",width = 3),
                              valueBoxOutput("total_patients_till_date_female",width = 3),
                              valueBoxOutput("total_patients_till_date_male_child",width = 3),
                              valueBoxOutput("total_patients_till_date_female_child",width = 3))
                          ),
                      br(),
                        fluidRow(
                          column(
                            width=12,
                            box(
                              background = 'purple',
                              title = "Average Patients Admitted Per Month (2017-18)",
                              width=8,
                             plotlyOutput('home') 
                            ),
                            box(
                              
                              title = "Top 10 Diseases",
                              width=4,
                              tableOutput('home1') 
                            )
                          )
                        )
                        ),
                        tabItem(
                          tabName = 'Adult',
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('disease',"Select Disease","")
                              ),
                              box(
                                background = 'purple',
                                selectInput('year2',"Select Year","")
                              )
                            )
                          ),
                          br(),
                          tabBox(
                            width = 12,selected = "Male",
                                 tabPanel(title="Male",
                                          fluidRow(
                                            column(
                                              width=12,
                                              plotOutput("view_male",height = '400px')
                                          )
                                 )
                                 ),
                                 tabPanel("Female",width=12,
                                          fluidRow(
                                            column(
                                              width=12,
                                              plotOutput("view_female",height = '400px')
                                            )
                                          )
                                 ),
                                 
                                 tabPanel("Male Child",width=12,
                                          fluidRow(
                                            column(
                                              width=12,
                                              plotOutput("view_male_child",height = '400px')
                                            )
                                          )
                                 ),
                                 tabPanel("Female Child",width=12,
                                          fluidRow(
                                            column(
                                              width=12,
                                              plotOutput("view_female_child",height = '400px')
                                            )
                                          )
                                 )
                                
                        
                        
                        )
                        ),
                      tabItem(
                        tabName = 'season',
                        fluidRow(
                          box(background = 'purple',
                              selectInput('weather',"Select Season",choices = list("Winter"="winter","Monsoon"="monsoon","Summer"="Summer"))
                              )
                          ),
                        br(),
                        fluidRow(
                          column(
                            width=12,
                            box(
                              background = 'purple',
                              title = "Patients admitted Season Wise",
                              width=8,
                              plotlyOutput("season12") 
                            ),
                            box(
                              
                              title = "Top 10 Diseases",
                              width=4,
                              tableOutput('home2') 
                            )
                          )
                         #
                          
                        )
                      ),
                      tabItem(
                          tabName = "widgets2"
                          #selectInput('cat',"Select Category",""),
                          #selectInput('year1',"Select Year",""),
                          
                        ),
                        tabItem(
                          tabName = "Admitted",
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('cat','Select Category',"")
                              ),
                              box(
                                background = 'purple',
                                selectInput('yr','Select Year',"")
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                title="Aggregation for Admitted Patients",
                                background = 'purple',
                                width = 12,
                                plotOutput("aggr1",height = '600px')
                              )
                            )
                          )
                          
                         
                        ),
                        tabItem(
                          tabName = "Death",
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('cat2','Select Category',"")
                              ),
                              box(
                                background = 'purple',
                                selectInput('yr1','Select Year',"")
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                width=12,
                                title="Aggregation For Deaths",
                                background = 'purple',
                                plotOutput("aggr2",height = '600px')
                              )
                            )
                          )
                          
                          
                        ),
                        
                        tabItem(
                          tabName = "widgets3",
                          #radioButtons("rd1",label = "Year",choices = list("2017"="2017","2018"="2018"),selected = "2017"),
                          #selectInput('month',"Select Month",""),
                          
                          
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('diss',"Select Disease","")
                              ),
                              box(
                                background = 'purple',
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
                                background = 'purple',
                                h3('Summary'),
                                verbatimTextOutput('txt')
                              ),
                              box(
                                background = 'purple',
                                h3('Prediction'),
                                collapsible = FALSE,
                                  verbatimTextOutput('txt1')
                              )
                            )
                          )
                          #selectInput('year1',"Select Year",""),
                          
                        ),
                        tabItem(
                          tabName = "widgets4",
                            #selectInput('month',"Select Month",""),
                          
                          fluidRow(
                            box(background = 'purple',
                              selectInput('year3',"Select Year","")),
                              valueBoxOutput("total_patients")
                            
                          ),
                          
                        br(),
                        fluidRow(
                          column(
                            width=12,
                            box(title = 'Average Patients Admitted Yearly',background = 'purple',
                              width=12,
                              plotlyOutput("adm",height = '500px')
                            )
                          )
                        )
                        ),
                        tabItem(
                          tabName = "widgets5",
                          #selectInput('month',"Select Month",""),
                          fluidRow(
                            box(background = 'purple',
                              selectInput('year4',"Select Year","")),
                              valueBoxOutput("total_patients_dead")
                            
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(title = 'Average Patients Death Yearly',background = 'purple',
                                width = 12,
                                plotlyOutput("dea",height = '500px')
                              )
                            )
                          )
                         
                        ),
                        
                        tabItem(
                          tabName = "widgets6"
                        ),
                        tabItem(
                          tabName = "Male",
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('dis','Select Disease',"")
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                width=12,
                                background = 'purple',
                                title = 'Admitted Males Monthly',
                                plotlyOutput("year_male")
                                
                              )
                            )
                          )
                          
                          
                        ),
                        tabItem(
                          tabName = "Female",
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('dis1','Select Disease',"")
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                width = 12,
                                background = 'purple',
                                title = 'Admitted Females Monthly',
                                plotlyOutput("year_female")
                              )
                            )
                          )
                          
                          
                        ),
                        tabItem(
                          tabName = "Male_Child",
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('dis2','Select Disease',"")
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                width=12,
                                title = 'Admitted Male Child Monthly',
                                plotlyOutput("year_male_child")
                              )
                            )
                          )
                          
                         
                        ),
                        tabItem(
                          tabName = "Female_Child",
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                selectInput('dis3','Select Disease',"")
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                width=12,
                                title = 'Admitted Female Child Monthly',
                                plotlyOutput("year_female_child")
                              )
                            )
                          )
                          
                          
                        ),
                        tabItem(
                          tabName = "widgets7",
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                h3("Clustering"),
                                selectInput('clus',"Select Year","")
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                width = 8,
                                background = "purple",
                                title = "Plot",
                                plotOutput("scat",height = '600px')
                              ),
                              box(
                                width=4,
                                title = "Clusters Information",
                                dataTableOutput('clusttb')
                              )
                            )
                          )
                          #selectInput('dis3','Select Disease',""),
                          
                        ),
                        tabItem(
                          tabName = "widgets8",
                          #selectInput('month',"Select Month",""),
                          #selectInput('year9',"Select Year",""),
                          #plotlyOutput("day",height = '500px')
                          fluidRow(
                            box(background = 'purple',
                                selectInput('year9',"Select Year","")),
                            valueBoxOutput("total_patients_daily")
                            
                          ),
                          br(),
                          fluidRow(
                            column(
                              width=12,
                              box(
                                background = 'purple',
                                width = 12,
                                plotlyOutput("day",height = '500px')
                                
                              )
                            )
                          )
                        )
                        )
                      )
                    )
                    

