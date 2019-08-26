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
# Define UI for application that draws a histogram
ui <-dashboardPage(skin = "black",
                    dashboardHeader(title = span(tagList(icon("file-medical-alt"),"HealthCare Analysis"))),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Month Wise", tabName = "widgets", icon = icon("th")),
                        menuItem("Diseases", tabName = "disease", icon = icon("th"),
                                 menuSubItem("Adult",tabName = "Adult",icon = icon("angle-double-right")),
                                 menuSubItem("Child",tabName = "Child",icon = icon("angle-double-right"))),
                        menuItem("Aggregation", tabName = "widgets2", icon = icon("th"),
                                 menuSubItem("Admitted",tabName = "Admitted",icon = icon("angle-double-right")),
                                 menuSubItem("Death",tabName = "Death",icon = icon("angle-double-right"))
                      ),
                        menuItem("Linear Regression", tabName = "widgets3", icon = icon("th")),
                        menuItem("Admission Year Vise", tabName = "widgets4", icon = icon("th")),
                        menuItem("Death Year Vise", tabName = "widgets5", icon = icon("th"))
                      
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "dashboard",
                          
                            fileInput("file_da","Upload A CSV File",accept = c("text/csv","text/comma-separated-values","text/plain",".csv")),
                            
                            plotOutput('da',height = "600px")
                          
                          
                        ),
                        tabItem(
                          tabName = "widgets",
                          selectInput('month',"Select Month",""),
                          selectInput('year1',"Select Year",""),
                          plotOutput("plot1")
                          ),
                        tabItem(
                          tabName = "Adult",
                          selectInput('disease',"Select Disease",""),
                          selectInput('year2',"Select Year",""),
                          tabBox(
                            
                            tabPanel(title="Male",fluidRow(column(width=12,plotOutput("view_male")))),
                            tabPanel("Female",fluidRow(column(width=12,plotOutput("view_female")))))
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
                          radioButtons("rd1",label = "Year",choices = list("2017"="2017","2018"="2018"),selected = "2017"),
                          #selectInput('month',"Select Month",""),
                          #selectInput('year1',"Select Year",""),
                          plotOutput("lgr")
                        ),
                        tabItem(
                          tabName = "widgets4",
                            #selectInput('month',"Select Month",""),
                          selectInput('year3',"Select Year",""),
                          plotOutput("adm")
                        ),
                        tabItem(
                          tabName = "widgets5",
                          #selectInput('month',"Select Month",""),
                          selectInput('year4',"Select Year",""),
                          plotOutput("dea")
                        )
                        )
                      )
                      
                    )

