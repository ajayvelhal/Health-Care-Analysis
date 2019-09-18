library(shiny)

# Define UI for application that draws a histogram
library(shinydashboard)


server <-shinyServer(function(input, output,session) {
  data <- reactive({ 
    req(input$file_da) ## ?req #  require that the input is available
    
    inFile <- input$file_da 
    
    df <- read.csv(inFile$datapath, header = T, sep = ',')
    colnames(df) <- c("Diseases","Month","Year","Admitted_Male","Admitted_Female","Admitted_Male_Child","Admitted_Female_Child","Death_male","Death_Female","Death_Male_Child","Death_Female_Child")
    
    
    output$da <- renderPlot({
      text <- readLines(inFile$datapath)
      docs<- Corpus(VectorSource(text))
      inspect(docs)
      
      toSpace<- content_transformer(function(x,pattern)gsub(pattern,"",x))
      docs<-tm_map(docs,toSpace,"/")
      docs<-tm_map(docs,toSpace,"@")
      docs<-tm_map(docs,content_transformer(tolower))
      docs<-tm_map(docs,removeNumbers)
      docs<-tm_map(docs,removeWords,stopwords("english"))
      docs<-tm_map(docs,removeWords,c("blabla1","blabla2"))
      docs<-tm_map(docs,removePunctuation)
      docs<-tm_map(docs,stripWhitespace)
      docs<-tm_map(docs,stemDocument)
      
      dtm<-TermDocumentMatrix(docs)
      m<-as.matrix(dtm)
      
      v<-sort(rowSums(m),decreasing = TRUE)
      d<-data.frame(word=names(v),freq=v)
      head(d,2)
      set.seed(1234)
      
      wordcloud(words=d$word,freq=d$freq,min.freq=1,max.words=100,
                random.order=FALSE,rot.per=0.30,
                colors=brewer.pal(8,"Dark2"))
      
    })
    
    updateSelectInput(session,inputId = 'month', label='Select Month', choices = df$Month, selected = df$Month)
    updateSelectInput(session,inputId = 'year1', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'year2', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'disease', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'cat',label = "Select Category",choices = names(df[4:7]),selected = names(df[4:7]))
    updateSelectInput(session,inputId = 'cat2',label = "Select Category",choices = names(df[8:11]),selected = names(df[8:11]))
    updateSelectInput(session,inputId = 'year3', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'year4', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'year5', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'year6', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'year7', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'year8', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'year9', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'yearrr', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'yearrr', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'clus', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'yr', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'yr1', label='Select Year', choices = df$Year, selected = df$Year)
    updateSelectInput(session,inputId = 'diss', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis1', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis2', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis3', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    return(df)
    
    
  })
  
  
  observe({
    
#HomePage Plot
#-------------------------------------------------------------------------------------------------------------------------------------------------------
   
    output$home<- renderPlotly({
      sum_dis<- data() %>% group_by(Month) %>% summarise(mean_male=mean(Admitted_Male),mean_female=mean(Admitted_Female),mean_child_male=mean(Admitted_Male_Child),mean_child_female=mean(Admitted_Female_Child))
      dis<-data.frame(sum_dis)
      sum_admm <- dis %>% group_by(Month) %>% summarise(sum_add=sum(mean_male,mean_female,mean_child_male,mean_child_female))
      Month_ordered<- ordered(sum_admm$Month,month.name)
      p <- plot_ly(sum_admm,x = ~sum_add, y = ~Month_ordered, type = 'bar', orientation = 'h',color = 'black')%>%
        layout(xaxis = list(title = 'Average Admitted Per Month'),
               yaxis = list(title = 'Months'))
    })
    
    output$home1<- renderTable({
      sum_dis<- data() %>% group_by(Diseases) %>% summarise(Count=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
      
      data1<- sum_dis %>% arrange(desc(Count))
      head(data1,10)
      
    })
    
    output$total_patients_till_date_male<- renderValueBox({
      male<-data()
      male%>%tally(male$Admitted_Male)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Males",color = 'purple')
    })
    
    output$total_patients_till_date_female<- renderValueBox({
      female<-data()
      female%>%tally(female$Admitted_Female)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Females",color = 'purple')
    })
    
    output$total_patients_till_date_male_child<- renderValueBox({
      male_child<-data()
      male_child%>%tally(male_child$Admitted_Male_Child)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Male Child",color = 'purple')
    })
    
    output$total_patients_till_date_female_child<- renderValueBox({
      female_child<-data()
      female_child%>%tally(female_child$Admitted_Female_Child)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Female Child",color = 'purple')
    })
    
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #seasonalPlot
    
    if(input$weather=='winter'){
      win<-data()%>%filter(data()$Month=='October')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res<-data.frame(list(c(win)))
      win1<-data()%>%filter(data()$Month=='November')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res1<-data.frame(list(c(win1)))
      win2<-data()%>%filter(data()$Month=='December')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res2<-data.frame(list(c(win2)))
      win3<-data()%>%filter(data()$Month=='January')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res3<-data.frame(list(c(win3)))
      
      sum_win<- rbind(win_res,win_res1,win_res2,win_res3)
      add<- sum_win%>% group_by(Diseases)%>% summarise(sum_sum1=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      data1<- add %>% arrange(desc(sum_sum1))
      resultseason <- data.frame(list(c(data1)))
      #print(head(data1,10))
      output$season12<- renderPlotly({
        p<-plot_ly(resultseason,x = ~sum_sum1, y = ~Diseases, type = 'bar',orientation='h',color = 'black')%>%
          layout(xaxis = list(title = 'Patients Count'),
                 yaxis = list(title = 'Diseasess'))
      })
    
      output$home2<- renderTable({
        colnames(data1)<-c("Diseases","Total Count")
        head(data1,10)
      })
    }
    else if(input$weather=='Summer'){
      summ<-data()%>%filter(data()$Month=='February')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res<-data.frame(list(c(summ)))
      
      
      summ1<-data()%>%filter(data()$Month=='March')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res1<-data.frame(list(c(summ1)))
      
      
      summ2<-data()%>%filter(data()$Month=='April')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res2<-data.frame(list(c(summ2)))
      
      
      summ3<-data()%>%filter(data()$Month=='May')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res3<-data.frame(list(c(summ3)))
      
      
      sum_sum<- rbind(summ_res,summ_res1,summ_res2,summ_res3)
      
      
      add<- sum_sum%>% group_by(Diseases)%>% summarise(sum_sum1=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      
      data1<- add %>% arrange(desc(sum_sum1))
      resultseason <- data.frame(list(c(data1)))
     
      output$season12<- renderPlotly({
        p<-plot_ly(resultseason,x = ~sum_sum1, y = ~Diseases, type = 'bar',orientation='h',color = 'black')%>%
           layout(xaxis = list(title = 'Average Admitted Per Month'),
                  yaxis = list(title = 'Months'))
      })
      
      output$home2<- renderTable({
        colnames(data1)<-c("Diseases","Total Count")
        head(data1,10)
      })
      
  }
    
    
    else if(input$weather=='monsoon'){
      mon<-data()%>%filter(data()$Month=='June')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res<-data.frame(list(c(mon)))
      
      
      mon1<-data()%>%filter(data()$Month=='July')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res1<-data.frame(list(c(mon1)))
      
      
      mon2<-data()%>%filter(data()$Month=='August')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res2<-data.frame(list(c(mon2)))
      
      
      mon3<-data()%>%filter(data()$Month=='September')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res3<-data.frame(list(c(mon3)))
      
      
      sum_mon<- rbind(mon_res,mon_res1,mon_res2,mon_res3)
     
      
      add<- sum_mon%>% group_by(Month)%>% summarise(sum_soon=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      
      add<- sum_mon%>% group_by(Diseases)%>% summarise(sum_sum1=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      
      data1<- add %>% arrange(desc(sum_sum1))
      resultseason <- data.frame(list(c(data1)))
 
      output$season12<- renderPlotly({
        p<-plot_ly(resultseason,x = ~sum_sum1, y = ~Diseases, type = 'bar',orientation='h',color = 'black')%>%
          layout(xaxis = list(title = 'Average Admitted Per Month'),
                 yaxis = list(title = 'Months'))
      })
      
      output$home2<- renderTable({
        colnames(data1)<-c("Diseases","Total Count")
        head(data1,10)
      })
    }

#---------------------------------------------------------------------------------------------------------------------------------------------------   
    
    #Admitted Patients
    plot2<- data()%>%filter(data()$Disease == input$disease & data()$Year == input$year2) %>% select(Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
    plot2dis<- data.frame(list(c(plot2)))
    
    Month_ordered <- ordered(plot2dis$Month,month.name)
    
    output$view_male<- renderPlot({
      ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Male)) + geom_bar(stat="identity",color='purple',fill='white') + xlab("Month") +
        ylab("Admitted Male Patients")  
        })
    
    output$view_female<- renderPlot({
      ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Female)) + geom_bar(stat="identity",color='purple',fill='white') + xlab("Month") +
        ylab("Admitted Female Patients") 
    })
    output$view_male_child<- renderPlot({
      ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Male_Child)) + geom_bar(stat="identity",color='purple',fill='white') + xlab("Month") +
        ylab("Admitted Male-Child Patients") 
    })
    output$view_female_child <- renderPlot({
      ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Female_Child)) + geom_bar(stat="identity",color='purple',fill='white') + xlab("Month") +
        ylab("Admitted Female-Child Patients") 
    })
    
#----------------------------------------------------------------------------------------------------------------------------------------------------------    
    #Linear Regression Reference
    lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
    lrgs <- data.frame(list(c(lgrres)))
   
    sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
    
    sum_dis$Month=as.numeric(sum_dis$Month)
    
    colours <- c("red", "orange", "blue")
    
   output$lgr <- renderPlot({
      lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
      lrgs <- data.frame(list(c(lgrres)))
      
      sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))

      sum_dis$Month=as.numeric(sum_dis$Month)
      
      ggplot(sum_dis,aes(Month,mean_adm)) + geom_point() + geom_smooth(method = "lm")+xlab("Months")+ylab("Admitted Patients Count")+ scale_x_continuous(labels = as.character(sum_dis$Month), breaks = sum_dis$Month)

    })
    output$txt<- renderPrint({
      lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
      lrgs <- data.frame(list(c(lgrres)))
     
      sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
      
      sum_dis$Month=as.numeric(sum_dis$Month)
      
      linear <- lm(mean_adm~Month,sum_dis)
      print(summary(linear))
    })
    output$txt1<- renderPrint({
      lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
      lrgs <- data.frame(list(c(lgrres)))
      sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
      
      sum_dis$Month=as.numeric(sum_dis$Month)
      
      linear <- lm(mean_adm~Month,sum_dis)
      
      print(predict(linear,Month='December',interval="predict"))
    })
    
    
#-----------------------------------------------------------------------------------------------------------------------------------------------    
    
#Average of Admitted persons based on Gender
    output$aggr1 <- renderPlot({
      aggr<- data()%>% filter(data()$Year ==input$yr)%>% select(Diseases,input$cat)
      aggr_res <- data.frame(list(c(aggr)))
      
      names(aggr_res) <- c("Diseases","Admit")
      aggr_res %>% group_by(Diseases) %>%
        summarise(mean_admit=mean(Admit)) %>%
        ggplot(aes(x = Diseases, y = mean_admit, fill = Diseases)) +ylab("Count")+
        geom_bar(stat = "identity") +
        theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
      
    })
#-------------------------------------------------------------------------------------------------------------------------------------------------------    
    #Average  death person based on gender
    
    output$aggr2<- renderPlot({
      aggr<- data()%>% filter(data()$Year ==input$yr1)%>% select(Diseases,input$cat2)
      aggr_res <- data.frame(list(c(aggr)))
      names(aggr_res) <- c("Diseases","Death")
      aggr_res %>% group_by(Diseases) %>%
        summarise(mean_death=mean(Death)) %>%
        ggplot(aes(x = Diseases, y = mean_death, fill = Diseases)) +ylab('Count')+
        geom_bar(stat = "identity") +
        theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
    })
    
#-------------------------------------------------------------------------------------------------------------------------------------------------------
     #Average Admitted Persons Per Year
     output$adm<- renderPlotly({
       admit<- data()%>% filter(data()$Year==input$year3) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       sum_adm<- admit_res %>% group_by(Diseases) %>% summarise(mean_male=mean(Admitted_Male),mean_female=mean(Admitted_Female),mean_child_male=mean(Admitted_Male_Child),mean_child_female=mean(Admitted_Female_Child))
       sum_admm <- sum_adm %>% group_by(Diseases) %>% summarise(sum_add=sum(mean_male,mean_female,mean_child_male,mean_child_female))
       
       
         p<- plot_ly(sum_admm,labels=~Diseases,values= ~sum_add,type='pie',
         textposition = 'inside',
         textinfo = 'label+percent',
         insidetextfont = list(color = '#FFFFFF'),
         hoverinfo = 'text',
         text = ~paste(Diseases),
         marker = list(colors = colors,
                       line = list(color = '#FFFFFF', width = 1)),
          showlegend = TRUE) %>%
         layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
        })
     
     output$total_patients<- renderValueBox({
       admit<- data()%>% filter(data()$Year==input$year3) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       
       
       sum_adm<- admit_res %>% group_by(input$year3) %>% summarise(mean_male=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       
         
       sum_adm%>%tally(sum_adm$mean_male)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "No. of Total Admitted Patients",color = 'purple')
     })
     
     
#----------------------------------------------------------------------------------------------------------------------------------------
     #Sum of dead Persons Per Year
     output$dea<- renderPlotly({
       death<- data()%>% filter(data()$Year==input$year4) %>% select(Diseases,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       death_res <- data.frame(list(c(death)))
       sum_death<- death_res %>% group_by(Diseases) %>% summarise(mean_death=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       
       
       p<- plot_ly(sum_death,labels=~Diseases,values= ~mean_death,type='pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(Diseases),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = TRUE) %>%
         layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       
       })
     output$total_patients_dead<- renderValueBox({
       admit<- data()%>% filter(data()$Year==input$year4) %>% select(Diseases,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       
       
       sum_adm<- admit_res %>% group_by(input$year4) %>% summarise(mean_male=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       
       
       sum_adm%>%tally(sum_adm$mean_male)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "No. of Total Deaths Patients",color = 'purple')
     })
#---------------------------------------------------------------------------------------------------------------------------------------------------------     
     #Monthly trend
     
     output$year_male<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis) %>% select(Year,Month,Admitted_Male)
       adm_res<- data.frame(list(c(adm)))
       
       
       Month_ordered <- ordered(adm_res$Month,month.name)
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Male, type = 'scatter', mode = 'lines+markers',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
         layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     output$year_female<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis1) %>% select(Year,Month,Admitted_Female)
       adm_res<- data.frame(list(c(adm)))
       
       Month_ordered <- ordered(adm_res$Month,month.name)
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Female, type = 'scatter', mode = 'lines+markers',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
         layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     
     output$year_male_child<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis2) %>% select(Year,Month,Admitted_Male_Child)
       adm_res<- data.frame(list(c(adm)))
       
       Month_ordered <- ordered(adm_res$Month,month.name)
      
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Male_Child, type = 'scatter', mode = 'lines+markers',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
         layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     output$year_female_child<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis3) %>% select(Year,Month,Admitted_Female_Child)
       adm_res<- data.frame(list(c(adm)))
       
       
       Month_ordered <- ordered(adm_res$Month,month.name)
       
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Female_Child, type = 'scatter', mode = 'lines+markers',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
              layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     
#----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Average daily admitted patients count     
     
     output$day<- renderPlotly({
       admit<- data()%>% filter(data()$Year==input$year9) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       
       sum_adm <- admit_res%>% group_by(input$year9,Diseases) %>% summarise(sum_admm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       sum_year<- admit_res%>% group_by(input$year9) %>% summarise(sum_admm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       sum_death<- admit_res%>% group_by(input$year9) %>% summarise(sum_admm=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       
       #print(sum_death)
       
       b<- sum_adm[-1:-2]
       
       
       c<- b/365
       
       
       roun<- round(c,digits=2)
      
       sum_adm$sum_admm <- ifelse(is.na(roun$sum_admm), sum_adm$sum_adm, roun$sum_admm)
       
       
       p<- plot_ly(sum_adm,labels=~Diseases,values= ~sum_admm,type='pie',
                   textposition = 'inside',
                   textinfo = 'label',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(Diseases,sum_admm),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = TRUE) %>%
         layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       
       
       
     })
     
     output$total_patients_daily <- renderValueBox({
       admit<- data()%>% filter(data()$Year==input$year9) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       
       sum_year<- admit_res%>% group_by(input$year9) %>% summarise(sum_admm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       
       b<- sum_year[-1]
       
       
       c<- b/365
       
       
       sum_year$sum_admm <- ifelse(is.na(c$sum_admm), sum_year$sum_admm, c$sum_admm)
       
       
       
       sum_year%>%tally(sum_year$sum_admm)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Total Admitted Patients",color = 'purple',width = 12)
       
     })
     
     
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
     #Clustering
     admit<- data()%>% filter(data()$Year==input$clus) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
     admit_res <- data.frame(list(c(admit)))
     
     
     colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
     
     sum_adm<- admit_res %>% group_by(Diseases) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
     
     
     death<- data()%>% filter(data()$Year==input$clus) %>% select(Diseases,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
     death_res <- data.frame(list(c(death)))
     
     
   
     
     sum_death<- death_res %>% group_by(Diseases) %>% summarise(mean_death=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
     
     
     bin <- cbind(sum_adm,sum_death)
     
     
     bin_f<- bin[-1]
     
     
     bin_ff<- bin_f[-2]
     
     
     a <- scale(bin_ff)
     
     
     a_rest<- data.frame((a))
     output$scat<- renderPlot({
      
       results<- kmeans(a_rest,4,nstart=25)
       fviz_cluster(results,data=a_rest,xlab = "Admitted Patients", ylab = "Death Patients")+theme_classic()
       
     })
     
     output$clusttb<-renderDataTable({
       results<- kmeans(a_rest,4,nstart=25)
       
        
        aj<- bin$Diseases
        abc<-data.frame(results$cluster)
        bind<- cbind(aj,abc)
        colnames(bind)<-c("Diseases","Cluster`")
       datatable(bind,options =list(searching= FALSE))
       
       
       #datatable(bind)
       
     })
#---------------------------------------------------------------------------------------------------------------------------------------------------------     
     
  }) # observe ends
  
  
  
})

# Run the application 
#shinyApp(ui = ui, server = server)
