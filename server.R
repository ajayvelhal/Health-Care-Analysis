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
    return(df)
    
    
  })
  
  
  observe({
    res<- data()%>%filter(data()$Month == input$month & data()$Year == input$year1) %>% select(Diseases,Admitted_Male)
    result<-data.frame(list(c(res)))
    #print(result)
    
    plot2<- data()%>%filter(data()$Disease == input$disease & data()$Year == input$year1) %>% select(Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
    plot2dis<- data.frame(list(c(plot2)))
    #plot2dis$month<- factor(plot2dis$month,levels = month.abb)
    #print(input$month)
    print(plot2dis)
    
    #plot_trans<- as.data.frame(t(plot2dis))
    #print(plot_trans)
    
    #group_by(plot2dis,variable_name="condition")
    
    #plot2dis$facet <- row.names(plot2dis)
    
    #plot2dis.long <- melt(plot2dis,"facet")
    #print(plot2dis.long)
    
    output$plot1<- renderPlot({
      ggplot(result,aes(Diseases,Admitted_Male)) + geom_point() + theme_classic()
      
    })
    
    colours <- c("red", "orange", "blue")
    
    #bp <- barplot(as.data.frame.matrix(plot2dis),main = "My Bar Plot",ylab="Numbers",cex.lab=1.5,cex.main=1.4,beside=TRUE,col = colours)
    
    #barplot(t(plot2dis), beside=TRUE, 
     #       legend.text = TRUE, col = c("red", "green"), 
      #      args.legend = list(x = "topleft", bty = "n", inset=c(-0.05, 0)))
    #print(plot2dis)
    
    output$view_male<- renderPlot({
     ggplot(plot2dis,aes(x=Month,y=Admitted_Male)) + geom_bar(stat="identity") 
    })
    
    output$view_female<- renderPlot({
      ggplot(plot2dis,aes(x=Month,y=Admitted_Female)) + geom_bar(stat="identity") 
    })
    output$view_male_child<- renderPlot({
      ggplot(plot2dis,aes(x=Month,y=Admitted_Male_Child)) + geom_bar(stat="identity") 
    })
    output$view_female_child <- renderPlot({
      ggplot(plot2dis,aes(x=Month,y=Admitted_Female_Child)) + geom_bar(stat="identity") 
    })
    
    #Clustering
    # output$clust <- renderTable({
    #   clustering <- data()
    #   clust_res <- data.frame(list(c(clustering)))
    #   #print(clust_res)
    #   
    #   plot(data()$Admitted_Male ~ data()$Death_Male)
    #   with(clust_res,text(Admitted_Male ~ Death_Male,labels= Diseases))
    # })
    
    #aggregation of admitted persons
    output$aggr1 <- renderPlot({
      aggr<- data()%>% filter(data()$Year =='2017')%>% select(Diseases,input$cat)
      aggr_res <- data.frame(list(c(aggr)))
      glimpse(aggr_res)
      names(aggr_res) <- c("Diseases","Admit")
      aggr_res %>% group_by(Diseases) %>%
        summarise(mean_admit=sum(Admit)) %>%
        ggplot(aes(x = Diseases, y = mean_admit, fill = Diseases)) +ylab(input$cat)+
        geom_bar(stat = "identity") +
        theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
      # summarise(aggr_res,mean_ad_male=mean(Admitted_Male),mean_ad_female=mean(Admitted_Female))
      # print(mean_ad_male,mean_ad_female)
      
     #  if(input$cat=="Admitted_Male"){
     #    aggr_res %>% group_by(Diseases) %>%
     #      summarise(mean_ad_male=mean(Admitted_Male)) %>%
     #      ggplot(aes(x = Diseases, y = mean_ad_male, fill = Diseases)) +
     #      geom_bar(stat = "identity") +
     #      theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
     #  }
     #  else if(input$cat=="Admitted_Female"){
     #    aggr_res %>% group_by(Diseases) %>%
     #      summarise(Female=mean(Admitted_Female)) %>%
     #      ggplot(aes(x = Diseases, y = Female, fill = Diseases)) +
     #      geom_bar(stat = "identity") +
     #      theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
     #  }
     #  else if(input$cat=="Admitted_Male_Child"){
     #     aggr_res %>% group_by(Diseases) %>%
     #     summarise(Male_Child=mean(Admitted_Male_Child)) %>%
     #     ggplot(aes(x = Diseases, y = Male_Child, fill = Diseases)) +
     #     geom_bar(stat = "identity") +
     #     theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
     # }
     #  else if(input$cat=="Admitted_Female_Child"){
     #     aggr_res %>% group_by(Diseases) %>%
     #     summarise(Female_Child=mean(Admitted_Female_Child)) %>%
     #     ggplot(aes(x = Diseases, y = Female_Child, fill = Diseases)) +
     #     geom_bar(stat = "identity") +
     #     theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
     #  }
     #  # 
      #   
      #   ggplot(aes(x = Diseases, y = mean_ad_male, fill = Diseases)) +
      #   geom_bar(stat = "identity") +
      #   theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2)) 
      #   # labs(
      #   #   x = "baseball league",
      #   #   y = "Average home run",
      #   #   title = paste(
      #   #     "Example group_by() with summarise()"
      #   #   ))
      # 
    })
    
    #Aggregation of death persons
    
    output$aggr2<- renderPlot({
      aggr<- data()%>% filter(data()$Year =='2017')%>% select(Diseases,input$cat2)
      aggr_res <- data.frame(list(c(aggr)))
      glimpse(aggr_res)
      names(aggr_res) <- c("Diseases","Death")
      aggr_res %>% group_by(Diseases) %>%
        summarise(mean_death=sum(Death)) %>%
        ggplot(aes(x = Diseases, y = mean_death, fill = Diseases)) +ylab(input$cat2)+
        geom_bar(stat = "identity") +
        theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
    })
    
    #Linear Regression
    
     output$lgr <- renderPlot({
       lgrres <- data()%>%filter(data()$Year == input$rd1) %>% select(Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       lrgs <- data.frame(list(c(lgrres)))
       #glimpse(lrgs)   
       
       #Sum of admitted patients
       sum_adm<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       print(sum_adm)
       
       #Sum of deaths of patients
       sum_death<- lgrres %>% group_by(Month) %>% summarise(mean_death=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       print(sum_death)
       
       
       lgr_bind<- cbind(sum_adm,sum_death)
       print(lgr_bind)
       
       lgr_bind_final<- lgr_bind[-3]
       print(lgr_bind_final)
       
       linear <- lm(mean_adm ~ mean_death,lgr_bind_final)
       print(summary(linear))
       
       ggplot(lgr_bind_final,aes(mean_adm,mean_death)) + geom_point() + geom_smooth(method = "lm")
      
    })
     
     #Pie graph for Admitted
     output$adm<- renderPlot({
       admit<- data()%>% filter(data()$Year==input$year3) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       print(admit_res)
       
       colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
       
       sum_adm<- admit_res %>% group_by(Diseases) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       print(sum_adm)
       
       p<- plot_ly(sum_adm,labels=~Diseases,values= ~mean_adm,type='pie',
       textposition = 'inside',
       textinfo = 'label+percent',
       insidetextfont = list(color = '#FFFFFF'),
       hoverinfo = 'text',
       text = ~paste('$', mean_adm, '%'),
       marker = list(colors = colors,
                     line = list(color = '#FFFFFF', width = 1)),
       #The 'pull' attribute can also be used to create space between the sectors
       showlegend = FALSE) %>%
       layout(title = 'United States Personal Expenditures by Categories in 1960',
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       # chart_link = api_create(p, filename="pie-styled")
       # chart_link
       # a <- sum_adm[-1]
       # print(a)
       # 
       # pie(a,Diseases)
       # pct<- round(sum_adm/sum(sum_adm)*100)
       # lbls<- paste(Diseases,pct)
       # lbls<- paste(labls,"%",sep="")
       # pie(sum_adm,labels = lbls,col=rainbow,main="Overall Admitted Patients")
       # 
       
     })
  }) # observe ends
  
  
  
})

# Run the application 
#shinyApp(ui = ui, server = server)
