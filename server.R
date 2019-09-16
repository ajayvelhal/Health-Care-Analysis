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
    updateSelectInput(session,inputId = 'diss', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis1', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis2', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    updateSelectInput(session,inputId = 'dis3', label='Select Disease', choices = df$Diseases, selected = df$Diseases)
    return(df)
    
    
  })
  
  
  observe({
    
    output$home<- renderPlotly({
      sum_dis<- data() %>% group_by(Month) %>% summarise(mean_male=mean(Admitted_Male),mean_female=mean(Admitted_Female),mean_child_male=mean(Admitted_Male_Child),mean_child_female=mean(Admitted_Female_Child))
      #print(sum_dis)
      dis<-data.frame(sum_dis)
      sum_admm <- dis %>% group_by(Month) %>% summarise(sum_add=sum(mean_male,mean_female,mean_child_male,mean_child_female))
      print(sum_admm)
      
      
      
      
      
      print(sum_admm)
      # p <- plot_ly(sum_admm,x = ~Month, y = ~sum_add, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
      #   layout(xaxis = list(title = 'Months'),
      #          yaxis = list(title = 'Average'))
      Month_ordered<- ordered(sum_admm$Month,month.name)
      
      p <- plot_ly(sum_admm,x = ~sum_add, y = ~Month_ordered, type = 'bar', orientation = 'h',color = 'black')%>%
        layout(xaxis = list(title = 'Average Admitted Per Month'),
               yaxis = list(title = 'Months'))
      
      
    })
    
    output$home1<- renderTable({
      sum_dis<- data() %>% group_by(Diseases) %>% summarise(sum_all=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
      #print(sum_dis)
      
      print(sum_dis)
      
      data1<- sum_dis %>% arrange(desc(sum_all))
      print(head(data1,10))
    })
    
    output$total_patients_till_date_male<- renderValueBox({
      male<-data()
      #print(male)
      
      male%>%tally(male$Admitted_Male)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Males",color = 'purple')
    })
    
    output$total_patients_till_date_female<- renderValueBox({
      female<-data()
      #print(male)
      
      female%>%tally(female$Admitted_Female)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Females",color = 'purple')
    })
    output$total_patients_till_date_male_child<- renderValueBox({
      male_child<-data()
      #print(male)
      
      male_child%>%tally(male_child$Admitted_Male_Child)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Male Child",color = 'purple')
    })
    output$total_patients_till_date_female_child<- renderValueBox({
      female_child<-data()
      #print(male)
      
      female_child%>%tally(female_child$Admitted_Female_Child)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Number of Admitted Female Child",color = 'purple')
    })
    output$s1<- renderPlot({
      paste("This")
    })
    #seasonal
    
    if(input$weather=='winter'){
      win<-data()%>%filter(data()$Month=='October')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res<-data.frame(list(c(win)))
      print(win_res)
      
      win1<-data()%>%filter(data()$Month=='November')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res1<-data.frame(list(c(win1)))
      print(win_res1)
      
      win2<-data()%>%filter(data()$Month=='December')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res2<-data.frame(list(c(win2)))
      print(win_res2)
      
      win3<-data()%>%filter(data()$Month=='January')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      win_res3<-data.frame(list(c(win3)))
      print(win_res3)
      
      sum_win<- rbind(win_res,win_res1,win_res2,win_res3)
      print(sum_win)
      add<- sum_win%>% group_by(Diseases)%>% summarise(sum_sum1=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      print(add)
      data1<- add %>% arrange(desc(sum_sum1))
      resultseason <- data.frame(list(c(data1)))
      print(head(data1,10))
      
      #add_frame<- order(data.frame(add$sum_sum1))
      # print(add_frame)
      output$season12<- renderPlotly({
        
        # ggplot(resultseason,aes(x = Diseases,y = sum_sum1)) + 
        #   geom_bar(stat="identity", fill="steelblue") + theme_light()
        p<-plot_ly(resultseason,x = ~sum_sum1, y = ~Diseases, type = 'bar',orientation='h',color = 'black')%>%
          layout(xaxis = list(title = 'Patients Count'),
                 yaxis = list(title = 'Diseasess'))
        
      })
      
      output$home2<- renderTable({
        print(head(data1,10))
      })
    }
    else if(input$weather=='Summer'){
      summ<-data()%>%filter(data()$Month=='February')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res<-data.frame(list(c(summ)))
      #print(summ_res)
      
      summ1<-data()%>%filter(data()$Month=='March')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res1<-data.frame(list(c(summ1)))
      #print(summ_res1)
      
      summ2<-data()%>%filter(data()$Month=='April')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res2<-data.frame(list(c(summ2)))
      #print(summ_res2)
      
      summ3<-data()%>%filter(data()$Month=='May')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      summ_res3<-data.frame(list(c(summ3)))
      #print(summ_res3)
      
      sum_sum<- rbind(summ_res,summ_res1,summ_res2,summ_res3)
      #print(sum_sum)
      
      add<- sum_sum%>% group_by(Diseases)%>% summarise(sum_sum1=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      print(add)
      data1<- add %>% arrange(desc(sum_sum1))
      resultseason <- data.frame(list(c(data1)))
      print(head(data1,10))
      
      #add_frame<- order(data.frame(add$sum_sum1))
      # print(add_frame)
      output$season12<- renderPlotly({
        print("ajay ")
        # ggplot(resultseason,aes(x = Diseases,y = sum_sum1)) + 
        #   geom_bar(stat="identity", fill="steelblue") + theme_light()
        p<-plot_ly(resultseason,x = ~sum_sum1, y = ~Diseases, type = 'bar',orientation='h',color = 'black')%>%
           layout(xaxis = list(title = 'Average Admitted Per Month'),
                  yaxis = list(title = 'Months'))
      })
      
      output$home2<- renderTable({
        print(head(data1,10))
      })
      
      
    }
    
    
    else if(input$weather=='monsoon'){
      mon<-data()%>%filter(data()$Month=='June')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res<-data.frame(list(c(mon)))
      #print(mon_res)
      
      mon1<-data()%>%filter(data()$Month=='July')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res1<-data.frame(list(c(mon1)))
      #print(mon_res1)
      
      mon2<-data()%>%filter(data()$Month=='August')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res2<-data.frame(list(c(mon2)))
      #print(mon_res2)
      
      mon3<-data()%>%filter(data()$Month=='September')%>% select(Diseases,Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
      mon_res3<-data.frame(list(c(mon3)))
      #print(mon_res3)
      
      sum_mon<- rbind(mon_res,mon_res1,mon_res2,mon_res3)
      #print(sum_mon)
      
      add<- sum_mon%>% group_by(Month)%>% summarise(sum_soon=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      print(add)
      add<- sum_mon%>% group_by(Diseases)%>% summarise(sum_sum1=sum(Admitted_Male,Admitted_Female, Admitted_Male_Child, Admitted_Female_Child))
      print(add)
      data1<- add %>% arrange(desc(sum_sum1))
      resultseason <- data.frame(list(c(data1)))
      print(head(data1,10))
      
      #add_frame<- order(data.frame(add$sum_sum1))
      # print(add_frame)
      output$season12<- renderPlotly({
        print("ajay ")
        # ggplot(resultseason,aes(x = Diseases,y = sum_sum1)) + 
        #   geom_bar(stat="identity", fill="steelblue") + theme_light()
        p<-plot_ly(resultseason,x = ~sum_sum1, y = ~Diseases, type = 'bar',orientation='h',color = 'black')%>%
          layout(xaxis = list(title = 'Average Admitted Per Month'),
                 yaxis = list(title = 'Months'))
      })
      
      output$home2<- renderTable({
        print(head(data1,10))
      })
    }
   
    
    
    plot2<- data()%>%filter(data()$Disease == input$disease & data()$Year == input$year2) %>% select(Month,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
    plot2dis<- data.frame(list(c(plot2)))
    #plot2dis$month<- factor(plot2dis$month,levels = month.abb)
    #print(input$month)
    #print(plot2dis)
    
    Month_ordered <- ordered(plot2dis$Month,month.name)
    
    #plot_trans<- as.data.frame(t(plot2dis))
    #print(plot_trans)
    
    #group_by(plot2dis,variable_name="condition")
    
    #plot2dis$facet <- row.names(plot2dis)
    
    #plot2dis.long <- melt(plot2dis,"facet")
    #print(plot2dis.long)
    
    # output$plot1<- renderPlot({
    #   ggplot(result,aes(Diseases,Admitted_Male)) + geom_point() + theme_classic()
    #   
    # })
    #Linear Regression Reference
    lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
    lrgs <- data.frame(list(c(lgrres)))
    #glimpse(lrgs)   
    
    #Sum of admitted patients
    # sum_adm<- lgrres %>% group_by(Month,Diseases) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
    # print(sum_adm)
    # 
    sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
    #print(sum_dis)
    # class(Month)
    # 
    # levels(Month)
    # tb<- full_join(sum_adm,sum_dis,by="Month")
    # print(tb)
    sum_dis$Month=as.numeric(sum_dis$Month)
    # sum_adm$Diseases=as.numeric(sum_adm$Diseases)
    #str(sum_dis)
    # print(sum_adm)
    # sc<- scale(sum_adm)
    # print(sc)
    # 
    # sc_frame<- data.frame(sc)
    #Month_ordered <- ordered(lgrs$Month,month.name)
    #Sum of deaths of patients
    # sum_death<- lgrres %>% group_by(Month) %>% summarise(mean_death=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
    # print(sum_death)
    # 
    # 
    # lgr_bind<- cbind(sum_adm,sum_dis)
    # print(lgr_bind)
    # 
    # lgr_bind_final<- lgr_bind[-3]
    # print(lgr_bind_final)
    
    colours <- c("red", "orange", "blue")
    
    #bp <- barplot(as.data.frame.matrix(plot2dis),main = "My Bar Plot",ylab="Numbers",cex.lab=1.5,cex.main=1.4,beside=TRUE,col = colours)
    
    #barplot(t(plot2dis), beside=TRUE, 
     #       legend.text = TRUE, col = c("red", "green"), 
      #      args.legend = list(x = "topleft", bty = "n", inset=c(-0.05, 0)))
    #print(plot2dis)
    
    output$view_male<- renderPlot({
     ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Male)) + geom_bar(stat="identity") + xlab("Month") +
        ylab("Admitted Male Patients") 
    })
    
    output$view_female<- renderPlot({
      ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Female)) + geom_bar(stat="identity") + xlab("Month") +
        ylab("Admitted Female Patients") 
    })
    output$view_male_child<- renderPlot({
      ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Male_Child)) + geom_bar(stat="identity") + xlab("Month") +
        ylab("Admitted Male-Child Patients") 
    })
    output$view_female_child <- renderPlot({
      ggplot(plot2dis,aes(x=Month_ordered,y=Admitted_Female_Child)) + geom_bar(stat="identity") + xlab("Month") +
        ylab("Admitted Female-Child Patients") 
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
    #Seasonal Diseases
    output$s1<- renderTable({
      
    })
    #aggregation of admitted persons
    output$aggr1 <- renderPlot({
      aggr<- data()%>% filter(data()$Year =='2017')%>% select(Diseases,input$cat)
      aggr_res <- data.frame(list(c(aggr)))
      glimpse(aggr_res)
      names(aggr_res) <- c("Diseases","Admit")
      aggr_res %>% group_by(Diseases) %>%
        summarise(mean_admit=sum(Admit)) %>%
        ggplot(aes(x = Diseases, y = mean_admit, fill = Diseases)) +ylab("Count")+
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
        ggplot(aes(x = Diseases, y = mean_death, fill = Diseases)) +ylab('Count')+
        geom_bar(stat = "identity") +
        theme(axis.text.x=element_text(size=15, angle=90,hjust = 0.95,vjust=0.2))
    })
    
    #Linear Regression
    
     output$lgr <- renderPlot({
       lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       lrgs <- data.frame(list(c(lgrres)))
       #glimpse(lrgs)   
       
       #Sum of admitted patients
       # sum_adm<- lgrres %>% group_by(Month,Diseases) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       # print(sum_adm)
       # 
        sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
        print(sum_dis)
       # class(Month)
       # 
       # levels(Month)
       # tb<- full_join(sum_adm,sum_dis,by="Month")
       # print(tb)
       sum_dis$Month=as.numeric(sum_dis$Month)
       # sum_adm$Diseases=as.numeric(sum_adm$Diseases)
       str(sum_dis)
       # print(sum_adm)
       # sc<- scale(sum_adm)
       # print(sc)
       # 
       # sc_frame<- data.frame(sc)
       #Month_ordered <- ordered(lgrs$Month,month.name)
       #Sum of deaths of patients
       # sum_death<- lgrres %>% group_by(Month) %>% summarise(mean_death=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       # print(sum_death)
       # 
       # 
       # lgr_bind<- cbind(sum_adm,sum_dis)
       # print(lgr_bind)
       # 
       # lgr_bind_final<- lgr_bind[-3]
       # print(lgr_bind_final)
       
       
       # #  
       
       # tb$Month = as.factor(tb$Month)
       # str(tb)
       # b<- ordered(tb$Month,month.abb)
       # print(b)
       
    
       ggplot(sum_dis,aes(Month,mean_adm)) + geom_point() + geom_smooth(method = "lm")
      
       
    })
     output$txt<- renderPrint({
       lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       lrgs <- data.frame(list(c(lgrres)))
       #glimpse(lrgs)   
       
       #Sum of admitted patients
       # sum_adm<- lgrres %>% group_by(Month,Diseases) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       # print(sum_adm)
       # 
       sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       #print(sum_dis)
       # class(Month)
       # 
       # levels(Month)
       # tb<- full_join(sum_adm,sum_dis,by="Month")
       # print(tb)
       sum_dis$Month=as.numeric(sum_dis$Month)
       # sum_adm$Diseases=as.numeric(sum_adm$Diseases)
       #str(sum_dis)
       linear <- lm(mean_adm~Month,sum_dis)
       print(summary(linear))
       #print(summary(predict(linear,sum_dis,interval="predict")))
       #print(predict(lm((Month~mean_adm), sum_dis, interval = "prediction")))
     })
     output$txt1<- renderPrint({
       lgrres <- data()%>%filter(data()$Diseases == input$diss & data()$Year==input$yearrr) %>% select(Month,Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       lrgs <- data.frame(list(c(lgrres)))
       #glimpse(lrgs)   
       
       #Sum of admitted patients
       # sum_adm<- lgrres %>% group_by(Month,Diseases) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       # print(sum_adm)
       # 
       sum_dis<- lgrres %>% group_by(Month) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       #print(sum_dis)
       # class(Month)
       # 
       # levels(Month)
       # tb<- full_join(sum_adm,sum_dis,by="Month")
       # print(tb)
       sum_dis$Month=as.numeric(sum_dis$Month)
       # sum_adm$Diseases=as.numeric(sum_adm$Diseases)
       #str(sum_dis)
       linear <- lm(mean_adm~Month,sum_dis)
       #print(summary(linear))
       print(predict(linear,Month='12',interval="predict"))
     })
     
     #Pie graph for Admitted
     output$adm<- renderPlotly({
       admit<- data()%>% filter(data()$Year==input$year3) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       print(admit_res)
       
       colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
       
       sum_adm<- admit_res %>% group_by(Diseases) %>% summarise(mean_male=mean(Admitted_Male),mean_female=mean(Admitted_Female),mean_child_male=mean(Admitted_Male_Child),mean_child_female=mean(Admitted_Female_Child))
       print(sum_adm)
       
       sum_admm <- sum_adm %>% group_by(Diseases) %>% summarise(sum_add=sum(mean_male,mean_female,mean_child_male,mean_child_female))
       print(sum_admm)
       # b<- sum_adm %>% group_by(Diseases) %>% summarise_at(vars(mean_adm),mean)
       # print(b)
       
         p<- plot_ly(sum_admm,labels=~Diseases,values= ~sum_add,type='pie',
         textposition = 'inside',
         textinfo = 'label+percent',
         insidetextfont = list(color = '#FFFFFF'),
         hoverinfo = 'text',
         text = ~paste(Diseases),
         marker = list(colors = colors,
                       line = list(color = '#FFFFFF', width = 1)),
         #The 'pull' attribute can also be used to create space between the sectors
         showlegend = TRUE) %>%
         layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
        
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
     
     output$total_patients<- renderValueBox({
       admit<- data()%>% filter(data()$Year==input$year3) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       #print(admit_res)
       
       sum_adm<- admit_res %>% group_by(input$year3) %>% summarise(mean_male=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       print(sum_adm)
         
       sum_adm%>%tally(sum_adm$mean_male)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "No. of Total Admitted Patients",color = 'purple')
     })
     
     output$total_patients_dead<- renderValueBox({
       admit<- data()%>% filter(data()$Year==input$year4) %>% select(Diseases,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       #print(admit_res)
       
       sum_adm<- admit_res %>% group_by(input$year4) %>% summarise(mean_male=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       print(sum_adm)
       
       sum_adm%>%tally(sum_adm$mean_male)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "No. of Total Deaths Patients",color = 'purple')
     })
     
     output$dea<- renderPlotly({
       death<- data()%>% filter(data()$Year==input$year4) %>% select(Diseases,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       death_res <- data.frame(list(c(death)))
       print(death_res)
       
       colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
       
       sum_death<- death_res %>% group_by(Diseases) %>% summarise(mean_death=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       print(sum_death)
       
       p<- plot_ly(sum_death,labels=~Diseases,values= ~mean_death,type='pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(Diseases),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = TRUE) %>%
         layout(
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
     
     #monthly trend
     
     output$year_male<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis) %>% select(Year,Month,Admitted_Male)
       adm_res<- data.frame(list(c(adm)))
       print(adm_res)
       
       # R <- sort(adm_res$Month)
       
       # R <- adm_res%>% arrange(match(Month,month.name))
       # print(R)
       
       Month_ordered <- ordered(adm_res$Month,month.name)
      #ggplot(adm_res) + geom_line(mapping = aes(x=Month,y=Admitted_Male,colour=variable))
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Male, type = 'scatter', mode = 'lines',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
         
       
         layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     output$year_female<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis1) %>% select(Year,Month,Admitted_Female)
       adm_res<- data.frame(list(c(adm)))
       print(adm_res)
       
       # R <- sort(adm_res$Month)
       
       # R <- adm_res%>% arrange(match(Month,month.name))
       # print(R)
       
       Month_ordered <- ordered(adm_res$Month,month.name)
       #ggplot(adm_res) + geom_line(mapping = aes(x=Month,y=Admitted_Male,colour=variable))
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Female, type = 'scatter', mode = 'lines',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
         
         
         layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     
     output$year_male_child<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis2) %>% select(Year,Month,Admitted_Male_Child)
       adm_res<- data.frame(list(c(adm)))
       print(adm_res)
       
       # R <- sort(adm_res$Month)
       
       # R <- adm_res%>% arrange(match(Month,month.name))
       # print(R)
       
       Month_ordered <- ordered(adm_res$Month,month.name)
       #ggplot(adm_res) + geom_line(mapping = aes(x=Month,y=Admitted_Male,colour=variable))
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Male_Child, type = 'scatter', mode = 'lines',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
         
         
         layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     output$year_female_child<- renderPlotly({
       adm<- data()%>% filter(data()$Disease==input$dis3) %>% select(Year,Month,Admitted_Female_Child)
       adm_res<- data.frame(list(c(adm)))
       print(adm_res)
       
       # R <- sort(adm_res$Month)
       
       # R <- adm_res%>% arrange(match(Month,month.name))
       # print(R)
       
       Month_ordered <- ordered(adm_res$Month,month.name)
       #ggplot(adm_res) + geom_line(mapping = aes(x=Month,y=Admitted_Male,colour=variable))
       p <- plot_ly(adm_res, x = ~Month_ordered, y = ~Admitted_Female_Child, type = 'scatter', mode = 'lines',linetype = ~Year ,showlegend= TRUE,width = 4,line = list(color = 'rgb(22, 96, 167)'))%>% 
              layout(width=1043,
                xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Month'),
                yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE,title='Admitted Count'))
       
     })
     
     output$day<- renderPlotly({
       admit<- data()%>% filter(data()$Year==input$year9) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       print(admit_res)
       
       sum_adm <- admit_res%>% group_by(input$year9,Diseases) %>% summarise(sum_admm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       sum_year<- admit_res%>% group_by(input$year9) %>% summarise(sum_admm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       sum_death<- admit_res%>% group_by(input$year9) %>% summarise(sum_admm=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       print(sum_adm)
       print(sum_year)
       #print(sum_death)
       
       b<- sum_adm[-1:-2]
       print(b)
       
       c<- b/365
       print(c)
       
       roun<- round(c,digits=2)
       print(roun)
       print(sum_adm$sum_admm <- ifelse(is.na(roun$sum_admm), sum_adm$sum_adm, roun$sum_admm))
       
       print(sum_adm)
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
       
       # d<- sum_adm[-3]
       # print(d)
       # 
       # e<- cbind(d,c)
       # print(e)
       
     })
     
     output$total_patients_daily <- renderValueBox({
       admit<- data()%>% filter(data()$Year==input$year9) %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       print(admit_res)
       
       sum_year<- admit_res%>% group_by(input$year9) %>% summarise(sum_admm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       
       b<- sum_year[-1]
       print(b)
       
       c<- b/365
       print(c)
       
       print(sum_year$sum_admm <- ifelse(is.na(c$sum_admm), sum_year$sum_admm, c$sum_admm))
       
       print(sum_year)
       
       sum_year%>%tally(sum_year$sum_admm)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle = "Total Admitted Patients",color = 'purple',width = 12)
       
     })
     
     #Clustering final
     output$scat<- renderPlot({
       admit<- data()%>% filter(data()$Year=='2018') %>% select(Diseases,Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child)
       admit_res <- data.frame(list(c(admit)))
       #print(admit_res)
       
       colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
       
       sum_adm<- admit_res %>% group_by(Diseases) %>% summarise(mean_adm=sum(Admitted_Male,Admitted_Female,Admitted_Male_Child,Admitted_Female_Child))
       #print(sum_adm)
       #print(mean_adm)
       
       death<- data()%>% filter(data()$Year=='2018') %>% select(Diseases,Death_male,Death_Female,Death_Male_Child,Death_Female_Child)
       death_res <- data.frame(list(c(death)))
       #print(death_res)
       
       #colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
       
       sum_death<- death_res %>% group_by(Diseases) %>% summarise(mean_death=sum(Death_male,Death_Female,Death_Male_Child,Death_Female_Child))
       #print(sum_death)
       
       bin <- cbind(sum_adm,sum_death)
       print(bin)
       
       bin_f<- bin[-1]
       #print(bin_f)
       
       bin_ff<- bin_f[-2]
       print(bin_ff)
       
       a <- scale(bin_ff)
       print(head(a))
       
       a_rest<- data.frame((a))
       print(head(a_rest))
       # a<-apply(sum_adm,2,mean)
       # s<-apply(sum_adm,2,sd)
       # sum_adm<-scale(sum_adm,a,s)
       # 
       # distance <- dist(sum_adm)
       # print(distance,digits = 5)
       #ggplot(a_rest,aes(a_rest$mean_adm,a_rest$mean_death)) + geom_point() + theme_classic()
       #s <- seq(-1, 10)
       #p <- plot_ly(data = a_rest, x = ~a_rest$mean_adm, y = ~a_rest$mean_death)
       
       results<- kmeans(a_rest,4,nstart=25)
       print(results)
       
       print(results$size)
       
       print(results$cluster)
       print(table(bin$Diseases,results$cluster))
       # # set.seed(123)
       # # k.max<- 15
       # # data<- a_rest
       # # 
       # # wss <- sapply(1:k.max, function(k){
       # #   kmeans(data,k,nstart=50,iter.max = 15)$tot.withinss
       # # })
       # # wssplot(1:k.max, wss,
       # #         type="b", pch = 19, frame = FALSE, 
       # #         xlab="Number of clusters K",
       # #         ylab="Total within-clusters sum of squares")
       # 
       # # fviz_nbclust(a_rest, kmeans, method = "wss") +
       # #   geom_vline(xintercept = 4, linetype = 2)+
       # #   labs(subtitle = "Elbow method")
       # # fviz_nbclust(a_rest, kmeans, method = "silhouette")+
       # #   labs(subtitle = "Silhouette method")
       # set.seed(123)
       # fviz_nbclust(a_rest, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
       #   labs(subtitle = "Gap statistic method")
       #clusplot(a_rest,results$cluster,main = '2D Representation',labels = 2,lines = 0)
       fviz_cluster(results,data=a_rest)
       # plot3d(pcdf$scores, col=newdf$K)#Create a 3D plot
     })
  }) # observe ends
  
  
  
})

# Run the application 
#shinyApp(ui = ui, server = server)
