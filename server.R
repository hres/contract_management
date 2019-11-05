shinyServer(function(input, output,session) {
  
  tab_list <- NULL
  
  contract_selected<-reactive({
    
    if(input$contract_type=='All'){
      df<-contract
    }
    
    if(input$contract_type=='Resource Based Vehicles'){
      df<-contract%>%filter(`Contract type`=='Resource Based Vehicles')
    }
    
    if(input$contract_type=='SBIPS, Sole Source, Non-ILA, Call Up, Proof of Concept'){
      df<-contract%>%filter(`Contract type`=='SBIPS, Sole Source, Non-ILA, Call Up, Proof of Concept')
    }
    
    return(df)
  })
  
  
  output$active<-renderValueBox({
    n<-contract_selected()%>%
       filter(Category=='Active')%>%
       nrow()
    
    valueBox(n,subtitle=tags$p('Active',style='font-weight: bold;'),icon=icon('check-circle'))
  })
  
  output$anticipated<-renderValueBox({
    n<-contract_selected()%>%
      filter(Category=='Anticipated')%>%
      nrow()
    
    valueBox(n,subtitle=tags$p('Anticipated',style='font-weight: bold;'),icon=icon('tasks'))
  })
  
  output$pending<-renderValueBox({
    n<-contract_selected()%>%
      filter(Category=='Pending')%>%
      nrow()
    
    valueBox(n,subtitle=tags$p('Pending',style='font-weight: bold;'),icon=icon('pause-circle'))
  })
  
  
  output$seats<-renderValueBox({
    n<-sum(contract_selected()$`Seats Available`,na.rm=T)
    
    valueBox(n,subtitle=tags$p('Seats Available',style='font-weight: bold;'),icon=icon('user-friends'))
  })
  
  
  
  contract_value<-reactiveValues()
  
  observe({
    
    df<-contract_selected()
    
    if(input$click=='All'){df<-df}
    
    if(input$click=='Active'){
      df<-contract_selected()%>%filter(Category=='Active')
    }
    
    if(input$click=='Pending'){
      df<-contract_selected()%>%filter(Category=='Pending')
    }
      
    if(input$click=='Anticipated'){
      df<-contract_selected()%>%filter(Category=='Anticipated')
    }
    
  
  contract_value$value<-df
  })
  
  
  
  output$plot1<-renderPlotly({
    df<-contract_value$value%>%
      filter(!is.na(`End date`))
    
    plot_ly(df,x=~`End date`,y=~Remaining,type='scatter',mode='markers',size=~`Seats Available`,
            sizes=c(5,40),
            marker=list(sizemode='diameter',opacity=0.8),
            hoverinfo='text',
            text=~paste('Remaining value:',dollar(Remaining),
                        '<br>Seats Available:', `Seats Available`,
                        '<br>End Date:',`End date`,
                        '<br>OA:',OA))%>%
      layout(xaxis=list(title='Contract end date',gridcolor='#DCDCDC'),
             yaxis=list(title='Remaining value',gridcolor='#DCDCDC'),
             annotations=list(x=c(df$`End date`),
                              y=c(df$Remaining+15000),
                              text=c(df$Contractor),
                              showarrow=F),
             dragmode='select'
      )
    
    
    # plot_ly(df,x=~`End date`,y=~Remaining,text=~Contractor)%>%
    #   add_markers()%>%
    #   add_text(textposition='middle left')%>%
    #   layout(xaxis=list(title='Contract end date',gridcolor='#DCDCDC'),
    #          yaxis=list(title='Remaining value',gridcolor='#DCDCDC'),
    #          showlegend=FALSE)
    #
    
  })
  
  
  
  
  output$table1<-DT::renderDataTable({
    
    df<-contract_value$value%>%
      select(Contractor,Description,`TA/PA`,CA,OA,`Contract value`,Remaining,`End date`,`Seats Available`)%>%
      mutate(`Contract value`=dollar(`Contract value`),
             `End date`=ymd(`End date`))
    
    d<-event_data('plotly_click')
    
    if(is.null(d)){
      
      DT::datatable(df,options=list(scrollX=TRUE))%>%
        DT::formatCurrency('Remaining')
    }
    
    else{
      
    selected_value<-d%>%select(y)%>%collect()
    
    df_select<-contract_value$value%>%
               filter(Remaining==as.double(selected_value))
    
    df<-rbind(df[which(df$Remaining==df_select$Remaining),],df[which(df$Remaining!=df_select$Remaining),])
    
    DT::datatable(df,options=list(scrollX=TRUE))%>%
       DT:: formatStyle('Remaining',target='row',
                    color = DT::styleEqual(df_select$Remaining,rep('red',length(df_select$Remaining))))%>%
       DT:: formatCurrency('Remaining')
    
    }
    
  })
  
  result_tb<-reactive({
    
    query<-sprintf('{ 
  "_source": ["position","stream","responsibility"], 
  "query": {
    "match": {
      "responsibility":{
      "query": "%s",
      "operator":"and"
      } 
    }
  }
}',input$search_text)
    
    
    rs<-Search(conn = conn, index='test_contract',body=query,raw=T,size=500)%>%fromJSON()%>%'['('hits')
    rs<-rs$hits$hits$`_source`
    
    if(!is.null(rs)){rs<-left_join(rs,contract[,c('Contractor','position')])%>%distinct()}
    
    return(rs)
  })
  
  
  output$search_rs<-DT::renderDataTable({
    
    rs<-result_tb()
    DT::datatable(rs,rownames=F)
    
  })
  
  
  output$selected_contract<-DT::renderDataTable({
    selected<-result_tb()$Contractor[input$search_rs_rows_selected]
    
    if(!is.null(selected)){selected<-contract%>%filter(Contractor %in% selected)}
    
    DT::datatable(selected,rownames=F,options=list(scrollX=TRUE))
    
  })
  
  
  
  observeEvent(event_data('plotly_click'),
   {
     d<-event_data('plotly_click')
     if(!is.null(d)){
       
       #code copied from above to get the oa code
       df<-contract_value$value%>%
         select(Contractor,Description,`TA/PA`,CA,OA,`Contract value`,Remaining,`End date`,`Seats Available`)%>%
         mutate(`Contract value`=dollar(`Contract value`),
                `End date`=ymd(`End date`))
       
       selected_value<-d%>%select(y)%>%collect()
       
       df_select<-contract_value$value%>%
         filter(Remaining==as.double(selected_value))
       
       oacode <-df[which(df$Remaining==df_select$Remaining),]$OA
       
       #use oa code to find the proper table to display
       index <- match(oacode, oa)
       df2 <- ds[[index]]
       df2$value <- format(round(as.numeric(df2$value), 2), nsmall=2, big.mark=",")
       
       removeTab(inputId = "tabs", 'OA_details')
       appendTab(inputId = "tabs",
                 tabPanel(
                   'OA_details',
                   DT::renderDataTable(df2,
                                       options=list(scrollX=T))
                 ))
       updateTabsetPanel(session, "tabs", selected = 'Data')
       
       if(!('OA_details' %in% tab_list)){
         tab_list <<- c(tab_list, 'OA_details')
       }
     }
  })
})