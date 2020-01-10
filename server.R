shinyServer(function(input, output,session) {
  
  tab_list <- NULL
  
  
  #fetch data from SQL database: using reactivePoll to pull most-up-to-date data
  data<-reactivePoll(5000,session=NULL,
                     
                     checkFunc = function(){
                       
                       query <- dbSendQuery(
                         con,
                         paste0('SELECT MAX(created_at) as lastCreated FROM "TA_summary" ;')
                       )
                       
                       lastFeedback <- dbFetch(query, -1)
                     },
                     
                     valueFunc=function(){
                       
                       tbl(con,'contract_vehicle_overall')%>%collect()
                       
                     }
                     
  )
  
  data2<-reactivePoll(5000,session=NULL,
                     
                     checkFunc = function(){
                       
                       query <- dbSendQuery(
                         con,
                         paste0('SELECT MAX(created_at) as lastCreated FROM "TA_summary" ;')
                       )
                       
                       lastFeedback <- dbFetch(query, -1)
                     },
                     
                     valueFunc=function(){
                       
                       tbl(con,'TA_summary')%>%collect()
                       
                     }
                     
  )
  
  contract<-reactive({
    contract<-data()
    
    for(i in 1:nrow(contract)){
      if(!is.na(contract$OA[i])){
        index_c <- match(contract$OA[i], spent$OA)
        contract$Remaining[i] <- contract$`Contract value`[i] - spent$used[index_c]
      }
    }
    
    current_day<-as.POSIXct.Date(Sys.Date())
    #contract<-read_excel('contract.xlsx',1)
    contract$`Days Remaining`<-as.numeric(contract$`End date`-current_day)
    
    contract<-contract%>%
      mutate(position=df$position[amatch(Contractor,df$position,method='cosine',maxDist=1)])
    
    return(contract)
    
  })
  
  
  
  
 
  
  ta_summary<-reactive({
    
    ta_summary<-data2()
    
    ta_summary<-ta_summary%>%dplyr::filter(OA %in% as.character(contract()$OA))%>%
      filter(!is.na(OA))%>%
      select(OA,Resource,`Resource Category`,`TA Number`,`Total Days`,`Days Utilized`,`Days Remaining`,`Start Date`,`Delivery Date`,Perdiem)
    
    ta_summary[,4:6]<-lapply(ta_summary[,4:6],round,2)
    
    ta_summary
  })
    
    
  #ta_summary[,c('Start Date','Delivery Date')]<-lapply(ta_summary[,c('Start Date','Delivery Date')],as.Date,format='%Y.%m.%d')
  
  

  contract_selected<-reactive({
    
    if(input$contract_type=='All'){
      df<-contract()
    }
    
    if(input$contract_type=='Resource Based Vehicles'){
      df<-contract()%>%filter(`Contract type`=='Resource Based Vehicles')
    }
    
    if(input$contract_type=='SBIPS, Sole Source, Non-ILA, Call Up, Proof of Concept'){
      df<-contract()%>%filter(`Contract type`=='SBIPS, Sole Source, Non-ILA, Call Up, Proof of Concept')
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
      filter(!is.na(Remaining))
    
    plot_ly(df,x=~`Days Remaining`,y=~Remaining,type='scatter',mode='markers',size=~`Seats Available`,
            sizes=c(5,30),
            marker=list(sizemode='diameter',opacity=0.5),
            hoverinfo='text',
            text=~paste('Contractor:', Contractor,
                        '<br>Remaining value:',dollar(Remaining),
                        '<br>Seats Available:', `Seats Available`,
                        '<br>End Date:',`End date`,
                        '<br>OA:',OA))%>%
      layout(xaxis=list(title='Days remaining on the contract',gridcolor='#DCDCDC'),
             yaxis=list(title='Remaining value',gridcolor='#DCDCDC'),
             annotations=list(x=c(df$`Days Remaining`+days(10)),
                              y=c(df$Remaining-30000),
                              text=ifelse(nchar(df$Contractor)>18,paste0(substring(df$Contractor,1,18),'...'),df$Contractor),
                              showarrow=F,
                              font=list(size=12)),
             dragmode='select')
      })
    
    
    output$plot2<-renderPlotly({
      df<-contract_value$value%>%
        filter(!is.na(Remaining))
      
      plot_ly(df,x=~`End date`,y=~`Days Remaining`,type='scatter',mode='markers',
              hoverinfo='text',
              text=~paste('Contractor:', Contractor,
                          '<br>Remaining value:',dollar(Remaining),
                          '<br>Seats Available:', `Seats Available`,
                          '<br>End Date:',`End date`,
                          '<br>OA:',OA))%>%
        layout(xaxis=list(title='End Date',gridcolor='#DCDCDC'),
               yaxis=list(title='Days remaining on the contract',gridcolor='#DCDCDC'))
               # annotations=list(x=df$`End date`,
               #                  y=df$`Days Remaining`,
               #                  text=ifelse(nchar(df$Contractor)>18,paste0(substring(df$Contractor,1,18),'...'),df$Contractor),
               #                  showarrow=F,
               #                  font=list(size=12)))
               
        })
    
    # plot_ly(df,x=~`End date`,y=~Remaining,text=~Contractor)%>%
    #   add_markers()%>%
    #   add_text(textposition='middle left')%>%
    #   layout(xaxis=list(title='Contract end date',gridcolor='#DCDCDC'),
    #          yaxis=list(title='Remaining value',gridcolor='#DCDCDC'),
    #          showlegend=FALSE)
    #
    

  
  
  
  
  output$table1<-DT::renderDataTable({
    
    ta_summary<-ta_summary()
    
    df<-contract_value$value%>%
      select(Contractor,`TA/PA`,CA,OA,`Contract value`,Remaining,`End date`,`Seats Available`)%>%
      mutate(Remaining = round(Remaining,2))%>%
      mutate(`Contract value`=dollar(`Contract value`),
             `End date`=ymd(`End date`))
    
    new_row<-df$OA[!df$OA %in% ta_summary$OA]
    new_row<-new_row[!is.na(new_row)]
    new_row<-data.frame(OA=new_row,
                        Resource=NA,
                        `Resource Category`=NA,
                        `TA Number`=NA,
                        `Total Days`=NA,
                        `Days Utilized`=NA,
                        `Days Remaining`=NA,
                        `Start Date`=NA,
                        `Delivery Date`=NA,
                        Perdiem=NA, check.names=F)
    
    ta_summary_complete<-rbind(ta_summary,new_row)
    #group by OA and collapse to list:
    ta_ls<-ta_summary_complete%>%
      group_split(OA,keep=FALSE)%>%
      setNames(sort(unique(ta_summary_complete$OA)))
    
    #sort each table by days left
    ta_ls<-map(ta_ls,arrange,desc(`Days Remaining`))
    
    
    d<-event_data('plotly_click')
    
    if(is.null(d)){
      
      
      ta_ls<- ta_ls[match(df$OA[!is.na(df$OA)],names(ta_ls))]
      ## merge the row details
      ta_ls <- lapply(ta_ls, purrr::transpose)
      ## dataframe for the datatable
      
     
      Dat <- cbind(" " = "&oplus;", df)
      Dat$`_details`<-NA
      Dat$`_details`[!is.na(Dat$OA)]<-I(ta_ls)
      
      
      DT::datatable(Dat, callback = callback, escape = -2,
                    options = list(
                      scrollX=TRUE,
                      columnDefs = list(
                        list(visible = FALSE, targets = ncol(Dat)),
                        list(orderable = FALSE, className = 'details-control', targets = 1),
                        list(className = "dt-center", targets = "_all")
                      )
                    ))%>%
        DT::formatCurrency('Remaining')
    }
    
    else{
      
    selected_value<-d%>%select(y)%>%collect()
    
    df_select<-df%>%
               filter(Remaining==as.double(selected_value))
    
    df<-rbind(df[which(df$Remaining==df_select$Remaining),],df[which(df$Remaining!=df_select$Remaining),])
    
    
    ta_ls<- ta_ls[match(df$OA[!is.na(df$OA)],names(ta_ls))]
    
    Dat <- cbind(" " = "&oplus;", df)
    Dat$`_details`<-NA
    Dat$`_details`[!is.na(Dat$OA)]<-I(ta_ls)
    
    DT::datatable(Dat, callback = callback, escape = -2,
                  options = list(
                    scrollX=TRUE,
                    columnDefs = list(
                      list(visible = FALSE, targets = ncol(Dat)),
                      list(orderable = FALSE, className = 'details-control', targets = 1),
                      list(className = "dt-center", targets = "_all")
                    )
                  ))%>%
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
    
    
    rs<-Search(index='test_contract',body=query,raw=T,size=500)%>%fromJSON()%>%'['('hits')
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
  
  
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
  
  
  # observeEvent(event_data('plotly_click'),
  #  {
  #    d<-event_data('plotly_click')
  #    if(!is.null(d)){
  #      
  #      #code copied from above to get the oa code
  #      df<-contract_value$value%>%
  #        select(Contractor,Description,`TA/PA`,CA,OA,`Contract value`,Remaining,`End date`,`Seats Available`)%>%
  #        mutate(`Contract value`=dollar(`Contract value`),
  #               `End date`=ymd(`End date`))
  #      
  #      selected_value<-d%>%select(y)%>%collect()
  #      
  #      df_select<-contract_value$value%>%
  #        filter(Remaining==as.double(selected_value))
  #      
  #      oacode <-df[which(df$Remaining==df_select$Remaining),]$OA
  #      
  #      #use oa code to find the proper table to display
  #      index <- match(oacode, oa)
  #      df2 <- ds[[index]]
  #      df2$value <- format(round(as.numeric(df2$value), 2), nsmall=2, big.mark=",")
  #      
  #      removeTab(inputId = "tabs", 'OA_details')
  #      appendTab(inputId = "tabs",
  #                tabPanel(
  #                  'OA_details',
  #                  DT::renderDataTable(df2,
  #                                      options=list(scrollX=T))
  #                ))
  #      updateTabsetPanel(session, "tabs", selected = 'Data')
  #      
  #      if(!('OA_details' %in% tab_list)){
  #        tab_list <<- c(tab_list, 'OA_details')
  #      }
  #    }
  # })
})