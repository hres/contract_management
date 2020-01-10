

ui<-tagList(
    useShinyjs(),
    
    dashboardPage(
    
    dashboardHeader(title='Contract Management'),
    
    dashboardSidebar(width=200,
        sidebarMenu(id='sidebar',
                    
                    selectizeInput('contract_type',label=div(style="color:white",'Select contract type'),
                                   c('All','Resource Based Vehicles','SBIPS, Sole Source, Non-ILA, Call Up, Proof of Concept'))
                    
                    )
    ),
    
    dashboardBody(
        
        shinyDashboardThemes(
            theme = "poor_mans_flatly"
        ),
        
        tabsetPanel(
        
        tabPanel('Summary',
        
        fluidRow(
            valueBoxOutput('active',3),
            valueBoxOutput('anticipated',3),
            valueBoxOutput('pending',3),
            valueBoxOutput('seats',3)
        ),
        
        fluidRow(
            column(12,
            radioButtons('click',label='Select contracts by status',choices=c('All','Active','Pending','Anticipated'),inline=T)
        )),
        
        fluidRow(
            
            tabsetPanel(
                
                tabPanel('Remaining value vs Remaining days',
                         
                         column(12,plotlyOutput('plot1',height='500px')),
                         tags$p('Note: Bubble size indicates available seats number',style="margin-left: 30px;")
                ),
                
                tabPanel('Remaining days vs End date',
                         
                         column(12,plotlyOutput('plot2',height='500px')))
            )
            
            ),
        br(),
        fluidRow(
                    
            tabsetPanel(id='tabs',
                        tabPanel(title='Data',
                        column(12, 
                        DT::dataTableOutput('table1')))
            ))
        
        ),
        
        tabPanel('Search',
                 HTML('<p> This page provides a full-text search engine for job responsibilities listed on site: 
                       <a href=https://www.tpsgc-pwgsc.gc.ca/app-acq/sptb-tbps/categories-eng.html target="_blank">Task-Based Informatics Profesisonal Service </a> and 
                       <a href=https://www.tpsgc-pwgsc.gc.ca/app-acq/spc-cps/spctscc-tspscc-eng.html target="_blank">Task and solutions professional services </a> <br>'  ),
                 searchInput('search_text',
                             label='Enter your search',
                             placeholder='Please type your key words to start searching, for example: project management',
                             btnSearch=icon('search'),
                             width='100%'),
                 DT::dataTableOutput('search_rs'),
                 DT::dataTableOutput('selected_contract'))
        
        )
        
    )
 )
)