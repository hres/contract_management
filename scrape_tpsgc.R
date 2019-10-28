library(rvest)
library(dplyr)

#scrape tpsgc website to get descriptions for positions:

url1<-'https://www.tpsgc-pwgsc.gc.ca/app-acq/spc-cps/spctscc-tspscc-eng.html'

url2<-'https://www.tpsgc-pwgsc.gc.ca/app-acq/sptb-tbps/categories-eng.html'

links<-url2%>%
       read_html()%>%
       html_nodes(xpath='//main/ul/li/a')%>%
       html_attr('href')
       
links<-gsub('\\#.+','',links)%>%unique()

links<-paste0('http://tpsgc-pwgsc.gc.ca',links)

stream<-url1%>%
        read_html()%>%
        html_nodes(xpath='//main/h2')%>%
        html_text()

stream2<-url2%>%
  read_html()%>%
  html_nodes(xpath='//main/p/a')%>%
  html_text()

extract_content<-function(link){
#for each link, parse content
job_title<-link%>%
           read_html()%>%
           html_nodes(xpath='//main/h2')%>%
           html_text()

content_list<-link%>%
         read_html()%>%
         html_nodes(xpath='//main/ul')

content<-list()

for (i in seq_along(content_list)){
  content[[i]]<-data.frame(position=job_title[i],
                           responsibility=content_list[[i]]%>%
                                          html_nodes('li')%>%
                                          html_text(),stringsAsFactors = F)
}


return(content)

}

extract_content2<-function(link){
  #for each link, parse content
  job_title<-link%>%
    read_html()%>%
    html_nodes(xpath='//main/h2')%>%
    html_text()
  
  content_list_title<-link%>%
    read_html()%>%
    html_nodes(xpath='//main/h3')%>%
    html_text()
  
  index<-grep('Responsibilities could include but are not limited to',content_list_title)
    
  
  content_list<-link%>%
      read_html()%>%
      html_nodes(xpath='//main/ul')%>%
      '['(index)
      
  
  content<-list()
  
  for (i in seq_along(content_list)){
    content[[i]]<-data.frame(position=job_title[i],
                             responsibility=content_list[[i]]%>%
                               html_nodes('li')%>%
                               html_text(),stringsAsFactors = F)
  }
  
  return(content)
  
}


task_solution_professional<-list()
task_informatic<-list()

for (i in seq_along(links)){
  
  task_solution_professional[[i]]<-extract_content(links[[i]])
}


add_col<-function(df,list_name){
  df<-df%>%
      mutate(stream=list_name)
  
  return(df)
}

for ( i in seq_along(stream)){
  
  task_solution_professional[[i]]<-lapply(task_solution_professional[[i]],add_col,stream[i])
}



for (i in seq_along(links)){
  
  task_informatic[[i]]<-extract_content2(links[[i]])
}


for ( i in seq_along(stream2)){
  
  task_informatic[[i]]<-lapply(task_informatic[[i]],add_col,stream2[i])
}


#reformat the lists, convert to json and index to elasticsearch:
library(elastic)
library(jsonlite)

#connect to elastic gate for meddra data:
connect(es_host = "elastic-gate.hc.local", es_port = 80,errors = "complete")


#combine lists and check json structure:

toJSON(task_solution_professional,pretty=TRUE)

task_solution_professional<-do.call(c,task_solution_professional)
task_solution_professional<-do.call(rbind,task_solution_professional)
task_informatic<-do.call(c,task_informatic)
task_informatic<-do.call(rbind,task_informatic)

df<-bind_rows(task_informatic,)

docs_bulk(task_solution_professional,index='test_contract')
docs_bulk(task_informatic,'test_contract')
