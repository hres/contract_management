library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(scales)
library(lubridate)
library(elastic)
library(jsonlite)
library(stringdist)

contract<-read_excel('contract.xlsx',1)
connect(es_host = "elastic-gate.hc.local", es_port = 80,errors = "complete")

query<-'
{
  "aggs": {
      "job": {
        "terms": {
          "field": "stream.keyword",
          "size":50
      },
      "aggs": {
        "stream": {
          "terms": {
            "field": "position.keyword"
          }
        }
      }
    }
  }
}'

positions<-Search(index='test_contract',body=query,size=0,raw=T)%>%fromJSON()
positions<-positions$aggregations$job$buckets

streams<-positions$key
positions<-positions$stream$buckets

df<-data.frame(stream=mapply(rep,streams,sapply(positions,nrow))%>%unlist(use.names=F),
               position=sapply(positions,'[','key')%>%unlist(use.names = F),stringsAsFactors = F)


contract<-contract%>%
          mutate(position=df$position[amatch(Contractor,df$position,method='cosine',maxDist=1)])
               

