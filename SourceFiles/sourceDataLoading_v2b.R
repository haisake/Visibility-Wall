# Script Description ####
#Purpose: To house functions necessary for intiatializing the A&F SPC Chart report and loading the data into R
#This includes packages and loading data functions.
#Author: Hans Aisake
#Date Created: July 17, 2018
#Date Modified: Jan 8, 2019
#Comments:
# End Description ####

#PURPOSE: To load required packages, set the library, and set the working directory
loadPack <- function()
{
  suppressWarnings(library(RODBC))
  suppressWarnings(library(qicharts))
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(ggExtra))
  suppressWarnings(library(openxlsx))
  suppressWarnings(library(sqldf))
  suppressWarnings(library(data.table))
  suppressWarnings(library(shiny))
  suppressWarnings(library(tinytex))
  suppressWarnings(library(RColorBrewer))
  suppressWarnings(library(zoo))
  suppressWarnings(library(pbapply))
}

#PURPOSE: Read in query text from a TXT file
getQuery <-function(fileName){
  rawQueryTxt <-readChar(fileName, file.info(fileName)$size)
  finalQueryTxt <-gsub( "[\r\n\t]", " ", rawQueryTxt)
}

#PURPOSE: To load in data from the decision Support Data Warehouse
loadIndicatorData <-function(connectionName, fn)
{
  channel <- odbcConnect(connectionName)  #set the connection details
  query <- getQuery(fn)        #get the query string from the external query file
  x <-sqlQuery(channel, query)          #query the database and bring the data in as a table
  
  odbcCloseAll() #close data base connection
  
  #make sure the data types of certain columns are as expected and standardize names. Not wholy necessary but makes the script more robust
  indicatorID      <- as.character(x$indicatorID)
  facility         <- as.character(x$facility)
  entity_group     <- as.character(x$entity_group)
  TimeFrame        <- as.Date(x$TimeFrame)
  TimeFrameLabel   <- as.character(x$TimeFrameLabel)
  TimeFrameType    <- as.character(x$TimeFrameType)
  IndicatorName    <- as.character(x$IndicatorName)
  Numerator        <- as.numeric(x$Numerator)
  Denominator      <- as.numeric(x$Denominator)
  Value            <- as.numeric(x$Value)
  DesiredDirection <- as.character(x$DesiredDirection)
  Format           <- as.character(x$Format)
  Target           <- as.numeric(x$Target)
  DataSource       <- as.character(x$DataSource)
  IsOverall        <- as.numeric(x$IsOverall)
  IndicatorClass   <- as.character(x$MYVCHCategory)
  ChartType        <- as.character(x$ChartType)
  V_ChartTitle     <- as.character(x$V_ChartTitle)
  V_ScaleFactor    <- as.numeric(x$V_ScaleFactor)
  MYVCHCategory    <- as.character(x$MYVCHCategory)
  key              <- as.factor(paste(x$indicatorID,x$facility, x$entity_group,sep="-"))  #add a unique key for indicator-entity pairs
  df <- data.frame(indicatorID,facility,entity_group,TimeFrame,TimeFrameLabel,TimeFrameType,IndicatorName,Numerator,Denominator,Value,DesiredDirection,Format,Target,DataSource,IsOverall,IndicatorClass,ChartType,V_ChartTitle,V_ScaleFactor,MYVCHCategory, key)
  
  #split the data into a list for parallel computing, not sure if we really get an advantage from this, but theoretically we could
  y<-split(as.data.table(df),f=df$key,drop=TRUE,sorted=TRUE,keep.by=FALSE)  #seperate each indicator and entity group into a seperate element of a long list
  y <-lapply(y,as.data.frame)
  return(y) #return the results
}


#Purpose: load other data sets given a query
loadDataGeneric <-function(connectionName, queryFileName)
{
  channel <- odbcConnect(connectionName)  #set the connection details
  query <- getQuery(queryFileName)        #get the query string from the external query file
  x <-sqlQuery(channel, query)          #query the database and bring the data in as a table
  
  odbcCloseAll() #close data base connection
  
  return(x) #return the results
}

#Purpose: load investigated patterns
getInvestigatedPatterns <- function(connectionName, fileName){
  x <- loadDataGeneric(connectionName,paste0("./Report Queries/", fileName  ,sep=""))
  x$key <- as.factor(paste(x$indicatorID
                            , x$facility
                            , x$entity_group
                            , x$pattern_start
                            , x$pattern_end
                            , x$PType
                            , sep="-")
  )  #add a unique key for indicator-entity pairs
  
  return(x)
}
