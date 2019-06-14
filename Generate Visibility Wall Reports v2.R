# File Description ####
#Purpose: Generates the visibility wall reports and deploys a UI to get user parameters.
#The final product will be a PDF, and will be opened in a browser.
#The actual report is produced by the Markdown file.
#Author: Hans Aisake
#Date Created: September 26, 2018
#Date Modified: Jan 8, 2019
#Comments:
#Requirements:
# - underlying master indicator table.
# - - under that is a series of supporting tables.
# - installation of MikTex and the xcolor package
# - requires several R packages: see the loadPack() function for details.
# - this script breaks if there is an indicator in the master table with less than 2 records. Effort has been made to make this an impossibility.
# End File Description ####

# Initialization ####
  #clear workspace
  rm(list=ls())
  
  #set library and working directories
  libPath <-"G:/VCHDecisionSupport/Patient Flow/Richmond RMarkdown Reports/Visibility Wall/R Library"
  .libPaths(libPath) #set the library path
  wd <- "G:/VCHDecisionSupport/Patient Flow/Richmond RMarkdown Reports/Visibility Wall"
  setwd(wd)
  mdFileName <- "MarkdownGeneration_v2.Rmd" #markdown file name; this is the file that builds the report
  
  #hard coded parameters tables
  tft_lookup_df <<- data.frame(timeFrameType =c("Weekly","Fiscal Period", "Monthly","CalendarQuarter")
                               ,valPerYear = c(52,13,12,4)
                               ,un =c(" weeks"," periods"," months"," quarters")
                               ,tag =c("W","P","M","Q")
  )
  annFontSize <<- 3   #Chart annotation font size
  
  #packages required to luanch the rend() function; further packages contained in the Rmarkdown
  require(knitr) # required for knitting from rmd to md
  require(markdown) # required for md to html
  require(tcltk) #msg box

# End Initialization ####

# Query File Names ####
  #specifies the file names of the queries to pull the indicator data, and any additional data sets for non-SPC charts
  #files are stored in a local folder caleld "Report Queries"
  
    # Indicator Tables
    indTableQueryFN <<-"pullIndicatorTableQuery.txt"
    
    #QPS special queries
    fallsAgeDistFileName <<-"pullFallsByAgeDistribution.txt"
  
    #A&F special queries
    rcWaitDistFileName <<-"pullRCDaysWaitingDistribution.txt"
    rcPlacementsFileName <<-"pullRCPlacementsFromLocation.txt"
    rcWaitlistLengFileName <<-"pullRCWaitlistLength.txt"
    surgicalWaitlistFileName <<-"pullSurgicalWaitlist.txt"
  
    #EE special queries
    turnoverFileName <<-"pullTurnover.txt"
    
    #explained patterns query
    investigatedPatternsFileName <<- "pullInvestigatedPatterns.txt"
    
# End Query File Names ####

# generate and open a copy of the report ####
  x <-rmarkdown::render(mdFileName, params = "ask") #render the report
  
  if(mdFileName %in% list.files()){
    
    file.rename(x,rpName) #rename the file to a better name from a global variable generated durring the report render
    system(paste0('open "', rpName, '"'))   #open a copy of the report
    
   }else{
     
     tkmessageBox(title="Warning",message="Report Generation Failed. Please refer to the rmarkdown console output for details.", icon="info", type="ok")
  }
# end report generation ####