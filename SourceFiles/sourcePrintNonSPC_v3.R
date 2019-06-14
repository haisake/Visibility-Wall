##########################
#Purpose: To house functions for the printing of Non-SPC Charts for the Visibility Wall Report
#Author: Hans Aisake
#Date Created: Sept 5, 2018
#Date Modified: Jan 8, 2019
#Comments:
##########################

#Purpose: Plot the current waiting days for wait listed clients
#input has fields Buckets, Counts
plotRCWaitDist <-function(rcWaitDist)
{
  #check input has the appropriate structure
  fields <- c("ThursdayWkEnd","Buckets","Order","Count")
  datNames <-names(rcWaitDist)
  
  if( !all( fields %in% datNames )  ){
    warning("plotRCWaitDist() did not recieve valid inputs.")
    return(NULL)
  } 
  
  #set values for the chart
  mainTitle<-"Snapshot of Days Waiting for RC on the FAB Wait List by Week"
  subTitle<-"By 30 day wait time buckets"
  horTitle<-"Week (year-month)"
  vertTitle<-"# Clients Waiting"
  dataSource <-"Data Source: CommunityMart"
  
  #colour pallet extension for number of buckets
  colourCount = length(unique(rcWaitDist$Buckets))
  getPalette = colorRampPalette(brewer.pal(11, "RdYlGn"))
  
  #generate the plot object
  p <- ggplot(rcWaitDist, aes(x=ThursdayWkEnd, y=Count, fill=Buckets), position = position_stack(reverse = TRUE)) +
    geom_area(stat="identity") +
    scale_fill_manual(values=rev(getPalette(colourCount))) +
    theme_bw(base_size = 18) +
    theme(plot.caption=element_text(margin=margin(t=0),face="italic", size=8)) +
    theme( panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) +
    labs(x=horTitle, y=vertTitle, title=mainTitle, subtitle=subTitle, caption=dataSource)+
    removeGrid()

  #print the plot
  suppressWarnings(plot(p))
  cat("\\newpage") #for PDF output only, need other symbols for html or other formats.
}

#Purpose: Plot RC placement volumes from location type
#input has fields FiscalPeriodLong, NumPlacements, PlacedFromLocation
plotRCPlacements <-function(rcPlacements)
{
  #check input has the appropriate structure
  fields <- c("FiscalPeriodLong","NumPlacements","PlacedFromLocation")
  datNames <-names(rcPlacements)
  
  if( !all( fields %in% datNames )  ){
      warning("plotRCPlacements() did not recieve valid inputs.")
      return(NULL)
  } 
  
  #set values for the chart
  mainTitle<-"RC Priority Access Placements by Placed From Location Type"
  subTitle<-paste0("Generated on ",format(Sys.time(), '%Y-%B-%d'))
  horTitle<-"Fiscal Period"
  vertTitle<-"# Placements"
  dataSource<-"Data Source: CommunityMart + ADTCMart"
  
  #create plot object  
  p <- ggplot(rcPlacements, aes(x=FiscalPeriodLong, y=NumPlacements, fill=PlacedFromLocation, label=NumPlacements), position = position_stack(reverse = TRUE)) +
        geom_bar(stat="identity") +
        geom_text( position=position_stack(vjust = 0.5),colour="black") +
        scale_fill_brewer(palette="Set2") +
        theme_bw(base_size = 18) +
        theme(plot.caption=element_text(margin=margin(t=0),face="italic", size=8)) +
        theme( panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) +
        labs(x=horTitle, y=vertTitle, title=mainTitle, subtitle=subTitle, caption=dataSource) +
        removeGridX()
 
  #print the plot
  suppressWarnings(plot(p))
  cat("\\newpage") #for PDF output only, need other symbols for html
}

#Purpose: Plot RC placement volumes from location type
#input has fields FiscalPeriodLong, LocationType, Numwaiting
plotRCWaitListLeng <-function(rcWaitListLeng)
{
  #check input has the appropriate structure
  fields <- c("TimeFrameLabel","TimeFrame","RegLocation","NumClientsAAP")
  datNames <- names(rcWaitListLeng)

  if( !all( fields %in% datNames )  ){
    warning("plotRCWaitListLeng() did not recieve valid inputs.")
    return(NULL)
  } 
  
  #chart parameters
  mainTitle<-"Residential Care Wait List (AAP)"
  subTitle<-"By Registration Location Category"
  horTitle<-"Fiscal Period"
  vertTitle<-"# AAP"
  dataSource<-"Data Source: CommunityMart"
  
  #create plot  
  p <- ggplot(rcWaitListLeng, aes(x=TimeFrameLabel, y=NumClientsAAP, fill=RegLocation, label=NumClientsAAP), position = position_stack(reverse = TRUE)) +
        geom_bar(stat="identity") +
        scale_fill_brewer(palette="Set2") +
        geom_text( position=position_stack(vjust = 0.5),colour="black") +
        theme_bw(base_size = 18) +
        theme(plot.caption=element_text(margin=margin(t=0),face="italic", size=8)) +
        theme( panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) +
        labs(x=horTitle, y=vertTitle, title=mainTitle, subtitle=subTitle, caption=dataSource) +
        removeGridX()

  suppressWarnings(plot(p)) 
  cat("\\newpage") #for PDF output only, need other symbols for html
}

#Purpose: Plot OR Current WL wait times
#fields: "ORService", "WithintDXTargetFlag", "NumCases", "MaxORDate"
plotSurgicalWaitlist <-function(surgicalWaitlist)
{
  #check input has the appropriate structure
  fields <- c("ORService", "WithinDxTargetFlag", "NumCases", "MaxORDate","PercentCases","PercentLabel") 
  datNames <-names(surgicalWaitlist)
  
  if( !all( fields %in% datNames )  ){
    warning("plotSurgicalWaitlist() did not recieve valid inputs.")
    return(NULL)
  } 

  #chart parameters
  mainTitle<-"Surgical Waitlist by OR Service"
  subTitle<-paste0("Snapshot for ",as.character(surgicalWaitlist$MaxORDate[1]))
  horTitle<-"Surgical Service"
  vertTitle<-"# People on the Waitlist"
  dataSource<-"Data Source: ORMart"
  
  #create plot object
  p <- ggplot(surgicalWaitlist, aes(x=ORService, y=NumCases, fill=WithinDxTargetFlag, label=paste0(PercentLabel,"%",sep="")), position = position_stack(reverse = TRUE)) +
    geom_bar(stat="identity") +
    geom_text( position=position_stack(vjust = 0.5),colour="black") +
    scale_fill_brewer(palette="Dark2") +
    theme_bw(base_size = 18) +
    theme(plot.caption=element_text(margin=margin(t=0),face="italic", size=8)) +
    theme( panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) +
    labs(x=horTitle, y=vertTitle, title=mainTitle, subtitle=subTitle, caption=dataSource) +
    removeGridX()
  
  #print the plot
  suppressWarnings(plot(p))
  cat("\\newpage") #for PDF output only, need other symbols for html
}

#Purpose: plot the falls by age stacked bar chart with age as the breakdown
#input has the fields AgeGroupName, NumFallsCases, TimeFrameLabel
plotFallsAge <-function(fallsAge)
{
  #check input has the appropriate structure
  fields <- c("AgeGroupName", "TimeFrameLabel","NumFallsCases")
  datNames <- names(fallsAge)
  
  if( !all( fields %in% datNames )  ){
    warning("plotFallsAge() did not recieve valid inputs.")
    return(-1)
  } 
  
  #set chart parameters
  mainTitle<-"Falls Cases by Age Group"
  subTitle<-"All Programs"
  horTitle<-"Year-Month"
  vertTitle<-"# Falls Cases"
  dataSource <-"Data Source: BCPSLS"
  
  #order by the timeframe
  x <-fallsAge[order(fallsAge$TimeFrameLabel),]
  
  #create plot object
  p<- ggplot(x, aes(x=TimeFrameLabel, y=NumFallsCases,fill=AgeGroupName, label=NumFallsCases )) +
    geom_bar(stat="identity") +
    geom_text( position=position_stack(vjust = 0.5),colour="black") +
    theme_bw(base_size = 18) +
    theme(plot.caption=element_text(margin=margin(t=0),face="italic", size=8)) +
    theme( panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) +
    labs(x=horTitle, y=vertTitle, title=mainTitle, subtitle=subTitle, caption=dataSource) +
    removeGridX()
  
  #scale_fill_brewer(palette="Greens") +
    
  #print the plot
  suppressWarnings(plot(p))
  cat("\\newpage") #for PDF output only, need other symbols for html
}

#Purpose: plot the turnoverdata by turnovertype in a stacked bar chart
#input has the fields FiscalPeriodLong, FiscalPeriodEndDate, ProgramDesc, TurnoverType, TurnoverVolume, ProgramHeadcount
plotTurnover <-function(turnover)
{
  #check input has the appropriate structure
  fields <- c("FiscalPeriodLong", "TurnoverType", "TurnoverVolume")
  datNames <- names(turnover)
  
  if( !all( fields %in% datNames )  ){
    warning("plotTurnover() did not recieve valid inputs.")
    return(-1)
  } 
  
  #set chart parameters
  mainTitle<-"Turnover Volumes"
  subTitle<-"All Programs"
  horTitle<-"Fiscal Period"
  vertTitle<-"#"
  dataSource <-"Data Source: EE Extract"
  
  #create plot object
  p<-ggplot(turnover, aes(x=FiscalPeriodLong, y=TurnoverVolume,fill=TurnoverType, label=TurnoverVolume )) +
      geom_bar(stat="identity") +
      geom_text( position=position_stack(vjust = 0.5),colour="black") +
      scale_fill_brewer(palette="Set1") +
      theme_bw(base_size = 18) +
      theme(plot.caption=element_text(margin=margin(t=0),face="italic", size=8)) +
      theme( panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) +
      labs(x=horTitle, y=vertTitle, title=mainTitle, subtitle=subTitle, caption=dataSource) +
      removeGridX()
  
  #print the plot
  suppressWarnings(plot(p))
  cat("\\newpage") #for PDF output only, need other symbols for html
}

#Purpose: Plot the nonSPC charts for a given report type.
#needs an ODBC connection name and a report type name
printNonSPCCharts <-function(reportType, ds_connectionName)
  {
  #check input
  if ( !reportType %in% c("All","Access and Flow","Quality and Patient Safety","Engagement") ){
    warning(paste0(reportType, " is not a supported report type in nonSPCCharts().",sep=""))
  }
  
  #Access and Flow plot
  if(  reportType %in% c("All","Access and Flow")  ){
    
    #load data
    rcWaitDist     <- loadDataGeneric(ds_connectionName,paste0("./Report Queries/",rcWaitDistFileName,sep=""))
    rcPlacements   <- loadDataGeneric(ds_connectionName,paste0("./Report Queries/",rcPlacementsFileName,sep=""))
    rcWaitListLeng <- loadDataGeneric(ds_connectionName,paste0("./Report Queries/",rcWaitlistLengFileName,sep=""))
    surgicalWaitlist     <- loadDataGeneric(ds_connectionName,paste0("./Report Queries/",surgicalWaitlistFileName,sep=""))
   
    #plot the additional RC charts
    plotRCWaitListLeng(rcWaitListLeng)
    plotRCPlacements(rcPlacements)
    plotRCWaitDist(rcWaitDist)
    plotSurgicalWaitlist(surgicalWaitlist)
  
  }
  
  if(  reportType %in% c("All","Quality and Patient Safety")  ){
   
    #load data for the Falls breakdown charts
    fallsAge     <- loadDataGeneric(ds_connectionName, paste0("./Report Queries/",fallsAgeDistFileName,sep=""))
    
    #plot the additional charts
    plotFallsAge(fallsAge)
    
  }
  
  if ( reportType %in% c("All","Engagement")  ){
    
    #load data
    turnover     <- loadDataGeneric(ds_connectionName,paste0("./Report Queries/",turnoverFileName,sep=""))
    
    #plot the additional RC charts
    plotTurnover(turnover)
    
  }

}


