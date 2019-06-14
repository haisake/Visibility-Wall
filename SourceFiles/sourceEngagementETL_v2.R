##########################
#Purpose: To execute the ETL process for the enagement data
#Author: Hans Aisake
#Date Created: October 4, 2018
#Date Modified: March 14, 2019
#Comments:
#March 14, 2019 - Removed the indicator building logics from this source file 
##########################

#Purpose: Pull data from the EE server
extractDataFromEE <-function(ee_connectionName)
  {
  #extract data from the EE RPT server SPDBSHRIS001
  query <- "SELECT * FROM VCH_RPT.rpt.vw_monthly_richmond_summary_by_fiscalperiod"
  channel <- odbcConnect(ee_connectionName)
  x <-sqlQuery(channel, query)
  odbcCloseAll()
  return(x)
}

#Purpose: Transform the format of the EE data to fit the location table
transformEE_Extract <-function(x)
  {
  
  #add fiscalperiodlong labels and drop uneeded columns.
  year    <-substr(x$fiscal_year_period,6,9)
  period  <-substr(x$fiscal_year_period,11,12)
  x$fiscalPeriodLong <-paste0(year,"-",period,sep="")
  rmv <-c("fiscal_year_period","Attendance Review", "Stage One","Stage Two", "Stage Three","# of postings per 100 staff (%)","Vacancy Rate")
  x <-x[, !names(x)%in% rmv]   #remove unneeded columns
  
  #order columns in the following order
  o<-c("fiscalPeriodLong","deptid","acct_cd","headcount","Convert to Casual","Leaving VCH Involuntarily","Leaving VCH Voluntarily","Retirement","Suspension","Transfer Out of Department (CC)" ,"Transfer out of Richmond (CoC)"  ,"Went On Leave","Sick Time","Overtime","Productive Hours","Vacancy Hours (Unassigned)","ESP Productive Hours","# of postings")                  
  x<-x[,o]
  
  return(x)

}

#Purpose: Import the extracted and transformed EE data into DSSI
loadEE_Extract <- function(x,ds_connectionName)
  { channel <- odbcConnect(ds_connectionName)

  #clear out old data
  query<- "TRUNCATE TABLE DSSI.dbo.RH_VisibilityWall_EE_Extract"
  try(sqlQuery(channel,query))
  
  #drop placeholder table if it exists
  query<- "DROP TABLE DSSI.dbo.RH_VisibilityWall_EE_Extract_temp"
  sqlQuery(channel,query)

  #insert new data
  sqlSave(channel,dat=x,append=FALSE,tablename="dbo.RH_VisibilityWall_EE_Extract_temp"
            ,fast=TRUE,rownames = FALSE,colnames=FALSE)
  
  #transfer from temp table into correct destination
  query<-"
  INSERT INTO dbo.RH_VisibilityWall_EE_Extract (fiscalperiodlong,deptid,acct_cd,headcount,[Convert to Casual],[Leaving VCH Involuntarily],[Leaving VCH Voluntarily],Retirement,Suspension,[Transfer Out of Department (CC)],[Transfer out of Richmond (CoC)],[Went On Leave],[Sick Time],Overtime,[Productive Hours],[Vacancy Hours (Unassigned)],[ESP Productive Hours],[# of postings])
  SELECT * FROM DSSI.dbo.RH_VisibilityWall_EE_Extract_temp"
  query<-gsub( "[\r\n\t]", " ", query)
  sqlQuery(channel,query)
  
  #transfer from temp table into correct destination
  query<- "DROP TABLE DSSI.dbo.RH_VisibilityWall_EE_Extract_temp"
  sqlQuery(channel,query)
  
  odbcCloseAll() #close odbc connections
}

#Purpose: Add program description and fiscal period start and end dates to the data
addAttributesEE_Extract <- function(ds_connectionName)
  {
    
  channel <- odbcConnect(ds_connectionName)
    
  #add fiscal period end date and start date to the table
  
  query2<-"
  UPDATE DSSI.[dbo].[RH_VisibilityWall_EE_Extract]
  SET fiscalperiodstartdate=Y.fiscalperiodstartdate
  , fiscalperiodenddate=Y.FiscalPeriodEndDate
  FROM DSSI.[dbo].[RH_VisibilityWall_EE_Extract] as X
  LEFT JOIN (SELECT DISTINCT fiscalperiodlong, cast(fiscalperiodstartdate as date) as 'fiscalperiodstartdate', cast(fiscalPeriodEndDate as date) as 'fiscalperiodenddate' FROM ADRMart.dim.[Date] WHERE fiscalperiodenddate < GETDATE()) as Y
  ON X.FiscalPeriodLong=Y.FiscalPeriodLong
  "
    
  query2<-gsub( "[\r\n\t]", " ", query2)
  sqlQuery(channel,query2)
    
  #add program description to the table
  query3<-"
  UPDATE DSSI.[dbo].[RH_VisibilityWall_EE_Extract]
  SET ProgramDesc=F.ProgramDesc
  FROM DSSI.[dbo].[RH_VisibilityWall_EE_Extract] as X
  LEFT JOIN [FinanceMart].[Finance].[EntityProgramSubProgram] F
  ON X.deptid=F.costcentercode
  AND RIGHT(X.[acct_cd],3)=F.finsiteid
  WHERE F.finsiteid in (650,652,653,654,655)		
  "
    
  query3<-gsub( "[\r\n\t]", " ", query3)
  sqlQuery(channel,query3)
    
  odbcCloseAll()#close odbc connections
}

#Purpose: Extract EE data from the EE server and load it in the decision support server
EEdata_ETL <-function(ds_connectionName,ee_connectionName)
  {
  #troubleshooting
  #ds_connectionName<-"AISAKE-DSSI"
  #ee_connectionName<-"AISAKE-EERPT"
  x<-extractDataFromEE(ee_connectionName)     #pull data from the EE server
  x<-transformEE_Extract(x)  #transform the data
  loadEE_Extract(x,ds_connectionName)
  addAttributesEE_Extract(ds_connectionName)

}




