# File Header ####
#Purpose: To house functions that control the updating of the indicator tables
#Author: Hans Aisake
#Date Created: Jan 8, 2019
#Date Modified: Jan 16, 2019
#Comments:
# End File Header ####

# create progress bar
updateIndicators <- function(params){

  #simplify argument code
  arg1 <-params$refresh =="Yes."

  #build EE metric table if selected
  if (arg1){
    #buildEEMetricTable(params$ds_connectionName, params$ee_connectionName)
    #buildAFMetricTable(params$ds_connectionName)
    #buildQPSMetricTable(params$ds_connectionName)
    channel<-odbcConnect(params$ds_connectionName)
    query<-"EXEC DSSI.dbo.sp_buildVWMasterTable_aisake"
    sqlQuery(channel,query)
    odbcCloseAll()
  }
}

