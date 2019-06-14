##########################
#Purpose: To generate the SPC charts in the report with associated messages as well as to generate the highlights page
#Author: Hans Aisake
#Date Created: July 17, 2018
#Date Modified: Jan 3, 2019
#Comments:
##########################

#PURPOSE: Assigns colour tags for PDF text msgs generated via LaTex
colFmt = function(x,color){
  
  #outputFormat = opts_knit$get("rmarkdown.pandoc.to")
  
  #if(outputFormat == 'latex')
    paste("\\textcolor{",color,"}{",x,"}",sep="")
  #else if(outputFormat == 'html')
  #  paste("<font color='",color,"'>",x,"</font>",sep="")
  #else
  #  x
}

#PURPOSE: Identify the Colour of the MSG
colMsg <- function(msg){
  arg1 <- grepl("Favorable",msg)
  arg2 <- grepl("Celebrate",msg)

  arg3 <- grepl("Unfavorable",msg)
  arg4 <- grepl("Investigate",msg)

  if( arg1 | arg2 ){
    return('forestgreen')
  }else if( arg3 | arg4 ){
    return('crimson')
  } else{
    return('black')
  }
}

#PURPOSE: Print SPC charts and messages
plotWithMSG <- function(x,shortVersion){
  
  if(shortVersion==TRUE & x$shortVersionInclusion==TRUE | shortVersion!=TRUE){
    suppressWarnings( plot(x$plot) )
    cat("Typical performance shows up as black dots. \\newline Typical is based on the baseline grey circles. \\newline")

    #generate summary messages in the report
    if(is.not.null(x$chartTxt)){
      
      titleText <- paste0(" \\newline {\\Large Recent Highlights as of ", format(Sys.time(), '%Y-%B-%d'),"} \\newline ")
      cat(titleText)
      str <- NULL
      for(mm in 1:length(x$chartTxt)){
        
        #if this is the last message add a period, otherwise a semi-colon
        if( mm == length(x$chartTxt)){
          s <-"."
        } else{
          s <-"; "
        }
        
        txt <- colFmt(x$chartTxt[[mm]],colMsg(x$chartTxt[[mm]]))
        
        #join message with earlier messages
        str<-paste0(str,txt, s)

      }
      #print the messages and close HTML tags
      cat(paste0(str, " \\newpage ",sep=""))
    }
  }
}

#PURPOSE: Generate a 1-2 page summary for messages that are short version eligable
genSummaryPage <- function(list_Charts_MSGs){
  
  #identify which indicator entity groups have summary messages to include
  temp <-unlist(lapply(list_Charts_MSGs,"[[","shortVersionInclusion"))
  ind <-as.numeric(which(temp==TRUE))
  
  #extract those indicators
  sub <- list_Charts_MSGs[ind]
  
  #to track indicator name
  lastIndName <- NULL
  msgCount <- 0

  #for each indicator in the given list print the indicator name, the sub group, and a list of the messages
  for(kk in 1:length(sub)){
    
    msgCount <- msgCount+1 #count how many lines have been printed to the current summary page
    
    #prepare html txt for the indicator name and the entity sub group
    indName <- gsub("#", "Number", sub[[kk]]$indicatorName)
    indName <- gsub("%", "Percent of ", indName)
    entityName <- paste0("",sub[[kk]]$entityName,": ","",sep="")
    entityName <- gsub("&", "and", entityName)
    str <- NULL
    
    #extract the summary message vector
    #chartTxt <-sub[[kk]]$chartTxt
    highlightTxt <- sub[[kk]]$highlightTxt  
      
    # each entity group needs a new line except for the first group which already has a new line.
    if( kk==1){
      tg<-""
    }else {
      tg<" \\newline "
    }

    #print an indicator title for the summary msg
    if(is.null(lastIndName)){
      cat(tg, "\\textbf{\\Large", indName,"} \\newline ")
    } else{
      if(indName!=lastIndName){
        if(msgCount >14){ #it's time to push the summary to a new page
          cat(" \\newpage \\textbf{ \\Large", indName,"} \\newline ")
          msgCount <-0 #reset counter to 0
        }else{
          cat(" \\newline \\textbf{ \\Large", indName,"} \\newline ")
        }
      }
    }
    cat(" \\normalsize{ ", entityName)
    #cat(" ", entityName)

    #for each message generate html txt
    for(mm in 1:length( highlightTxt ) ){
      
      #add a period if this is the last message otherwise add a semi-colon
      if(mm <length(highlightTxt) ){
        s <-"; "
      }else if ( mm==length(highlightTxt) ){
        s <-"."             #place a period
      }else{
        warning("Error in summary msg index mm.")
      }
      
      #append ; for between msgs and . for the last message
      msg <- paste0( highlightTxt[mm], s, sep="")
      
      #create text message with HTML tags
      txt <- colFmt(msg,colMsg(msg))

      #join message with earlier messages
      str <- paste0(str,txt)
      
    }#end for
    
    #print the messages and close HTML tags
    cat(str, "} \\newline ") #end the normal size tag for the entity
    #cat(str, " \\newline \\newline ") #end the normal size tag for the entity
    lastIndName<-indName
    
  }#end for

}#end function


#PURPOSE: Generate a consolidated patterns table for the appendix file.
genAppendixLog <- function(list_Charts_MSGs)
{
  #pull out the appendix tables
  temp  <- lapply(list_Charts_MSGs,"[[","appendixTable")
  
  #remove tables that are NA
  naInd <- unname(is.na(temp))
  temp  <- temp[!naInd] 
  
  #reduce tables to 1 large table via uninion
  df    <-  do.call("rbind", temp) 
  rownames(df) <- NULL
  
  return(df)  #returns a NULL if no tables are found
}
      
 