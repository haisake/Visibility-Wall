# File Header ####
#Purpose: To house functions necessary for generating the SPC charts
#Author: Hans Aisake
#Date Created: July 17, 2018
#Date Modified: Jan 8, 2019
#Comments:
# END File Header ####

# Chart Parameter Functions ####

#PURPOSE: To identify the main title text
chartTitle <-function(indicatorName,groupName, chartType)
{ main <-as.character(indicatorName)
  sub <- paste0(as.character(groupName), ":",toupper(chartType),"-Chart")
  return(list(Main=main,Sub=sub)) #put the data elements into a list with named elements
}

#PURPOSE: Identify  horizontal tick labels
hLabels <-function(timeLabs, timeFrameType)
{ #troubleshoot
  # timeLabs <- as.character(dat$TimeFrameLabel)
  # timeFrameType <- as.character(dat$TimeFrameType[1])
  
  #if smart labeling isn't possible return all labels
  if(  !any(  tft_lookup_df$timeFrameType==timeFrameType  )  ){
    warning(paste0("Smart x-axis labeling for ",timeFrameType, "is not supported."))
    return(timeLabs)
  }

  #parameters to identify which labels to show
  n <- length(timeLabs)  #amount of labels
  z <- tft_lookup_df$valPerYear[tft_lookup_df$timeFrameType == timeFrameType]  #scaling cutoff multiplier based on report type
  if(n>=2*z) { 
    #3 years or more of data use annual labels, otherwise use monthly, period, or quarterly labels
    y <- z
    index   <- which( 1:n %% y == 1)   #all important labels + first
  } else if (n>=z/2 & timeFrameType=="Weekly") { 
    y <- 4   
    index   <- which( 1:n %% y == 1)   #all important labels + first
  } else { 
    #otherwise use all labels
    y <-1
    index   <- 1:n #all labels
  }
  
  ind_Last  <- n   #last label
  
  if( ind_Last - index[length(index)] < z/2 ){
    index[length(index)] <-ind_Last  #replaced last interval label with the last index
  }else{
    index <- unique(  c(index,ind_Last)  ) #add the last index directly
  }
  index[order(index,decreasing=FALSE)]      #order index in increasing order
  
  l <- rep("", n)   #create placeholder variable full of blank's for labels
  l[index] <- timeLabs[index]
  
  return(l) #return the labels
}

#PURPOSE: Set how many data points to include in the SPC chart calibration a.k.a the baseline data
setBaselineCutoff <- function(n, freqType)
{ 
  # troubleshooting
  # n<-length(dat$Value)
  # freqType <-as.character(dat$TimeFrameType[1])
  
  z <- tft_lookup_df$valPerYear[which(tft_lookup_df$timeFrameType==freqType)]  #scaling cutoff multiplier based on report type
  
  if(n>=3*z)        { bline_cut <- z*2        #try to use 2 years worth of data
  } else if (n>=2*z){ bline_cut <- z*1        #otherwise try to use 1 years worth of data
  } else if (n>=z/2){ bline_cut <- floor(z/2) #half a year
  } else            { bline_cut <- NA         #otherwise return NA. All data will be used as the baseline and the post. Insufficient data
  }
  
  return(bline_cut)    
}

#PURPOSE: Create a list with mu, sigma which are needed for the SPC special cause detection rules function
muSigma <- function(dat, chartType, bline_cutoff, is_integer )
{ # bline_cutoff <- bline
  # is_integer <- format=="D0"
  
  #no bline_cut specified, then use all data to set mu and sigma
  if(  is.na(bline_cutoff)  ){  
    bline_cutoff <-length(dat$Value)  
  }
  b_index <- 1:bline_cutoff
  
  #needed parameters on the clibration data
  if(  chartType=="i"  ){
    mu    <- mean( dat$Value[  b_index  ],na.rm = TRUE )
    sigma <- sqrt( var(dat$Value[  b_index ],na.rm = TRUE) )
    if( is_integer ){  
      mu<-round(mu)
    }  #If integer round mu to an integer. Reduces the number of patterns found.
  } else if (  chartType=="p"  ){
    mu <-sum(dat$Numerator[ b_index ],na.rm = TRUE)/sum(dat$Denominator[  b_index  ],na.rm = TRUE)
    sigma <-sqrt(mu*(1-mu)/mean(dat$Denominator[  b_index  ],na.rm = TRUE))
  } else if (  chartType=="c"  ){ #https://en.wikipedia.org/wiki/C-chart
    mu <-mean(dat$Value[  b_index  ])
    #x_med <-median(dat$Numerator[  b_index  ]/dat$Denominator[  b_index  ])
    sigma <-sqrt(mu)
  }else if (  chartType=="u"  ){ #https://en.wikipedia.org/wiki/U-chart
    mu <-sum(dat$Numerator[  b_index  ],na.rm = TRUE)/sum(dat$Denominator[  b_index  ],na.rm = TRUE)
    #x_med <-median(dat$Numerator[  b_index  ]/dat$Denominator[  b_index  ])
    sigma <-sqrt(mu/mean(dat$Denominator[  b_index  ],na.rm = TRUE))
  }else    {
    warning( paste0("Chart type",chartType," is not supported in muSigma().") )
    return(NULL)
  }
  
  return(  list(mu=mu,sigma=sigma)  )  #return a list of statistics with names
}

#PURPOSE: Compute a long term moving average
longTermTrendMAVG <- function(dat, para)
{ 
  k <- tft_lookup_df$valPerYear[ tft_lookup_df$timeFrameType == para$timeFrameType  ]
  n <- nrow(dat)
  
  if( n >2*k){
    z <-2*k
  } else if (n> k){
    z <-k
  } else {
    return( as.numeric(c(rep(NA,n))) )
  }
  
  # compute the rolling middle line
  if(  para$chartType %in% c("i","c")){
    x <- rollmean(dat$Value,z)
  } else if (  para$chartType %in% c("p","u")   ){
    x <- rollsum(dat$Numerator, z)/rollsum(dat$Denominator, z)
  } else    {
    warning( paste0("Chart type",para$chartType," is not supported in longTermTrendMAVG ().") )
    return( as.numeric(c(rep(NA,n))) )
  }
  
  #add NA's for early values and make sure the MAVG is at least 3 points long for GGPLOT dashlines
  n2 <- length(x)
  if( n2 <3){
    return( as.numeric(c(rep(NA,n))) )
  }
  
  x2   <- c( rep( NA,ceiling( (n-n2)/2) ) , x , rep( NA,floor( (n-n2)/2) ) )  #center the rolling average
  
  return(  x2 )  
}

#PURPOSE: Create a list with the chart parameters for easier plot construction; colours, sizes, and shapes have to come later
#Also includes an assortment of labels and attributes about the data
chartParameters <- function(dat)
{ #troubleshooting
  #dat <-data[[70]]
  para <-list()   #placeholder list to take in results

  #parameters inherited from the indicator table
  indicatorID    <- as.character(dat$indicatorID[1])        #Indicator ID
  indicatorName  <- as.character(dat$IndicatorName[1])      #Indicator name
  facility       <- as.character(dat$facility[1])           #facility
  entityName     <- as.character(dat$entity_group[1])       #Entity Name
  groupName      <- as.character(dat$entity_group[1])       #Group the indicator is for
  dir            <- as.character(dat$DesiredDirection[1])   #Desired direction for the indicator
  hTitle         <- as.character(dat$TimeFrameType[1])      #Horizontal axis title
  vTitle         <- as.character(dat$V_ChartTitle[1])      #Vertical axis title
  datasource     <- as.character(dat$DataSource[1])         #data source
  format         <- as.character(dat$Format[1])             #indicator format and DPL
  chartType      <- as.character(dat$ChartType[1])          #indicator chart type
  tfType         <- as.character(dat$TimeFrameType[1])      #time frame type
  VscaleFactor   <- as.numeric(dat$V_ScaleFactor[1])        #scale factor for vertical axis plot
  hLabsFull      <- as.character(dat$TimeFrameLabel)        #full horizontal labels
  numObs         <- nrow(dat)                               #number of observations
  MYVCHCategory  <- as.character(dat$MYVCHCategory[1])      #MY VCH category
  
  #create a shift in the precision for percentages
  if (substr(format,1,1)=="P"){
    z <- 2
  }else{
    z <- 0
  }
  prec           <- as.numeric(substr(format,2,2))+z  #precission for y values
  
  #parameters derived from the indicator table data
  titles <- chartTitle(  indicatorName, groupName, chartType  )    #build chart main and sub titles
  bline  <- setBaselineCutoff(  length(dat$Value), tfType  )       #baseline cutoff index
  hLabs  <- hLabels(  as.character(dat$TimeFrameLabel), tfType  )  #which x-axis labels to show
  stats  <- muSigma(  dat, chartType, bline, format=="D0"  )   #get mu and sigma of the baseline data

  #assign parameter values to a list
  para[[ length(para)+1 ]] <- getElement(titles,"Main")  #main title
  para[[ length(para)+1 ]] <- getElement(titles,"Sub")   #sub title
  para[[ length(para)+1 ]] <- vTitle
  para[[ length(para)+1 ]] <- hTitle
  para[[ length(para)+1 ]] <- hLabsFull 
  para[[ length(para)+1 ]] <- hLabs     
  para[[ length(para)+1 ]] <- dir
  para[[ length(para)+1 ]] <- chartType
  para[[ length(para)+1 ]] <- tfType
  para[[ length(para)+1 ]] <- bline
  para[[ length(para)+1 ]] <- stats$mu
  para[[ length(para)+1 ]] <- stats$sigma
  para[[ length(para)+1 ]] <- datasource
  para[[ length(para)+1 ]] <- format
  para[[ length(para)+1 ]] <- VscaleFactor
  para[[ length(para)+1 ]] <- prec
  para[[ length(para)+1 ]] <- indicatorName
  para[[ length(para)+1 ]] <- indicatorID
  para[[ length(para)+1 ]] <- entityName
  para[[ length(para)+1 ]] <- facility
  para[[ length(para)+1 ]] <- numObs
  para[[ length(para)+1 ]] <- MYVCHCategory

  #assign names to the variables
  names(para)<-c("Main","Sub","vTitle","hTitle","hLabelsFull","hLabels","dir","chartType","timeFrameType","bline_cut","mu","sigma","datasource","format","VscaleFactor","prec","indicatorName","indicatorID","entityName","facility","numObs","MYVCHCategory")
  return(para)
}

#PURPOSE: Generate the SPC chart object using the QICCharts2 package. Will probably want to change this to either formulas or the QCC package which is maintained. qiCharts stoped being maintained in 2018
generateQICChart <- function(dat,para)
{
  #generate qic values: UCL, LCL, CL, etc....
  if(para$chartTyp=="p"){
    pp<-qic(x=TimeFrame, n=Denominator, y=Numerator, chart=para$chartType, bline_cut=para$bline_cut, data=dat, plot.chart=FALSE)       #computes UCL, LCL, CL, etc..
    
    #replace NA's
    pp$lcl[which(is.na(pp$lcl))] <--0.1
    pp$ucl[which(is.na(pp$ucl))] <-1.1
    
  } else if(   para$chartType %in% c("i","c")   ){
    pp<-qic(x=TimeFrame, y=Value, chart=para$chartType, bline_cut=para$bline_cut, data=dat, plot.chart=FALSE) #computes UCL, LCL, CL, etc..
  } else if(  para$chartType=="u"){
    pp<-qic(x=TimeFrame, n=Denominator, y=Numerator, chart=para$chartType, bline_cut=para$bline_cut, data=dat, plot.chart=FALSE)       #computes UCL, LCL, CL, etc..
    
    #replace NA's
    pp$lcl[which(is.na(pp$lcl))] <- -0.001
    #pp$ucl[which(is.na(pp$ucl))] <-1.1
  }
  else{
    return(NULL)
  }
  return(pp)
}

#PURPOSE: add point sizes
addSize <-function(pp)
{
  #set points size
  pp$size <-rep(2,length(pp$y))
  pp$size[length(pp$y)]<-5
  return(pp)
}

#PURPOSE: Add point shapes
addShapes<-function(pp,bline_cut)
{
  #set points shape
  if(is.na(bline_cut)){
    #pp$shape <- rep(1,length(pp$y))
    pp$shape <- rep(16,length(pp$y))
  }else{
    pp$shape <- c(rep(1,bline_cut),rep(16,length(pp$y)-bline_cut))
    #pp$shape <- c(rep(16,bline_cut),rep(16,length(pp$y)-bline_cut))
  }
  pp$shape[length(pp$y)]<-18
  return(pp)
}

#PURPOSE: Identify the colours for the points. Reduced distinctions from v4.
addColour<-function(pp, spcPoints, bline_cut)
{
  #troubleshooting
  #bline_cut<-para$bline_cut
  
  if(  is.na(bline_cut)  ){
    #when there is no bline_cut the concept of a baseline breaksdown. There is both no baseline, and everything is the baseline.
    #the baseline = normal
    
    #set default colours: highlight calibration/baseline data and post data
    pp$colour <- rep("black", length(pp$y))
    pp$colourLab <- rep("normal", length(pp$y))
    index <- 1:length(pp$y)
    
  }else{
    #set default colours: highlight calibration/baseline data and post data
    pp$colour <- c(rep("gray80",bline_cut), rep("black", length(pp$y) - bline_cut ))
    pp$colourLab <- c(rep("baseline_obs",bline_cut), rep("post", length(pp$y) - bline_cut))
    index <- (bline_cut+1):length(pp$y)
  }
  
  #identify indicies for colouring SPC points
  spcIndexBad <- spcPoints$ID[which(spcPoints$Status =="B")]
  spcIndexGood <- spcPoints$ID[which(spcPoints$Status =="G")]
  spcIndexUnknown <- spcPoints$ID[which(spcPoints$Status=="U")]
  
  pp$colour[spcIndexBad]     <- "red"
  pp$colour[spcIndexGood]    <- "forestgreen"
  pp$colour[spcIndexUnknown] <- "brown"
  
  pp$colourLab[spcIndexBad]     <- "investigate"
  pp$colourLab[spcIndexGood]    <- "celebrate"
  pp$colourLab[spcIndexUnknown] <- "monitor"
  
  #last points colour
  pp$colour[length(pp$colour)]    <- "dodgerblue3"
  pp$colourLab[length(pp$colour)] <- "latest"
  
  return(pp)
}

# End Chart Parameters ####

# Generate the SPC charts ####

#PURPOSE: Create a list of 2 tables(df). 1 data needed to construct the SPC charts using GGPLOT. 2 special patterns data set
#COMMENT: I couldn't find a way to split this into 2 functions because they are very inter connnected opperations.
createTables <- function(dat, para)
 { 
  # trouble shooting ####
  # dat  <- data[[5]]
  # para <- chartParameters(dat)
  # end ####
  
  # create a data set with the data, CL, LCL, and UCL ####
  pp <- generateQICChart(dat,para)                           # generate the CL, LCL, and UCL
  df <- data.frame(y=pp$y, cl=pp$cl, lcl=pp$lcl, ucl=pp$ucl) # only keep the useful columns; I ignore the test results to run my own tests
  # end ####
  
  # tweaks needed if the is sufficient data for a baseline ####
  if(  is.na(para$bline_cut)   ){
    bline_index <-1:length(df$y)   #points analyzed for special patterns; all points
    s <-0                          #index shift for the pattern points
    t <-dat$Target[1:length(df$y)] #show the target for all points
  }else{ 
    bline_index <- (para$bline_cut+1):length(df$y)  #points analyzed for special patterns; after baseline
    s <- para$bline_cut                             #index shift for the pattern points
    t <- c( rep(NA, para$bline_cut), dat$Target[ (para$bline_cut+1):length(df$y) ]) #show targets after the baseline
  }
  # end ####
  
  # find patterns ####
  #run the modified nelson rules and schwarts rules to identify outliers and return a table
  patterns<-findAllPatterns(val = df$y[bline_index]
                            , ucl = df$ucl[bline_index]
                            , lcl = df$lcl[bline_index]
                            , mu = para$mu
                            , sigma = para$sigma
                            , dir = para$dir
                            , tfType = para$hTitle)

  #identify pattern points and update the patterns table with the baseline cutoff
  if( is.not.null(patterns) ){
    #change the index on patterns to reflect the actual indicies of the data
    patterns$starts <- patterns$starts + s
    patterns$ends <- patterns$ends +s
    
    #add indicator attributes to the patterns table
    patterns$indicatorID<- rep( para$indicatorID, nrow(patterns) )
    patterns$entity_group <- rep( para$entityName, nrow(patterns) )
    patterns$facility <- rep( para$facility, nrow(patterns) )
    patterns$startTFLab <- para$hLabelsFull[patterns$starts]
    patterns$endTFLab <- para$hLabelsFull[patterns$ends]
   
    #add a unique key to the patterns table to match records in iP
    #patterns$key <-  as.factor(paste(patterns$indicatorID
    #                                 , patterns$facility
    #                                 , patterns$entity_group
    #                                 , patterns$startTFLab
    #                                 , patterns$endTFLab
    #                                 , patterns$PType
    #                                 ,sep="-"))
                               
    #retrieve patterns in iP for the indicator to remove patterns from consideration; Decided not to include feature
    # if (  nrow(iP)>0  ){
    #   patterns <- patterns[!patterns$key %in% iP$key,]
    #   patterns <- patterns[, !names(patterns)=="key"]
    # }
   
    #extract spc points and reconcile each point to a single status
    temp <- extractPatternPoints(patterns)   
    temp <- reduceStatus(temp)                              #for points with G and B status reclassify as U.
    spcPoints    <- temp[order(temp$ID,decreasing=FALSE),]  #store results

    # end ####
  }else{
    spcPoints <- NULL
  }
    
  #add additional attributes ####
  df$x <- 1:nrow(df)   #add x index
  df   <- addSize(df)  #plot points sizes
  df   <- addShapes(df, para$bline_cut)            #plot points shapes
  df   <- addColour(df, spcPoints, para$bline_cut) #plot point colours and colour labels
  df$target   <- t            #add target
  df$tl <- longTermTrendMAVG(dat,para)  #add a long term trend line
  df <- df[,c("x","y","target","ucl","lcl","cl","colour","colourLab","size","shape","tl")] #reorder columns
  # end ####
  
  return(list(df=df,patterns=patterns))
}

#Purpose: Add custom entry for legend
addItemLegend <- function(chart_data,colour,label)
  {
  
  z <- chart_data[nrow(chart_data),]
  z$colour <-colour
  z$x <-NA
  z$y <-NA
  z$ucl <-NA
  z$lcl<-NA
  z$cl <-NA
  z$target <-NA
  z$colourLab <-label
  z$size <-1
  z$shape <-16
  z$tl <-NA
  
  chart_data <- rbind(chart_data,z)
  
  return(chart_data)
}

#PURPOSE: To create a SPC chart for a given indicator. 
createChart <-function(chart_data,para)
{
  #annotation locations
  xAnn_loc    <- max(1, round(nrow(chart_data)*0.1,0))          # at 10% of the total length of the data
  yAnn_loc_cl <- 1.05*round(chart_data$cl[xAnn_loc], para$prec) # value at the x annotation location
  yAnn_loc_l  <- 0.95*round(mean(chart_data$lcl, na.rm=TRUE), para$prec) # average lower bound
  yAnn_loc_u  <- 1.05*round(mean(chart_data$ucl, na.rm=TRUE), para$prec) # average upper bound
  
  #clTxt <-paste0("CL ",round(p$cl[1],prec)*scaleFactor,symb)
  clTxt  <- "Typical"
  lclTxt <- "Lower Limit"
  uclTxt <- "Upper Limit"
  
  #horizontal tick locations; break indicators
  bInd <- which(sapply(para$hLabels,FUN=nchar)>0)
  
  #add extra rows so colours show up in the legend.
  tl_colour     <- "darkorchid4"
  target_colour <- "darkorange3"
  
  if(  !all( is.na(chart_data$tl) ) ){
    chart_data    <- addItemLegend(chart_data,tl_colour,"trendline")
  }
  if(  !all( is.na(chart_data$target) ) ){
    chart_data    <- addItemLegend(chart_data,target_colour,"target")
  }
  
  #there is probably a better way to store that data in a tablea to build the GGPLOT
  #create the plot; warnings is removing the target NA's and the MAVG NAs. Target and MA must have type numeric.
  pl <- ggplot( data=chart_data, aes(x, y) ) +
    geom_point( aes(y=y, colour=colour, shape=shape, size=size)) +
    scale_shape_identity() +
    scale_size_identity() +
    scale_colour_identity("", guide="legend", breaks=unique(chart_data$colour), labels=unique(chart_data$colourLab) ) +
    scale_x_continuous(breaks=bInd, labels=para$hLabels[bInd]) +
    theme_bw(base_size = 18) +
    theme( panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) +
    theme(plot.caption=element_text(margin=margin(t=0),face="italic", size=8)) +
    labs(x=para$hTitle, y=para$vTitle, title=para$Main, subtitle=para$Sub, caption=paste0("Data Source:",para$datasource)) +
    geom_line(aes(x,cl)    , colour="black"      , linetype=2 ) +
    geom_line(aes(x,ucl)   , colour="grey20"     , linetype=3 ) +
    geom_line(aes(x,lcl)   , colour="grey20"     , linetype=3 ) +
    geom_line(aes(x,target), colour=target_colour, linetype=3 ) +
    geom_line(aes(x,tl)    , colour=tl_colour    , linetype=1 ) +
    annotate("text",label=clTxt , x=xAnn_loc, y=yAnn_loc_cl ,colour="black", size=annFontSize, fontface = "bold") +
    annotate("text",label=lclTxt, x=xAnn_loc, y=yAnn_loc_l, colour="black",size=annFontSize, fontface = "bold") +
    annotate("text",label=uclTxt, x=xAnn_loc, y=yAnn_loc_u, colour="black",size=annFontSize, fontface = "bold")

  #element_line(linetype = "dashed", colour = "grey80", size=0.25)
  #scale the y-axis where appropriate for percentages
  if(  para$VscaleFactor > 1  ){
    yl <- c(floor(max(0,.9*min(chart_data$y,chart_data$lcl))*para$VscaleFactor)/para$VscaleFactor, ceiling(min(1,max(chart_data$y,chart_data$ucl)*1.1)*para$VscaleFactor)/para$VscaleFactor)
    pl <- pl + scale_y_continuous(labels =function(x) para$VscaleFactor*x, limits=yl)
  }
  
  #add moving average if there is sufficient points to add a line
  if (  !all(is.na(chart_data$tl))  ){
    pl <- pl+ geom_line(aes(x,tl)    , colour=tl_colour )
  }
  
  return(pl)
}

#PURPOSE: Crafts a summary message to go under the SPC chart, on the highlights page, in the appendix
constructPatternMSGs <- function(patterns,para)
{
  #return NULL if there were no patterns
  if(is.null(patterns)){  
    return(list(chartTxt=NULL,highlightTxt="Typical", appendixTable=NA)) #no patterns to describe; may not use highlight txt
  }
  
  #appendix table
  patterns$indicatorName <- rep(para$indicatorName, nrow(patterns)) #add indicator name to patterns
  patterns$entityName    <- rep(para$entityName, nrow(patterns))    #add indicator entity names to patterns
  patterns$frequency     <- rep(para$timeFrameType, nrow(patterns)) #add frequency
  
  #pull in parameters based on the indicator frequency ####
  temp     <- tft_lookup_df[ which(tft_lookup_df$timeFrameType == para$timeFrameType),]
  hPara    <- histParaTable$histCut[ which(histParaTable$timeFrameType == para$timeFrameType ) ]
  un       <- temp$un
  tag      <- temp$tag
  # end ####
  
  #identify patterns touching recent history
  eligable <- patterns$ends >= (para$numObs-hPara+1)
  
  if (any(eligable) ){
    # create message text ####
    #text for the start and end of patterns
    starts  <- para$hLabelsFull[patterns$starts]
    ends    <- para$hLabelsFull[patterns$ends]
    timeTxt <- paste0(starts, " to ", ends)
    
    #pattern text ####
    xTxt <- patterns$PType
    xTxt[ patterns$PType == "shift" ] <- "jump"
    xTxt[ patterns$PType == "stability"] <- "stability change"
    yTxt <- patterns$Status
    yTxt[ patterns$Status=="G" ] <-"Favorable"
    yTxt[ patterns$Status=="B" ] <-"Unfavorable"
    patTxt <- paste0(yTxt," ", xTxt)
    # end ####
    
    #length text ####
    lengthTxt <- paste0( "(",patterns$ends - patterns$starts+1,rep(un,nrow(patterns)), ")" )
    # end ####
    
    #spc chart txt
    chartTxt <- paste0( patTxt[eligable], " ", lengthTxt[eligable])
    
    #highlight text
    if (  all( patterns$Status[eligable]=="G" )  ){
      highlightTxt <- "Celebrate"
    }else {
      highlightTxt <- "Investigate"  
    }
    
    return(  list(chartTxt=chartTxt, highlightTxt=highlightTxt, appendixTable=patterns)  )  #return a list with the chart text and highlights text
  } else{
    return(  list(chartTxt=NULL, highlightTxt=NULL, appendixTable=patterns)  )  #return a list with the chart text and highlights text
  }
}

#PURPOSE: to run the main function
main <- function(dat) #need to rename function
 {
  # trouble shooting ####
  # dat<- data[[1]]
  # end ####
  
  # generate chart parameters and data tables for ploting and patterns ####
  para <- chartParameters(dat)   # generate chart parameters
  l    <- createTables(dat, para) # generate data frames for plotting and patterns
  chart_data   <- l$df                       # pull out the chart data table
  patterns     <- l$patterns                 # pull out the special patterns data table
  rm(l) #remove the list
  # end ####
  
  # generate the SPC chart ####
  pl <-createChart(chart_data,para)
  # end ####
  
  ml <-constructPatternMSGs(patterns,para)          #get chart txt and highlights page txt
  shortVersionInclusion <- is.not.null(ml$chartTxt) #should the chart be shown in the short version of the report?

  #return the plot, the patterns table for the appendix, the text to accompany the SPC chart, the text to go on the highlights page.
  result <- list(pl, patterns, ml$chartTxt, ml$highlightTxt, ml$appendixTable, shortVersionInclusion, para$indicatorName,para$entityName, para$MYVCHCategory)
  names(result) <-c("plot","patterns","chartTxt","highlightTxt", "appendixTable", "shortVersionInclusion","indicatorName","entityName","MYVCHCategory")
  return(result)
}
# End Generate the SPC charts ####

