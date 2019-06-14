# SOURCE HEADER ####
#Purpose: To house functions necessary for running the SPC chart special detection rules
#Author: Hans Aisake
#Date Created: July 17, 2018
#Date Modified: Jan 8, 2018
#Comments:
# END ####

# Pattern Detection    #####

#PURPOSE: Identify patterns of 11, 111, 1111, 11111,..... within XX1111XXXX, or 111XXX111XXXX, etc...
contiguousSequence <-function(seq)
{
  if(length(seq)<2 | any(seq==1)==FALSE){
    return(NULL)
  }
  
  #starts of contiguous sequences can be found where the difference of the sequences values =1
  #ends can be found when the difference is -1
  startVal=1
  endVal=-1
  
  #find the differences of the sequences and indecies of rows starting and ending runs
  seq_dif <- diff(seq)
  
  seq_start <- which(seq_dif==startVal)+1
  seq_end <- which(seq_dif==endVal)

  #does the first record needed to be noted as the start of a contiguous sequence?
  if(seq[1]==1){
    seq_start <- c(1, seq_start)
  }
  
  #does the last record need to be noted as the end of a run?
  if( seq[length(seq)]==1){
     seq_end <- c(seq_end,length(seq))
  }
  
  #no contiguous values
  if(length(seq_start)==0){
    return(NULL)
  }
  
  result <- data.frame( starts =seq_start, ends =seq_end)
  result$length <- result$ends-result$starts+1
  
  return(result)
}

#PURPOSE: Identify oscillations
oscillationDetection <-function(val){
  
  #need at least 3 values
  if(length(val)<3){
    return(NULL)
  }
  
  #internal var
  z<-val
  
  #standardize Z to 1 or -1
  z[which(z!=0)] <- z[which(z!=0)]/abs(z[which(z!=0)])
  z[which(z==0)] <- 9
  
  #find difs of the standardized data
  z_dif <- diff(z)
  
  #flip change 0's to -1's
  temp_ind <- which(z_dif!=0)
  temp_ind2 <-which(z_dif==0)
  temp<-z_dif
  temp[temp_ind]<-0
  temp[temp_ind2]<--1
  
  #find difs of the flip
  temp_dif <- diff(temp)
  
  #find starts and ends
  startVal <-1
  endVal <--1
  
  starts <- which(temp_dif==startVal)
  ends <- which(temp_dif==endVal)
  
  #shift starts and ends
  starts <- starts +1
  ends<- ends+1
  
  #does the first value need to be a start?
  if(z_dif[1]!=0){
    starts<-c(1,starts)
  }
  
  #does the last value need to be an end?
  if(z_dif[length(z_dif)]!=0){
    ends<-c(ends,length(z_dif)+1)
  }
  
  #combine starts and ends
  result <- data.frame( starts, ends)
  result$length <- result$ends-result$starts+1
  
  return(result)
  
}
# ** FIND TRENDS    ####
# Rule 3: 6+ consecutive points continually increasing (or decreasing)

#PURPOSE: Identify trends in the data
identifyTrends <- function(val,targetDir)
{
  #return NULL for cases that can't have trends. Causes errors if you try to find them.
  if(length(val)<7){  return(NULL)  }   #need at least 7 values to find trends of sufficient length
  if ( length(unique(val)==0)  ){  return(NULL)  }    #if all values are equal return NULL
  
  #convert values into increase and decreasing sequences of 0,1 nature 1 if inc or dec depending
  ii<-incSeq(val)
  dd<-decSeq(val)
  
  #find trends and their lengths
  incTrends <-contiguousSequence(ii)
  decTrends <- contiguousSequence(dd)
  
  #correct end points due to index shifts with differencing. Ends +1
  incTrends$ends<-incTrends$ends+1
  decTrends$ends<-decTrends$ends+1

  #add context messages and fields
  if(is.not.null(incTrends)){
    if(targetDir=="Below"){ x<-"B"} else {x <-"G"}
    
    incTrends$dir <- rep("Increasing",nrow(incTrends))
    incTrends$Status <- rep(x,nrow(incTrends))
    incTrends$PType <- rep("trend" ,nrow(incTrends))
  }
  if(is.not.null(decTrends)){
    if(targetDir=="Below"){ y<-"G"} else {y <-"B"}
    
    decTrends$dir <- rep("Decreasing",nrow(decTrends))
    decTrends$Status <- rep(y,nrow(decTrends))
    decTrends$PType <- rep("trend" ,nrow(decTrends))
  }
  
  #combine results
  result <-rbind(incTrends,decTrends)
  
  #keep only trends of a sufficient length
  result<-result[which(result$length>=6),]
  
  #if there are no large enough trends return a null, otherwise return the result
  if(is.not.null(result)){
    if(nrow(result)==0){  return(NULL)}
    else{  return(result)  }
    #end if
  }else{  return(NULL)  }
  #end if
}

# ** FIND OUTLIERS ####
#Rule 1:   #One point is above the UCL or below the LCL

#PURPOSE: Find streaks of outliers above and seperately outliers below using rule 1.
identifyOutliers <- function(val,targetDir, ucl, lcl, sigma)
{
  #convert values into shift/not shift 1,0 sequence
  aa <-aboveUCLSeq(val, ucl, sigma)
  bb <-belowLCLSeq(val, lcl, sigma)
  
  #find beyond outliers and their lengths.
  aaOut <- contiguousSequence(aa)
  bbOut <- contiguousSequence(bb)

  #add context messages and fields
  if(is.not.null(aaOut)){
  
    if(targetDir=="Below"){ x<-"B"} else {x <-"G"}
    
    aaOut$dir <- rep("Above",nrow(aaOut))
    aaOut$Status <- rep(x,nrow(aaOut))
    aaOut$PType <- rep("outlier",nrow(aaOut))
  }
  if(is.not.null(bbOut)){
    
    if(targetDir=="Below"){ y<-"G"} else {y <-"B"}
    
    bbOut$dir <- rep("Below",nrow(bbOut))
    bbOut$Status <- rep(y,nrow(bbOut))
    bbOut$PType <- rep("outlier",nrow(bbOut))
  }
  
  #combine results
  result <-rbind(aaOut,bbOut)
  
  #if there are no outliers return a null, otherwise return the result
  if(is.not.null(result)){
    if(nrow(result)==0){
      return(NULL)
    }else{
      return(result)
    }
  }else{
    return(NULL)
  }
  
}

# ** FIND SHIFTS ####
#Rule 2: Nine+ consecutive points on the same side of the mean.
#Rules 5: 2/3 or 3/3 points beyond 2 sigma
#Rules 6: 4/5 or 5/5 points beyond 1 sigma

#PURPOSE: Identify the starts and ends of shifts using rule 2.
identifyShiftsR2 <- function(val, targetDir, cl)
{
  #need at least 7 values to find trends of sufficient length
  if(length(val)<9){    return(NULL)  }
  
  #convert values into shift/not shift 1,0 sequence
  aa <-aboveCLSeq(val,cl)
  bb <-belowCLSeq(val,cl)
  
  #find shifts and their lengths.
  aaShift <- contiguousSequence(aa)
  bbShift <-contiguousSequence(bb)

  
  #only want shifts that are at least 2 in length
  aaShift <-aaShift[which(aaShift$length>=2),]
  bbShift <-bbShift[which(bbShift$length>=2),]
  
  #add context messages and fields
  if(is.not.null(aaShift)){
    if(targetDir=="Below"){ x<-"B"} else {x <-"G"}
    
    aaShift$dir <- rep("Above",nrow(aaShift))
    aaShift$Status <- rep(x,nrow(aaShift))
    aaShift$PType <- rep("shift",nrow(aaShift))
  }
  if(is.not.null(bbShift)){
    if(targetDir=="Below"){ y<-"G"} else {y <-"B"}
    
    bbShift$dir <- rep("Below",nrow(bbShift))
    bbShift$Status <- rep(y,nrow(bbShift))
    bbShift$PType <- rep("shift",nrow(bbShift))
  }
  
  #combine results
  result <-rbind(aaShift,bbShift)

  #keep only shifts of a sufficient length
  result<-result[which(result$length>=9),]
  
  #if not shifts exists return a null, else return the shifts
  if(is.not.null(result)){
    if(nrow(result)==0){
      return(NULL)
    }else{
      return(result)
    }
  }else{
    return(NULL)
  }
  
}

#PURPOSE: Find streaks where the values are beyond 2 stdeviations (sigma) from the mean (mu). Slight modificaiton of rule 5.
identifyShiftsR5 <- function(val,targetDir, mu, sigma)
{
  #not able to compute a sigma no result
  if(is.na(sigma)){  return(NULL)  }
  
  #convert values into shift/not shift 1,0 sequence
  aa <-above2SigmaSeq(val,mu, sigma)
  bb <-below2SigmaSeq(val,mu, sigma)
  
  #find shifts and their lengths.
  aaShift <-  contiguousSequence(aa) 
  bbShift <-  contiguousSequence(bb) 
  
  #only want shifts that are at least 2 in length
  aaShift <-aaShift[which(aaShift$length>=2),]
  bbShift <-bbShift[which(bbShift$length>=2),]
  
  #add context messages and fields
  if(is.not.null(aaShift)){
    if(targetDir=="Below"){ x<-"B"} else {x <-"G"}
    
    aaShift$dir <- rep("Above",nrow(aaShift))
    aaShift$Status <- rep(x,nrow(aaShift))
    aaShift$PType <- rep("shift",nrow(aaShift))
  }
  if(is.not.null(bbShift)){
    if(targetDir=="Below"){ y<-"G"} else {y <-"B"}
    
    bbShift$dir <- rep("Below",nrow(bbShift))
    bbShift$Status <- rep(y,nrow(bbShift))
    bbShift$PType <- rep("shift",nrow(bbShift))
  }
  
  #combine results
  result <-rbind(aaShift,bbShift)
  
  #keep only shifts of a sufficient length
  result<-result[which(result$length>=3),]
  
  #if not shifts exists return a null, otherwise return the shifts
  if(is.not.null(result)){
    if(nrow(result)==0){
      return(NULL)
    }else{
      return(result)
    }
  }else{
    return(NULL)
  }
}

#PURPOSE: Find streaks where the values are beyond 1 stdeviations (sigma) from the mean (mu).Slight modificaiton of rule 6.
identifyShiftsR6 <- function(val,targetDir, mu, sigma)
{
  #not able to compute a sigma no result
  if(is.na(sigma)){    return(NULL)  }
  
  #convert values into shift/not shift 1,0 sequence
  aa <-aboveSigmaSeq(val,mu, sigma)
  bb <-belowSigmaSeq(val,mu, sigma)
  
  #find shifts and their lengths.
  aaShift <- contiguousSequence(aa) ##might want to change to groupSequence()
  bbShift <- contiguousSequence(bb) ##might want to change to groupSequence()
  
  #only want shifts that are at least 2 in length
  aaShift <-aaShift[which(aaShift$length>=2),]
  bbShift <-bbShift[which(bbShift$length>=2),]
  
  #add context messages and fields
  if(is.not.null(aaShift)){
    if(targetDir=="Below"){ x<-"B"} else {x <-"G"}
    
    aaShift$dir <- rep("Above",nrow(aaShift))
    aaShift$Status <- rep(x,nrow(aaShift))
    aaShift$PType <- rep("shift",nrow(aaShift))
  }
  if(is.not.null(bbShift)){
    if(targetDir=="Below"){ y<-"G"} else {y <-"B"}
    
    bbShift$dir <- rep("Below",nrow(bbShift))
    bbShift$Status <- rep(y,nrow(bbShift))
    bbShift$PType <- rep("shift",nrow(bbShift))
  }

  #combine results
  result <-rbind(aaShift,bbShift)
  
  #keep only shifts of a sufficient length
  result<-result[which(result$length>=5),]
  
  #if not shifts exists return a null, otherwise return the shifts
  if(is.not.null(result)){
    if(nrow(result)==0){
      return(NULL)
    }else{
      return(result)
    }
  }else{
    return(NULL)
  }
}

# ** FIND STABILITY CHANGES ####
#Rule 7:15 consequtive points within 1 sigma of mu
#Rule 8:8 consequtive points beyond 1 sigma of mu

#PURPOSE: Only keep records with with values on both sides
filterValuesBothSides <- function(df,val,mu)
  {
  
  if(is.null(df)){    return(NULL)  } #if the df is null return NULL
  
  tracker <-NULL #need to remove shifts in ii and dd which don't have values on both sides of mu
  
  #for each record in the df identify which records have data on both sides
  for( kk in 1:nrow(df)){
    ind <-df$starts[kk]:df$ends[kk]
    arg1 <- any(val[ind]>mu)
    arg2 <- any(val[ind]<mu)
    
    if(arg1 & arg2){  tracker<-c(tracker,kk)  }
  }
  
  #only keep items in the tracker or return NULL
  if(length(tracker)==0){  return(NULL)  }
  else{   return(df[tracker,])}

}

#PURPOSE: Identify the starts and ends of changes in variability using rules 7 and 8.
identifyStability <- function(val, mu, sigma)
{
  #troubleshooting
  # val <-c(0,1,2,1,2,0,-1,-2,1,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,-1,2,0,-1,1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,-2,2,-2,-2,2,3,4,-2,-4,0,1,0,1,-1,-3,3,4,-4,7,6,-6,1,0,1,0)
  # mu <-0
  # sigma <-1
  
  #need at least 15 values to find shifts in stability; and need a sigma
  if(length(val)<15 | is.na(sigma) | sigma==0){   return(NULL)  }
  
  #convert values into 1/0 series identifying periods of changed stability and not 1,0
  ii <-betweenSigmaSeq(val,mu,sigma)  #stability improved
  dd <-beyondSigmaSeq(val,mu,sigma)   #stability decreased
  
  #find shifts in stability and the length of durration
  iiShift <-contiguousSequence(ii)
  ddShift <- contiguousSequence(dd)
  
  #need to remove shifts in ii and dd which don't have values on both sides of mu
  iiShift<-filterValuesBothSides(iiShift, val, mu)
  ddShift<-filterValuesBothSides(ddShift, val, mu)
  
  #only want shifts that are at least 2 in length
  iiShift <-iiShift[which(iiShift$length>=2),]
  ddShift <-ddShift[which(ddShift$length>=2),]
  
  #add context messages and fields
  if(is.not.null(iiShift)){
    iiShift$dir <- rep("Improved",nrow(iiShift))
    iiShift$Status <- rep("G",nrow(iiShift))
    iiShift$PType <- rep("stability",nrow(iiShift))
  }
  if(is.not.null(ddShift)){
    ddShift$dir <- rep("Decreased",nrow(ddShift))
    ddShift$Status <- rep("B",nrow(ddShift))
    ddShift$PType <- rep("stability",nrow(ddShift))
  }
  
  #combine results
  result <-rbind(iiShift,ddShift)
  
  #keep only shifts of a sufficient length
  result<-result[which(result$length>=15),]
  
  #if there are no long enough changes in stability return a null
  if(is.not.null(result)){
    if(nrow(result)==0){
      return(NULL)
    }else{
      return(result)
    }
  }else{
    return(NULL)
  }
  
}

# ** FIND OSCILLATIONS ####
#Rule 4: 14+ consecutive points alternating in direction, increasing then decreasing (bimodal, 2 or more factors in data set)

#PURPOSE: Identify the starts and ends of oscillations uses rule 4.
identifyOscillations <- function(val)
{
  #troubleshooting test series
  # val <-c(1,5,3,4,6,8,10,3,3,2,4,7,8,9,10,11,8,9,10,14,8,7,5,6,2,3,1,3,4,2,0,1,0,0,0,1,2,3,2,1,10,11,14,10,8,8,9,7,8,5,14,5,6,8,9,12,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,4,4,4,4,4,4,4,4,5,3,5,3,7)
  # val <-c(1,-1,1,0,1,-1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,1,-1,1,-1,1,-1,1,0,1,0,0,1,1,1,1,0,1,0,1,1,0,1,0,1,1,1,1,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0,1,0,1,1,1,1,0,1,1,0,0,1,0,1,0,1,0,1)
  
  
  #identify the start, end, and length of oscillations
  osc <-oscillationDetection(val)
  
  #add context messages and fields
  if(is.not.null(osc)){
    osc$MSG <- rep("Oscillation",nrow(osc))
    osc$Status <- rep("B",nrow(osc))
    osc$PType <-rep("oscillation",nrow(osc))
  }

  #keep only oscillations of sufficient length. 14 is given by the Nelson rules.
  osc <-osc[which(osc$length>=14),]
  
  #if there are no long enough oscillations return a null
  if(is.not.null(osc)){
    if(nrow(osc)==0){
      return(NULL)
    }else{
      return(osc)
    }
  }else{
    return(NULL)
  }
}

# End Pattern Detection ####

# FUNCTIONS FOR PATTERNS #####

#PURPOSE: Some points are both G and B and have ambiguous status. The status label is set to U for unknown.
#only one label is returned for each point.
reduceStatus <-function(df){
  
  #need to assign an unknown status when the point is both G and B
  gInd <-which(df$Status=="G")
  bInd <-which(df$Status=="B")
  
  #find ID's with both Good and Bad and assign status U
  ind <- intersect(gInd,bInd)
  #where there is an overlap change status to U
  if( length(ind)>0 ){
    df$Status[ind]<-"U"
  }
  
  #needed to reduce it back to a single point
  df<-unique(df)
  return(df)
}

#PURPOSE: Extract the indicies of statistically interesting points from a patterns table
extractPatternPoints <- function(df){
  
  #if there are no patterns return a NULL
  if(  is.null(df)  ){
    return(NULL)
  }else{
    
    #intializes a results variable with a junk row
    results<-data.frame(ID=1,Status=1)
    
    #for each pattern identify statistically interesting points and if they have good or bad status
    for(ii in 1:nrow(df)){
      x<-seq(from=df[ii,"starts"], to=df[ii,"ends"])
      y<-rep(df[ii,"Status"],length(x))
      d <-data.frame(ID=as.numeric(x),Status=y) 
      results<-rbind(results,d)
    }
    
    #remove the junk row from the results variable
    results<-results[-1,]
    
    #if a point is statistically interesting in 2+1 patterns with similar status remove duplicate indications 
    results <- unique(results)
    
    results <- reduceStatus(results)
    
    #order results
    results<-results[order(results$ID,decreasing=FALSE),]
    
    return(results)
  }
}

#PURPOSE: Consolidate overlaping patterns into single patterns based on start and end overlaps. Distinction of pattern type or status
#must be specified by spliting the input data
consolidateOverlaps <- function(dFrame)
  {
  
  #find overlaps
  m <- sqldf("SELECT distinct CASE WHEN Y.starts is NULL THEN X.starts
                          WHEN X.starts <= Y.starts THEN X.starts
                          WHEN Y.starts < X.starts THEN Y.starts
                          END as 'starts'
                    ,CASE WHEN Y.ends is NULL THEN X.ends
                          WHEN X.ends >= Y.ends THEN X.ends 
                          WHEN X.ends < Y.ends THEN Y.ends 
                    END as 'ends'
              FROM dFrame AS X 
              LEFT JOIN dFrame AS Y 
              ON (Y.starts BETWEEN X.starts AND X.ends OR X.starts BETWEEN Y.starts AND Y.ends) 
              AND (X.starts !=Y.starts OR X.ends!=Y.ends)
             ")

  if( nrow(dFrame)==nrow(m)  ){
    #return the data there are no more overlaps
    return(dFrame)  
  } else{
    #look for more overlaps
    m2<-consolidateOverlaps(m)

    return(m2)
  }
  
}

#PURPOSE: Add back dir, ptype, and Status; only colapses records with one status?
addBackAttributes <- function(df, dir, ptype, status)
  {
  n<-nrow(df)
  
  df$length <- df$ends-df$starts+1
  df$dir    <- rep(dir,n)
  df$PType  <- rep(ptype,n)
  df$Status <- rep(status,n)
  
  return(df)
  
}

#PURPOSE: reduce shifts to single disjoint intervals
reduceShifts <- function(df)
  {
  #identify if there are good and bad shifts and which records
  bShiftID <- which(df$PType=="shift" & df$Status=="B")
  gShiftID <- which(df$PType=="shift" & df$Status=="G")
  oID   <- which(df$PType!="shift")
  
  #if no shifts detected break
  if(length(bShiftID)+length(gShiftID)==0){
    return(df)  
  }else{
    
    #reduce overlaping shifts to single records
    bShifts<-consolidateOverlaps(df[bShiftID,]) 
    gShifts<-consolidateOverlaps(df[gShiftID,])

    #add back any missing attributes
    if(nrow(bShifts)>0){  bShifts <- addBackAttributes(bShifts, dir=df$dir[bShiftID[1]],  ptype=df$PType[bShiftID[1]], status="B") }
    else{  bShifts <- NULL  }
    
    if(nrow(gShifts)>0){ gShifts <- addBackAttributes(gShifts, dir=df$dir[gShiftID[1]],  ptype=df$PType[gShiftID[1]], status="G")  }
    else{  gShifts <- NULL  }
    
    if(length(oID)==0){  other <- df[oID,]  }
    else{   other <- NULL     }
    
    #reconstruct df
    x <- rbind(other, bShifts,gShifts)
    x<-x[order(x$ends, decreasing=TRUE),]
    return(x)
  }
}

# END FUNCTIONS ####

# Main Function ####
# PURPOSE: Runs pattern and outlier detection algorithms and then stores the results in a data frame 
findAllPatterns <- function(val, ucl, lcl, mu, sigma, dir,tfType)
{
  #hardcoded paramters
  #hist para found in the main environment
  
  # trouble shooting
  # index <- bline_index
  # val <- pp$y[index]
  # ucl <- pp$ucl[index]
  # lcl <- pp$lcl[index]
  # mu  <- para$mu
  # sigma <- para$sigma
  # dir <- para$dir
  # tfType <-"Weekly"

  #run the detection algorithms
  trends <- identifyTrends(val,dir)
  outliers <- identifyOutliers(val, dir, ucl, lcl, sigma)
  shiftsR2 <- identifyShiftsR2(val, dir, mu)
  shiftsR5 <- identifyShiftsR5(val, dir, mu, sigma)
  shiftsR6 <- identifyShiftsR6(val, dir, mu, sigma)
  #we have several shift detecting algorithms, this consolidates the results into single shift notifications.
  #this comes at a cost of oscuring the pvalue of the shift although we could guess it's max.
        shifts <- reduceShifts(rbind(shiftsR2,shiftsR5,shiftsR6))
  oscillations <-identifyOscillations(val)
  stability <- identifyStability(val, mu, sigma)
  
  #combine patterns into one df
  patterns <- rbind(trends, outliers, shifts, oscillations, stability)
  
  if(is.null(patterns)){  return(NULL)   #if nothing was found return nulls
  } else{
    #order patterns based on start date
    patterns <- patterns[order(patterns$starts,decreasing=FALSE),]   #signifcant changes needed here
    return(patterns)
  }
  
}

# End Main Function ####