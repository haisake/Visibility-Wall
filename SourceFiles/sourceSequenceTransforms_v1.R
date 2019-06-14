##########################
#Purpose: To house functions necessary for turning data into binary sequences representing certain underlying patterns
#Author: Hans Aisake
#Date Created: December 20, 2018
#Date Modified: 
#Comments:
# I split this off from sourceSpecialCauseRules because that source file was too long.

#PURPOSE: Turns a vector of numbers into a binary sequence identifying increasing trends
incSeq <- function(val)
{ z <-diff(val)           #find differences
  z <-as.numeric( z>0 )   #>0 implies increases and numeric changes to int
  
  z<-which(is.na(z))  
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying decreasing trends
decSeq <- function(val)
{ z <-diff(val)           #find differences
  z <-as.numeric( z<0 )   #<0 implies decreases and numeric changes to int
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points above the UCL. If sigma=0 return all 0s. If NA element return 0.
aboveUCLSeq <- function(val, ucl,sigma)
{ if(sigma==0){ z <-as.numeric(val>ucl) }
  else{  z <-as.numeric(val>=ucl)  }

  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur.
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points below a given LCL. If sigma=0 striclty less than. If NA element return 0.
belowLCLSeq <- function(val, lcl, sigma)
{ #if there is no variation ignore the equality
  if(sigma==0){ z <-as.numeric(val<lcl) }
  else{  z <-as.numeric(val<=lcl) }

  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points above mu+2sigma. If sigma=0 striclty above. If NA element return 0.
above2SigmaSeq<- function(val, mu, sigma)
{ 
  if(sigma==0){ z <-as.numeric(val>mu)  }
  else{ z <-as.numeric(val>=mu+2*sigma) }
  
  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points below mu-2sigma. If sigma=0 striclty below. If NA element return 0.
below2SigmaSeq<- function(val, mu, sigma)
{ 
  if(sigma==0){  z <-as.numeric(val<mu) }
  else{ z <-as.numeric(val<=mu-2*sigma) }
  
  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points above mu+sigma. If sigma=0 striclty above. If NA element return 0.
aboveSigmaSeq<- function(val, mu, sigma)
{ if(sigma==0){    z <-as.numeric(val>mu)  }
  else{ z <-as.numeric(val>=mu+sigma)  }
  
  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points below mu-sigma. If sigma=0 striclty below. If NA element return 0.
belowSigmaSeq<- function(val, mu, sigma)
{ if(sigma==0){    z <-as.numeric(val<mu)  }
  else{    z <-as.numeric(val<=mu-sigma)  }
  
  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z)
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points between mu +/- sigma. If sigma=0 return all 0s. If NA element return 0.
betweenSigmaSeq<- function(val, mu, sigma) 
{ if(sigma==0){    z <-rep(0,length(val))  }
  else{ z <-as.numeric( abs(val-mu)<=sigma  )  }
  
  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z) 
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points between mu +/- sigma. If sigma=0 return all 0s. If NA element return 0.
beyondSigmaSeq<- function(val, mu, sigma) 
{ 
  if(sigma==0){    z <-rep(0,length(val))  }
  else{ z <-as.numeric( abs(val-mu)>=sigma  )  }
  
  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z) 
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points above CL.
aboveCLSeq<- function(val, cl)  
{ z <-as.numeric( val>cl) 

  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z)  
}

#PURPOSE: Turns a vector of numbers into a binary sequence identifying points below CL.
belowCLSeq<- function(val, cl)  
{ z <-as.numeric( val<cl) 

  z[which(is.na(z))]<-0 #map N/A to 0 to avoid down stream crashes. This case should never actually occur
  return(z)
}