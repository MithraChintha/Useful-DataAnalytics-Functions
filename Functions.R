#MergeData function is to merge All the data sets provided
MergeData =function(){
  datapt1 <- read.csv("test data pt1/orders_export.csv",stringsAsFactors = F)
  datapt2 <- read.csv("test data pt2/orders_export_1.csv",stringsAsFactors = F)
  datapt3 <- read.csv("test data pt3/orders_export_2.csv",stringsAsFactors = F)
  mergedFile <- rbind(datapt1,datapt2,datapt3)
  return(mergedFile)
}

#function to convert Datatime column to weekday and Hour columns and delete the original column
convertDateTime <- function(inputData,columnNames){
  #load the lubridate package 
  if(!require(lubridate)){
    install.packages("lubridate")
    library(lubridate)
  }
  #loop to check the date time features in the data frame and convert them into weekday and hour
  for(i in seq(1:length(columnNames))){
      if(is.Date(as.Date(inputData[,columnNames[i]]))){
      #Create Derived column names
      colweekday <- paste(columnNames[i],'day',sep = "")
      colHour <- paste(columnNames[i],'Hour',sep = "")
      #generate Values based on the Date time
      inputData[,colweekday] <- weekdays(as.Date(inputData[,columnNames[i]]))
      inputData[,colHour] <-  format(as.POSIXct(inputData[,columnNames[i]], format="%Y-%m-%d %H:%M"), format="%H")
      inputData[,columnNames[i]] <- NULL
    }
  }
  return(inputData)
}

#function to collapse levels in all the columns provided in the data frame
#inputData is data frame, collapseLevls is how many levels should be preserved 
#defaultLevel is others which can be changed based on the column
collapseLevels=function(inputData,collapseLevels=6,defaultLevel='others'){
  #get the count of observations 
  totRows=nrow(inputData)
  #loop to reduce the each column levels based on collapseLevels given
  for(i in seq(1:ncol(inputData))){
    #collapse levels if the column is of type character or factor
    if(is.factor(inputData[,i]) | is.character(inputData[,i])){
      levels(inputData[,i])
      x <- data.frame(table(inputData[,i]))
      y <- as.vector(head(x %>% arrange(desc(Freq)),collapseLevels)[,1])
      inputData[,i] <- as.character(inputData[,i])
      #assign the default level to all the rows other than top collapse levels
      inputData[!(inputData[,i] %in% y),i] <- defaultLevel
      inputData[,i] <- as.factor(inputData[,i])
    }
   }
  return(inputData)
}



#function for missing values detection in the given data frame
Missing=function(data){
  if(!is.data.frame(data))
  {
    return(warning("given input is not a data frame"))
  }
  #To get the number of columns
  cols =ncol(data)
  #generate the numbers from 1 to number of columns
  seqNumber =seq(1,cols)
  #create a default data frame
  result <- data.frame()
  #for loop to get the column names and missing values
  for(p in seqNumber)
  {
    #get the column name of the feature based on the indexing
    colName=colnames(data)[p]
    #Get the number of missing values
    NoofMissingValues=sum(is.na(data[,p]))
    #Calculate unique values
    NoofUniqueValues =length(unique(data[,p]))
    #Calculate the missing values in proportion
    MissingValueproportion= NoofMissingValues/length(data[,p])
    #use cbind to merge colname and missing values
    x= cbind(colName,NoofMissingValues,NoofUniqueValues,round(MissingValueproportion,5))
    #bind the rows to the data frame
    result= rbind(result,x)
  }
  #Assign column names to the data frame
  colnames(result) =c("Column Name","No. of Missing Values","No of Unique Values","Missing Value Proportion")
  #return the result
  return(result)
}

#Mode function returns the mode of a vector input with highest count
Mode=function(dfColumn){
  #get the frequency counts of the each unique value
  x=table(as.factor(dfColumn))
  #get the value with highest frequency counts
  y=names(x[which.max(x)])
  #return the mode of the vector provided
  return(y)
}  


#Model Metrics Function returns model metrics based on actual and predicted
#returns Mean absolute deviation(MAD), Mean Square Error(MSE),
#RSquare, Trimmed Mean absolute deviation(TMAD)
ModelMetrics=function(actual,predicted){
  a=actual
  m=predicted
  metrics <- c(MAD=0,MSE=0,R2=0,TMAD=0,p90=0)
  metrics["MAD"] <- mean(abs(a-m))
  metrics["MSE"] <- mean((a-m)^2)
  #How much variation in a(Target) is explained by the m(model)
  #1- (sse/sst)
  #sse - a-mean(a) sqaure and sum
  #SST - a- m square and sum
  SST= sum((a-mean(a))^2)
  SSE= sum((a-m)^2)
  metrics["R2"]= 1- (SSE/SST)
  metrics["TMAD"]= mean(abs(a-m),tri=0.05)
  metrics["p90"]= quantile(abs(a-m),probs = 0.9)
  return(metrics)
}




