df1 <- data.frame(school = c('MSU','VT','Mines'),
                  state= c('MT','VA','CO'), stringsAsFactors = F)
df2 <- data.frame(school = c('Mines','MSU','VT'),
                  enrollment = c(5794,15688,30598), stringsAsFactors = F)
df2

# Run this function

MergeData <- function(data1, data2, key1, key2){
  # function to merge two data sets
  # Args: data1 - first dataset
  #       data2 - second dataset
  #       key1 - key name in first dataset
  #       key2 - key name in second dataset
  # Returns: merged dataframe if key matches, 
  #          otherwise print an error  
  if (key1 = key2){
    data.out <- join(data1,data2, by = key1)
    return(dataout)
  } else {
    stop('keys are not the same')
  }
}

MergeData(df1,df2,"school","school")