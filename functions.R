#Functions for analysis

#reorder the AbbrevName factor by volume requires a reverse order function
reverse_order <- function(x){
  rev_order <- -1*mean(x)
}

clean_up_df <- function(df=df1){
  names(df)
  for(i in c(2:4,9,11)){
    df[,i] <- as.factor(df[,i])
  }
  
  for(i in c(7:8,10)){
    df[,i] <- as.numeric(df[,i])
  }
  
  df$TimePeriod <- as.Date(df$TimePeriod,"%m/%d/%Y")
  
  #delete Test Team from data set
  dfZ <- droplevels(df[df$SeriesName != "Test Team",])
  #summary(df2)
  
  #drop measure of HCAHPS Willingness to Recommend, SCIP VTE 6 measure and VTE/DVT Prophylaxis
  dfZ <- droplevels(dfZ[dfZ$MeasureName != "HCAHPS Percent \"Top Box\" Willingness to Recommend", ])
  dfZ <- droplevels(dfZ[dfZ$MeasureName != "Percent SCIP-VTE-6 Hospital Acquired Potentially-Preventable Venous Thrombolism",])
  dfZ <- droplevels(dfZ[dfZ$MeasureName != "Percent VTE/DVT Prophylaxis",])
  
  
  dfZ$AbbrevName <- reorder(dfZ$AbbrevName,dfZ$Volume,reverse_order)
  dfZ$MeasureName_ID <- as.numeric(dfZ$MeasureName)
  return(dfZ)
}

#get table of short measure names and measure types
df_measure_names<- read.csv("MeasureNameTable1.csv",colClasses="character")

measure_type <- function(x,df=df_measure_names) {
  #given a measure name x, looks up measure type in table contained in df
  dfA <- df[df$Extranet.Name==x,]
  measure_type <- dfA$MeasureType[1]
}


short_name <- function(x,df=df_measure_names) {
  #given a measure name x, looks up a short measure name
  dfB <- df[df$Extranet.Name==x,]
  short_name <- dfB$NewName2[1]
}


