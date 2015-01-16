#Functions for analysis

#reorder the AbbrevName factor by volume requires a reverse order function
reverse_order <- function(x){
  rev_order <- -1*mean(x)
}


clean_big <- function(x) {
  if(x > 1800) {    
x <- NA
  }   
  return(x)
}
vclean_big <- Vectorize(clean_big) 

#make a function that compares two character strings and if they are equal, return the 2nd sting;
#if they are not equal, return the concatenation of the first string and the 2nd string
#clean TeamName and SeriesName into one name to by grouped by
name_maker <- function(x,y) {
  if(identical(x,y)){
    y <- y
  } else {
    y <- paste0(x,"_",y) 
  }  
  return(y)
}
vname_maker <- Vectorize(name_maker)

trim.trailing <- function (x) sub("\\s+$", "", x)

Vtrim.trailing <- Vectorize(trim.trailing)

clean_up_df <- function(df=df1){
  names(df)
  
  df$SeriesName <-  gsub("- ","-",df$SeriesName)
  df$SeriesName <-  gsub(" -","-",df$SeriesName)
  #df$TeamName   <-  gsub(".","",df$TeamName)
  df$SeriesName <-  Vtrim.trailing(df$SeriesName)
  #df$TeamName   <-  Vtrim.trailing(df$TeamName)
  
  
  for(i in c(2:4,6)){
    df[,i] <- as.factor(df[,i])
  }
  
  df$Value1<-gsub("=0","0",df$Value1)
  
  for(i in c(7:8)){
    df[,i] <- as.numeric(df[,i])
  }
  #call after 7:8 trans to numeric
  df$Value1<-vclean_big(df$Value1)
  #switched $seriesName from TeamName
  df$SeriesName<-vname_maker(df$TeamName,df$SeriesName)
  
  df$TimePeriod <- as.Date(df$TimePeriod,"%m/%d/%Y")
  
  #delete records where Team Active Status != true from data set
  dfZ <- droplevels(df[df$Team.Active.Status != "False",])
  summary(dfZ)
  
  #dfZ$AbbrevName <- reorder(dfZ$AbbrevName,dfZ$Volume,reverse_order)
  #assign MeasureName_ID as internal integer identifier
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


