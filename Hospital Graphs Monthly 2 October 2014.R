# KL 4 June 2014

############
# Plots by hospital name
# prepare the CSV data dump, appending volume, type, and abbreviation in Excel--round 1, redo in R if time
# run the hospital level graphs after the monthly graphs
############
library(Rcpp)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)


#df1 <- read.xlsx("MeasureDataDump_4_29_2014.xlsx", sheetIndex=1,colIndex=c(1:11),colClasses="character")
# error message on java--the Excel sheet has a vlookup and pivot table and now java is throwing out of memory errors
# changed to csv file
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data_dump <- "MeasureDataDump_12_10_2014.csv"

df1 <- read.csv(data_dump,colClasses="character")

#delete the look up table information on the right side of the data table
df1 <- df1[,c(1:12)]

source("functions.R")
df2 <- clean_up_df(df1)


# checknames <- levels(df2$MeasureName)
# levels(df2$TeamName)
# levels(df2$AbbrevName)
#set vector of names to loop over for writing out files
AbbrevName <- levels(df2$AbbrevName)

# #get table of short measure names and measure types
# df_measure_names<- read.csv("MeasureNameTable1.csv",colClasses="character")
# 
# measure_type <- function(x,df=df_measure_names) {
#   #given a measure name x, looks up measure type in table contained in df
#   dfA <- df[df$Extranet.Name==x,]
#   measure_type <- dfA$MeasureType[1]
# }
# 
# 
# short_name <- function(x,df=df_measure_names) {
#   #given a measure name x, looks up a short measure name
#   dfB <- df[df$Extranet.Name==x,]
#   short_name <- dfB$NewName2[1]
# }

# test <- measure_type(checknames[1])
# check2 <- df2[,c(3,12,13)]
# head(check2,40)
# now append measure type as factor column to data set to enable proper plotting
MeasureTypes <- sapply(df2$MeasureName,measure_type)
df2$MeasureType <- as.factor(MeasureTypes)
levels(df2$MeasureType)

ShortNames <- sapply(df2$MeasureName,short_name)
df2$ShortNames <- as.factor(ShortNames)

#remove the measures Pct VTE prophylaxis and overall PRO measure?
df2 <- df2[complete.cases(df2$ShortNames),]

# #Test of subsettting by hospital name
# i <-  1
# i <- i+1
newdir <- paste0("Extranet graphs by Hospital_",format(Sys.time(),"%Y-%m-%d-%H-%M"))

dir.create(paste0(getwd(),"/",newdir))

#loop through the hospitals

for(i in 1:length(AbbrevName)){
  df2_team <- droplevels(df2[df2$AbbrevName == AbbrevName[i],])
  mypathP<-file.path(newdir,paste0("/",AbbrevName[i],"-1.jpg"))
  jpeg(file=mypathP,width=500,height=600)


  ##########do plots of low counts first
  dcounts <- droplevels(df2_team[df2_team$MeasureType=="CountLow",])
  
  #create baseline median
  dcounts_baseline <- dcounts[dcounts$TimePeriod <= "2014-04-15",]
  medianscore <- as.vector(tapply(dcounts_baseline$Value1,dcounts_baseline$ShortNames,median,na.rm=TRUE))
  ShortNames <- levels(dcounts_baseline$ShortNames)
  df.hlines <- data.frame(ShortNames,medianscore)

  p1 <- ggplot(dcounts,aes(x=TimePeriod,y=Value1))+theme_bw()+
  facet_wrap(~ShortNames,nrow=1)+geom_point(size=2.5) + geom_line()+ ylab("Count")+
 # ggtitle(paste0(dcounts$TeamName[1],"\n","Baseline Jan-Apr 2014 updated ",Sys.Date()))+
  scale_x_date(limits=c(as.Date("2014-01-01"),as.Date("2014-12-01")))+theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))+
  geom_vline(xintercept=as.numeric(as.Date("2014-04-15")),lty=2,colour="green",size=1)+
  scale_colour_manual(values = cb_palette)+
  scale_y_continuous(limits=c(0,max(dcounts$Value1,5)),breaks=pretty_breaks())+
  theme(axis.title.x=element_blank())
 
 p11 <- p1 + geom_hline(aes(yintercept=medianscore),data=df.hlines,lty=2)

################plots of Hi counts next
  dcounts2 <- droplevels(df2_team[df2_team$MeasureType=="CountHi",])

#create baseline median
dcounts2_baseline <- dcounts2[dcounts2$TimePeriod <= "2014-04-15",]
medianscore <- as.vector(tapply(dcounts2_baseline$Value1,dcounts2_baseline$ShortNames,median,na.rm=TRUE))
ShortNames <- levels(dcounts2_baseline$ShortNames)
df.hlines <- data.frame(ShortNames,medianscore)


  p2 <- ggplot(dcounts2,aes(x=TimePeriod,y=Value1))+theme_bw()+
  facet_wrap(~ShortNames,nrow=1)+geom_point(size=2.5) + geom_line()+ ylab("Count")+
  #ggtitle(paste0(dcounts$TeamName[1],"\n","Baseline Jan-Apr 2014"))+
  scale_x_date(limits=c(as.Date("2014-01-01"),as.Date("2014-12-01")))+theme(axis.text.x=element_blank())+
  geom_vline(xintercept=as.numeric(as.Date("2014-04-15")),lty=2,colour="green",size=1)+
  scale_colour_manual(values = cb_palette)+
  scale_y_continuous(breaks=pretty_breaks())+
  theme(axis.title.x=element_blank())

p21 <- p2 + geom_hline(aes(yintercept=medianscore),data=df.hlines,lty=2)

##############
#########plot of days
  d_days <- droplevels(df2_team[df2_team$MeasureType=="Days",])

#create baseline median
d_days_baseline <- d_days[d_days$TimePeriod <= "2014-04-15",]
medianscore <- as.vector(tapply(d_days_baseline$Value1,d_days_baseline$ShortNames,median,na.rm=TRUE))
ShortNames <- levels(d_days_baseline$ShortNames)
df.hlines <- data.frame(ShortNames,medianscore)


  p4 <- ggplot(d_days,aes(x=TimePeriod,y=Value1))+theme_bw()+
  facet_wrap(~ShortNames,ncol=2)+geom_point(size=2.5) + geom_line()+ ylab("Days")+
  #ggtitle(paste0(df2_sub$MeasureName[1],"\n","Baseline Jan-Apr 2014"))+
  scale_x_date(limits=c(as.Date("2014-01-01"),as.Date("2014-12-01")))+theme(axis.text.x=element_blank())+
  xlab("Date")+geom_vline(xintercept=as.numeric(as.Date("2014-04-15")),lty=2,colour="green",size=1)+
  scale_colour_manual(values = cb_palette)

p41 <- p4 + geom_hline(aes(yintercept=medianscore),data=df.hlines,lty=2)


#########Now stack the counts and Days graphs
  print(grid.arrange(p41,p21,p11, nrow=3, main=paste0(d_days$TeamName[1]," updated ",Sys.Date(),"\n","Baseline Jan-Apr 2014 median reference; LOS, procedures and 30 day readmits")))
  dev.off()

###############plots of percents
  mypathP<-file.path(newdir,paste0("/",AbbrevName[i],"-2.jpg"))
  jpeg(file=mypathP,width=1000,height=743)
  d_pct <- droplevels(df2_team[df2_team$MeasureType=="Percent",])

#get median
d_pct_baseline <- d_pct[d_pct$TimePeriod <= "2014-04-15",]
medianscore <- as.vector(tapply(d_pct_baseline$Value1,d_pct_baseline$ShortNames,median,na.rm=TRUE))
ShortNames <- levels(d_pct_baseline$ShortNames)
df.hlines <- data.frame(ShortNames,medianscore)

  p3 <- ggplot(d_pct,aes(x=TimePeriod,y=Value1))+theme_bw()+
  facet_wrap(~ShortNames,ncol=4)+geom_point(size=2.5) + geom_line()+ ylab("Percent")+
  ggtitle(paste0(d_pct$TeamName[1],"\n","Baseline Jan-Apr 2014 Percent measures updated ",Sys.Date()))+
  scale_x_date(limits=c(as.Date("2014-01-01"),as.Date("2014-12-01")))+theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))+
  xlab("Date")+geom_vline(xintercept=as.numeric(as.Date("2014-04-15")),lty=2,colour="green",size=1)+
  scale_colour_manual(values = cb_palette)

  p31 <- p3 + geom_hline(aes(yintercept=medianscore),data=df.hlines,lty=2)
  print(p31)
  dev.off()
}



# write.csv(df2_team,"check.csv")
# write.csv(df2,"check2.csv")
# unique(ShortNames)
