#AL Khor Intensive Care Unit (ICU)
hospname <- "Al Khor Hospital - Medical and Surgical Wards_Al Khor Hospital-Medical and Surgical Wards"                                                   
df2_team <- droplevels(df2[df2$SeriesName == hospname,])
mypathP<-file.path(newdir,paste0("/",hospname,"-1.jpg"))
jpeg(file=mypathP,width=500,height=600)

levels(df2$SeriesName)

d_pct <- droplevels(df2_team[df2_team$MeasureType=="Percent",])

#get median    BECAUSE HAMAD DOES NOT HAVE A SINGLE SET BL PERIOD
#d_pct_baseline <- d_pct[d_pct$TimePeriod <= "2014-04-15",]
medianscore <- as.vector(tapply(d_pct$percent,d_pct$ShortNames,median,na.rm=TRUE))
ShortNames <- levels(d_pct$ShortNames)
df.hlines <- data.frame(ShortNames,medianscore)

d_pct$percent <- 100*d_pct$Value1/d_pct$Value2

p3 <- ggplot(d_pct,aes(x=TimePeriod,y=percent))+theme_bw()+
  facet_wrap(~ShortNames,ncol=2)+geom_point(size=2.5) + 
  geom_line()+ 
  ylab("Percent")+
  ggtitle(paste0(d_pct$TeamName[1],"\n","Percent measures updated ",Sys.Date()))+
  #scale_x_date(limits=c(as.Date("2014-01-01"),as.Date("2014-12-01")))+
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))+
  xlab("Date")+
  #geom_vline(xintercept=as.numeric(as.Date("2014-04-15")),lty=2,colour="green",size=1)+
  scale_colour_manual(values = cb_palette)

p31 <- p3 + geom_hline(aes(yintercept=medianscore),data=df.hlines,lty=2)
p3
print(p3)
dev.off()
dev.off()
