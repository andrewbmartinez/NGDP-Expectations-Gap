rm(list=ls())

library(readxl)
library(tidyquant)
library(tidyverse)
temp = tempfile(fileext = ".xlsx")

#Getting latest data from FRED
ngdp_ALL <- tq_get("GDP", get="economic.data",from="1968-01-01")
ngdp_ALL <- ngdp_ALL %>% mutate(YEAR=year(date), QUARTER=(month(date)-1) %/% 3 + 1)
ngdp_ALL <- subset(ngdp_ALL,select=-c(symbol,date))
names(ngdp_ALL) = c("NGDP", "YEAR", "QUARTER")
ngdp_ALL = ngdp_ALL[c("YEAR","QUARTER","NGDP")]

#Getting SPF Forecast data
dataurl2 <- "https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/spfmicrodata.xlsx"
download.file(dataurl2, destfile=temp, mode='wb')

startyr = 1967
ngdp_forc <- subset(read_excel(temp, sheet="NGDP"), YEAR>startyr) 
pgdp_forc <- subset(read_excel(temp, sheet="PGDP"), YEAR>startyr)
cpi_forc <- subset(read_excel(temp, sheet="CPI"), YEAR>startyr)
rgdp10_forc <- subset(read_excel(temp, sheet="RGDP10"), YEAR>startyr)
cpi10_forc <- subset(read_excel(temp, sheet="CPI10"), YEAR>startyr)

ngdp_forc[] <- lapply(ngdp_forc, function(x) { as.numeric(as.character(x)) })
pgdp_forc[] <- lapply(pgdp_forc, function(x) { as.numeric(as.character(x)) })
cpi_forc[] <- lapply(cpi_forc, function(x) { as.numeric(as.character(x)) })
rgdp10_forc[] <- lapply(rgdp10_forc, function(x) { as.numeric(as.character(x)) })
cpi10_forc[] <- lapply(cpi10_forc, function(x) { as.numeric(as.character(x)) })

ngdp_forc$date <- as.yearqtr(paste0(ngdp_forc$YEAR," Q",ngdp_forc$QUARTER))
pgdp_forc$date <- as.yearqtr(paste0(pgdp_forc$YEAR," Q",pgdp_forc$QUARTER))
cpi_forc$date <- as.yearqtr(paste0(cpi_forc$YEAR," Q",cpi_forc$QUARTER))
rgdp10_forc$date <- as.yearqtr(paste0(rgdp10_forc$YEAR," Q",rgdp10_forc$QUARTER))
cpi10_forc$date <- as.yearqtr(paste0(cpi10_forc$YEAR," Q",cpi10_forc$QUARTER)) 

#Generating growth rates from levels
for(i in 2:6){
  ngdp_forc[[paste0("DGDP",i)]]=(ngdp_forc[[paste0("NGDP",i)]]/ngdp_forc[[paste0("NGDP",i-1)]])-1 
  for(j in 1:nrow(ngdp_forc)){
    #filling in missing quarters
    if(i>2){
      if( is.na( ngdp_forc[[paste0("DGDP",i)]][j] )==TRUE ){ ngdp_forc[[paste0("DGDP",i)]][j]=ngdp_forc[[paste0("DGDP",i-1)]][j]   }
    }
  } 
}

pgdp_forc[[paste0("DPGDPB")]]=100*(pgdp_forc$PGDPB-pgdp_forc$PGDPA)/pgdp_forc$PGDPA

ngdp_forc <- ngdp_forc[,c(13:18)]
pgdp_forc <- pgdp_forc[,c(11:14)]
cpi_forc <-  cpi_forc[,c("date","CPI2","CPI3","CPI4","CPI5","CPI6","CPIB")]
cpi10_forc <-  cpi10_forc[,c("date","CPI10")]
rgdp10_forc <-  rgdp10_forc[,c("date","RGDP10")]

ngdp_forc_low <- ngdp_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.1, na.rm=TRUE))
ngdp_forc_hi <- ngdp_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.9, na.rm=TRUE))
ngdp_forc_med <- ngdp_forc %>% group_by(date) %>% summarize_all(list(~median(.,na.rm=TRUE)))

pgdp_forc_low <- pgdp_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.1, na.rm=TRUE))
pgdp_forc_hi <-  pgdp_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.9, na.rm=TRUE))
pgdp_forc_med <- pgdp_forc %>% group_by(date) %>% summarize_all(list(~median(.,na.rm=TRUE)))

cpi_forc_low <- cpi_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.1, na.rm=TRUE))
cpi_forc_hi <-  cpi_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.9, na.rm=TRUE))  
cpi_forc_med <- cpi_forc %>% group_by(date) %>% summarize_all(list(~median(.,na.rm=TRUE)))

cpi10_forc_low <- cpi10_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.1, na.rm=TRUE))
cpi10_forc_hi <-  cpi10_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.9, na.rm=TRUE))    
cpi10_forc_med <- cpi10_forc %>% group_by(date) %>% summarize_all(list(~median(.,na.rm=TRUE)))

rgdp10_forc_low <- rgdp10_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.1, na.rm=TRUE))
rgdp10_forc_hi <-  rgdp10_forc %>% group_by(date) %>% summarize_all(~quantile(.,probs=0.9, na.rm=TRUE))    
rgdp10_forc_med <- rgdp10_forc %>% group_by(date) %>% summarize_all(list(~median(.,na.rm=TRUE)))

#Generating the Long-term GDP deflator / CPI gap
pgdp_forc_low$PGAP = cpi_forc_low$CPIB - pgdp_forc_low$DPGDPB
pgdp_forc_hi$PGAP = cpi_forc_hi$CPIB - pgdp_forc_hi$DPGDPB
pgdp_forc_med$PGAP = cpi_forc_med$CPIB - pgdp_forc_med$DPGDPB

#filling in missing real GDP growth forecasts
rgdp10_forc_low$RGDP10 <- na.locf(rgdp10_forc_low$RGDP10, na.rm=FALSE)
rgdp10_forc_hi$RGDP10 <- na.locf(rgdp10_forc_hi$RGDP10, na.rm=FALSE)
rgdp10_forc_med$RGDP10 <- na.locf(rgdp10_forc_med$RGDP10, na.rm=FALSE)

rgdp10_forc_low$PGDP10 <- cpi10_forc_low$CPI10 - pgdp_forc_low$PGAP
rgdp10_forc_hi$PGDP10 <- cpi10_forc_hi$CPI10 - pgdp_forc_hi$PGAP
rgdp10_forc_med$PGDP10 <- cpi10_forc_med$CPI10 - pgdp_forc_med$PGAP

rgdp10_forc_low$NGDP10 <- rgdp10_forc_low$RGDP10+rgdp10_forc_low$PGDP10
rgdp10_forc_hi$NGDP10 <- rgdp10_forc_hi$RGDP10+rgdp10_forc_hi$PGDP10
rgdp10_forc_med$NGDP10 <- rgdp10_forc_med$RGDP10+rgdp10_forc_med$PGDP10

rgdp10_forc_low$DGDP10 <- (1+rgdp10_forc_low$NGDP10/100)^(1/4)-1
rgdp10_forc_hi$DGDP10 <- (1+rgdp10_forc_hi$NGDP10/100)^(1/4)-1
rgdp10_forc_med$DGDP10 <- (1+rgdp10_forc_med$NGDP10/100)^(1/4)-1

rgdp10_forc_med$DGDP10[is.na(rgdp10_forc_med$DGDP10)==TRUE] <-  ngdp_forc_med$DGDP6[is.na(rgdp10_forc_med$DGDP10)==TRUE]
rgdp10_forc_hi$DGDP10[is.na(rgdp10_forc_hi$DGDP10)==TRUE] <-  ngdp_forc_hi$DGDP6[is.na(rgdp10_forc_hi$DGDP10)==TRUE]
rgdp10_forc_low$DGDP10[is.na(rgdp10_forc_low$DGDP10)==TRUE] <-  ngdp_forc_low$DGDP6[is.na(rgdp10_forc_low$DGDP10)==TRUE]

# generating nominal GDP growth forecasts in levels 
ngdp <-ngdp_ALL
ngdp$NGDPL <- lag(ngdp$NGDP,n=1)
ngdp <- subset(ngdp, YEAR>startyr+1 | QUARTER>3)

ngdp_low <- ngdp
ngdp_hi <- ngdp
ngdp_med <- ngdp

ngdp_low$NGDP1 <- ngdp_low$NGDPL*(1+ngdp_forc_low$DGDP2[-c(nrow(rgdp10_forc_low))])
ngdp_hi$NGDP1 <- ngdp_hi$NGDPL*(1+ngdp_forc_hi$DGDP2[-c(nrow(rgdp10_forc_hi))])
ngdp_med$NGDP1 <- ngdp_med$NGDPL*(1+ngdp_forc_med$DGDP2[-c(nrow(rgdp10_forc_med))])

for(i in 2:5){
  ngdp_low[[paste("NGDP",i,sep="")]]=ngdp_low[[paste("NGDP",i-1,sep="")]]*(1+ngdp_forc_low[[paste("DGDP",i+1,sep="")]][-c(nrow(ngdp_forc_low))])
  ngdp_hi[[paste("NGDP",i,sep="")]]=ngdp_hi[[paste("NGDP",i-1,sep="")]]*(1+ngdp_forc_hi[[paste("DGDP",i+1,sep="")]][-c(nrow(ngdp_forc_hi))])
  ngdp_med[[paste("NGDP",i,sep="")]]=ngdp_med[[paste("NGDP",i-1,sep="")]]*(1+ngdp_forc_med[[paste("DGDP",i+1,sep="")]][-c(nrow(ngdp_forc_med))])
}

for(i in 5:20){
  ngdp_low[[paste("NGDP",i,sep="")]]=ngdp_low[[paste("NGDP",i-1,sep="")]]*(1+rgdp10_forc_low[[paste0("DGDP",10)]][-c(nrow(rgdp10_forc_low))])
  ngdp_hi[[paste("NGDP",i,sep="")]]=ngdp_hi[[paste("NGDP",i-1,sep="")]]*(1+rgdp10_forc_hi[[paste0("DGDP",10)]][-c(nrow(rgdp10_forc_hi))])
  ngdp_med[[paste("NGDP",i,sep="")]]=ngdp_med[[paste("NGDP",i-1,sep="")]]*(1+rgdp10_forc_med[[paste0("DGDP",10)]][-c(nrow(rgdp10_forc_med))])
}

#shifting the forecasts so that they are aligned
for(i in 1:20){
  ngdp_low[[paste("NGDP",i,sep="")]]=lag(ngdp_low[[paste("NGDP",i,sep="")]],n=i-1)
  ngdp_hi[[paste("NGDP",i,sep="")]]=lag(ngdp_hi[[paste("NGDP",i,sep="")]],n=i-1)
  ngdp_med[[paste("NGDP",i,sep="")]]=lag(ngdp_med[[paste("NGDP",i,sep="")]],n=i-1)
}

#generating the Natural GDP level and Gap measures
ngdp_low <- transform(ngdp_low, NLNGDP = rowMeans(ngdp_low[,-c(1,2,3,4)]))
ngdp_hi <- transform(ngdp_hi, NLNGDP = rowMeans(ngdp_hi[,-c(1,2,3,4)]))
ngdp_med <- transform(ngdp_med, NLNGDP = rowMeans(ngdp_med[,-c(1,2,3,4)]))

ngdp$NGAPlow <- (ngdp$NGDP-ngdp_low$NLNGDP)/ngdp_low$NLNGDP*100
ngdp$NGAPhi <- (ngdp$NGDP-ngdp_hi$NLNGDP)/ngdp_hi$NLNGDP*100
ngdp$NGAPmed <- (ngdp$NGDP-ngdp_med$NLNGDP)/ngdp_med$NLNGDP*100

ngdp$date <- as.yearqtr(paste0(ngdp$YEAR," Q",ngdp$QUARTER))
ngdp <- ngdp %>% mutate(date = as.yearqtr(date))
ngdp$zero <- 0  

#generating recessions data
recessions = read.table(textConnection(
  "Peak, Trough
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01    
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01
2020-03-01, 2020-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

recessions <- recessions %>%
  mutate(Peak = as.yearqtr(Peak)) %>%
  mutate(Trough = as.yearqtr(Trough))

latest_year <- max(as.numeric(ngdp$date), na.rm = TRUE)
upper_limit <- latest_year + 1

# Generate axis breaks every 5 years dynamically
dynamic_breaks <- seq(1975, ceiling(upper_limit), by = 5)

# Find the last non-missing value of NGAPmed
last_ngapmed <- tail(ngdp$NGAPmed[!is.na(ngdp$NGAPmed)], 1)

# Calculate dynamic y-axis limits with a buffer 
y_min <- min(ngdp$NGAPhi, na.rm = TRUE) - 0.1
y_max <- max(ngdp$NGAPlow, na.rm = TRUE) + 0.1

gx <- ggplot()
gx <- gx + geom_rect(recessions, mapping= aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=Inf), fill='grey', alpha=0.2)
gx <- gx + geom_line(ngdp, mapping=aes(y=zero, x=date), color="black" , size=0.5, linetype="dotted" ) 
gx <- gx + geom_line(ngdp, mapping=aes(y=last_ngapmed, x=date), color="grey" , size=0.5, linetype="dotted" )
gx <- gx + theme_classic()
gx <- gx + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))
gx <- gx + theme(text=element_text(family="Times", size=12), axis.text.x=element_text(size=14), axis.text.y=element_text(size=16))
gx <- gx + geom_line(ngdp, mapping=aes(y=NGAPmed, x=date, color="A"), lwd=1)
gx <- gx + geom_ribbon(ngdp, mapping=aes(ymin=NGAPlow,ymax=NGAPhi, x=date), fill="Red", alpha=0.15)
gx <- gx + scale_color_manual(name="", labels = c("SPF Median + 10-90 Percentile Range"), values= c("Red"))
gx <- gx + theme(plot.caption=element_text(hjust=0),
                 plot.title = element_text(hjust = 0.5),
                 axis.text = element_text(size = 12),
                 legend.title=element_blank(),
                 legend.text=element_text(size=18),
                 legend.background = element_rect(fill="transparent", colour=NA, size=0),
                 legend.position = c(0.4, 0.95),
                 legend.justification = c(0, 1))
gx <- gx + scale_x_yearqtr(
  breaks = dynamic_breaks,  
  format = "%y", 
  limits = c(1973.5, upper_limit), 
  expand = c(0,0)
)

gx <- gx + xlab("") + ylab("Percent")
gx <- gx + ylim(y_min, y_max)
ggsave("ngdp_gap_figure.png", plot = gx, width = 10, height = 6, dpi = 300)
