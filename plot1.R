##Run plot1.R as function

plot1<-function(x) {
##Download data, add libraries
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

download.file(url,destfile = paste0(getwd(),"/NEI.zip"),
              method="curl")

unzip(paste0(getwd(),"/NEI.zip"))

NEI<-readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

require(dplyr)
require(ggplot2)

## Change dataframe column classes
NEI$fips<-as.numeric(NEI$fips)
NEI$Emissions<-as.numeric(NEI$Emissions)
NEI$type<-as.factor(NEI$type)

nei<-subset(NEI,NEI$Emissions>0&(!is.na(NEI$fips)))

## Subset yearly totals
total<-nei[,c(4,6)]
total$Emissions<-(total$Emissions)/10^6
total2<-total%>%group_by(year)%>%summarize_each(sum)

## Base plot of yearly emissions
par(mar=c(5,5,4,3))
with(total2, barplot(total2$Emissions, 
        names.arg=total2$year, col="blue", ylim=c(0,8), xlab="Year",
        ylab=expression('Total ' *PM[2.5]* ' Emissions (in millions of tons)'), 
        main=expression('Total Yearly ' *PM[2.5]* ' Emissions in the US')))


## Save plot to png device
dev.copy(png,filename="plot1.png",width=500,height=500)
dev.off()
message("Plot 1 saved to working directory") }
