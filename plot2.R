##Run plot2.R as function

plot2<-function(x) {
##Download data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

download.file(url,destfile = paste0(getwd(),"/NEI.zip"),
              method="curl")

unzip(paste0(getwd(),"/NEI.zip"))

NEI<-readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

require(dplyr)
require(ggplot2)

## Change frame col classes
NEI$fips<-as.numeric(NEI$fips)
NEI$Emissions<-as.numeric(NEI$Emissions)
NEI$type<-as.factor(NEI$type)

nei<-subset(NEI,NEI$Emissions>0&(!is.na(NEI$fips)))

## Subset Baltimore City yearly totals
bc<-subset(nei,fips=="24510")
minibc<-bc[,c(4,6)]
totalbc<-minibc%>%group_by(year)%>%summarize_each(sum)

## Base plot of yearly emissions
par(mar=c(5,5,4,2))
with(totalbc, barplot(Emissions, names.arg=totalbc$year, col="red", xlab="Year",
 ylab=expression('Total ' *PM[2.5]* ' Emissions (tons)'), ylim=c(0,3500),
 main=expression('Total Yearly ' *PM[2.5]* ' Emissions in Baltimore')) )


## Save plot to png device
dev.copy(png,filename="plot2.png",width=500,height=500)
dev.off()
message("Plot 2 saved to working directory") }
