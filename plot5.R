##Run plot5.R as function

plot5<-function(x) {
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

nei<-subset(NEI,NEI$Emissions>0)

scc$SCC<-as.character(scc$SCC)

## ggplot of Coal Combustion
bc<-subset(nei,fips=="24510")
vehicle<-scc[grep("[Vv]ehicle",scc$SCC.Level.Two),]

bcvehicle<-bc[bc$SCC  %in% vehicle$SCC,]
five<-aggregate(Emissions ~ year, bcvehicle, sum)

g<-ggplot(five, aes(year, Emissions))+
        scale_x_continuous(breaks=seq(1999,2008,3))+
        labs(title=expression('Yearly Total ' *PM[2.5]* ' Emissions in Baltimore'),subtitle="from Motor Vehicles",y=expression(PM[2.5]* ' Emissions (tons)'), x="Year")+
        theme_bw()+geom_point(color="darkred",cex=2)+
        geom_smooth(method="lm",col="darkred",lwd=.75,lty=2,se=F)
print(g)

## Save plot to png device
dev.copy(png,filename="plot5.png",width=500,height=500)
dev.off()
message("Plot 5 saved to working directory") }

