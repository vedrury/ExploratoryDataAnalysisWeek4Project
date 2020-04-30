##Run plot3.R as function

plot3<-function(x) {
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

## Subset Baltimore City yearly totals
bc<-subset(nei,fips=="24510")
minibc2<-bc[,c(4,5,6)]
bcbytype<-aggregate(Emissions ~ year + type, minibc2, sum)

## ggplot of yearly emissions in Baltimore
g<-ggplot(bcbytype, aes(year, Emissions))+
        scale_x_continuous(breaks=seq(1999,2008,3))+
        labs(title=(expression('Yearly Total ' *PM[2.5]* ' Emissions in Baltimore')) , 
             subtitle= 'by Source Type', 
             y=expression(PM[2.5]* ' Emissions (tons)'), x="Year")+
        theme_bw()+geom_point(aes(color=type))+
        facet_grid(bcbytype$type,scales="free_y",space="fixed")+
        theme(legend.position= "none")+
        geom_smooth(method="lm",aes(col=type),lwd=.5,se=F)

print(g)

## Save plot to png device
dev.copy(png,filename="plot3.png",width=500,height=500)
dev.off()
message("Plot 3 saved to working directory") }
