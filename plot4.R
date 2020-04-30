##Run plot4.R as function

plot4<-function(x) {
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

## Coal combustion sources
comb<-scc[grep("[Cc]ombustion",scc$SCC.Level.One),]
cc<-comb[grep("[Cc]oal",comb$EI.Sector),]

ccintersect<-nei[nei$SCC  %in% cc$SCC,]
four<-aggregate(Emissions ~ year, ccintersect, sum)

## ggplot of yearly total emission from coal combustion sources per year
g<-ggplot(four, aes(year, Emissions))+
        scale_x_continuous(breaks=seq(1999,2008,3))+
        labs(title=expression('Yearly Total ' *PM[2.5]* ' Emissions'), 
             subtitle='from Coal Combustion',
             y=expression(PM[2.5]* ' Emissions (tons)'), x="Year")+
        theme_bw()+geom_point(color="steelblue",cex=2)+
        geom_smooth(method="lm",col="steelblue",lwd=.75,lty=2,se=F)
print(g)

## Save plot to png device
dev.copy(png,filename="plot4.png",width=500,height=500)
dev.off()
message("Plot 4 saved to working directory") }


