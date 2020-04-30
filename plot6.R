##Run plot6.R as function

plot6<-function(x) {
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

## ggplot Baltimore vs LA motor vehicle emissions by year
bcla<-subset(nei,fips==24510|fips==06037)
vehicle<-scc[grep("[Vv]ehicle",scc$SCC.Level.Two),]

bclavehicle<-bcla[bcla$SCC  %in% vehicle$SCC,]
six<-aggregate(Emissions ~ year+fips, bclavehicle, sum)
six$fips<-factor(six$fips)

z<-ifelse(six$fips==24510, "Baltimore", "Los Angeles")
six$city<-as.factor(z)

g<-ggplot(six, aes(year, Emissions))+
        scale_x_continuous(breaks=seq(1999,2008,3))+
        labs(title=expression('Yearly Total ' *PM[2.5]* ' Emissions from Motor Vehicles'),
             y=expression(PM[2.5]* ' Emissions (tons)'), x="Year")+
        theme_bw()+scale_color_manual(values=c("darkred","goldenrod1"))+
        facet_grid(.~six$city)+
        geom_point(aes(col=city),cex=2)+
        theme(legend.position= "none")+
        geom_smooth(method="lm",aes(col=city,),lwd=.75,lty=3,se=F)
print(g)

## Save plot to png device
dev.copy(png,filename="plot6.png",width=500,height=500)
dev.off()
message("Plot 6 saved to working directory") }
