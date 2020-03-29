if(!require("ggthemes")) { install.packages("ggthemes"); require("ggthemes") }
if(!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }
library(ggrepel)
library(ggplot2)

data1 <- read_csv("c:/Users/randy/Downloads/big-mac-full-index.csv")
data2 <- read_csv("C:/Users/randy/Dropbox/Study/MSBA/ISOM 675 Data Visualization/Rgraphics/dataSets/EconomistData.csv")
#https://data.worldbank.org/indicator/NY.ADJ.NNTY.PC.CD
data10 <- read_csv("c:/Users/randy/Downloads/API_NY.ADJ.NNTY.PC.CD_DS2_EN_csv_v2_217041.csv",skip=4)


data3 <- data1[which(data1$date == '2018-01-01'),c('name','dollar_price','GDP_dollar','iso_a3')]
data4 <- merge(data3,data2,by.x='name',by.y='Country')
data4$Region <-factor(data4$Region,levels =c("EU W. Europe","Americas","Asia Pacific","East EU Cemt Asia","MENA","SSA"),labels =c("Western Europe","Americas","Asia Pacific","Eastern Europe","Middle East & North Africa","Sub-Saharan Africa"))
data11 <- data10[,c('Country Code','2017')]
data5 <- merge(data4,data11,by.x='iso_a3',by.y='Country Code')
names(data5) <- c('code','name','dollar_price','GDP_dollar','a','b','c','Region','income')
#https://tradingeconomics.com/hong-kong/gdp-per-capita
data5$income[14] <- 37949.3
model <- lm(dollar_price ~ income, data=data5)
s <- summary(model)

img1 <- ggplot(data5, aes(x=income, y=dollar_price, color = Region)) + 
  geom_point(shape=1, size=3,stroke=2) +
  geom_smooth(aes(linetype=paste("Linear Fit")), method = "lm", formula=y~x, se = FALSE, color = "red") + 
  scale_linetype(name=NULL) +
  scale_color_brewer(palette='Paired')

img2 <- img1 + geom_text_repel(aes(label=name), color = "gray20", data=data5, force=2) + 
  ggtitle('Which Country Has The Most Affordable Big Macs?','Big Mac price and per capita annual income in selected countries and areas, 2018, US$') +
  xlab('Per Capita Annual Income ($)') +
  ylab('Selling Price of Big Mac ($)')

img3 <- img2 + theme_economist_white() + 
  theme(plot.title=element_text(face='bold',size=16),plot.subtitle=element_text(face='italic',size=12),legend.position="top",legend.title = element_blank(),legend.text=element_text(size=11),axis.title=element_text(face='bold'),plot.caption = element_text(hjust=0))

img4 <- img3 + labs(caption = "Data Source: The Economist; The World Bank; Trading Economics.")
img4

ggsave("HW1.png",path="c:/Users/randy/Downloads/",dpi="retina",device='png')

data5$workhours <- 1800 
data5$hourwage <- data5$income / data5$workhours
data5$time <- data5$dollar_price / data5$hourwage * 60

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

world <- ne_countries(scale = "small", returnclass = "sf")
data6 <- merge(world,data5,by.x='adm0_a3',by.y='code',all.x=TRUE)
data7 <- merge(world,data5,by.x='adm0_a3',by.y='code',all.x=FALSE)
h <- 0.09
img11<- ggplot(data = data6) +
  geom_sf(data=data6,fill='#E8E6E7',color=NA) +
  geom_sf(data=data7,aes(fill=time),color=NA) +
  scale_fill_gradient(low = "#EFE8D1",high = "red", space = "Lab", na.value = "grey50", guide = "colourbar", trans="log", aesthetics = "fill",breaks = c(20,50,150)) +
  theme_map() +
  theme(legend.position=c(0.67,0.1),legend.direction = "horizontal",legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),plot.caption=element_text(hjust=h),plot.title=element_text(hjust=h),plot.subtitle = element_text(hjust=h+0.05,face="italic")) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.5)) +
  labs(title = "How Much Working Time for One Big Mac?", subtitle = "Average working time to earn one Big Mac in selected countries and areas, 2018, mins", caption = "Data Source: The Economist; The World Bank; Trading Economics.")

img12 <- ggplot(data5[order(data5$time),][1:15,], aes(x=reorder(name, -time), y=time)) +
  geom_bar(stat="identity",aes(fill=time)) +
  scale_fill_gradient(low = "#EFE8D1",high = "#FFAA85", space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  theme_economist_white(horizontal = FALSE) +
  geom_text(aes(label=format(round(time,1),nsmall=1),y=0.5*time),size=3,color='#4A4A4A') +
  theme(legend.position = "none", panel.background = element_rect(fill="white"), plot.background = element_rect(fill="white")) +
  coord_flip() +
  xlab('') +
  ylab('Working Time (mins)') +
  theme(axis.title.x=element_text(face='bold',size=10),axis.text.y = element_text(size=9), plot.title=element_text(size=11,hjust=0)) + 
  labs(title = "Top 15 Fastest Earned")

library("cowplot")
img13 <- ggdraw() +
  draw_plot(img11,x=0,y=0,width=0.75,height=1) +
  draw_plot(img12,x=0.712,y=-0.005,width=0.25,height=1)
img13

ggsave("HW2.png",path="c:/Users/randy/Downloads/",dpi="retina",device='png')

