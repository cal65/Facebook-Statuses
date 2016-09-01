library('RColorBrewer')
options(stringsasfactors=F)
setwd("/Users/christopherlee/Documents/CAL/Facebook-Statuses")
F1<-read.csv("Facebook Statuses.csv", TRUE, sep=",",dec=".", na.strings="NA", stringsAsFactors=FALSE, encoding="UTF-8")
F1<-F1[-nrow(F1),]
d1<-F1[,2]
F1[is.na(F1)]<-0
Puns<-which(F1$Pun==1)
Pics<-which(F1$Picture==1)
Time<-as.POSIXlt(d1, '%I:%M%p %B %d, %Y', tz = '')
Dates<-as.Date(d1, '%B %d, %Y')
D<-as.POSIXlt(Dates)
DateTime<-c(Time[!is.na(Time)], as.POSIXlt(Dates[!is.na(Dates)]))

Pal<-brewer.pal(9, 'RdYlBu')
Greens<-c('light blue','darkgreen')
plot(DateTime, F1[,3], log='y', pch=(21+F1[,5]), col=Pal[8+F1[,7]], bg=Pal[1+5*F1[,6]], cex=(1+F1[,17]), ylab='# of Likes', main='Cals Statuses')
lines(DateTime[-c(1:2,189:191)],rollmean(F1[,3],6))
intervals<-as.numeric((DateTime[-1]-DateTime[-length(DateTime)])/(24*3600)*-1)
intervals[which(intervals==0)]<-runif(1,0,1)
barplot(rbind(F1[,3], F1[,4]), col=colors()[1:length(F1[,3])],space=intervals)
plot(DateTime[-Pics], F1[-Pics,3], pch=(21), col=Greens[1+F1[,9]], bg=Pal[1+5*F1[,6]], cex=(1+F1[,17]), ylab='# of Likes', xlab='Time',main='Cals Statuses')
F1[which(F1[,3]>60),1]
BB<-brewer.pal(9,'Blues')
wordcloud(F1[which(F1$Picture==0),1], random.color=TRUE, colors=BB[3:9],scale=c(3.2,.5))

Likes_weekdays<-plot(DateTime$wday, F1[,3])
plot(density(F1[which(DateTime$wday==3), 3], ),xlim=c(-10,100), main='Wednesday')
polygon(density(F1[which(DateTime$wday==3), 3]), col='red')
plot(density(F1[which(DateTime$wday==0), 3]), main='Sunday')
polygon(density(F1[which(DateTime$wday==0), 3]), col='blue')

G<-list(Sunday=0,Monday=0,Tuesday=0,Wednesday=0,Thursday=0,Friday=0,Saturday=0)
for (i in 1:7) {
	G[[i]]<-F1[Pics,3][which(DateTime[Pics]$wday==i-1),3]
}
Pal2<-brewer.pal(7,'Accent')
boxplot(G, col=Pal2, main='Status Likes by Day of Week', ylab =' # of Likes')
plot(difftime(DateTime[1:391], DateTime[2:392])/24/60/60)

barplot(rbind(F1$Likes, F1$Comments), col=Pal2[2:3], main = 'Comments + Likes')
library('ggplot2')
library(zoo)
G1<-F1 #convert dataframe for ggplot purposes
G1$Date<-DateTime
mapvalues(G1$Picture, from = c(0, 1), to = c('Not Picture', 'Picture'))->G1$Picture
n<-17
G1$rolling_average=c(rep(NA, floor(n/2)), rollmean(G1$Likes, n), rep(NA, floor(n/2)))
ggplot(data=G1, aes(x=Date, y=Likes)) + geom_point(aes(colour=as.factor(Picture), shape=as.character(Pun))) + geom_line(aes(x=Date, y=rolling_average)) 
G1$DateSimple<-format(G1$Date, format="%m-%d")
G1$Year<-G1$Date$year+1900

#Density plot
qplot(data=subset(G1,!is.na(Time)), Date$hour + Date$min/60, Likes)

V=as.data.frame(cbind(Likes = subset(G1,!is.na(Time))$Likes, Hour = (subset(G1,!is.na(Time))$Date$hour + subset(G1,!is.na(Time))$Date$min/60), Pic = subset(G1, !is.na(Time))$Picture))
V1<-subset(G1, !is.na(Time)) #For doing time sensitive analysis

lm5<-lm(data=V, Likes ~ Hour + I(Hour^2) + I(Hour^3) + I(Hour^4) + I(Hour^5) + I(Hour^6) + Pic + I(Hour^7))
Training<-as.data.frame(cbind(Hour=seq(0.5, 24, by=0.5), Pic=rep(0, 48)))
predict(lm5, Training)
lm6<-lm(data=V, Likes ~ Hour + I(Hour^2) + I(Hour^3) + I(Hour^4) + I(Hour^5) + I(Hour^6) + Pic + I(Hour^7) + I(Hour^8))
predict(lm6, Training)

lm3<-lm(data=V, Likes ~ Hour + I(Hour^2) + I(Hour^3) + Pic ) #Full out regression
lmG1<-lm(data=G1, Likes ~ Picture + Pun + Topical + Announcement + Holiday + Video)

#Year Plots
ggplot(data=subset(G1, G1$Date$year == 115), aes(x=Date, y=Likes)) + geom_point(aes(colour=as.factor(Picture), shape=as.character(Joke))) + scale_color_manual('Picture', values=c('purple', 'orange')) + scale_shape(name='Joke') + ggtitle('Statuses of 2015') + ylim(0, 200)
ggsave('Statuses_2015.jpeg', width=6, height=4)
ggplot(data=subset(G1, G1$Date$year == 114), aes(x=Date, y=Likes)) + geom_point(aes(colour=as.factor(Picture), shape=as.character(Joke))) + scale_color_manual('Picture', values=c('purple', 'orange')) + scale_shape(name='Joke') + ggtitle('Statuses of 2014')+ ylim(0, 200)
ggsave('Statuses_2014.jpeg', width=6, height=4)
ggplot(data=G1, aes(x=DateSimple, y=Likes)) + geom_point(aes(colour=as.factor(Picture), shape=as.character(Joke)), alpha=.8, size=0.9) + scale_color_manual('Picture', values=c('purple', 'orange')) + scale_shape(name='Joke') + ggtitle('Statuses over the Years') + ylim(0, 180) + facet_grid(facets = Year~.) + scale_x_discrete(breaks=c('01-15', '02-15', '03-15', '04-15', '05-15', '06-15', '07-15', '08-15', '09-15', '11-15', '12-15'))
ggplot(data=G1, aes(x=Date, y=Likes)) + geom_point(aes(colour=as.factor(Picture), shape=as.character(Joke)), alpha=.8, size=.5) + scale_color_manual('Picture', values=c('purple', 'orange')) + scale_shape(name='Joke') + ggtitle('Statuses over the Years') + ylim(0, 180) + facet_grid(facets = Year~., scale="free_x") +scale_x_datetime()
ggplot(data=G1, aes(Likes), geom_type='density')

ddply(G1, .(Year), summarize, TotalLikes = sum(Likes), TotalPosts=length(Year), Avg = TotalLikes/TotalPosts)
