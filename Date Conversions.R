library('RColorBrewer')
options(stringsasfactors=F)
setwd("/Users/christopherlee/Documents/CAL/FB Study")
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