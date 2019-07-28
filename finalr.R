ny= (new_york_city)
names(ny)
summary(ny)
head(ny)
library(ggplot2)



##### EXERCICIO HISTOGRAMA

qplot(x=Birth.Year, data=subset(ny, !Gender==""), binwidth= 2, 
      color= I('black'), fill= I('#099DD9'), 
      ylab= '# of bicycle users',
      xlab= 'Birth Year')+
  scale_x_continuous(breaks = seq(1930, 2000, 10))+
  ggtitle(' Bicycle users by birth year and gender in New York')+
  facet_wrap(~Gender)




#####



ggplot(aes(x = Birth.Year, y = Trip.Duration/60), data = chi) +
  geom_jitter(alpha=1/10, color= 'blue')+
  xlim (1930,2000) +
  ylim(0,80) +
  ggtitle('Trip duration in minutes by birth year in Chicago')+
  geom_line (stat= 'summary', fun.y=mean, linetype=1, color= 'red') +
  geom_line (stat= 'summary', fun.y=quantile, fun.args=list (probs=.5), linetype=2, color= 'black') 


mean(chi$Trip.Duration)
median(chi$Trip.Duration)


##???#




qplot(x= User.Type, y=Trip.Duration, ylim= c(0,5000), 
      data= subset (wash, !User.Type==""), geom='boxplot') +
  ggtitle('Trip duration by User Types in Washington')





#???#

names(wash)
wash


par(mfrow = c(1, 3))
qplot(x= User.Type, y=Trip.Duration, ylim= c(0,5000), 
      data= subset (wash, !User.Type==""), geom='boxplot') +
      ggtitle('Trip duration by User Type in Washington')  
qplot(x= User.Type, y=Trip.Duration, ylim= c(0,5000), 
      data= subset (chi, !User.Type==""), geom='boxplot') +
  ggtitle('Trip duration by User Type in Chicago')  
qplot(x= User.Type, y=Trip.Duration, ylim= c(0,5000), 
      data= subset (ny, !User.Type==""), geom='boxplot') +
  ggtitle('Trip duration by User Type in New York')   

     
par(mfrow = c(1, 3))
qplot(x= User.Type, y=Trip.Duration, ylim= c(0,5000), data= subset (wash, !User.Type==""), geom='boxplot') 
qplot(x= User.Type, y=Trip.Duration, ylim= c(0,5000), data= subset (chi, !User.Type==""), geom='boxplot') 
qplot(x= User.Type, y=Trip.Duration, ylim= c(0,5000), data= subset (ny, !User.Type==""), geom='boxplot')


qplot(y=Trip.Duration, ylim= c(0,5000), 
      data= wash, geom='boxplot') +
  ggtitle('Trip duration in minutes by birth year in Chicago')



### El siguiente gráfico de dispersion relaciona años de nacimiento de ususrios con duracion del trip en NYC 

summary(chi$Trip.Duration)


head(chi$10)



age_calc(Birth.Year, enddate = Sys.Date(), units = "years", precise = TRUE)

, binwidth= 2, 
      color= I('black'), fill= I('#F79420'))+
  scale_x_continuous(breaks = seq(1930, 2000, 10))



qplot(x = Trip.Duration, data = ny, bindwidth=30) +
  facet_grid(Gender~Start.Time)
+
  geom_jitter(alpha=1/20, color= 'orange') 
+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9)) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color= 'red')




library(ggthemes)
ny= new_york_city
chi= chicago
wash= washington

names(wash)
names(ny)
names(wash)

### abajo HISTOGRAMA
attach(ny)
summary(Trip.Duration)

ES EL DE ACA ABAJO EL QUE ESTÁ BIEN



head(ny,10)



qplot(x=Birth.Year, data=ny, binwidth= 2, 
      color= I('black'), fill= I('#F79420'))+
  scale_x_continuous(breaks = seq(1930, 2000, 10))+
  facet_wrap(~Gender)







qplot(x = Trip.Duration, data = subset(ny, !is.na(Gender)), binwidth = 10)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(Gender~Gender)

qplot(x=Trip.Duration, data=new_york_city, bins=31) +
  scale_x_continuous(breaks=1:31)

ggplot(aes(x = Trip.Duration), data = ny) +
  geom_histogram(binwidth = 50) +
  scale_x_continuous(breaks = 1:50) +
  facet_grid(Gender~User.Type)