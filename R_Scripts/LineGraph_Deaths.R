# load the data
Deaths <- read.csv("C:/Users/Deepti/Documents/R_Workshop/Workshop_6/time_series_19-covid-Deaths.csv")
View(Deaths)
# convert data from wide to long 
Deaths<-Deaths[,c(-1,-3,-4)]%>%melt(id="Country.Region")
View(Deaths)
# Aggregate data by country and day
Deaths1<-Deaths%>%group_by(Country.Region,variable)%>%summarise(Cases=sum(value))
View(Deaths1)
#Rename columns
colnames(Deaths1)<-c("Country","Date","Cases")
View(Deaths1)
#Covert factors to date format
Deaths1$Date<-str_replace(Deaths1$Date,"X","")
Deaths1$Date<-gsub("[[:punct:]]","/",Deaths1$Date)
Deaths1$Date<-as.Date(Deaths1$Date,"%m/%d/%y")
#Select the country 
Deaths1_subset<-Deaths1%>%filter(Country=="US"|Country=="Canada"|Country=="Italy"|Country=="Mainland China"|Country=="Iran"|Country=="India")
View(Deaths1_subset)

#
# Create a static plot 
lastDate<-max(Deaths1_subset$Date)
ggplot(Deaths1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  scale_y_log10("Number of Deaths", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Date", date_breaks ="1 week", date_labels = "%b %d")+
  ggtitle(paste("Cumulative Deaths Over Time As Of ",lastDate)) +
  theme_classic() + theme(legend.position = "right")
# Create an animation gif file

p<-ggplot(Deaths1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  geom_text(aes(x = max(Date)+.1, label = sprintf("%5.0f", Cases)), hjust=-0.5) +
  transition_reveal(Date) + 
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') +
  scale_y_log10("Number of Deaths", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Date", date_breaks ="1 week", date_labels = "%b %d")+
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_classic() +
  theme(legend.position = "right",
        plot.margin = margin(5.5, 40, 5.5, 5.5))

animate(p, fps=5,renderer = gifski_renderer("Animated_Deaths.gif"))


