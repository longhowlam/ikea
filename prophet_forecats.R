library(prophet)
library(dplyr)
library(corrplot)
library(ggplot2)

######################################################
## plot by day of week and by hour
AllBilliesVerkocht2 = readRDS("AllBilliesVerkocht2.Rds")

p = AllBilliesVerkocht2 %>% 
  group_by(dag,  kleur) %>% 
  summarise(
    n=n(),v = mean(verkocht, na.rm=TRUE), s = sum(verkocht, na.rm=TRUE) 
  ) %>%
  ggplot(
    aes(x = dag, y = s, fill=kleur), color = "black"
  )

p +
  geom_bar(color = "black", stat="identity") +
  scale_fill_manual(values=c("black", "white", "brown")) +
  scale_y_continuous("Index") +
  scale_x_discrete("day of week") +
  ggtitle("Ikea Billy index per day of week and color")


p = AllBilliesVerkocht2 %>% 
  group_by(uur,  kleur) %>%
  summarise(
    n=n(),v = mean(verkocht, na.rm=TRUE), s = sum(verkocht, na.rm=TRUE) 
  ) %>%
  ggplot(
    aes(x = uur, y = s, fill=kleur), color = "black"
  )

p +
  geom_bar(color = "black", stat="identity") + 
  scale_fill_manual(values=c("black", "white", "brown")) +
  scale_y_continuous("index") +
  scale_x_continuous(breaks=8:21, "hour of the day")+
  ggtitle("Ikea Billy index per hour")



######################################################################


BillyTotal = AllBilliesVerkocht2 %>% 
  group_by(datum) %>%
  summarise(y = sum(verkocht, na.rm=TRUE)) %>%
  rename(ds = datum)

BillyF = prophet(BillyTotal, n.changepoints = 10, changepoint.prior.scale = 0.0132 )
future <- make_future_dataframe(BillyF, periods = 90)
#tail(future)

forecast <- predict(BillyF, future)
#tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

zz = plot(BillyF, forecast)
zz + ggtitle("The IKEA Billy Index") + scale_y_continuous("Index")


prophet_plot_components(BillyF, forecast)


##########################################################################################

### KNMI weather data
KNMI_20170309 = read_csv("~/RProjects/IkeaAnalytics/KNMI_20170309.txt")
KNMI = KNMI_20170309 %>% 
  mutate(
    ds = as.POSIXct(anytime::anydate(YYYYMMDD))
  )

BillyTotalKNMI = BillyTotal %>%
  left_join(KNMI) %>%
  mutate(FG = FG/10) %>%
  rename(
    IkeaIndex = y,
    WindSpeed = FG,
    Temperature = TG,
    Rain = RH,
    CloudCover = NG,
    Sunshine = SQ
  )

M = cor(BillyTotalKNMI %>% select(IkeaIndex, WindSpeed,Temperature, Rain, CloudCover, Sunshine))
corrplot(
  M, 
  method = "ellipse",
  
  diag = FALSE,
  addCoef.col = "black"
)

ggplot(data = BillyTotalKNMI, aes(x=WindSpeed, y=IkeaIndex)) +
  geom_point() +
  geom_smooth(method = "lm",size = 2) + 
  scale_y_continuous(limits = c(0,500))+
scale_x_continuous(limits = c(.5,6.5), "Wind Speed (m/s)")

lm(IkeaIndex ~ WindSpeed, data = BillyTotalKNMI   )


