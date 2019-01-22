library('ggplot2')
library('readr')
library('dplyr')

payroll <- read_csv("Downloads/jupyter/City_Employee_Payroll.csv")

#Descriptive Statistics
str(payroll)
summary(payroll)
head(payroll)
glimpse(payroll)

payroll%>%
  select(Year,Annual_sal,`Total Payments`,`Employment Type`)%>%
  summary()

cat("Variance of Annual_Sal:",var(payroll$Annual_sal))

cat("Standard deviation of Annual_Sal:",sd(payroll$Annual_sal))


#Change names of some columns
colnames(payroll)[9]<-"Annual_sal"

#Let's see how the Annual salary break down by Employment Type
Annual_sal_et<-payroll%>%
               group_by(`Employment Type`)%>%
               summarize(mean_annual_sal=mean(Annual_sal),count=n())%>%
               arrange(desc(count),desc(mean_annual_sal))
Annual_sal_et

#Let's see how the Annual salary break down by Year
Annual_sal_yr<-payroll%>%
               group_by(Year)%>%
               summarize(mean_annual_sal=mean(Annual_sal),count=n())%>%
               arrange(desc(count),desc(mean_annual_sal))
Annual_sal_yr

#Let's see how the payment changes according to the department title
q_payments_ft<-payroll%>%
               filter(`Employment Type`=='Full Time')%>%
               group_by(`Department Title`)%>%
               summarize(mean_annual_sal=mean(Annual_sal),count=n())%>%
               arrange(desc(count),desc(mean_annual_sal))
head(q_payments_ft,10)

#Generating null and alternate hypothesis
#Whether the annual salary increases from 2017 to 2018
as.character(payroll$Year)

ggplot(payroll,aes(x=Annual_sal,fill=Year))+
  geom_histogram(alpha=0.5,position="identity")+
  ggtitle("Histogram of Annual Salary by Year")

payroll %>%
  filter(Year %in% c("2017","2018"))%>%
  group_by(Year)%>%
  mutate(mean=mean(Annual_sal))%>%
  ggplot(aes(x=Annual_sal,color=Year))+
  geom_density()+
  geom_vline(aes(xintercept=mean,color=Year),linetype="dashed")+
  ggtitle("Distribution of Annual Salary by Year",subtitle="2017 vs 2018")+
  scale_x_continuous(name="Annual Salary",labels=scales::comma)

#replicate and resample
p2017<-payroll%>%
  filter(Year=="2017")%>%
  select(Year,Annual_sal)
p2018<-payroll%>%
  filter(Year=="2018")%>%
  select(Year,Annual_sal)

payroll%>%
  select(Year,Annual_sal,`Total Payments`,`Employment Type`)%>%
  summary()

p1718<-payroll %>%
  filter(Year %in% c("2017","2018"))%>%
  select(Year,Annual_sal)%>%
  group_by(Year)%>%
  mutate(mean=mean(Annual_sal))

t.test(Annual_sal~Year,p1718)

var.test(Annual_sal~Year,p1718)


#Visualization
pp2018<-payroll%>%
  filter(Year=="2018")
ggplot(pp2018,aes(x=Annual_sal,y=`Base Pay`))+
  geom_point(color="blue")+
  geom_smooth(method=lm,linetype="dashed",color="darkred",fill="blue")+
  scale_x_continuous(name="Annual Salary",labels=scales::comma)+
  scale_y_continuous(name="Base Pay",labels=scales::comma)+
  ggtitle("Scatter Plot of Base Pay and Annual Salary in 2018")

lm<-lm(Annual_sal~`Base Pay`,data=pp2018)
summary(lm)
