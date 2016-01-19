library(sqldf)
#loading files
data=read.csv('D:/Fall/MA/Project/grocery_data.csv')
# getting the column names
colnames(data)

#penetration by commodity
sqldf('SELECT commodity,count(distinct household) household from data group by 1')

#Penetration by brand
#pasta
sqldf('SELECT brand,count(distinct household) household from data where commodity="pasta" group by 1  order by 2 desc')
#pancakes
sqldf('SELECT brand,count(distinct household) household from data where commodity="pancake mixes" group by 1  order by 2 desc')
#pasta sauce
sqldf('SELECT brand,count(distinct household) household from data where commodity="pasta sauce" group by 1  order by 2 desc')		 
#syrups
sqldf('SELECT brand,count(distinct household) household from data where commodity="syrups" group by 1  order by 2 desc')


#Penetration by combinations of brand of different complimentary commodity
##########################################################################
Commodity_by_Trip=sqldf('
select 
basket,
brand,
max(case when commodity = "pasta" then 1 else 0 end) as pasta,
max(case when commodity = "pasta sauce" then 1 else 0 end) as pasta_sauce,
max(case when commodity = "pancake mixes" then 1 else 0 end) as pancake_mixes,

max(case when commodity = "syrups" then 1 else 0 end)  as syrups

from data
group by basket,brand

')

Pancake_and_Syrup =sqldf('
select 
basket, 
pancake_mixes, 
syrups, 
brand

from Commodity_by_Trip
where pancake_mixes = 1 or syrups =1
')

Pancake_and_Syrup_1 =sqldf('
select 
basket,
sum(pancake_mixes) pancake_mixes,
sum(syrups) syrups

from Pancake_and_Syrup
group by basket
having sum(pancake_mixes) > 0 and sum(syrups) > 0 ')

Pancake_and_Syrup_2=sqldf('
SELECT
a.*
FROM Pancake_and_Syrup a
join Pancake_and_Syrup_1 b
ON a.basket=b.basket
')

Pancake =sqldf('
                 select basket, brand
                 from Pancake_and_Syrup_2
                 where pancake_mixes >= 1
                 order by basket
                 ')

Syrup =sqldf('
               select basket, brand
               from Pancake_and_Syrup_2
               where syrups >=1
               order by basket
               ')

Pancake_and_Syrup_3 =sqldf('
                            select b.basket, a.brand as pancake_brand, b.brand as syrup_brand
                            from Pancake a, Syrup b
                            where a.basket = b.basket
                            order by b.basket, a.brand, b.brand
                            ')
############## results for pan cakes and syrup
sqldf('
select pancake_brand, syrup_brand, count(basket)
      from Pancake_and_Syrup_3
      group by pancake_brand, syrup_brand
      ')

##############
pasta_sauce_and_pasta =sqldf('
select 
                             basket, 
                             pasta_sauce, 
                             pasta, 
                             brand
                             
                             from Commodity_by_Trip
                             where pasta_sauce = 1 or pasta =1
                             ')

pasta_sauce_and_pasta_1 =sqldf('
          select 
          basket,
          sum(pasta_sauce) pasta_sauce,
          sum(pasta) pasta
          
          from pasta_sauce_and_pasta
          group by basket
          having sum(pasta_sauce) > 0 and sum(pasta) > 0 ')

pasta_sauce_and_pasta_2=sqldf('
                              SELECT
                              a.*
                              FROM pasta_sauce_and_pasta a
                              join pasta_sauce_and_pasta_1 b
                              ON a.basket=b.basket
                              ')

pasta_sauce =sqldf('
                   select basket, brand
                   from pasta_sauce_and_pasta_2
                   where pasta_sauce >= 1
                   order by basket
                   ')


pasta =sqldf('
             select basket, brand
             from pasta_sauce_and_pasta_2
             where pasta >=1
             order by basket
             ')


         
pasta_sauce_and_pasta_3 =sqldf('
 select b.basket, a.brand as pasta_sauce_brand, b.brand as pasta_brand
 from pasta_sauce a, pasta b
 where a.basket = b.basket
 order by b.basket, a.brand, b.brand
         ')   
                              
############## results for pasta and pasta sauce
sqldf('
      select pasta_sauce_brand, pasta_brand, count(basket) num_basket
      from pasta_sauce_and_pasta_3
      group by pasta_sauce_brand, pasta_brand order by count(basket)  desc
      ')
###############
################################################################################

##Brand loyality INDEX

BL=sqldf('
select 
         household, 
         commodity, 
         brand, sum(units) as quantity
         from data
         group by household, commodity, brand'
)

BL1=sqldf('
select 
          household, 
          commodity, 
          sum(units) as total_quantity
          from data
          group by household, commodity')


BL2=sqldf('
          select 
          a.household, 
          a.commodity, 
          a.brand,
          a.quantity/b.total_quantity as bli
          from BL a, BL1 b
          where a.household = b.household and a.commodity = b.commodity
          group by a.household, a.commodity, a.brand
          ')

###output for perceptual maps######
#Pasta
pasta_bli=sqldf('select brand , avg(bli) pasta_bli,count(household) pasta_hd  from BL2 
      where commodity="pasta" group by 1 order by count(household) desc ' )

#Patsa Sauce
pasta_sauce_bli=sqldf('select brand , avg(bli) pasta_Sauce_bli,count(household) pasta_Sauce_hd  from BL2 
      where commodity="pasta sauce" group by 1 order by count(household) desc ' )

pasta_past_sauce_bli=sqldf('select a.brand, a.pasta_bli, a.pasta_hd,b.pasta_Sauce_bli,
                           b.pasta_Sauce_hd from pasta_bli a ,pasta_sauce_bli b where
                           a.brand=b.brand')
View(pasta_past_sauce_bli)
#Pancakes
pancake_mixes_bli=sqldf('select brand , avg(bli) pancake_mixes_bli,count(household) pancake_mixes_hd  from BL2 
      where commodity="pancake mixes" group by 1 order by count(household) desc ' )
#Syrup
syrups_bli=sqldf('select brand , avg(bli) syrups_bli,count(household) syrups_hd  from BL2 
      where commodity="syrups" group by 1 order by count(household) desc ' )

pancake_syrups_bli=sqldf('select a.brand,a.pancake_mixes_bli, a.pancake_mixes_hd,b.syrups_bli,
                           b.syrups_hd from pancake_mixes_bli a ,syrups_bli b where
                         a.brand=b.brand')
View(pancake_syrups_bli)
##########################################################################
#first coupon usage
coupon=sqldf('select brand,commodity,household,min(day) min_day from data where coupon =1 group by 1,2,3');
non_coupon=sqldf('select brand,commodity,household,min(day) min_day from data where coupon =0 group by 1,2,3');
first_household=sqldf('select a.brand,a.commodity,count(distinct a.household) num_household  FROM coupon a,non_coupon b where a.brand=b.brand and  a.commodity=b.commodity
and a.household=b.household  and a.min_day<b.min_day group by 1,2')
coupon_household=sqldf('select brand,commodity, count(distinct(household)) num_household FROM coupon group by 1,2');

#output to find for which branch coupon has been effective
coupon =sqldf('select a.brand, a.commodity, a.num_household total_h, b.num_household from first_household a,coupon_household b 
where a.brand=b.brand and  a.commodity=b.commodity and b.num_household>a.num_household group by 1,2')
coupon$household_onboarded=round((coupon$total_h/coupon$num_household)*100,1)
coupon_pasta=sqldf('select * from coupon where commodity="pasta"')
coupon_pasta_sauce=sqldf('select * from coupon where commodity="pasta sauce"')
coupon_pancake_mixes=sqldf('select * from coupon where commodity="pancake mixes"')
coupon_syrups=sqldf('select * from coupon where commodity="syrups"')

library(ggplot2)
A=ggplot(coupon_pasta,aes(x=brand,y=household_onboarded))+geom_bar(stat="identity", fill='red')+ ggtitle("Coupon Effect On Pasta Brand Adoption") 
A=A + theme(plot.title=element_text(size=20,face='bold',color="dark gray"))
A=A +theme(axis.title.x=element_blank())
A=A +theme(axis.title.y=element_blank())
A=A+geom_text(aes(label=round(household_onboarded,3)),vjust=-0.2,color="dark blue",size=8)
A=A+theme(axis.text=element_text(size=16))
A

A=ggplot(coupon_pasta_sauce,aes(x=brand,y=household_onboarded))+geom_bar(stat="identity", fill='salmon2')+ ggtitle("Coupon Effect On Pasta Sauce Brand Adoption") 
A=A + theme(plot.title=element_text(size=20,face='bold',color="dark gray"))
A=A +theme(axis.title.x=element_blank())
A=A +theme(axis.title.y=element_blank())
A=A+geom_text(aes(label=round(household_onboarded,3)),vjust=-0.2,color="dark blue",size=8)
A=A+theme(axis.text=element_text(size=16))
A

A=ggplot(coupon_pancake_mixes,aes(x=brand,y=household_onboarded))+geom_bar(stat="identity", fill='tan3')+ ggtitle("Coupon Effect On Pankcake Mixes Brand Adoption") 
A=A + theme(plot.title=element_text(size=20,face='bold',color="dark gray"))
A=A +theme(axis.title.x=element_blank())
A=A +theme(axis.title.y=element_blank())
A=A+geom_text(aes(label=round(household_onboarded,3)),vjust=-0.2,color="dark blue",size=8)
A=A+theme(axis.text=element_text(size=16))
A

A=ggplot(coupon_syrups,aes(x=brand,y=household_onboarded))+geom_bar(stat="identity", fill='gold1')+ ggtitle("Coupon Effect On Syrup Brand Adoption") 
A=A + theme(plot.title=element_text(size=20,face='bold',color="dark gray"))
A=A +theme(axis.title.x=element_blank())
A=A +theme(axis.title.y=element_blank())
A=A+geom_text(aes(label=round(household_onboarded,3)),vjust=-0.2,color="dark blue",size=8)
A=A+theme(axis.text=element_text(size=16))
A
ggplot(coupon_pasta_sauce,aes(x=brand,y=household_onboarded))+
  geom_bar(stat="identity", fill='salmon2')+xlab("Brand")+ ylab("% household onboarded using coupon")+ ggtitle("Pasta Sauce Brand adoption") 

ggplot(coupon_pancake_mixes,aes(x=brand,y=household_onboarded))+
  geom_bar(stat="identity", fill='tan3')+xlab("Brand")+ ylab("% household onboarded using coupon")+ ggtitle("Pancake Mixes Brand adoption") 

ggplot(coupon_syrups,aes(x=brand,y=household_onboarded))+
  geom_bar(stat="identity", fill='gold1')+xlab("Brand")+ ylab("% household onboarded using coupon")+ ggtitle("Syrups Brand adoption") 


#####################################################################################

#Modelling price elasticity
agg_data=sqldf('
select 
               week,
              sum(coupon) num_coupon,
               sum(Case when commodity ="pasta" then units else 0 end) pasta_sales,
               avg(Case when commodity ="pasta" then price else 0 end) pasta_price,
               sum(Case when commodity ="pancake mixes" then units else 0 end) pancake_mixes_sales,
               avg(Case when commodity ="pancake mixes" then price else 0 end) pancake_mixes_price,
               sum(Case when commodity ="pasta sauce" then units else 0 end) pasta_sauce_sales,
               avg(Case when commodity ="pasta sauce" then price else 0 end) pasta_sauce_price,
               sum(Case when commodity ="syrups" then dollar_sales else 0 end) syrups_sales,
               avg(Case when commodity ="syrups" then price else 0 end) syrups_price,
               sum(Case when commodity ="pasta" then coupon else 0 end) pasta_coupon,
               sum(Case when commodity ="pancake mixes" then coupon else 0 end) pancake_mixes_coupon,
               sum(Case when commodity ="pasta sauce" then coupon else 0 end) pasta_sauce_coupon,
               sum(Case when commodity ="syrups" then coupon else 0 end) syrups_coupon

               from data 
               group by week
               ')
agg_data=as.data.frame(agg_data)
row=nrow(agg_data)
agg_data$pasta_sales_lag[1]=0;
agg_data$pasta_sales_lag[2:row]=agg_data$pasta_sales[1:row-1]
colnames(agg_data)

##models for price elasticty and cross price ealsticity
agg_data$pasta_sales_lag[1]=0;
agg_data$pasta_sales_lag[2:row]=agg_data$pasta_sales[1:row-1]

agg_data$pancake_mixes_sales_lag[1]=0;
agg_data$pancake_mixes_sales_lag[2:row]=agg_data$pancake_mixes_sales[1:row-1]

agg_data$pasta_sauce_sales_lag[1]=0;
agg_data$pasta_sauce_sales_lag[2:row]=agg_data$pasta_sauce_sales[1:row-1]

agg_data$syrups_sales_lag[1]=0;
agg_data$syrups_sales_lag[2:row]=agg_data$syrups_sales[1:row-1]

agg_data$pasta_coupon_lag[1]=0;
agg_data$pasta_coupon_lag[2:row]=agg_data$pasta_coupon[1:row-1]

agg_data$pancake_mixes_coupon_lag[1]=0;
agg_data$pancake_mixes_coupon_lag[2:row]=agg_data$pancake_mixes_coupon[1:row-1]

agg_data$pasta_sauce_coupon_lag[1]=0;
agg_data$pasta_sauce_coupon_lag[2:row]=agg_data$pasta_sauce_coupon[1:row-1]

agg_data$syrups_coupon_lag[1]=0;
agg_data$syrups_coupon_lag[2:row]=agg_data$syrups_coupon[1:row-1]



str(agg_data)

model1=lm(log(pasta_sales)~week+log(pasta_price)+log(pasta_sauce_price)+log(pasta_sales_lag+1)+log(pasta_coupon+1), data=agg_data)
summary(model1)

model2=lm(log(pasta_sauce_sales)~week+log(pasta_sauce_price)+log(pasta_price)+log(pasta_sauce_sales_lag+1)+log(pasta_sauce_coupon+1), data=agg_data)
summary(model2)

model3=lm(log(pancake_mixes_sales)~log(pancake_mixes_price)+log(syrups_price)+log(pancake_mixes_sales_lag+1)+log(pancake_mixes_coupon+1), data=agg_data)
summary(model3)

model4=lm(log(syrups_sales)~log(pancake_mixes_price)+log(syrups_price)+log(syrups_sales_lag+1)+log(syrups_coupon+1), data=agg_data)
summary(model4)

####### instrument variance


library(systemfit)
library(AER)

model5=ivreg(log(pasta_sales)~log(pasta_price)+log(pasta_sauce_price)+ log(pasta_coupon+1)|log(pasta_sales_lag+1)+log(pasta_sauce_price)+ log(pasta_coupon+1), data=agg_data)
summary(model5)

model5=ivreg(log(pasta_sauce_sales)~log(pasta_sauce_price)+log(pasta_sauce_sauce_price)+log(pasta_sauce_coupon+1)|log(pasta_sauce_sauce_price)+log(pasta_sauce_sales_lag+1) +log(pasta_sauce_coupon+1), data=agg_data)
summary(model5)


library(dlpyr)
