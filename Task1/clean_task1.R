###*** Retail Strategy and Analytics - Task 1****###
###*
###**Clear the environment**##
rm(list = ls())

#Load the packages
library(tidyverse)
library(data.table)
library(ggthemes)
library(ggmosaic)
library(janitor)

#pkgs <- c("tidyverse","data.table","ggthemes","ggmosaic","janitor","readxl")
#pacman::p_load(pkgs,character.only = T)

#Load the dataset
library(readxl)
transaction_data <- read_excel("Data/QVI_transaction_data.xlsx")

###**EDA**###
# 1.Understand the data
str(transaction_data)
glimpse(transaction_data)
head(transaction_data)
skimr::skim_without_charts(transaction_data)
psych::describe(transaction_data)

# 2.Data Cleaning and Wrangling
transaction_data <- clean_names(transaction_data)
names(transaction_data)

#Convert the date from int to date
transaction_data <- transaction_data %>% mutate(date = as.Date(date,origin = "1899-12-30"))

#Checking for duplicates and there is indeed one duplicate which we removed.
sum(duplicated(transaction_data))
transaction_data <- distinct(transaction_data)

# 3.Summary statistic
 transaction_data %>% select_if(is.numeric) %>% 
  map(~summary(.))
 
#There are no outliers, confirming that.
 transaction_data %>%
   ggplot(aes(y= tot_sales,x = 1))+
   geom_boxplot(fill = "4AA4DE")+
   coord_cartesian(ylim=c(0,650))+
   scale_y_continuous(limits = c(0,650),breaks = seq(0,650,by=50))+
   theme_wsj()+
   theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.25,face = "bold"))+
   theme(axis.text.x = element_text(vjust = 0.5,hjust = 1))+
   labs(title = "A Boxplot of Total Sales",caption = glue::glue("Created by Bari on {now()}"), x="",
        y="Total sales")

#Outlier
num <- transaction_data %>% select_if(is.numeric)
num <- num %>% select(-lylty_card_nbr,-txn_id)
glimpse(num)
ggplot(stack(num),aes(x= ind, y=values))+
  geom_boxplot(fill="4AA4DE")+
  theme_hc()+
 theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(axis.text.x = element_text(vjust = 0.5,hjust = 1))+
  labs(title = "A Boxplot of Indices", x="Indices",
       y="Values")

#Number of transaction by day
 transaction_data %>% group_by(date) %>% summarise(Count =n()) %>% 
   ggplot(aes(x = date, y = Count)) +
   geom_line() +
   labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
   scale_x_date(breaks = "1 month") +
   theme_wsj()+
   theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.25,face = "bold"))+
   theme(axis.text.x = element_text(angle=90,vjust = 0.5,hjust = 1))

# We can see that there is an increase in purchases in December and a break in late December. Let's zoom in
 #on this. 
 transaction_data %>% filter(month(date)==12) %>% group_by( Day = mday(date)) %>% summarise(Count =n()) %>% 
   ggplot(aes(x =Day, y = Count)) +
   geom_line(na.rm = F)+ 
   labs(x = "Day", y = "Number of transactions", title = "Transactions over the month of December") +
   scale_x_continuous(limits = c(0,30),breaks = seq(0,30,by = 1))+
   theme_wsj()+
   theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.25,face = "bold"))+
   theme(axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))

 #AS we can see that the increase in sales occurs in the lead-up to Christmas and that there are zero sales on
# Christmas day itself. This is due to shops being closed on Christmas day.
 
 #Let's examine the pack size
 transaction_data <- transaction_data %>% 
   mutate(Size_in_grammes = str_extract(prod_name,"\\d{1,3}"))
 
 #transaction_data$prod_name %>%  gsub("[[:alpha:]]","") %>% 
   #gsub("[gG]","") %>% 
 #table()

transaction_data$Size_in_grammes <- as.numeric(transaction_data$Size_in_grammes)
summary(transaction_data$Size_in_grammes)
glimpse(transaction_data)
#Plot
transaction_data %>% 
  ggplot()+
  geom_histogram(mapping = aes(x=Size_in_grammes,y=..density..),fill="4AA4DE",color="black",binwidth = 25)+
  coord_cartesian(xlim = c(50,400) )+
   # scale_x_continuous(limits = c(50,400),breaks = seq(50,400,by = 50))+
  labs(x = "Pack Size", y = "Frequency", title = "Distribution of Pack Size") +
  theme_wsj()+
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))

#hist(transaction_data$Size_in_grammes)

#Let's create bins for package size
#Which package size are popular to each customer segment.


group_package <- function(Size_in_grammes){
  if (Size_in_grammes >= 110 & Size_in_grammes <= 150){
    return('Small_Packs')
  }else if(Size_in_grammes >150  & Size_in_grammes <= 200){
    return('Medium_Packs')
  }else if (Size_in_grammes > 200 & Size_in_grammes <= 330){
    return('Large_Packs')
  }else if (Size_in_grammes > 330){
    return('Extra_Large_Packs')
  }
}

#Use case_when()

ftttable(transaction_data$Size_in_grammes)
 transaction_data %>% 
mutate(Pack_Size = cut(Size_in_grammes,c(70,150,200,330,380) 
                       ,labels = c("Small_Packs","Medium_Packs","Large_Packs","Extra_Large_Packs") ) ) %>% 
  ggplot(aes(x = Pack_Size)) +
  #geom_bar(position = "dodge",na.rm = T) +
   geom_histogram(position = position_dodge(),stat = "count",na.rm = T)
  theme_few()+
  labs(title = "Popular package size.",x= "Size of the Package",y="Total Count")+
  theme(legend.position = "bottom",text = element_text(size = 15),plot.title = element_text(hjust = 0.5,face = "bold"))

library(dplyr)
transaction_data$Pack_size <- sapply(transaction_data$Size_in_grammes,group_package)
glimpse(transaction_data)

#transaction_data$package <- sapply(transaction_data$pack_size, group_package)
transaction_data$package <- as.factor(transaction_data$package)
glimpse(transaction_data)
levels(chips$Packs_Size)
ggplot(transaction_data, aes(x = package)) +
  geom_bar(position = "dodge") +
  theme_few()+
  labs(title = "Popular package",x= "Size of the Package",y="Total Count",fill ="Customer Segement")+
  theme(legend.position = "bottom",text = element_text(size = 15),plot.title = element_text(hjust = 0.5,face = "bold"))

#Brands
transaction_data <- transaction_data %>% 
  mutate(brand = str_extract(prod_name,"(\\w+)"))
glimpse(transaction_data$brand)
transaction_data %>% 
  select(brand) %>% 
  count(brand,name = "count") %>% 
  arrange(brand) %>% 
  View()

#Load the purchase dataset
filepath<-"C:/Users/kamau/Desktop/Google_Data_Analytics_cert/Quantium_DA/Data/"
purchaseData <-fread(paste0(filepath,"QVI_purchase_behaviour.csv"))

#Examine
purchaseData <- clean_names(purchaseData)

#Merge
df <- merge(transaction_data,purchaseData,all.x = T)
sum(duplicated(df))
sum(is.na(df))
glimpse(df)

#using tabyl
tabyl(df,lifestage,premium_customer)
count(df,lifestage,premium_customer,brand)
tabyl(df,Size_in_grammes,brand)
#Saving df as a csv
fwrite(df,paste0(filepath,"QVI_df.csv"))
a <- df %>% select(-package)
write.csv(a,paste0(filepath,"df.csv"))

#Data analysis
#How many customers are in each segement
customer_segement <- df %>% 
  group_by(premium_customer) %>% 
  summarise(Total = n()) %>% 
  mutate(Totals = sum(Total)) %>% 
  group_by(premium_customer) %>% 
  summarise(Total_percent = Total/Totals) %>% 
  mutate(labels = scales::percent(Total_percent))
#Ploting a pie chart
customer_segement %>% 
  ggplot(aes_(fill=customer_segement$premium_customer,y =customer_segement$labels,x= "" ))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  scale_fill_manual(values = c("#006699","#e6e600","#f08080"))+
  theme_hc()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.border = element_blank(),panel.grid = element_blank(),
        axis.ticks = element_blank(),axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust = -5,size = 20,face = "bold"))+
  geom_text(aes_(label = customer_segement$labels,x=1.2),position = position_stack(vjust = 0.5))+
  labs(title = "Customer Segement Distribution")+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(title = "Customer Segement"))



tab <- df %>% 
  group_by( lifestage,premium_customer) %>% 
  count() %>% 
  rename(Count = n) %>% 
  ungroup() %>% 
  group_by(lifestage) %>% 
  mutate(percent = scales::percent(Count/sum(Count)))
  mutate(Percentage = round(Count/ sum(Count) *100,1))

## Generate the graph
  ggplot(data = tab, aes(x = lifestage, y = percent, fill=premium_customer))+
  geom_bar(stat = "identity", position = "dodge",color="#000000")+
  geom_text(aes(label = percent), vjust = -0.25, hjust = 0.5, size=4,position = position_dodge(width = 0.9))+
  scale_fill_brewer(palette = "Blues")+
  labs(title = "Number of customers by LIFESTAGE and PREMIUM CUSTOMER", x="Lifestage",
       y="Percentage",fill="Customer Segement")+
  theme_wsj()+
  #theme(legend.position = "right")+
  theme(plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(text = element_text(size = 9),axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))

#Who spends more on chips
  #On average which customer segement spends more on chips.
  tab <- df %>% 
    group_by(premium_customer,lifestage) %>% 
    summarise(avg_tot_sales = mean(tot_sales)) %>% 
    ungroup() %>% 
    arrange()
  
  ggplot(tab)+
    geom_col(aes(premium_customer,avg_tot_sales))+
   # facet_wrap(~lifestage)+
    facet_grid(~lifestage)+
    labs(title = "Customer Segement that spends more chips.",x=             "Customer Segement",y="Total sales")+
    theme(text = element_text(size = 13),plot.title =       element_text(hjust = 0.5,face = "bold"))

####  Total sales by LIFESTAGE and PREMIUM_CUSTOMER
  tab <- df %>% 
    group_by(lifestage, premium_customer) %>% 
    summarise(total_sale = round(sum(tot_sales, na.rm = T), 2))
  
  ## Generate the graph
    ggplot(data = tab, aes(x = lifestage, y = total_sale, group = premium_customer, color = premium_customer))+
    geom_line(stat = "summary", size = 2)+
    geom_text(aes(label = total_sale), vjust = -0.25, hjust = 0.5, size=4)+
    theme_wsj()+
    scale_color_manual(values = c("Yellow", "Red","Blue"))+
    labs(title = "Proprotion of Sales", x="Lifestage",y="Total Sales", color="Customer Segement")+
    theme(text = element_text(size = 9),plot.title = element_text(hjust = 0.25,face = "bold"))+
    theme(axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))
    
  
 #### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER    
tab <- df %>% 
  group_by(lifestage,premium_customer) %>% 
  summarise(avg_units = sum(prod_qty)/uniqueN(lylty_card_nbr)) %>% 
  arrange(-avg_units)
tab

ggplot(data = tab, aes(weight = avg_units, x = lifestage, fill = premium_customer)) +
  geom_bar(position = "dodge") +
  theme_wsj()+
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per Customer",fill="Customer Segement") +
  theme(text = element_text(size = 9),plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))

ggplot(data = tab, aes(y= avg_units, x = lifestage, fill = premium_customer)) +
  geom_bar(stat = "identity",position = "dodge") +
  theme_wsj()+
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per Customer",fill="Customer Segement") +
  theme(text = element_text(size = 9),plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))
### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
cust <- df %>% 
  group_by(lifestage,premium_customer) %>% 
  summarise(Customers = uniqueN(lylty_card_nbr))
cust

p <- ggplot(data = cust) +
  geom_mosaic(aes(weight = Customers, x = product(premium_customer,lifestage), fill = premium_customer)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers",fill="Customer Segement") +
  theme_few()+
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(axis.text.x = element_text(angle=90,vjust = 0.5,hjust = 1))
  
  p+geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))


####A Bar plot showing the proportion of customers    
  ggplot(data = cust, aes(weight = Customers,x =lifestage, fill = premium_customer))+
    geom_bar(position =   position_dodge(),color="#000000")+
    theme_wsj()+
    scale_fill_brewer(palette = "Blues")+
    labs(title =  "Proportion of Customers",fill="Customer Segement", x="Lifestage")+
    theme(text = element_text(size = 9),plot.title = element_text(hjust = 0.25,face = "bold"))+
    theme(axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))

  #### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
  Avg_price <- df %>% 
    group_by(lifestage,premium_customer) %>% 
    summarise(avg_price = sum(tot_sales)/sum(prod_qty)) %>% 
    arrange(-avg_price)
  
  #### Create plot
  ggplot(data = Avg_price, aes(weight = avg_price, x = lifestage, fill =premium_customer)) +
    geom_bar(position = position_dodge()) +
    theme_wsj()+
    labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit",fill="Customer Segement") +
    theme(text = element_text(size = 9),plot.title = element_text(hjust = 0.25,face = "bold"))+
    theme(axis.text.x = element_text(angle=90,vjust = 0.5,hjust = 1))

  
# As the difference in average price per unit isn't large, we can check if this difference is statistically different.
  #### Perform an independent t-test between mainstream vs premium and budget
  # midage and
  #### young singles and couples
df <- df %>% mutate( price=tot_sales/prod_qty)

t.test(df[which(df$lifestage %in% c("YOUNG SINGLES/COUPLES","MIDAGE SINGLES/COUPLES") & df$premium_customer== "Mainstream"),"price"],
df[which(df$lifestage%in% c("YOUNG SINGLES/COUPLES","MIDAGE SINGLES/COUPLES") & df$premium_customer !="Mainstream"),"price"],
alternative = "greater")


#Most popular brand based on the amount spent on it.
tab <- df[which(str_detect(df$prod_name,"Chip")),]
View(tab)

tab1 <- tab %>% 
  select(brand,premium_customer,lifestage) %>% 
  group_by(brand,premium_customer,lifestage) %>% 
  count() %>% 
  rename(Count = n) %>% 
  ungroup() %>% 
  group_by(brand) %>% 
  mutate(Percentage = round(Count/ sum(Count) *100,1))

## Generate the graph
ggplot(data = tab1, aes(x = brand, y = Percentage, fill=lifestage))+
  geom_bar(stat = "identity", position = "dodge",color="#000000")+
  facet_wrap(~premium_customer)+
  coord_flip()+
  #geom_text(aes(label = Percentage), vjust = 1, hjust = 1, size=3,position = position_dodge(width = 1))+
  #scale_fill_brewer(palette = "Blues")+
  labs(title = "Number of customers by LIFESTAGE and PREMIUM CUSTOMER", x="Lifestage",
       y="Percentage",fill="Customer Segement")+
  theme_wsj()+
  #theme(legend.position = "right")+
  theme(plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(text = element_text(size = 9),axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))

#Another approach.
ggplot(data = tab1, aes(x = brand, y = Percentage, fill=lifestage))+
  geom_bar(stat = "identity", position = "dodge",color="#000000")+
  #facet_wrap(~premium_customer == "Budget")+
  coord_flip()+
  #geom_text(aes(label = Percentage), vjust = 1, hjust = 1, size=3,position = position_dodge(width = 1))+
  #scale_fill_brewer(palette = "Blues")+
  labs(title = "Number of customers by LIFESTAGE and PREMIUM CUSTOMER", x="Lifestage",
       y="Percentage",fill="Customer Segement")+
  theme_wsj()+
  #theme(legend.position = "right")+
  theme(plot.title = element_text(hjust = 0.25,face = "bold"))+
  theme(text = element_text(size = 9),axis.text.x = element_text(angle=0,vjust = 0.5,hjust = 1))

 tab %>% 
  group_by(brand) %>% 
  summarise(average_amount= mean(tot_sales)) %>% 
  ggplot()+
  geom_col(mapping = aes(brand, 
                         average_amount,fill=average_amount),position = 
             position_stack(reverse = TRUE))+
  coord_flip()+
  scale_fill_gradient(low = "green",high = "red")+ 
  theme_wsj()+
  labs(title = "Popular Brand Based On The Average Amount.",y=                  "Average Amount",x="Brand Names")+
  theme(legend.position = "top",plot.title = element_text(hjust = 0.2          ,face = "bold"))


## Let's see the most preferred brand by each customer segement.

tab %>% 
  group_by(brand) %>% 
  ggplot( aes(x = brand, fill = premium_customer)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
       # coord_flip()+
        facet_wrap(~lifestage)+
        scale_fill_manual(values = c("#006699","#e6e600","#f08080"))+
        theme_minimal()+
        labs(title = "Most Preferred Brands Among Different Customers.",y= "Total Counts",x="Brand Names",fill ="Customer Segement")+
        theme(plot.title = element_text(hjust = 0.25,face = "bold"))+
        theme(text = element_text(size = 15),axis.text.x = element_text(size = 10,vjust = 0.5,hjust = 1))
