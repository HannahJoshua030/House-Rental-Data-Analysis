#HANNAH MEGHNA JOSHUA 
#TP063243
#PROGRAMMING FOR DATA ANALYSIS INDIVIDUAL CODE

#DATA IMPORT
#PACKAGE INSTALLATION
package = function(){
  install.packages("ggplot2")
  install.packages("dplyr")
  install.packages("tidyverse")
  install.packages("ggridges")
  install.packages("ggExtra")
  install.packages("hrbrthemes")
  install.packages("RColorBrewer")
  install.packages("waffle")
  
  library(ggplot2)
  library(dplyr)
  library(tidyverse)
  library(ggridges)
  library(ggExtra)
  library(hrbrthemes)
  library(RColorBrewer)
  library(waffle)
}
package()

#READ AND IMPORT DATA SET 
data = read.csv("/Users/hannahjoshua/Desktop/PFDA/Assignment/House_Rent_Dataset.csv",header = ,sep = ",")

#CHEKING FOR NULL VALUES
is.null(data)

#CHECKING FOR N/A VALUES
colSums(is.na(data))

#CHANGE COLUMN NAMES
colnames(data) <- c("Date_Posted",
                    "BHK","Renting_Price",
                    "House_Size","Floor",
                    "Area_Type",
                    "Locality",
                    "City",
                    "Furnishing_Status",
                    "Preferred_Tenant",
                    "No._of_Bathrooms",
                    "Person_of_Contact")

#DATA EXPLORATION

#VIEW DATA
View(data)

#OUTPUT THE DATA STRUCTURE OF THE HOUSE RENT DATA
class(data)

#SHOW NUMBER OF ROWS AND COLUMNS 
nrow(data)
ncol(data)

#SUMMARY OF FULL DATA SET
summary(data)
glimpse(data)

#DIMENSIONS OF DATA SET 
dim(data)

#10 ROWS FROM HEAD AND TAIL OF THE DATA 
View(head(data,10))
View(tail(data,10))

#CATEGORIZE DATA USING FACTOR
data$City<- as.factor(data$City)
data$Area_Type <- as.factor (data$Area_Type)
data$BHK <- as.factor(data$BHK)
data$Furnishing_Status <- as.factor (data$Furnishing_Status)
data$Preferred_Tenant<-as.factor (data$Preferred_Tenant)
data$Person_of_Contact<-as.factor (data$Person_of_Contact)
data$Locality <- as.factor(data$Locality)

#ELIMINATING ANOMALIES
mean(data$Renting_Price)
summary(data$Renting_Price)
data[data$Renting_Price==3500000, ]
data[1838,3]=34993
data[data$Renting_Price==1200000, ]
data[1002,3]=34993
data[data$Locality == 5000, ]
data[3565,7] = "Velachery"

#CREATE A DATA FRAME FOR EACH CITY 
Kolkata <- subset(data,City == "Kolkata")
Chennai <- subset(data,City == "Chennai")
Bangalore <-subset(data, City == "Bangalore")
Delhi <- subset(data,City == "Delhi")
Mumbai <- subset(data,City == "Mumbai")
Hyderabad <- subset(data,City == "Hyderabad")

#COLOR PALETTE
pastel_rainbow <- c("#DC5B6E","#F19748","#EAD04B","#55A973","#2D8FB6","#6A54B4","#993654")

#SIZE OF CITY DATA FRAME - EXTRA INFORMATION 
city_Size <- data_frame(city_Name,city_Size_v)
city_Size_v <- c(206,43,650,741,426,157)

#POPULATION OF EACH CITY - EXTRA INFORMATION
city_pop <- c(15134000,32066000,10534000,13193000,11503293,20961000)
city_pop <- data_frame(city_Name,city_pop)



                                      #DATA ANALYSIS 



#QUESTION 1 - WHAT KIND OF HOUSES ARE POPULAR FOR RENTING AND WHERE

#ANALYSIS 1.1 - NUMBER OF HOUSES IN EACH CITY 

houses_in_each_city  <- as.table(summary(data$City))                            #MAKE A FREQUENCY TABLE OF THE NUMBER OF HOUSES IN EACH CITY
levels(data$City)                                                               #SHOWS THE CATEGORIES OF THE COLUMN
city_Name <- c("Kolakta","Delhi","Hyderabad","Bangalore","Chennai","Mumbai")    #MAKE A VECTOR WITH THE NAMES OF THE CITIES
numberofHouses <- data.frame(city_Name,houses_in_each_city)                     #CREATING A DATA FRAME
colnames(numberofHouses) <- c("City","Number of Houses")                        #GIVE NEW COLUMN NAMES FOR THE DATA FRAME

ggplot(numberofHouses,aes(x=reorder(city_Name,-houses_in_each_city),y=houses_in_each_city,fill=city_Name))+
  geom_segment(aes(x=city_Name, xend=city_Name, y=0, yend=houses_in_each_city),
               size=1,
               color = "blue",
               linetype = "dotted")+
  geom_text(aes(label = houses_in_each_city,vjust = -2))+
  geom_point(color = "pink", size=7, alpha=0.8)+
  theme_light()+
  coord_flip()+
  labs(title = "Number of houses in each city",x="City",y="Number of Houses")+
  theme(legend.position="none")+
  theme_light()

#ANALYSIS 1.2 - MOST COMMON AREA TYPE 
Type_of_Area <- c(levels(data$Area_Type))
areaType_frequency <- as.integer(c(table(data$Area_Type)))
df_areaType <- data.frame(Type_of_Area,areaType_frequency)

ggplot(df_areaType,aes(x=reorder(Type_of_Area,-areaType_frequency),y=areaType_frequency,fill=Type_of_Area))+
  geom_bar(stat = "identity")+labs(title = "Frequency of Area Types",x="Area Type",y="Frequency",fill = "Area Type")+
  scale_fill_manual(values =(pastel_rainbow))+
  geom_text(aes(label = areaType_frequency), vjust = -0.2)+
  theme(legend.position="none")+
  theme_light()

#ANALYSIS 1.3 - MOST COMMON FURNISHING TYPE  
Type_of_furnishing <- c(levels(data$Furnishing_Status))
furnishingType_frequency <- as.integer(c(table(data$Furnishing_Status)))
df_furnishingType <- data.frame(Type_of_furnishing,furnishingType_frequency)

ggplot(df_furnishingType,aes(x=reorder(Type_of_furnishing,-furnishingType_frequency),
                             y=furnishingType_frequency,
                             fill=Type_of_furnishing))+
  geom_bar(stat = "identity")+labs(title = "Frequency of Furnishing Type",
                                   x="Furnishing Type",
                                   y="Frequency",
                                   fill = "Furnishing Type")+
  scale_fill_manual(values =(pastel_rainbow))+
  geom_text(aes(label = furnishingType_frequency), vjust = -0.2)+
  theme(legend.position="none")+
  theme_light()

#ANLAYSIS 1.4 - MOST COMMON TENANT PREFERRED
Type_of_tenant <- c(levels(data$Preferred_Tenant)) 
TenantType_frequency <- as.integer(c(table(data$Preferred_Tenant)))
df_TenantType <- data.frame(Type_of_tenant,TenantType_frequency)

ggplot(df_TenantType,aes(x=reorder(Type_of_tenant,-TenantType_frequency),
                         y=TenantType_frequency,fill=Type_of_tenant))+
  geom_bar(stat = "identity")+
  labs(title = "Frequency of Tenant Type",
       x="Tenant Type",
       y="Frequency",
       fill = "Tenant Type")+
  scale_fill_manual(values =(pastel_rainbow))+
  geom_text(aes(label = TenantType_frequency), vjust = -0.2)+
  theme(legend.position="none")+theme_light()

#ANALYSIS 1.5 - MOST COMMON CONTACT PERSON
Type_of_contact <- c(levels(data$Person_of_Contact))
ContactType_frequency <-$ c(table(data$Person_of_Contact))
ContactType_frequency <- as.integer(ContactType_frequency)
df_ContactType <- data.frame(Type_of_contact,ContactType_frequency)

ggplot(df_ContactType,aes(x=reorder(Type_of_contact,-ContactType_frequency),y=ContactType_frequency,fill=Type_of_contact))+
  geom_bar(stat = "identity")+
  labs(title = "Frequency of Contact Type",
       x="Contact Person",y="Frequency",
       fill = "Type of Contact")+
  scale_fill_manual(values =(pastel_rainbow))+
  geom_text(aes(label = ContactType_frequency), vjust = -0.2)+
  theme(legend.position="none")+theme_light()


#ANALYSIS 1.6 - RELATIONSHIP BETWEEN HOUSE SIZE AND RENT
ggplot(Kolkata,aes(x=House_Size,y=Renting_Price,group = Furnishing_Status))+
  geom_point(aes(shape=Furnishing_Status,color = Furnishing_Status))+
  labs(title = "Relationship between house size and rent",
       x ="House Size",
       y ="Renting Price",
       colour = "Furnishing Status",
       shape = "Furnishing Status")+
  facet_wrap(~Furnishing_Status)+
  theme_light()

#ANALYSIS 1.7 - MOST COMMON FURNISHING TYPE IN EACH CITY
ggplot(data,aes(x=City,fill=Furnishing_Status))+
  geom_bar(position = position_dodge(width=0.9),alpha = 1)+
  scale_fill_manual(values =(pastel_rainbow))+
  labs(title = "Most common Furnishing Type for in each City",
       y="Number of each Furnishing Type",
       fill = "Type of Furnishing")+
  theme_light()

#ANALYSIS 1.8 - MOST COMMON TENANT PREFERRED IN EACH CITY 
ggplot(data,aes(x=City,fill=Preferred_Tenant))+
  geom_bar(position = position_dodge(width=0.9),alpha = 1)+
  coord_polar()+
  scale_fill_manual(values =(pastel_rainbow))+
  labs(title = "Most common Tenant Type in each City",
       y = "Number of each tenant type",
       fill = "Tenant Preferred")+
  theme_light()


#QUESTION 2 - WHAT KIND OF HOUSES DO BACHELORS PREFER BASED ON LIVING THEIR LIVING CONDITIONS 


Bachelors <- data[data$Preferred_Tenant=="Bachelors", ]          #RUN DATA FRAME FIRST

#ANALYSIS 2.1 - GRAPH FOR FINDING THE PREFERRED FURNISHING TYPE FOR BACHELORS 
ggplot(Bachelors,aes(y=Preferred_Tenant,fill=Furnishing_Status))+
  geom_bar(position = position_dodge(width=0.7),alpha = 0.8)+
  scale_fill_manual(values =(pastel_rainbow))+theme(legend.position = "top")+
  labs(title = "Relationship between Tenant and Furnishing Status",
       x = "Count",
       y = "Preferred Tenant",
       fill = "Furnishing Status")+
  theme_light()

#ANALYSIS 2.2 - RELATIONSHIP BETWEEN RENTING PRICE AND FURNISHING STATUS
avg <- c(mean(data[data$Furnishing_Status == "Furnished","Renting_Price"]),
         mean(data[data$Furnishing_Status == "Semi-Furnished","Renting_Price"]),
         mean(data[data$Furnishing_Status == "Unfurnished","Renting_Price"]))
furnishing_Type <- c(levels(data$Furnishing_Status))
Avg_Rent_Furnishing <- data.frame(furnishing_Type,avg)

ggplot(Avg_Rent_Furnishing,aes(x=furnishing_Type,y=avg,fill=furnishing_Type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values =(pastel_rainbow))+
  labs(title = "Relationship between renting price and furnishing status",
         x = "Furnishing Type",
         y = "Average Rent",)+
  theme(legend.position = "none")+theme_light()

#ANALYSIS 2.3 - RELATIONSHIP BETWEEN BHK AND FURNISHING STATUS
ggplot(data,aes(x=BHK,y=Furnishing_Status))+
  geom_count(color = "#DC5B6E")+
  labs(title = "BHK and Furnishing Status",
       y="Furnishing Type")+
  theme_light()

#ANALYSIS 2.4 - NUMBER OF BATHROOMS PREFERRED BY BACHELORS
Bachelors$No._of_Bathrooms <- as.factor(Bachelors$No._of_Bathrooms )

ggplot(Bachelors,aes(x=No._of_Bathrooms,y=No._of_Bathrooms,fill=No._of_Bathrooms))+
  geom_bar(stat = "identity",alpha=10)+
  coord_polar()+labs(title = "Number of Bathrooms preferred by Bachelors",
                     x=".",
                     y="Number of Batrooms",
                     fill = "Number of Bathrooms")+
  scale_fill_manual(values =(pastel_rainbow))+
  theme_light()

#ANALYSIS 2.5 - CORRELATION BETWEEN NUMBER OF BEDROOOMS TO NUMBER OF BATHROOMS
ggplot(Bachelors,aes(x=No._of_Bathrooms,y=BHK,color = factor(BHK)))+
  geom_jitter(alpha = 0.5)+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Number of Bathroom to BHK",x="Number of Bathrooms",color = "BHK")+
  theme(legend.position = "none")+
  theme_light()

#ANALYSIS 2.6 - WHICH CITY DO MOST BACHELORS RESIDE IN
Bachelor_Count <- c(nrow(Kolkata[Kolkata$Preferred_Tenant=="Bachelors", ]),
                    nrow(Delhi[Delhi$Preferred_Tenant=="Bachelors", ]),
                    nrow(Hyderabad[Hyderabad$Preferred_Tenant=="Bachelors", ]),
                    nrow(Bangalore[Bangalore$Preferred_Tenant=="Bachelors", ]),
                    nrow(Chennai[Chennai$Preferred_Tenant=="Bachelors", ]),
                    nrow(Mumbai[Mumbai$Preferred_Tenant=="Bachelors", ]))

df_Bachelor_Count <- data.frame(city_Name,Bachelor_Count)

ggplot(df_Bachelor_Count,aes(x=city_Name,y=Bachelor_Count))+
  geom_point(size=5, color="#1C7293", fill=alpha("#FED98E", 0.5), alpha=1, shape=21, stroke=2)+
  geom_segment( aes(x=city_Name, xend=city_Name, y=50, yend=Bachelor_Count))+
  labs(title = "Number of Bachelors in each city",x="City Name",y="Number of Bachelors")+
  theme_light()

#ANALYSIS 2.7 - MOST COMMON FURNISHING TYPE IN MUMBAI
Mumbai$Furnishing_Status <- as.factor(Mumbai$Furnishing_Status)
FStatus_Mumbai <- c(nrow(Mumbai[Mumbai$Furnishing_Status == 'Furnished', ]),
                    nrow(Mumbai[Mumbai$Furnishing_Status == 'Semi-Furnished', ]),
                    nrow(Mumbai[Mumbai$Furnishing_Status == 'Unfurnished', ]))
df_FStatus_Mumbai <- data.frame(furnishing_Type,FStatus_Mumbai)

pie(FStatus_Mumbai,labels=c("Furnished","Semi-Furnished","Unfurnished"),
    edges=15,col=pastel_rainbow,radius = 1)


#Analysis 2.8 - LOCALTIY IN MUMBAI POPULATED BY MOST BACHELORS
Mumbai_Bachelors <- Mumbai[Mumbai$Preferred_Tenant=="Bachelors", ]
Mumbai_Bachelors$Locality <- as.factor(Mumbai_Bachelors$Locality)
Popular_Mumbai_Bachelor_Locality <- data.frame(head(as.table(summary(Mumbai_Bachelors$Locality))))
colnames(Popular_Mumbai_Bachelor_Locality) <- c("Locality","Frequency")

ggplot(Popular_Mumbai_Bachelor_Locality,aes(x=Locality, y=Frequency))+
  geom_point(color = "pink", size=7, alpha=0.8)+
  geom_segment(aes(x=Locality,xend=Locality,y=0,yend=Frequency))+
  labs(title = "Most popular Locality among Bachelors")+
  theme_light()

#ANALYSIS 2.9 - BHK AND HOUSE SIZE IN MOST COMMON LOCALITY
Mumbai_Bachelors_Goregaon <- Mumbai_Bachelors[Mumbai_Bachelors$Locality=="Goregaon West", ]
Mumbai_Bachelors_Goregaon$BHK <- as.factor(Mumbai_Bachelors_Goregaon$BHK)

ggplot(Mumbai_Bachelors_Goregaon,aes(x=BHK,y=House_Size,fill = BHK))+
  geom_boxplot()+
  geom_jitter()+
  scale_fill_manual(values = pastel_rainbow)+
  labs(title = "BHK and House size in Goregaon West",y="House Size")+
  theme_light()

#ANALYSIS 2.10 - AVERAGE RENTING AND HOUSE SIZE IN EACH LOCALITY
Mumbai_Bachelors_Top6_Locality <- Mumbai_Bachelors[Mumbai_Bachelors$Locality == "Goregaon West"|
                                                     Mumbai_Bachelors$Locality == "Bandra West"|
                                                     Mumbai_Bachelors$Locality == "Mulund West"|
                                                     Mumbai_Bachelors$Locality == "Andheri West"|
                                                     Mumbai_Bachelors$Locality == "Kandivali West"|
                                                     Mumbai_Bachelors$Locality == "Mahim West", ]
Mumbai_Bachelors_Top6_Locality$Locality <- as.factor(Mumbai_Bachelors_Top6_Locality$Locality)

ggplot(Mumbai_Bachelors_Top6_Locality, aes(x=Renting_Price,y=House_Size,color=Locality))+
  geom_point()+
  labs(title="Average Renting price and house size in each locality",
       x="Renting Price",
       y="House Size")+
  scale_color_brewer(palette = "Set1")+
  theme_light()

#ANALYSIS 2.11 - PREFERRED CONTACT BY BACHELORS
Bachelors$Person_of_Contact <- as.factor(Bachelors$Person_of_Contact)
Commom_Contact_Bachelors <- c(nrow(Bachelors[Bachelors$Person_of_Contact == 'Contact Owner', ]),
                              nrow(Bachelors[Bachelors$Person_of_Contact == 'Contact Agent', ]),
                              nrow(Bachelors[Bachelors$Person_of_Contact == 'Contact Builder', ]))

pie(Commom_Contact_Bachelors,
    labels = c("Contact Owner","Contact Agent","Contact Builder"),
    edges = 15,col = pastel_rainbow)



#QUESTION 3 - WHAT KIND OF HOUSES DO FAMILIES PREFER


Family <- data[data$Preferred_Tenant == "Family", ]
Mumbai <- subset(data,City == "Mumbai")

#ANALYSIS 3.1 - MOST PREFERRED BHK BY FAMILIES 
Family$BHK <- as.factor(Family$BHK)
Family$City <- as.factor(Family$City)

ggplot(Family,aes(x=BHK,fill=Preferred_Tenant))+
  geom_bar()+
  coord_polar()+
  labs(title = "Most preferred BHK by Families",x = "BHK",y="Count",fill = ".")+
  scale_fill_manual(values =(pastel_rainbow))+
  theme_light()

#ANALYSIS 3.2 - MOST PREFERRED FURNISHING TYPE BY FAMILES
PBHK_Family <- Family[Family$BHK == 2 | Family$BHK == 3, ]
PBHK_Family$BHK <- as.factor(PBHK_Family$BHK)

ggplot(PBHK_Family,aes(x=Furnishing_Status,fill=BHK))+
  geom_bar()+scale_fill_manual(values =(pastel_rainbow))+
  labs(title = "Most preferred Furnishing Staus by Families",
       x = "Furnishing Type",
       y="Count")+
  theme_light()

#ANALYSIS 3.3 - AVERAGE HOUSE SIZE OF TOP 2 BHK
#RUN DATA FRAMES FIRST
PBHK_Family <- Family[Family$BHK == 2 | Family$BHK == 3, ]
PBHK_Family$BHK <- as.factor(PBHK_Family$BHK)

ggplot(PBHK_Family,aes(x=BHK,y=House_Size,fill=BHK))+
  geom_violin()+
  labs(title = "Average House size of 2 and 3 BHK",y = "House Size")+
  scale_fill_manual(values =(pastel_rainbow))+
  theme_light()

#ANALYSIS 3.4 - AVERAGE RENTING PRICE FOR BHK 2 AND 3
PBHK_Family <- Family[Family$BHK == 2 | Family$BHK == 3, ]
BHK2_3_SemiFurnished <- PBHK_Family[PBHK_Family$Furnishing_Status == "Semi-Furnished", ]

ggplot(BHK2_3_SemiFurnished,aes(y=BHK,x=Renting_Price,fill=BHK))+
  geom_boxplot()+
  geom_jitter(color = "pink",alpha = 0.5,size=3)+
  scale_fill_manual(values =(pastel_rainbow))+
  labs(title = "Average Renting price for 2 and 3 BHK",x="Renting Price")+
  theme_light()

#ANALYSIS 3.5 - CITY WHERE MOST FAMILIES RESIDE IN
Family <- data[data$Preferred_Tenant == "Family", ]

ggplot(Family,aes(x=City,y=Preferred_Tenant))+
  geom_jitter(aes(color = City),shape = 19)+
  labs(title = "City that most families reside in",y = "Family")+
  scale_color_brewer(palette = "Set2")+
  theme_light()

#ANALYSIS 3.6 - AVERAGE HOUSE SIZE OF BHK 2 AND 3 IN MUMBAI
PBHK_Family <- Family[Family$BHK == 2 | Family$BHK == 3, ]
Family_in_Mumbai <- PBHK_Family[PBHK_Family$City == "Mumbai", ]
Family_in_Mumbai$BHK <- as.factor(Family_in_Mumbai$BHK)

ggplot(Family_in_Mumbai,aes(x=BHK,y=House_Size,fill=BHK))+
  geom_violin()+
  labs(title="Average House size of each BHK in Mumbai",y = "House Size")+
  scale_fill_manual(values = pastel_rainbow)+
  theme_light()

#CITY SIZE
pie(city_Size_v,labels = c("Kolkata","Delhi","Hyderabad","Bangalore","Chennai","Mumbai"),
    col=pastel_rainbow,radius = 1,edges=20)

#ANALYSIS 3.7 - AVERAGE RENTING PRICE FOR BHK 2 AND 3 IN MUMBAI
PBHK_Family <- Family[Family$BHK == 2 | Family$BHK == 3, ]
Family_in_Mumbai <- PBHK_Family[PBHK_Family$City == "Mumbai", ]
Semi_Furnished_Mumbai <- Family_in_Mumbai[Family_in_Mumbai$Furnishing_Status=="Semi-Furnished", ]
Semi_Furnished_Mumbai$BHK <- as.factor(Semi_Furnished_Mumbai$BHK )

ggplot(Semi_Furnished_Mumbai,aes(y=BHK,x=Renting_Price,fill=BHK))+
  geom_density_ridges()+
  labs(title = "Average Renting price of each BHK in Mumbai",
       x = "Renting Price")+
  scale_fill_manual(values =(pastel_rainbow))+
  theme_light()

#ANALYSIS 3.8 - LOCALITY WHERE MOST FAMLILIES RESIDE IN MUMBAI
Mumbai <- subset(data,City == "Mumbai")

Mumbai_Family <- Mumbai[Mumbai$Preferred_Tenant=="Family", ]
Mumbai_Family$Locality <- as.factor(Mumbai_Family$Locality)
Popular_Mumbai_Family_Locality <- data.frame(head(as.table(summary(Mumbai_Family$Locality))))
colnames(Popular_Mumbai_Family_Locality) <- c("Locality","Frequency")

ggplot(Popular_Mumbai_Family_Locality,aes(x=Locality, y=Frequency))+
  geom_point(color = "pink",size=10)+
  geom_segment(aes(x=Locality,xend=Locality,y=0,yend=Frequency))+
  labs(title = "Top 6 localities where most families reside in Mumbai")+
  coord_flip()+
  theme_light()

#ANALYSIS 3.9 - AVERAGE RETNTING PRICE FOR EACH BHK IN TOP 4 LOCALITIES
Mumbai_Family_Top4_Locality <- Mumbai_Family[Mumbai_Family$Locality == "JVPD Scheme"|
                                                     Mumbai_Family$Locality == "L&T Emerald Isle, Powai"|
                                                     Mumbai_Family$Locality == "Lokhandwala Complex"|
                                                     Mumbai_Family$Locality == "Runwal Elegante, Andheri West", ]
Mumbai_Family_Top4_Locality$Locality <- as.factor(Mumbai_Family_Top4_Locality$Locality)
Mumbai_Family_Top4_Locality$BHK <- as.factor(Mumbai_Family_Top4_Locality$BHK)

ggplot(Mumbai_Family_Top4_Locality, aes(x=BHK,y=Renting_Price,color=Locality))+
  geom_point(size = 5)+
  labs(title = "Average renting price for each BHK in Top 4 Localities",y="Renting Price")+
  scale_color_brewer(palette = "Set2")+
  theme_light()

#ANALYSIS 3.10 - AVERAGE RENTING PRICE AND HOUSE SIZE IN TOP 4 LOCALITIES
Mumbai_Family_Top4_Locality <- Mumbai_Family[Mumbai_Family$Locality == "JVPD Scheme"|
                                               Mumbai_Family$Locality == "L&T Emerald Isle, Powai"|
                                               Mumbai_Family$Locality == "Lokhandwala Complex"|
                                               Mumbai_Family$Locality == "Runwal Elegante, Andheri West", ]
Mumbai_Family_Top4_Locality$Locality <- as.factor(Mumbai_Family_Top4_Locality$Locality)
Mumbai_Family_Top4_Locality$BHK <- as.factor(Mumbai_Family_Top4_Locality$BHK)

ggplot(Mumbai_Family_Top4_Locality, aes(x=House_Size,y=Renting_Price,color=Locality))+
  geom_point(size = 5)+
  labs(title = " Average house size and renting price in Top 4 Localities",
       x = "House Size",
       y = "Renting Price")+
  scale_color_brewer(palette = "Set2")+
  theme_light()

#ANALYSIS 3.11 - NUMBER OF BATHROOMS FAMILIES PREFER
Mumbai <- subset(data,City == "Mumbai")
Mumbai_Family <- Mumbai[Mumbai$Preferred_Tenant=="Family", ]
Mumbai_Family$No._of_Bathrooms <- as.factor(Mumbai_Family$No._of_Bathrooms)

ggplot(Mumbai_Family,aes(x=No._of_Bathrooms,fill=No._of_Bathrooms))+
  geom_bar()+
  labs(title = "Number of Bathrooms families prefer",
       x = "Number of Bathrooms",
       y = "Count")+
  theme(legend.position = "none")+
  scale_fill_manual(values=pastel_rainbow)+
  theme_light()

#ANALYSIS 3.12 - MOST PREFERRED FLOOR BY FAMILIES
Family$Floor <- as.factor(Family$Floor)
summary(Family$Floor)

Floor1_Family <- Family[str_detect(Family$Floor,"1 out of"), ]
Floor2_Family <- Family[str_detect(Family$Floor,"2 out of"), ]
GroundFloor_Family <- Family[str_detect(Family$Floor,"Ground out of"), ]

Most_Common_Floor_Family <- c(nrow(Floor1_Family),nrow(Floor2_Family),nrow(GroundFloor_Family))

pie(Most_Common_Floor_Family, labels = c("First Floor","Second Floor","Ground Floor"),
    edges = 15, col = pastel_rainbow)


#ANALYSIS 3.13 - MOST PREFERRED PERSON OF CONTACT FOR FAMILIES
ggplot(Family_in_Mumbai,aes(x=Person_of_Contact,fill=Person_of_Contact))+
  geom_bar()+
  scale_fill_manual(values =(pastel_rainbow))+
  labs(title = "Most preferred contact by families",
       x="Person of Contact",
       y = "Count",
       fill = "Person of Contact")+
  theme_light()




#QUESTION 4 - WHAT KIND OF HOUSES SHOULD AGENTS CONSIDER RENTING

df_Agent <- data[data$Person_of_Contact=="Contact Agent", ]

#ANALYSIS 4.1 - MOST COMMON TENANT THAT CONTACT AGENTS
ggplot(df_Agent,aes(x=Preferred_Tenant,fill=Preferred_Tenant))+geom_bar()+
  labs(title = "Most popular Tenant type that contact agents",
       x = "Preferred Tenant",
       y = "Count",
       fill = "Preferred Tenant")+
  scale_fill_manual(values=pastel_rainbow)+
  theme_light()

#ANALYSIS 4.2 - REALATIONSHIP BETWEEN HOUSE SIZE AND FURNISHING STATUS PREFERRED BY BACHELORS/FAMILY
df_Agent_BothTenant <- df_Agent[df_Agent$Preferred_Tenant =="Bachelors/Family", ]

ggplot(df_Agent_BothTenant,aes(x=Furnishing_Status,y=House_Size,colour = factor(BHK)))+
  geom_jitter(shape = 16)+
  geom_line(aes(colour=factor(BHK)),alpha = 0.2)+scale_fill_manual(values =(pastel_rainbow))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Furnishing Status and house size preferred by Bachelors/Family",
       x = "Furnishing Status",
       y = "House Size",
       colour = "BHK")+
  theme_light()

#ANALYSIS 4.3 - CORRELATION BETWEEN HOUSE SIZE AND RENT FOR HOUSES AVAILABLE FOR BOTH TENANTS
df_Agent_BothTenant$BHK <- as.factor(df_Agent_BothTenant$BHK)

p <- ggplot(df_Agent_BothTenant,aes(x=House_Size,y=Renting_Price,colour = BHK))+
  geom_point()+
  geom_smooth(method = lm,colour = "black", se = FALSE)+
  labs(title = "Correlation between House size and Rent",
       x = "House Size",
       y = "Renting Price")+
  scale_color_brewer(palette = "Dark2")+
  stat_ellipse()+
  theme_light()
p1 <- ggMarginal(p,type="boxplot",fill = "pink")
print(p1)

#ANALYSIS 4.4 - SPECIFIC SEASON WHERE HOUSE SALES ARE HIGH
df_Agent_BothTenant$Date_Posted <- as.factor(df_Agent$Date_Posted)
df_Agent_BothTenant$Date_Posted <- strptime(df_Agent$Date_Posted,format = "%m/%d/%Y")
df_Agent_BothTenant$Date_Posted <- as.Date(df_Agent$Date_Posted,format="%d/%m/%Y")

ggplot(df_Agent_BothTenant,aes(y=Date_Posted,fill=factor(Date_Posted)))+
  geom_bar()+
  labs(title = "Date when most houses are sold",
       x = "Furnishing Status",
       y = "Count",
       fill = "Furnishing Status")+
  theme(legend.position = "none")

#ANALYSIS 4.5 - MOST COMMON FURNISHING TYPE BEING SOLD IN THIS PERIOD
df_Agent_BothTenant_Peak_Dates <- df_Agent_BothTenant[df_Agent_BothTenant$Date_Posted > "2022-6-30", ]

ggplot(df_Agent_BothTenant_Peak_Dates,aes(x=Furnishing_Status,fill=Furnishing_Status))+
  geom_bar()+scale_fill_manual(values=pastel_rainbow)




#QUESTION 5 - WHAT KIND OF HOUSES DO NEWLY MARRIED COUPLES PREFER AND WHICH CITY DO THEY SETTLE IN

df_BF_F_Tenant <- data[data$Preferred_Tenant=="Bachelors/Family" | data$Preferred_Tenant=="Family", ]
df_BF_F_Tenant$BHK <- as.factor(df_BF_F_Tenant$BHK)

#ANALYSIS 5.1 - MOST COMMON BHK PREFERRED BY COUPLES
ggplot(df_BF_F_Tenant,aes(x=BHK,fill=BHK))+
  geom_bar()+
  labs(title = "BHK preferred by families")+
  scale_fill_manual(values =(pastel_rainbow))+
  theme_light()

#ANALYSIS 5.2 - AVERAGE RENTING PROCE FOR 2 BHK
df_BF_F_Tenant_BHK2 <- df_BF_F_Tenant[df_BF_F_Tenant$BHK==2, ]
df_BF_F_Tenant_BHK2$BHK <- as.factor(df_BF_F_Tenant_BHK2$BHK)

ggplot(df_BF_F_Tenant_BHK2,aes(x=BHK,y=Renting_Price,fill = BHK))+
  geom_boxplot()+
  labs(title = "Average rent for 2 BHK",y = "Renting Price")+
  scale_fill_manual(values = pastel_rainbow)+
  theme_light()

#ANALYSIS 5.3 - 
df_BF_F_Tenant_BHK2$City <- as.factor(df_BF_F_Tenant_BHK2$City)
df_BF_F_Tenant_BHK2$Furnishing_Status <- as.factor(df_BF_F_Tenant_BHK2$Furnishing_Status)

ggplot(df_BF_F_Tenant_BHK2,aes(y=City,x=Renting_Price<22000,color = Furnishing_Status))+
  geom_jitter()+
  labs(title = "Houses availbale in each City with renting price below 22 000",
       x= "Renting Price < 22 000",
       color = "Furnishing Type")+
  scale_color_brewer(palette = "Set2")+
  theme_light()

#ANALYSIS 5.4 - AVERAGE HOUSE SIZE IN EACH CITY FOR RENT LESS THAN 22 000
df_BF_F_Tenant_BHK2_22k <- df_BF_F_Tenant_BHK2[df_BF_F_Tenant_BHK2$Renting_Price<22000, ]
df_BF_F_Tenant_BHK2_22k$City <- as.factor(df_BF_F_Tenant_BHK2_22k$City)

ggplot(df_BF_F_Tenant_BHK2_22k,aes(y=City,x=House_Size,color = City))+
  geom_density_ridges(fill = "white",size = 1)+
  labs(title = "Average house size in each city for rent less than 22 000",
       x = "House Size")+
  scale_color_manual(values = pastel_rainbow)+
  theme_light()

#ANALYSIS 5.5 - DOES PERSON OF CONTACT AFFECT RENTING PRICE AND HOUSE SIZE
ggplot(df_BF_F_Tenant_BHK2_22k,aes(y=Renting_Price,x=House_Size,color = Person_of_Contact))+
  geom_point(alpha = 0.5)+
  scale_color_brewer(palette = "Set2")+
  labs(x = "House Size",y = "Renting Price",color = "Person of Contact")+
  facet_wrap(~Person_of_Contact)+
  theme_light()

#ANALYSIS 5.6 - MOST POPULAR LOCALITIES IN CHENNAI
df_BF_F_Tenant_BHK2_22k_Chennai <- df_BF_F_Tenant_BHK2_22k[df_BF_F_Tenant_BHK2_22k$City == "Chennai", ]
df_BF_F_Tenant_BHK2_22k_Chennai$Locality <-  as.factor(df_BF_F_Tenant_BHK2_22k_Chennai$Locality)
summary(df_BF_F_Tenant_BHK2_22k_Chennai$Locality)

Popular_Locality_Chennai_22k_2BHK <- data.frame(head(as.table(summary(df_BF_F_Tenant_BHK2_22k_Chennai$Locality))))
colnames(Popular_Locality_Chennai_22k_2BHK) <- c("Locality","Frequency")

ggplot(Popular_Locality_Chennai_22k_2BHK,aes(x=Locality, y=Frequency))+
  geom_point(color = "light pink",size=10)+
  labs(title = "Most popular localities in Chennai")+
  geom_segment(aes(x=Locality,xend=Locality,y=0,yend=Frequency))+
  coord_flip()+
  theme_light()

#ANALYSIS 5.7 - MOST POPULAR LOCALITIES IN HYDERABAD
df_BF_F_Tenant_BHK2_22k_Hyderabad <- df_BF_F_Tenant_BHK2_22k[df_BF_F_Tenant_BHK2_22k$City == "Hyderabad", ]
df_BF_F_Tenant_BHK2_22k_Hyderabad$Locality <-  as.factor(df_BF_F_Tenant_BHK2_22k_Hyderabad$Locality)

Popular_Locality_Hyderabad_22k_2BHK <- data.frame(head(as.table(summary(df_BF_F_Tenant_BHK2_22k_Hyderabad$Locality))))
colnames(Popular_Locality_Hyderabad_22k_2BHK) <- c("Locality","Frequency")

ggplot(Popular_Locality_Hyderabad_22k_2BHK,aes(x=Locality, y=Frequency))+
  geom_point(color = "light blue",size=10)+
  labs(title = "Most popular localities in Hyderabad")+
  geom_segment(aes(x=Locality,xend=Locality,y=0,yend=Frequency))+
  coord_flip()+
  theme_light()

#ANALYSIS 5.8 - MOST POPULAR LOCALITIES IN BANGALORE
df_BF_F_Tenant_BHK2_22k_Bangalore <- df_BF_F_Tenant_BHK2_22k[df_BF_F_Tenant_BHK2_22k$City == "Bangalore", ]
df_BF_F_Tenant_BHK2_22k_Bangalore$Locality <-  as.factor(df_BF_F_Tenant_BHK2_22k_Bangalore$Locality)

Popular_Locality_Bangalore_22k_2BHK <- data.frame(head(as.table(summary(df_BF_F_Tenant_BHK2_22k_Bangalore$Locality))))
colnames(Popular_Locality_Bangalore_22k_2BHK) <- c("Locality","Frequency")

ggplot(Popular_Locality_Bangalore_22k_2BHK,aes(x=Locality, y=Frequency))+
  geom_point(color = "light green",size=10)+
  labs(title = "Most popular localities in Bangalore")+
  geom_segment(aes(x=Locality,xend=Locality,y=0,yend=Frequency))+
  coord_flip()+
  theme_light()


#QUESTION 6 - WHAT FACTORS SHOULD OWNERS CONSIDER WHEN BUYING A HOUSE

#ANALYSIS 6.1 - AVERAGE HOUSE SIZE

mean(data$House_Size)

ggplot(data,aes(y=House_Size))+
  geom_boxplot(fill = "pink")+
  labs(title = "Average house size", y = "House Size")+
  theme_light()

#ANALYSIS 6.2 - AVERAGE PRICE AND SIZE OF HOUSES FOR EACH FURNISHING STATUS WITHIN THE AVERAGE HOUSE SIZE RANGE
House_Size_Average_Overall <- data[data$House_Size>950&data$House_Size<1000, ]

ggplot(House_Size_Average_Overall,aes(y=House_Size,x=Renting_Price,color=Furnishing_Status))+
  geom_point(size = 3)+
  labs(title = "Average price and size of each furnishing type",
       x = "Renting Price",
       y = "House Size",
       color = "Furnishing Type")+
  scale_color_brewer(palette = "Set2")+
  theme_light()

#ANALYSIS 6.3 - AVERAGE HOUSE SIZE FOR EACH AREA TYPE
House_Size_1000 <- data[data$House_Size==1000, ]
summary(House_Size_1000$Area_Type)

ggplot(House_Size_1000,aes(x=Area_Type,y=Renting_Price,color = Area_Type))+
  geom_boxplot()+
  labs(title = "Average House size for each Area Type",
       x = "Area Type",
       y = "Renting Price",
       fill = "Area Type")+
  geom_jitter(alpha=0.5)+
  scale_color_brewer(palette = "Set2")+
  theme_light()

#ANALYSIS 6.4 - AVERAGE HOUSE SIZE FOR EACH BHK
ggplot(data,aes(x=BHK,y=House_Size,fill = BHK))+
  scale_fill_manual(values = pastel_rainbow)+
  geom_boxplot(notch = TRUE)+
  labs(title = "Average house size for each BHK",
       y = "House Size")+
  theme_light()

#ANALYSIS 6.5 - 
Overall_Top6_Locality <- data.frame(head(as.table(summary(data$Locality))))
colnames(Overall_Top6_Locality) <- c("Locality","Frequency")

ggplot(Overall_Top6_Locality,aes(x=Locality,y=Frequency,fill= Locality))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = pastel_rainbow)+
  theme_light()



#QUESTION 7 - WHAT HOUSES DO CHENNAI,BANGALORE AND HYDERABAD OFFER

#ANALYSIS 7.1 - AVERAGE RENTING PRICE FOR EACH FURNISHING TYPE IN EACH OF THESE CITIES

BCH <- data[data$City == "Chennai"|
              data$City == "Hyderabad"|
              data$City == "Bangalore", ]
BCH$City <- as.factor(BCH$City)
BCH$Furnishing_Status <- as.factor(BCH$Furnishing_Status)

ggplot(BCH,aes(x=Furnishing_Status,y=Renting_Price,fill = City))+
  geom_violin(alpha = 0.7)+
  labs(title = "Average renting price for each furnishing type",
       x = "Furnishing Type",
       y = "Renting Price")+
  scale_fill_manual(values = pastel_rainbow)+
  facet_wrap(~City)+
  theme_light()

#ANALYSIS 7.2 - AVERAGE RENTING PRICE FOR EACH BHK
BCH$BHK <- as.factor(BCH$BHK)

ggplot(BCH,aes(x=BHK,y=Renting_Price,fill = City))+
  labs(title = "Average renting price in each City",
       y = "Renting Price")+
  scale_fill_manual(values = pastel_rainbow)+
  geom_boxplot(alpha=0.7)+
  facet_wrap(~City)+
  theme_light()

#ANALYSIS 7.3 - Correlation between house size and renting price
ggplot(BCH,aes(y=Renting_Price,x=House_Size, color = City))+
  geom_jitter()+
  geom_smooth(method = lm,color = "black",fill ="#69b3a2" ,se=TRUE)+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Correlation of house size and renting price",
       x = "House Size",
       y = "Renting Price")+
  facet_wrap(~City)+
  theme_light()

#ANALYSIS 7.4 - TOP LOCALITIES IN EACH CITY
Bangalore$Locality <- as.factor(Bangalore$Locality)
head(summary(Bangalore$Locality))

Chennai$Locality <- as.factor(Chennai$Locality)
head(summary(Chennai$Locality))

Hyderabad$Locality <- as.factor(Hyderabad$Locality)
head(summary(Hyderabad$Locality))

#ANALYSIS 7.5 - 
summary(Bangalore$Renting_Price)
summary(Bangalore[Bangalore$Locality == "Electronic City","Renting_Price"])

ggplot(Bangalore,aes(y=Bangalore$Locality == "Electronic City",x=Renting_Price))+
  labs(title = "Renting price in Electronic City",y = "Locality = Electronic City",x = "Renting Price")+
  geom_violin(fill = "pink")

#ANALYSIS 7.6
summary(Chennai$Renting_Price)
summary(Chennai[Chennai$Locality == "Velachery","Renting_Price"])

ggplot(Chennai,aes(y=Chennai$Locality == "Velachery",x=Renting_Price))+
  labs(title = "Renting price in Velachery",y = "Locality = Velachery",x = "Renting Price")+
  geom_violin(fill = "pink")

#ANALYSIS 7.7
summary(Hyderabad$Renting_Price)
summary(Hyderabad[Hyderabad$Locality == "Gachibowli","Renting_Price"])

ggplot(Hyderabad,aes(y=Hyderabad$Locality == "Gachibowli",x=Renting_Price))+
  labs(title = "Renting price in Gachibowli",y = "Locality = Gachibowli",x = "Renting Price")+
  geom_violin(fill = "pink")

#ANALYSIS 7.8
ElectronicCity_df <- Bangalore[Bangalore$Locality == "Electronic City", ]
ElectronicCity_df$Preferred_Tenant <- as.factor(ElectronicCity_df$Preferred_Tenant)

ggplot(ElectronicCity_df, aes(x=Preferred_Tenant, fill = Preferred_Tenant))+
  scale_fill_manual(values = pastel_rainbow)+
  labs(title = "Most common tenant type in Electronic City",
       x = "Preferred Tenant",
       fill = "Preferred Tenant")+
  geom_bar()+
  theme_light()

#ANALYSIS 7.9
Velachery_df <-  Chennai[Chennai$Locality == "Velachery", ]
Velachery_df$Preferred_Tenant <- as.factor(Velachery_df$Preferred_Tenant)

ggplot(Velachery_df, aes(x=Preferred_Tenant, fill = Preferred_Tenant))+
  geom_bar()+
  scale_fill_manual(values = pastel_rainbow)+
  labs(title = "Most common tenant type in Velachery",
       x = "Preferred Tenant",
       fill = "Preferred Tenant")+
  geom_bar()+
  theme_light()

#ANALYSIS 7.10
Gachibowli_df <- Hyderabad[Hyderabad$Locality == "Gachibowli", ]
Gachibowli_df$Preferred_Tenant <- as.factor(Gachibowli_df$Preferred_Tenant)

ggplot(Gachibowli_df, aes(x=Preferred_Tenant, fill = Preferred_Tenant))+
  geom_bar()+
  scale_fill_manual(values = pastel_rainbow)+
  labs(title = "Most common tenant type in Gachibowli",
       x = "Preferred Tenant",
       fill = "Preferred Tenant")+
  geom_bar()+
  theme_light()

#QUESTION 8 - WHAT KIND OF HOUSES ARE POPULAR IN KOLKATA

Kolkata <- subset(data$City == "Kolkata")

#ANALYSIS 8.1 - MOST COMMON TENANT 
ggplot(Kolkata,aes(x = Preferred_Tenant,fill =Preferred_Tenant))+
  geom_bar()+
  scale_fill_manual(values = pastel_rainbow)+
  labs(title = "Most common tenant in Kolkata",
       x = "Preferred Tenant",
       y = "Count",
       fill = "Preferred Tenant")+
  theme_light()

#ANALYSIS 8.2 - MOST COMMON BHK
Kolkata$BHK <- as.factor(Kolkata$BHK)
BHK_vector <- as.vector(summary(Kolkata$BHK))
BHK_vector <- c(B1 = 142,B2 = 276,B3 = 92,B4 = 11,B5 = 2,B6=1)

waffle(BHK_vector,rows =21)+scale_color_brewer(palette = "Dark2")+
  labs(title = "Most common BHK in Kolkata",x = "B1 -> 1 Bedroom  1 square -> 1")

#ANALYSIS 8.3 - AVERAGE RENT IN KOLKATA
ggplot(Kolkata,aes(y = BHK,x = Renting_Price,fill = stat(x)))+
  geom_density_ridges_gradient()+
  scale_fill_viridis_c(name = "Depth",option = "H")+
  labs(title = "Average rent in Kolkata with BHK ",x = "Renting Price")+
  theme_light()

#ANALYSIS 8.4 - AVERAGE HOUSE SIZE 
ggplot(Kolkata,aes(x = Furnishing_Status,y = House_Size,fill = Furnishing_Status))+
  geom_violin()+geom_boxplot(alpha = 0.7)+
  labs(title = "Average house size with furnishing status",
       x = "Furnishing Status",
       y = "House Size")+
  scale_fill_manual(values = pastel_rainbow)+
  theme_light()

#ANALYSIS 8.5 - MOST COMMON FURNISHING TYPE 
ggplot(Kolkata,aes(x = Furnishing_Status,fill = Furnishing_Status))+
  geom_bar()+
  labs(title = "Most common Furnishing Status in Kolkata",
       x = "Furnishing Status",
       y = "Count",
       fill = "Furnishing Status")+
  scale_fill_manual(values = pastel_rainbow)+
  theme_light()

#ANALYSIS 8.6 - AVERAGE RENT FOR EACH FURNISHING TYPE
ggplot(Kolkata,aes(x = Furnishing_Status,y=Renting_Price,fill = Furnishing_Status))+
  geom_violin()+
  labs(title = "Average rent with furnishing status",
       x = "Furnishing Status",
       y = "Renting Price")+
  scale_fill_manual(values = pastel_rainbow)+
  theme_light()
