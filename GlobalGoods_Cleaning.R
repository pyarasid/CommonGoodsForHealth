
##=============Data wrangling and cleaning to prepare the data for input in Machine Learning algorithms================##
#Created by: Sid

#Loading libraries
library(readxl)
library(tidyverse)
library(cld2) #language detection package from google

#reading the raw CSV file
GlobalFunc <- read_excel("C:\\Users\\sd282\\Box\\R\\NLP with NLTK\\Global Goods_Calssification\\Global_Function_healthcare_new analysis\\Coding Master 20190710 vPC.xlsx")
View(GlobalFunc)
dim(GlobalFunc)

names(GlobalFunc)
summary(GlobalFunc)
sapply(GlobalFunc, class)

#Use the following command to hide the warnings 
#options(warn=-1)

#attached the dataframe
attach(GlobalFunc)

#Converting "NAs" in the vector to "0"
GlobalFunc$`11_RD_New_Health_Tools` <- replace(`11_RD_New_Health_Tools`, is.na(`11_RD_New_Health_Tools`), 0)
GlobalFunc$`12_Dev_Harmonization_Int_Reg` <- replace(`12_Dev_Harmonization_Int_Reg`, is.na(`12_Dev_Harmonization_Int_Reg`), 0)
GlobalFunc$`13_Knowledge_Gen_Sharing` <- replace(`13_Knowledge_Gen_Sharing`, is.na(`13_Knowledge_Gen_Sharing`), 0)
GlobalFunc$`14_Sharing_IP` <- replace(`14_Sharing_IP`, is.na(`14_Sharing_IP`), 0)
GlobalFunc$`15_Market_Shaping` <- replace(`15_Market_Shaping`, is.na(`15_Market_Shaping`), 0)

#Converting "NAs" in the vector to "0"
GlobalFunc$`21_Epidemic_Pandemic_Prep` <- replace(`21_Epidemic_Pandemic_Prep`, is.na(`21_Epidemic_Pandemic_Prep`), 0)
GlobalFunc$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)` <- replace(`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`, is.na(`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`), 0)
GlobalFunc$`22_AMR_Response` <- replace(`22_AMR_Response`, is.na(`22_AMR_Response`), 0)
GlobalFunc$`23_Response_Unhealthy_Products` <- replace(`23_Response_Unhealthy_Products`, is.na(`23_Response_Unhealthy_Products`), 0)
GlobalFunc$`24_Control_Cross_Border_Disease` <- replace(`24_Control_Cross_Border_Disease`, is.na(`24_Control_Cross_Border_Disease`), 0)

#Converting "NAs" in the vector to "0"
GlobalFunc$`31_Health_Advocacy` <- replace(`31_Health_Advocacy`, is.na(`31_Health_Advocacy`), 0)
GlobalFunc$`32_Promotion_Aid_Effectiveness` <- replace(`32_Promotion_Aid_Effectiveness`, is.na(`32_Promotion_Aid_Effectiveness`), 0)

#Converting "NAs" in the vector to "3"
GlobalFunc$`Uncertainty?` <- replace(`Uncertainty?`, is.na(`Uncertainty?`), 3)

dim(GlobalFunc)
unique(`Global Function?`)
unique(`Uncertainty?`)
unique(YEAR)

#creating a new df with no uncertainty and global function either equal to 0 or 1
GlobalFunc_processed <- GlobalFunc %>% 
  filter(`Uncertainty?`==3) %>% 
  filter(`Global Function?`==1| `Global Function?`==0) 

#getting column names
colnames(GlobalFunc_processed)

#counting NAs in Long description
table(is.na(GlobalFunc_processed$Long.Description))

#Removing the rows when COUNT==2
# GlobalFunc_processed <- GlobalFunc_processed %>% 
#   filter(Long.Description!=is.na(Long.Description)) 

GlobalFunc_processed <- GlobalFunc_processed %>% 
  filter(COUNT!=2) 

#checking whether the NAs have been removed from long.description or not
#View(GlobalFunc_processed[is.na(GlobalFunc_processed$Long.Description),])

#counting observation in different global functions
count(GlobalFunc_processed[GlobalFunc_processed$'11_RD_New_Health_Tools'==1,])#1687
count(GlobalFunc_processed[GlobalFunc_processed$'12_Dev_Harmonization_Int_Reg'==1,])#259
count(GlobalFunc_processed[GlobalFunc_processed$'13_Knowledge_Gen_Sharing'==1,]) #3680
count(GlobalFunc_processed[GlobalFunc_processed$'14_Sharing_IP'==1,]) #7
count(GlobalFunc_processed[GlobalFunc_processed$'15_Market_Shaping'==1,]) #1107

count(GlobalFunc_processed[GlobalFunc_processed$'21_Epidemic_Pandemic_Prep'==1,])#1417
count(GlobalFunc_processed[GlobalFunc_processed$'211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)'==1,]) #214
count(GlobalFunc_processed[GlobalFunc_processed$'22_AMR_Response'==1,])#210
count(GlobalFunc_processed[GlobalFunc_processed$'23_Response_Unhealthy_Products'==1,]) #302
count(GlobalFunc_processed[GlobalFunc_processed$'24_Control_Cross_Border_Disease'==1,])#1598

count(GlobalFunc_processed[GlobalFunc_processed$'31_Health_Advocacy'==1,])#1082
count(GlobalFunc_processed[GlobalFunc_processed$'32_Promotion_Aid_Effectiveness'==1,]) #268

count(GlobalFunc_processed[GlobalFunc_processed$'COUNT'==0,])#7831

#checking if we have negative values
sum(GlobalFunc_processed$Value<0)

#removing the negative values
GlobalFunc_processed <- GlobalFunc_processed %>% 
  dplyr::filter(Value>=0) 

#dropping '14_Sharing_IP' as there are only 7 observations
GlobalFunc_processed <- GlobalFunc_processed %>% 
  filter(`14_Sharing_IP`!=1)

#number of words in the long.description
len_des=vector()
for (i in seq_along(GlobalFunc_processed$Long.Description)){
  len_des[i]= lengths(gregexpr("\\W+", GlobalFunc_processed$Long.Description[i])) + 1 
}

#getting the table with different length words
a <- as.data.frame(table(len_des))
View(a)
a[a["len_des"]==255]
GlobalFunc_processed <-  cbind(GlobalFunc_processed, len_des)


#Language detection in the description column of the dataframe
#language detection based on long description
lang <-  detect_language(GlobalFunc_processed$Long.Description)
table(lang)
lang_perc <- as.data.frame((table(lang))) %>% 
  mutate(perc=Freq/sum(Freq)*100)
  
GlobalFunc_processed <- cbind(GlobalFunc_processed, lang)

#language detection in short description
lang_short <- detect_language(GlobalFunc_processed$Short.Description)
table(lang_short)
GlobalFunc_processed <- cbind(GlobalFunc_processed, lang_short)

#creating new vector and getting language from short description in case of NAs in long description
lang_new <- c()
for (i in seq_along(GlobalFunc_processed$YEAR)){
  if(is.na(GlobalFunc_processed$lang[i])){
    
    lang_new[i]=GlobalFunc_processed$lang_short[i]
    
  }else{
    lang_new[i]=GlobalFunc_processed$lang[i]
  }
}

#binding new vector to the df
GlobalFunc_processed <- cbind(GlobalFunc_processed, lang_new) 

#filtering only the english and NAs rows
GlobalFunc_processed <- GlobalFunc_processed %>% 
  filter(lang_new=="en" | is.na(lang_new)) 

 
#Dropping unnecessary columns and keeping only the relevant columns and placing it in new df
globalfunc_long <- GlobalFunc_processed %>% 
  select(YEAR, Donor.Name, Recipient.Name,Project.title, Purpose.Code,  Short.Description, Long.Description,Value,`Global Function?`, `11_RD_New_Health_Tools`,
         `12_Dev_Harmonization_Int_Reg`, `13_Knowledge_Gen_Sharing`, `15_Market_Shaping`, `21_Epidemic_Pandemic_Prep`,
         `211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`,`23_Response_Unhealthy_Products`,
         `24_Control_Cross_Border_Disease`, `31_Health_Advocacy`, `32_Promotion_Aid_Effectiveness`)  

View(globalfunc_long)

#replacing the 1s with a character string in all the categories

#checking the number of 1s and 0s in the column
table(globalfunc_long$`11_RD_New_Health_Tools`)
##replacing with RD_New_Health_Tools
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`11_RD_New_Health_Tools`[i]==1){
    globalfunc_long$`11_RD_New_Health_Tools`[i]="RD_New_Health_Tools"
  }else{
    globalfunc_long$`11_RD_New_Health_Tools`[i]=0
  }
}


#checking the number of 1s and 0s in the column
table(globalfunc_long$`12_Dev_Harmonization_Int_Reg`)
#replacing with Dev_Harmonization_Int_Reg
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`12_Dev_Harmonization_Int_Reg`[i]==1){
    globalfunc_long$`12_Dev_Harmonization_Int_Reg`[i]="Dev_Harmonization_Int_Reg"
  }else{
    globalfunc_long$`12_Dev_Harmonization_Int_Reg`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`13_Knowledge_Gen_Sharing`)
#replacing with Knowledge_Gen_Sharing
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`13_Knowledge_Gen_Sharing`[i]==1){
    globalfunc_long$`13_Knowledge_Gen_Sharing`[i]="Knowledge_Gen_Sharing"
  }else{
    globalfunc_long$`13_Knowledge_Gen_Sharing`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`15_Market_Shaping`)
#replacing with Market_Shaping
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`15_Market_Shaping`[i]==1){
    globalfunc_long$`15_Market_Shaping`[i]="Market_Shaping"
  }else{
    globalfunc_long$`15_Market_Shaping`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`21_Epidemic_Pandemic_Prep`)
#replacing with Epidemic_Pandemic_Prep
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]==1){
    globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]="Epidemic_Pandemic_Prep"
  }else{
    globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`)
#replacing with RD_Blueprint_Diseases_Pandemic_Flu
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`[i]==1){
    globalfunc_long$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`[i]="RD_Blueprint_Diseases_Pandemic_Flu"
  }else{
    globalfunc_long$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`23_Response_Unhealthy_Products`)
#replacing with Response_Unhealthy_Products
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`23_Response_Unhealthy_Products`[i]==1){
    globalfunc_long$`23_Response_Unhealthy_Products`[i]="Response_Unhealthy_Products"
  }else{
    globalfunc_long$`23_Response_Unhealthy_Products`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`24_Control_Cross_Border_Disease`)
#replacing with Control_Cross_Border_Disease
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`24_Control_Cross_Border_Disease`[i]==1){
    globalfunc_long$`24_Control_Cross_Border_Disease`[i]="Control_Cross_Border_Disease"
  }else{
    globalfunc_long$`24_Control_Cross_Border_Disease`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`31_Health_Advocacy`)
#replacing with Health_Advocacy
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`31_Health_Advocacy`[i]==1){
    globalfunc_long$`31_Health_Advocacy`[i]="Health_Advocacy"
  }else{
    globalfunc_long$`31_Health_Advocacy`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`32_Promotion_Aid_Effectiveness`)
#replacing with Promotion_Aid_Effectiveness
for (i in seq_along(globalfunc_long$`Global Function?`)){
  if (globalfunc_long$`32_Promotion_Aid_Effectiveness`[i]==1){
    globalfunc_long$`32_Promotion_Aid_Effectiveness`[i]="Promotion_Aid_Effectiveness"
  }else{
    globalfunc_long$`32_Promotion_Aid_Effectiveness`[i]=0
  }
}

#creating a vector with all the categories in a column
category <- vector()
for (i in seq_along(globalfunc_long$Donor.Name)){
  if(globalfunc_long$`11_RD_New_Health_Tools`[i]!=0){
    category[i] =globalfunc_long$`11_RD_New_Health_Tools`[i]
  } else if (globalfunc_long$`12_Dev_Harmonization_Int_Reg`[i]!=0 ){
    category[i] = globalfunc_long$`12_Dev_Harmonization_Int_Reg`[i]
  }else if (globalfunc_long$`13_Knowledge_Gen_Sharing`[i]!=0){
    category[i]=globalfunc_long$`13_Knowledge_Gen_Sharing`[i]
  }else if(globalfunc_long$`15_Market_Shaping`[i]!=0){
    category[i]=globalfunc_long$`15_Market_Shaping`[i]
  }else if(globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]!=0){
    category[i]= globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]
  }else if (globalfunc_long$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`[i]!=0){
    category[i]= globalfunc_long$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`[i]
  }else if (globalfunc_long$`23_Response_Unhealthy_Products`[i]!=0){
    category[i]=globalfunc_long$`23_Response_Unhealthy_Products`[i]
  }else if (globalfunc_long$`24_Control_Cross_Border_Disease`[i]!=0){
    category[i]=globalfunc_long$`24_Control_Cross_Border_Disease`[i]
  }else if (globalfunc_long$`31_Health_Advocacy`[i]!=0){
    category[i]= globalfunc_long$`31_Health_Advocacy`[i]
  }else if (globalfunc_long$`32_Promotion_Aid_Effectiveness`[i]!=0){
    category[i]=globalfunc_long$`32_Promotion_Aid_Effectiveness`[i]
  }else{
    category[i]="not_global_func"
  }
}

#combining the category vector with the globalfunc_long df
globalfunc_long <- cbind(globalfunc_long, category)

#checking whether the category column matches the Global.Function? column in the df
globalfunc_long %>% 
  select(`Global Function?`, category) %>% 
  View()

#subsetting the df and keeping only the relevant columns which might be used for the NLP analysis
globalfunc_long <- globalfunc_long %>% 
  select(YEAR, Donor.Name, Recipient.Name, Project.title,Purpose.Code, Short.Description, Long.Description, Value, category) 


#combining long and short description
# globalfunc_long$Long.Description <- paste(globalfunc_long$Short.Description,
#                                           globalfunc_long$Long.Description, sep = ". ")

#combining long and short description, purpose code and project title.
globalfunc_long$Long.Description <- paste(globalfunc_long$Purpose.Code, globalfunc_long$Project.title, globalfunc_long$Short.Description,
                                          globalfunc_long$Long.Description, sep = ". ")


#Writing the long dataframe to export it for NLP analysis========###
#write.csv(globalfunc_long, "globalfunc_long.csv", row.names = FALSE)

#Writing the long dataframe to export it for NLP analysis========###
 write.csv(globalfunc_long, "globalfunc_long_withpuposecode.csv", row.names = FALSE)

#============================
#####Filtering the df with different lengths of long description and plotting and analysing different variables
GlobalFunc_150 <- GlobalFunc_processed %>% 
  filter(len_des>50 & len_des<=150) 
  #count(`Global Function?`)#0=228, 1=2487 (total= 2715)
  #filter(`Global Function?`==1) %>% 
  #ggplot(aes(Donor.Name))+
  #geom_bar(stat = "count")+
  #geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  #facet_wrap(~`Global Function?`)+
  #coord_flip()

#writing a new csv with specific length of long description
write.csv(GlobalFunc_150 , "GlobalFunc_150 .csv", row.names = FALSE)
write.csv(GlobalFunc_processed , "GlobalFunc_processed .csv", row.names = FALSE)

GlobalFunc_processed %>% 
  filter(len_des>10 & len_des<=50) %>% 
  count(`Global Function?`)# 0=1416, 1= 5600 (Total= 7016)
  #filter(`Global Function?`==0) %>% 
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip()

GlobalFunc_processed %>% 
  filter(len_des>50 & len_des<=100) %>% 
  count(`Global Function?`)# 0=1424, 1= 688 (Total= 2112)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip()#  US=1193 projects=0, & 267 projects=1

GlobalFunc_processed %>% 
  filter(len_des>100 & len_des<=150) %>% 
  count(`Global Function?`) # 0= 1875, 1= 663 (Total=2538)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip() #US=1760 projects=0 & 296=0, Canada= 226=0, & 17=1

GlobalFunc_processed %>% 
  filter(len_des>150 & len_des<=200) %>% 
  count(`Global Function?`) # 0= 296, 1= 197 (Total= 493)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip()

GlobalFunc_processed %>% 
  filter(len_des>200 & len_des<=250) %>% 
  # filter(Donor.Name=="Canada") %>% 
  # select(Recipient.Name,Short.Description, Long.Description, `Global Function?`) %>% 
  # #filter(Recipient.Name=="Nepal") %>% 
  # View()
  count(`Global Function?`)# 0 = 106, 1= 247 (Total = 353)
  #ggplot(aes(Donor.Name))+
  #geom_bar(stat = "count")+
  #geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  #facet_wrap(~`Global Function?`)+
  #coord_flip() #look at canada & sweden

GlobalFunc_processed %>% 
  filter(len_des>250 & len_des<300) %>% 
  count(`Global Function?`)# 0 = 47, 1= 173 (Total= 220)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip() #look at canada

GlobalFunc_processed %>% 
  filter(len_des>300 & len_des<=350) %>% 
  count(`Global Function?`) # 0=48, 1= 133 (Total= 181)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip() #look at canada

GlobalFunc_processed %>% 
  filter(len_des>350 & len_des<=400) %>% 
  count(`Global Function?`)# 0= 37, 1= 117 (Total= 154)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip() #look at canada
  
GlobalFunc_processed %>% 
  filter(len_des>400 & len_des<=450) %>% 
  # filter(Donor.Name=="Canada") %>% 
  # View()
  count(`Global Function?`)# 0= 42, 1= 97 (Total= 139)
  #ggplot(aes(Donor.Name))+
  #geom_bar(stat = "count")+
  #geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  #facet_wrap(~`Global Function?`)+
  #coord_flip() #look at canada

GlobalFunc_processed %>% 
  filter(len_des>450 & len_des<500) %>% 
  count(`Global Function?`)# 0= 17, 1= 36 (Total= 53)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip()# look at canada

GlobalFunc_processed %>% 
  filter(len_des>500 & len_des<=550) %>% 
  count(`Global Function?`)#0=0, 1=0 (Total =0)
  # ggplot(aes(Donor.Name))+
  # geom_bar(stat = "count")+
  # geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  # facet_wrap(~`Global Function?`)+
  # coord_flip() # look at spain and belgium

GlobalFunc_processed %>% 
  filter(Donor.Name=="World Health Organisation") %>% 
  #count(`Global Function?`) %>% #1=1033, 0=0
  ggplot(aes(Donor.Name))+
  geom_bar(stat = "count")+
  facet_wrap(~`Global Function?`) #All WHO projects were global functions

#looking at how the data is divided based on purpose name
GlobalFunc_processed %>% 
  ggplot(aes(Purpose.Name))+
  geom_bar(stat = "count")+
  geom_text(aes(label=..count..), stat = "count", hjust=-.1, size=2.5) +
  facet_wrap(~`Global Function?`)+
  coord_flip()

GlobalFunc_processed %>% 
  filter(Purpose.Name=="Medical research") %>% 
  ggplot(aes(Purpose.Name))+
  geom_bar(stat = "count")+
  facet_wrap(~`11_RD_New_Health_Tools`)+
  geom_text(aes(label=..count..), stat = "count",  size=2.5) 


GlobalFunc_processed %>% 
  filter(YEAR=="2017") %>% 
  View()

#Trying ttest, but not sure that the data is normally distributted
# Glfn_basichlth <- GlobalFunc_processed %>% 
#   filter(Purpose.Name=="Basic health care") %>% 
#   filter(`Global Function?`==1) %>% 
#   select(Value) 
# 
# Glfn_basichlth_no <- GlobalFunc_processed %>% 
#   filter(Purpose.Name=="Basic health care") %>% 
#   filter(`Global Function?`==0) %>% 
#   select(Value) 
# 
# t.test(Glfn_basichlth, Glfn_basichlth_no)

#checking if the description has "polio" in it does it fall into global function category
#1= 475, 0= 498 ==> No conclusive answer
polio <- vector()
for (i in seq_along(GlobalFunc_processed$YEAR)){
  if (str_detect(GlobalFunc_processed$Long.Description[i], "polio")==TRUE){
    polio[i]=(GlobalFunc_processed$`Global Function?`)[i]
  }else{
    polio[i]="NA"
  }
}

#checking whether the column has been removed or not
count(GlobalFunc_processed[GlobalFunc_processed$'14_Sharing_IP'==1,]) #0

#combining recepient, donor, short description and long description
GlobalFunc_processed$sample <- paste(GlobalFunc_processed$Short.Description, 
                                     GlobalFunc_processed$Long.Description, sep=". ")
GlobalFunc_processed$sample <- paste(GlobalFunc_processed$Donor.Name, 
                                     GlobalFunc_processed$sample, sep=". ")
GlobalFunc_processed$sample <- paste(GlobalFunc_processed$Recipient.Name, 
                                     GlobalFunc_processed$sample, sep=". ")
#GlobalFunc_processed$sample <- paste(GlobalFunc_processed$Purpose.Name, 
#                                     GlobalFunc_processed$sample, sep=". ")

GlobalFunc_processed$sample[7]

#Combining 11, 12, 13, 14 and 15 sections(columns) into RD_tools vector
RD_tool <- vector()
for (i in seq_along(GlobalFunc_processed$YEAR)){
  RD_tool[i] <- (GlobalFunc_processed$`11_RD_New_Health_Tools`[i]+GlobalFunc_processed$`12_Dev_Harmonization_Int_Reg`[i]+
                   GlobalFunc_processed$`13_Knowledge_Gen_Sharing`[i]+GlobalFunc_processed$`15_Market_Shaping`[i])
}

#combining the vector with the dataframe
GlobalFunc_processed <-  cbind(GlobalFunc_processed, RD_tool)

#Checking whether the RD_tool function worked correctly--It worked correctly
GlobalFunc_processed %>% 
  select(`11_RD_New_Health_Tools`, `12_Dev_Harmonization_Int_Reg`,`13_Knowledge_Gen_Sharing`, `15_Market_Shaping`, RD_tool) %>% 
  View()

isTRUE(table(GlobalFunc_processed$RD_tool)[2]== (table(GlobalFunc_processed$`11_RD_New_Health_Tools`)[2]+
table(GlobalFunc_processed$`12_Dev_Harmonization_Int_Reg`)[2]+
table(GlobalFunc_processed$`13_Knowledge_Gen_Sharing`)[2]+
table(GlobalFunc_processed$`15_Market_Shaping`)[2]))
 
#Combining 21, 21.1, 22,23,24 sections(columns) into epp_prep_resp vector
epp_prep_resp <- vector()
for(i in seq_along(GlobalFunc_processed$YEAR)){
  epp_prep_resp[i] <- (GlobalFunc_processed$`21_Epidemic_Pandemic_Prep`[i]+GlobalFunc_processed$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`[i]+
                         GlobalFunc_processed$`22_AMR_Response`[i]+GlobalFunc_processed$`23_Response_Unhealthy_Products`[i]+
                         GlobalFunc_processed$`24_Control_Cross_Border_Disease`[i])
}

#combining the vector with the dataframe
GlobalFunc_processed <-  cbind(GlobalFunc_processed, epp_prep_resp)

#Checking whether the epp_prep_resp function worked correctly--It worked correctly
GlobalFunc_processed %>% 
    select(`21_Epidemic_Pandemic_Prep`, `211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`,`22_AMR_Response`,`23_Response_Unhealthy_Products`, `24_Control_Cross_Border_Disease`,`epp_prep_resp`) %>% 
  View()

isTRUE(table(GlobalFunc_processed$epp_prep_resp)[2]== (table(GlobalFunc_processed$`21_Epidemic_Pandemic_Prep`)[2]+
                                                   table(GlobalFunc_processed$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`)[2]+
                                                   table(GlobalFunc_processed$`22_AMR_Response`)[2]+
                                                   table(GlobalFunc_processed$`23_Response_Unhealthy_Products`)[2]+
                                                   table(GlobalFunc_processed$`24_Control_Cross_Border_Disease`)[2]))

#Combining 31 and 32 sections (columns) into health_adv_eff vector
health_adv_eff <- vector()
for (i in seq_along(GlobalFunc_processed$YEAR)){
  health_adv_eff[i] <- (GlobalFunc_processed$`31_Health_Advocacy`[i]+GlobalFunc_processed$`32_Promotion_Aid_Effectiveness`[i])
}

#combining the vector with the dataframe
GlobalFunc_processed <-  cbind(GlobalFunc_processed, health_adv_eff)


#Checking whether the health_adv_eff function worked correctly--It worked correctly
GlobalFunc_processed %>% 
  select(`31_Health_Advocacy`,`32_Promotion_Aid_Effectiveness`, health_adv_eff) %>% 
  View()

isTRUE(table(GlobalFunc_processed$health_adv_eff)[2]==
         table(GlobalFunc_processed$`31_Health_Advocacy`)[2]+ table(GlobalFunc_processed$`32_Promotion_Aid_Effectiveness`)[2])

#Creating a count1 vector to count if every row/description has been given a unique category of Global function. If not
#then that categorization is wrong and we will delete it from our analysis
count1 <- vector()
for (i in seq_along(GlobalFunc_processed$YEAR)){
  count1[i] <- GlobalFunc_processed$RD_tool[i]+GlobalFunc_processed$epp_prep_resp[i]+GlobalFunc_processed$health_adv_eff[i]
}

#Combining the count1 vector to our Dataframe and removing the NAs from the data
GlobalFunc_processed <- cbind(GlobalFunc_processed, count1)

#No count1>1 found. So, things done correctly
GlobalFunc_processed %>% 
  filter(count1>1) %>% 
  View()

#Converting the numeric observations in the Global function categories
#into strings for better human interpretation and future machine learning process
#GlobalFunc_processed <- GlobalFunc_processed %>% 
#  mutate(RD_tool1=ifelse(RD_tool==1, "RD_Tool", "NotGlobalFunc")) %>% 
#  mutate(epp_prep_resp1=ifelse(epp_prep_resp==1, "epp_prep_resp", "NotGlobalFunc")) %>%  
#  mutate(health_adv_eff1=ifelse(health_adv_eff==1, "health_adv_eff", "NotGlobalFunc")) 

#Creating a label vector which has all the three labels of Global Functions, and also the non-global function label
label <- vector()
for (i in seq_along(GlobalFunc_processed$YEAR)){
  if (GlobalFunc_processed$RD_tool[i]==1){
    label1="RD_tool"
  }else if(GlobalFunc_processed$epp_prep_resp[i]==1){
    label1="epp_prep_resp"
  }else if (GlobalFunc_processed$health_adv_eff[i]==1){
    label1="health_adv_eff"
  }else{
    label1="NotGlobalFunc"
  }
  label[i]=label1
}

#combining the label vector with the dataframe
GlobalFunc_processed <- data.frame(GlobalFunc_processed, label)

#renaming sample column
GlobalFunc_processed <- GlobalFunc_processed %>% 
  rename(description=sample)

#Language detection in the description column of the dataframe
lang <-  detect_language(GlobalFunc_processed$description)
table(lang)

GlobalFunc_processed <- cbind(GlobalFunc_processed, lang)

GlobalFunc_en <- GlobalFunc_processed %>% 
  filter(lang=="en")

dim(GlobalFunc_en)
View(GlobalFunc_en)

#removing all the columns except "description", "label"
GlobalFunc_en <- GlobalFunc_en %>% 
  select(label, description, lang) 

# Saving english description data file 
write.table(GlobalFunc_en, file='GlobalFunc_en.tsv', quote=FALSE, sep='\t', row.names = F)
write.csv(GlobalFunc_en, "GlobalFunc_en.csv", row.names = FALSE)

#============================================================================================================#
#============================================================================================================#
#============================================================================================================#
#============================================================================================================#
#======================R&D New Health Tools ================================#

#Converting "NAs" in the vector to "0"
GlobalFunc$`11_RD_New_Health_Tools` <- replace(`11_RD_New_Health_Tools`, is.na(`11_RD_New_Health_Tools`), 0)
GlobalFunc$`12_Dev_Harmonization_Int_Reg` <- replace(`12_Dev_Harmonization_Int_Reg`, is.na(`12_Dev_Harmonization_Int_Reg`), 0)
GlobalFunc$`13_Knowledge_Gen_Sharing` <- replace(`13_Knowledge_Gen_Sharing`, is.na(`13_Knowledge_Gen_Sharing`), 0)
GlobalFunc$`14_Sharing_IP` <- replace(`14_Sharing_IP`, is.na(`14_Sharing_IP`), 0)
GlobalFunc$`15_Market_Shaping` <- replace(`15_Market_Shaping`, is.na(`15_Market_Shaping`), 0)

#Combining 11, 12, 13, 14 and 15 sections(columns) into RD_tools vector
RD_tool <- vector()
for (i in seq_along(YEAR)){
  RD_tool[i] <- (GlobalFunc$`11_RD_New_Health_Tools`[i]+GlobalFunc$`12_Dev_Harmonization_Int_Reg`[i]+
                   GlobalFunc$`13_Knowledge_Gen_Sharing`[i]+GlobalFunc$`14_Sharing_IP`[i]+GlobalFunc$`15_Market_Shaping`[i])
}

#======================Epidemic and Pandemic Preparedness and Response ================================#

#Converting "NAs" in the vector to "0"
GlobalFunc$`21_Epidemic_Pandemic_Prep` <- replace(`21_Epidemic_Pandemic_Prep`, is.na(`21_Epidemic_Pandemic_Prep`), 0)
GlobalFunc$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)` <- replace(`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`, is.na(`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`), 0)
GlobalFunc$`22_AMR_Response` <- replace(`22_AMR_Response`, is.na(`22_AMR_Response`), 0)
GlobalFunc$`23_Response_Unhealthy_Products` <- replace(`23_Response_Unhealthy_Products`, is.na(`23_Response_Unhealthy_Products`), 0)
GlobalFunc$`24_Control_Cross_Border_Disease` <- replace(`24_Control_Cross_Border_Disease`, is.na(`24_Control_Cross_Border_Disease`), 0)

#Combining 21, 21.1, 22,23,24 sections(columns) into epp_prep_resp vector
epp_prep_resp <- vector()
for(i in seq_along(YEAR)){
  epp_prep_resp[i] <- (GlobalFunc$`21_Epidemic_Pandemic_Prep`[i]+GlobalFunc$`211_RD_Blueprint_Diseases_Pandemic_Flu (Exclude Narrow Surveillance)`[i]+
                     GlobalFunc$`22_AMR_Response`[i]+GlobalFunc$`23_Response_Unhealthy_Products`[i]+
                     GlobalFunc$`24_Control_Cross_Border_Disease`[i])
}

#======================Health_Advocacy and Promotion of Aid Effectiveness ================================#

#Converting "NAs" in the vector to "0"
GlobalFunc$`31_Health_Advocacy` <- replace(`31_Health_Advocacy`, is.na(`31_Health_Advocacy`), 0)
GlobalFunc$`32_Promotion_Aid_Effectiveness` <- replace(`32_Promotion_Aid_Effectiveness`, is.na(`32_Promotion_Aid_Effectiveness`), 0)

#Combining 31 and 32 sections (columns) into health_adv_eff vector
health_adv_eff <- vector()
for (i in seq_along(YEAR)){
  health_adv_eff[i] <- (GlobalFunc$`31_Health_Advocacy`[i]+GlobalFunc$`32_Promotion_Aid_Effectiveness`[i])
}

#Creating a Dataframe by by combining the three new vectors to the GlobalFunc dataframe
GlobalFunc <- cbind(GlobalFunc, RD_tool, epp_prep_resp, health_adv_eff)

#Removing all the columns except Long.Description, RD_tool, epp_prep_resp,and health_adv_eff
GlobalFunc <- GlobalFunc[,-1:-9]
GlobalFunc <- GlobalFunc[,-2:-18]

#Creating a count1 vector to count if every row/description has been given a unique category of Global function. If not
#then that categorization is wrong and we will delete it from our analysis
count1 <- vector()
for (i in seq_along(Long.Description)){
  count1[i] <- GlobalFunc$RD_tool[i]+GlobalFunc$epp_prep_resp[i]+GlobalFunc$health_adv_eff[i]
}

#Combining the count1 vector to our Dataframe and removing the NAs from the data
GlobalFunc <- cbind(GlobalFunc, count1)

sum(is.na((GlobalFunc$Long.Description)))
GlobalFunc <- na.omit(GlobalFunc)

#Removing all the values rows which have been categorized into more than one global function
GlobalFunc <- GlobalFunc %>% filter(count1!=2)
GlobalFunc <- GlobalFunc[,-5]

#Converting the numeric observations in the Global function categories
#into strings for better human interpretation and future machine learning process
GlobalFunc <- GlobalFunc %>% mutate(RD_tool1=ifelse(RD_tool==1, "RD_Tool", "NotGlobalFunc")) 
GlobalFunc <- GlobalFunc %>% mutate(epp_prep_resp1=ifelse(epp_prep_resp==1, "epp_prep_resp", "NotGlobalFunc")) 
GlobalFunc <- GlobalFunc %>% mutate(health_adv_eff1=ifelse(health_adv_eff==1, "health_adv_eff", "NotGlobalFunc")) 

#Creating a label vector which has all the three labels of Global Functions
label <- vector()
for (i in seq_along(GlobalFunc$Long.Description)){
  if (GlobalFunc$RD_tool[i]==1){
    label1="RD_tool"
  }else if(GlobalFunc$epp_prep_resp[i]==1){
    label1="epp_prep_resp"
  }else if (GlobalFunc$health_adv_eff[i]==1){
    label1="health_adv_eff"
  }else{
    label1="NotGlobalFunc"
  }
  label[i]=label1
}

#Combining label vector to our Dataframe, removing the unnecessary columns and renaming Long.Descrition column to description
GlobalFunc <- data.frame(GlobalFunc, label)
GlobalFunc <- GlobalFunc[,-2:-7]
GlobalFunc <- GlobalFunc %>% rename(description=Long.Description)
dim(GlobalFunc)

GlobalFunc <- GlobalFunc[,c(2,1)]
View(GlobalFunc)

#Writing/Outputing out cleaned data into text and csv format
write.table(GlobalFunc, file='GlobalFunc.tsv', quote=FALSE, sep='\t', row.names = F)
write.csv(GlobalFunc, "GlobalFunc.csv", row.names = FALSE)

#Just a Check: to see if we can read our .tsv and .csv back in r: Reading the.tsv and.csv formats in r
x <- read.table("C:\\Users\\sd282\\Box\\R\\NLP with NLTK\\Global_Goods\\GlobalFunc.tsv", sep="\t", header = T)
y <- read.csv("GlobalFunc.csv")

#===========Language detection in the description column of the dataframe=============#
library(cld2) #language detection package from google
lang <-  detect_language(GlobalFunc$description)
table(lang)

GlobalFunc <- data.frame(GlobalFunc, lang)

GlobalFunc_en <- GlobalFunc %>% filter(lang=="en")
dim(GlobalFunc_en)
View(GlobalFunc_en)

GlobalFunc_es <- GlobalFunc %>% filter(lang=="es")
dim(GlobalFunc_es)

GlobalFunc_fr <- GlobalFunc %>% filter(lang=="fr")
dim(GlobalFunc_fr)

GlobalFunc_nl <- GlobalFunc %>% filter(lang=="nl")
dim(GlobalFunc_nl)

GlobalFunc_na <- GlobalFunc %>% filter(is.na(lang))
View(GlobalFunc_na)
table(GlobalFunc_na$label)

# Saving english description data file 
write.table(GlobalFunc_en, file='GlobalFunc_en.tsv', quote=FALSE, sep='\t', row.names = F)
write.csv(GlobalFunc_en, "GlobalFunc_en.csv", row.names = FALSE)

#Saving french description data file
write.table(GlobalFunc_fr, file='GlobalFunc_fr.tsv', quote=FALSE, sep='\t', row.names = F)
write.csv(GlobalFunc_fr, "GlobalFunc_fr.csv", row.names = FALSE)

# Saving spanish description data file 
write.table(GlobalFunc_es, file='GlobalFunc_es.tsv', quote=FALSE, sep='\t', row.names = F)
write.csv(GlobalFunc_es, "GlobalFunc_es.csv", row.names = FALSE)

# Saving dutch description data file
write.table(GlobalFunc_nl, file='GlobalFunc_nl.tsv', quote=FALSE, sep='\t', row.names = F)
write.csv(GlobalFunc_nl, "GlobalFunc_nl.csv", row.names = FALSE)





