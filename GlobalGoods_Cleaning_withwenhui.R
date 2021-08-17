
##=============Data wrangling and cleaning to prepare the data for input in Machine Learning algorithms================##
#Created by: Sid

#Loading libraries
library(readxl)
library(tidyverse)

#reading the raw CSV file
GlobalFunc <- read_excel("C:\\Users\\sd282\\Box\\R\\NLP with NLTK\\Global Goods_Calssification\\Global_Function_healthcare_new analysis\\with wenhui\\globalGoods_wenhui\\Coding Master 20210422_foranalysis.xlsx")
View(GlobalFunc)
dim(GlobalFunc)

names(GlobalFunc)
summary(GlobalFunc)
sapply(GlobalFunc, class)

#Use the following command to hide the warnings 
#options(warn=-1)

#attached the dataframe
#attach(GlobalFunc)

#Converting "NAs" in the vector to "0"
GlobalFunc$`1 R&D (exclude emerging diseases)` <- replace(`1 R&D (exclude emerging diseases)`, is.na(`1 R&D (exclude emerging diseases)`),0)
GlobalFunc$`11_RD_New_Health_Tools` <- replace(`11_RD_New_Health_Tools`, is.na(`11_RD_New_Health_Tools`), 0)
GlobalFunc$`12_Dev_Harmonization_Int_Reg` <- replace(`12_Dev_Harmonization_Int_Reg`, is.na(`12_Dev_Harmonization_Int_Reg`), 0)
GlobalFunc$`13_Knowledge_Gen_Sharing` <- replace(`13_Knowledge_Gen_Sharing`, is.na(`13_Knowledge_Gen_Sharing`), 0)
GlobalFunc$`14_Sharing_IP` <- replace(`14_Sharing_IP`, is.na(`14_Sharing_IP`), 0)
GlobalFunc$`15_Market_Shaping` <- replace(`15_Market_Shaping`, is.na(`15_Market_Shaping`), 0)

#Converting "NAs" in the vector to "0"
GlobalFunc$`21_Epidemic_Pandemic_Prep` <- replace(`21_Epidemic_Pandemic_Prep`, is.na(`21_Epidemic_Pandemic_Prep`), 0)
GlobalFunc$`22_AMR_Response` <- replace(`22_AMR_Response`, is.na(`22_AMR_Response`), 0)
GlobalFunc$`23_Response_Unhealthy_Products` <- replace(`23_Response_Unhealthy_Products`, is.na(`23_Response_Unhealthy_Products`), 0)
GlobalFunc$`24_Control_Cross_Border_Disease` <- replace(`24_Control_Cross_Border_Disease`, is.na(`24_Control_Cross_Border_Disease`), 0)

#Converting "NAs" in the vector to "0"
GlobalFunc$`31_Health_Advocacy` <- replace(`31_Health_Advocacy`, is.na(`31_Health_Advocacy`), 0)
GlobalFunc$`32_Promotion_Aid_Effectiveness` <- replace(`32_Promotion_Aid_Effectiveness`, is.na(`32_Promotion_Aid_Effectiveness`), 0)

#filtering out the description with laguages other thn english
GlobalFunc <- GlobalFunc %>% 
  filter(is.na(`Primary purpose for GF (1=yes)`) |`Primary purpose for GF (1=yes)`==0 | `Primary purpose for GF (1=yes)`==1 ) 

#dropping unnecessary columnss
GlobalFunc_processed <- GlobalFunc %>% 
  select(-c("YEAR", "Donor.Name", "Multi/bilateral", "Recipient.Name", "Recipient (Single country)", 
            "Income.Group", "Channel.of.Delivery.Name", "Purpose.Code", "Purpose.Name", "Value", "Project.title",
            "Primary purpose for GF (1=yes)",  "...15", "Uncertainty?", "Why Uncertain?", "...30", "Global Function?",
            "reasonining for classification")) 

#getting column names
colnames(GlobalFunc_processed)

#counting observation in different global functions
count(GlobalFunc_processed[GlobalFunc_processed$`1 R&D (exclude emerging diseases)`==1,])#6
count(GlobalFunc_processed[GlobalFunc_processed$'11_RD_New_Health_Tools'==1,])#9
count(GlobalFunc_processed[GlobalFunc_processed$'12_Dev_Harmonization_Int_Reg'==1,])#3
count(GlobalFunc_processed[GlobalFunc_processed$'13_Knowledge_Gen_Sharing'==1,]) #206
count(GlobalFunc_processed[GlobalFunc_processed$'14_Sharing_IP'==1,]) #0
count(GlobalFunc_processed[GlobalFunc_processed$'15_Market_Shaping'==1,]) #8

count(GlobalFunc_processed[GlobalFunc_processed$'21_Epidemic_Pandemic_Prep'==1,])#97
count(GlobalFunc_processed[GlobalFunc_processed$'22_AMR_Response'==1,])#29
count(GlobalFunc_processed[GlobalFunc_processed$'23_Response_Unhealthy_Products'==1,]) #12
count(GlobalFunc_processed[GlobalFunc_processed$'24_Control_Cross_Border_Disease'==1,])#126

count(GlobalFunc_processed[GlobalFunc_processed$'31_Health_Advocacy'==1,])#142
count(GlobalFunc_processed[GlobalFunc_processed$'32_Promotion_Aid_Effectiveness'==1,]) #0


#dropping variables which have very low number of "1" observation
GlobalFunc_processed <- GlobalFunc_processed %>% 
  select(-c(`1 R&D (exclude emerging diseases)`, '11_RD_New_Health_Tools', '12_Dev_Harmonization_Int_Reg',
            '14_Sharing_IP','15_Market_Shaping','32_Promotion_Aid_Effectiveness')) 


#number of words in the long.description
len_des=vector()
for (i in seq_along(GlobalFunc_processed$Long.Description)){
  len_des[i]= lengths(gregexpr("\\W+", GlobalFunc_processed$Long.Description[i])) + 1 
}

#getting the table with different length words
a <- as.data.frame(table(len_des))
View(a)
GlobalFunc_processed <-  cbind(GlobalFunc_processed, len_des)

#renaming the df to match the name from previous code
globalfunc_long <- GlobalFunc_processed 

#replacing the 1s with a string in all the categories
#checking the number of 1s and 0s in the column
table(globalfunc_long$`13_Knowledge_Gen_Sharing`)
#replacing with Knowledge_Gen_Sharing
for (i in seq_along(globalfunc_long$Short.Description)){
  if (globalfunc_long$`13_Knowledge_Gen_Sharing`[i]==1){
    globalfunc_long$`13_Knowledge_Gen_Sharing`[i]="Knowledge_Gen_Sharing"
  }else{
    globalfunc_long$`13_Knowledge_Gen_Sharing`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`21_Epidemic_Pandemic_Prep`)
#replacing with Epidemic_Pandemic_Prep
for (i in seq_along(globalfunc_long$Short.Description)){
  if (globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]==1){
    globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]="Epidemic_Pandemic_Prep"
  }else{
    globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`22_AMR_Response`)
#replacing with Response_Unhealthy_Products
for (i in seq_along(globalfunc_long$Short.Description)){
  if (globalfunc_long$`22_AMR_Response`[i]==1){
    globalfunc_long$`22_AMR_Response`[i]="AMR_Response"
  }else{
    globalfunc_long$`22_AMR_Response`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`23_Response_Unhealthy_Products`)
#replacing with Response_Unhealthy_Products
for (i in seq_along(globalfunc_long$Short.Description)){
  if (globalfunc_long$`23_Response_Unhealthy_Products`[i]==1){
    globalfunc_long$`23_Response_Unhealthy_Products`[i]="Response_Unhealthy_Products"
  }else{
    globalfunc_long$`23_Response_Unhealthy_Products`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`24_Control_Cross_Border_Disease`)
#replacing with Control_Cross_Border_Disease
for (i in seq_along(globalfunc_long$Short.Description)){
  if (globalfunc_long$`24_Control_Cross_Border_Disease`[i]==1){
    globalfunc_long$`24_Control_Cross_Border_Disease`[i]="Control_Cross_Border_Disease"
  }else{
    globalfunc_long$`24_Control_Cross_Border_Disease`[i]=0
  }
}

#checking the number of 1s and 0s in the column
table(globalfunc_long$`31_Health_Advocacy`)
#replacing with Health_Advocacy
for (i in seq_along(globalfunc_long$Short.Description)){
  if (globalfunc_long$`31_Health_Advocacy`[i]==1){
    globalfunc_long$`31_Health_Advocacy`[i]="Health_Advocacy"
  }else{
    globalfunc_long$`31_Health_Advocacy`[i]=0
  }
}


#creating a category vector
category <- vector()
for (i in seq_along(globalfunc_long$Short.Description)){
  if (globalfunc_long$`13_Knowledge_Gen_Sharing`[i]!=0){
    category[i]=globalfunc_long$`13_Knowledge_Gen_Sharing`[i]
  }else if(globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]!=0){
    category[i]= globalfunc_long$`21_Epidemic_Pandemic_Prep`[i]
  }else if (globalfunc_long$`22_AMR_Response`[i]!=0){
    category[i]=globalfunc_long$`22_AMR_Response`[i]
  } else if (globalfunc_long$`23_Response_Unhealthy_Products`[i]!=0){
    category[i]=globalfunc_long$`23_Response_Unhealthy_Products`[i]
  }else if (globalfunc_long$`24_Control_Cross_Border_Disease`[i]!=0){
    category[i]=globalfunc_long$`24_Control_Cross_Border_Disease`[i]
  }else if (globalfunc_long$`31_Health_Advocacy`[i]!=0){
    category[i]= globalfunc_long$`31_Health_Advocacy`[i]
  }else{
    category[i]="not_global_func"
  }
}

#combining the category vector with the globalfunc_long df
globalfunc_long <- cbind(globalfunc_long, category)


#combining lon and short description
globalfunc_long$description <- paste(globalfunc_long$Short.Description,
                                     globalfunc_long$Long.Description, sep = ". ")


#keeping only the description and category column that will be used for nlp
globalfunc_long <- globalfunc_long %>% 
  select(description, category) 


#Writing the long dataframe to export it for NLP analysis========###
write.csv(globalfunc_long, "globalfunc_16aug2021.csv", row.names = FALSE)

