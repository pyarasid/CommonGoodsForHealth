library(magrittr)
library(dplyr)
library(cld2) #language detection package from google
library(tidyr)

#reading the text file
crs_2019 <- read.delim("CRS 2019 data.txt", header = TRUE, sep ="|") 

#checking whether these codes are present in the purpose column
c(12110, 12181, 12182, 12191, 12220, 12230, 12240, 12250, 12261, 12262, 12263, 12264, 12281, 13010,
  13020, 13030, 13040, 13081, 16064,72010, 72040, 72050, 73010, 74010) %in% crs_2019$PurposeCode

#subsetting the dataframe to the purpose code of interest
crs_2019_health <- crs_2019 %>% 
  dplyr::filter(PurposeCode==c(12110, 12181, 12182, 12191, 12220, 12230, 12240, 12250, 12261, 12262, 12263, 12264, 12281, 13010,
                               13020, 13030, 13040, 13081, 16064,72010, 72040, 72050, 73010, 74010)) 


#subsetting the dataframe to get the columns we are most interested in
crs_2019_health <- crs_2019_health %>%
select(c("Year", "DonorName", "RecipientName", "ProjectTitle", "PurposeCode",
         "ShortDescription", "LongDescription", "USD_Disbursement_Defl"))

#removing the negative disbursement values
crs_2019_health<- crs_2019_health %>% 
  dplyr::filter(USD_Disbursement_Defl>=0) 

#Language detection in the description column of the dataframe
#language detection based on long description
lang <-  detect_language(crs_2019_health$LongDescription)
table(lang)

crs_2019_health_processed <- cbind(crs_2019_health, lang)

#language detection in short description
lang_short <- detect_language(crs_2019_health_processed$ShortDescription)
table(lang_short)
crs_2019_health_processed <- cbind(crs_2019_health_processed, lang_short)

#fill in "unknown" for "NA"
crs_2019_health_processed <- crs_2019_health_processed %>% replace_na(list(lang="unknown"))%>%
  replace_na(list(lang_short="unknown"))

#creating new vector and getting language if any of short of long description is in english
lang_new <- c()
for (i in seq_along(crs_2019_health_processed$Year)){
  if(crs_2019_health_processed$lang[i]=="en" | crs_2019_health_processed$lang_short[i]=="en"){
    
    lang_new[i]="en"
    
  }else{
    lang_new[i]=crs_2019_health_processed$lang[i]
  }
}

#binding new vector to the df
crs_2019_health_processed <- cbind(crs_2019_health_processed, lang_new) 

#filtering only the english and NAs rows
crs_2019_health_processed <-  crs_2019_health_processed %>% 
  filter(lang_new=="en" | is.na(lang_new)) 

#dropping the language columns
crs_2019_health_processed <- crs_2019_health_processed %>%
  select(-c("lang", "lang_short", "lang_new"))

#combining long and short description, purpose code and project title.
crs_2019_health_processed$LongDescription <- paste(crs_2019_health_processed$PurposeCode, crs_2019_health_processed$ProjectTitle, 
                                                   crs_2019_health_processed$ShortDescription,
                                         crs_2019_health_processed$LongDescription, sep = ". ")


#write.csv(crs_2019_health_processed, "crs_2019_health.csv")


