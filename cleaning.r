library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

setwd("~/Desktop/dataScience")

hp2019 = read_csv("housePricing/pp-2019.csv", show_col_types = FALSE)
hp2020 = read_csv("housePricing/pp-2020.csv", show_col_types = FALSE)
hp2021 = read_csv("housePricing/pp-2021.csv", show_col_types = FALSE)
hp2022 = read_csv("housePricing/pp-2022.csv", show_col_types = FALSE)


colnames(hp2019) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "Country", "Type1", "Type2" )
colnames(hp2020) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "Country", "Type1", "Type2")
colnames(hp2021) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "Country" , "Type1", "Type2")
colnames(hp2022) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "Country" , "Type1", "Type2")

HousePrices = hp2022 %>%
  add_row(hp2021)%>%
  add_row(hp2020)%>%
  add_row(hp2019)

write.csv(HousePrices, "housePricing/UncleanedHousePrices.csv")

# Filtering Lancashire and Leicestershire data

FilteredHousePrices = filter(HousePrices, Country == 'LANCASHIRE' | Country == 'LEICESTERSHIRE')

FilteredHousePrices = FilteredHousePrices %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  select(PostCode,shortPostcode,Year,PAON,Price) %>% 
  na.omit()

# exporting filteredhouseprices data set to  csv
write.csv(FilteredHousePrices, "CleanedData/HousePrices.csv")

#________________________----------------------------_________________________________

uncleanedhouseprices = read_csv('housePricing/UncleanedHousePrices.csv')

Population = read_csv("population/population.csv", show_col_types = FALSE)

# Filtering Lancashire and Leicestershire data
FilteredTown = filter(uncleanedhouseprices, Country == 'LANCASHIRE' | Country == 'LEICESTERSHIRE')

Population = Population %>%  
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>%
  group_by(shortPostcode) %>%
  summarise_at(vars(Population),list(Population2011 = sum)) %>%
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>%
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>%
  mutate(Population2021= (1.00561255390388033 * Population2020)) %>%
  mutate(Population2022= (1.00561255390388033 * Population2021)) %>%
  select(shortPostcode,Population2019,Population2020,Population2021,Population2022)

FilteredTown = FilteredTown %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  left_join(Population,by="shortPostcode") %>% 
  select(PostCode, shortPostcode, Year, Town, District, Country, Population2019,Population2020,Population2021,Population2022) %>% 
  group_by(shortPostcode) %>%
  filter(row_number()==1) %>%
  arrange(Country) %>% 
  na.omit()

write.csv(FilteredTown, "CleanedData/Towns.csv")


#--------------------------------------------------------------------------------------------------------------------------------------



# BROADBAND DATA CLEANING


Broadband = read_csv("broadBand/broadband1.csv", show_col_types = FALSE)



BroadbandData = Broadband %>%
  mutate(shortPostcode = str_trim(str_sub(postcode_space, 1,4))) %>% 
  mutate( ID = row_number()) %>% 
  select(ID, `postcode area`, shortPostcode, `Average download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Maximum download speed (Mbit/s)`,
         `Maximum upload speed (Mbit/s)`) %>% 
  na.omit()

colnames(BroadbandData)=c("ID", "postcode area","shortPostcode", "AverageDownload","AverageUpload", "MaximumDownload","MaximumUpload")

write.csv(BroadbandData, "CleanedData/Broadband.csv")


#___________________School Data Cleaning________________________________________


EnglandSchool19 = read_csv('school/school2019.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2019)
EnglandSchool21 = read_csv('school/school2021.csv', show_col_types = FALSE) %>%  
  mutate(Year = 2021)

EnglandSchool19 = select(EnglandSchool19, Year, PCODE, SCHNAME, ATT8SCR)
EnglandSchool21 = select(EnglandSchool21, Year, PCODE, SCHNAME, ATT8SCR)


schoolData = EnglandSchool19 %>% 
  add_row(EnglandSchool21) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(schoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")

write.csv(schoolData, "CleanedData/School.csv") 


#___________________Crime Data Cleaning________________________________________

# Merging the crime data
#Lancashire data

cd201911L = read_csv('crimeData/2019-11/2019-11-lancashire-street.csv', show_col_types = FALSE)
cd201912L = read_csv('crimeData/2019-12/2019-12-lancashire-street.csv', show_col_types = FALSE)

cd202001L = read_csv('crimeData/2020-01/2020-01-lancashire-street.csv', show_col_types = FALSE)
cd202002L = read_csv('crimeData/2020-02/2020-02-lancashire-street.csv', show_col_types = FALSE)
cd202003L = read_csv('crimeData/2020-03/2020-03-lancashire-street.csv', show_col_types = FALSE)
cd202004L = read_csv('crimeData/2020-04/2020-04-lancashire-street.csv', show_col_types = FALSE)
cd202005L = read_csv('crimeData/2020-05/2020-05-lancashire-street.csv', show_col_types = FALSE)
cd202006L = read_csv('crimeData/2020-06/2020-06-lancashire-street.csv', show_col_types = FALSE)
cd202007L = read_csv('crimeData/2020-07/2020-07-lancashire-street.csv', show_col_types = FALSE)
cd202008L = read_csv('crimeData/2020-08/2020-08-lancashire-street.csv', show_col_types = FALSE)
cd202009L = read_csv('crimeData/2020-09/2020-09-lancashire-street.csv', show_col_types = FALSE)
cd202010L = read_csv('crimeData/2020-10/2020-10-lancashire-street.csv', show_col_types = FALSE)
cd202011L = read_csv('crimeData/2020-11/2020-11-lancashire-street.csv', show_col_types = FALSE)
cd202012L = read_csv('crimeData/2020-12/2020-12-lancashire-street.csv', show_col_types = FALSE)



cd202101L = read_csv('crimeData/2021-01/2021-01-lancashire-street.csv', show_col_types = FALSE)
cd202102L = read_csv('crimeData/2021-02/2021-02-lancashire-street.csv', show_col_types = FALSE)
cd202103L = read_csv('crimeData/2021-03/2021-03-lancashire-street.csv', show_col_types = FALSE)
cd202104L = read_csv('crimeData/2021-04/2021-04-lancashire-street.csv', show_col_types = FALSE)
cd202105L = read_csv('crimeData/2021-05/2021-05-lancashire-street.csv', show_col_types = FALSE)
cd202106L = read_csv('crimeData/2021-06/2021-06-lancashire-street.csv', show_col_types = FALSE)
cd202107L = read_csv('crimeData/2021-07/2021-07-lancashire-street.csv', show_col_types = FALSE)
cd202108L = read_csv('crimeData/2021-08/2021-08-lancashire-street.csv', show_col_types = FALSE)
cd202109L = read_csv('crimeData/2021-09/2021-09-lancashire-street.csv', show_col_types = FALSE)
cd202110L = read_csv('crimeData/2021-10/2021-10-lancashire-street.csv', show_col_types = FALSE)
cd202111L = read_csv('crimeData/2021-11/2021-11-lancashire-street.csv', show_col_types = FALSE)
cd202112L = read_csv('crimeData/2021-12/2021-12-lancashire-street.csv', show_col_types = FALSE)



cd202201L = read_csv('crimeData/2022-01/2022-01-lancashire-street.csv', show_col_types = FALSE)
cd202202L = read_csv('crimeData/2022-02/2022-02-lancashire-street.csv', show_col_types = FALSE)
cd202203L = read_csv('crimeData/2022-03/2022-03-lancashire-street.csv', show_col_types = FALSE)
cd202204L = read_csv('crimeData/2022-04/2022-04-lancashire-street.csv', show_col_types = FALSE)
cd202205L = read_csv('crimeData/2022-05/2022-05-lancashire-street.csv', show_col_types = FALSE)
cd202206L = read_csv('crimeData/2022-06/2022-06-lancashire-street.csv', show_col_types = FALSE)
cd202207L = read_csv('crimeData/2022-07/2022-07-lancashire-street.csv', show_col_types = FALSE)
cd202208L = read_csv('crimeData/2022-08/2022-08-lancashire-street.csv', show_col_types = FALSE)
cd202209L = read_csv('crimeData/2022-09/2022-09-lancashire-street.csv', show_col_types = FALSE)
cd202210L = read_csv('crimeData/2022-10/2022-10-lancashire-street.csv', show_col_types = FALSE)


#Leicestershire data

cd201911LC = read_csv('crimeData/2019-11/2019-11-leicestershire-street.csv', show_col_types = FALSE)
cd201912LC = read_csv('crimeData/2019-12/2019-12-leicestershire-street.csv', show_col_types = FALSE)


cd202001LC = read_csv('crimeData/2020-01/2020-01-leicestershire-street.csv', show_col_types = FALSE)
cd202002LC = read_csv('crimeData/2020-02/2020-02-leicestershire-street.csv', show_col_types = FALSE)
cd202003LC = read_csv('crimeData/2020-03/2020-03-leicestershire-street.csv', show_col_types = FALSE)
cd202004LC = read_csv('crimeData/2020-04/2020-04-leicestershire-street.csv', show_col_types = FALSE)
cd202005LC = read_csv('crimeData/2020-05/2020-05-leicestershire-street.csv', show_col_types = FALSE)
cd202006LC = read_csv('crimeData/2020-06/2020-06-leicestershire-street.csv', show_col_types = FALSE)
cd202007LC = read_csv('crimeData/2020-07/2020-07-leicestershire-street.csv', show_col_types = FALSE)
cd202008LC = read_csv('crimeData/2020-08/2020-08-leicestershire-street.csv', show_col_types = FALSE)
cd202009LC = read_csv('crimeData/2020-09/2020-09-leicestershire-street.csv', show_col_types = FALSE)
cd202010LC = read_csv('crimeData/2020-10/2020-10-leicestershire-street.csv', show_col_types = FALSE)
cd202011LC = read_csv('crimeData/2020-11/2020-11-leicestershire-street.csv', show_col_types = FALSE)
cd202012LC = read_csv('crimeData/2020-12/2020-12-leicestershire-street.csv', show_col_types = FALSE)


cd202101LC = read_csv('crimeData/2021-01/2021-01-leicestershire-street.csv', show_col_types = FALSE)
cd202102LC = read_csv('crimeData/2021-02/2021-02-leicestershire-street.csv', show_col_types = FALSE)
cd202103LC = read_csv('crimeData/2021-03/2021-03-leicestershire-street.csv', show_col_types = FALSE)
cd202104LC = read_csv('crimeData/2021-04/2021-04-leicestershire-street.csv', show_col_types = FALSE)
cd202105LC = read_csv('crimeData/2021-05/2021-05-leicestershire-street.csv', show_col_types = FALSE)
cd202106LC = read_csv('crimeData/2021-06/2021-06-leicestershire-street.csv', show_col_types = FALSE)
cd202107LC = read_csv('crimeData/2021-07/2021-07-leicestershire-street.csv', show_col_types = FALSE)
cd202108LC = read_csv('crimeData/2021-08/2021-08-leicestershire-street.csv', show_col_types = FALSE)
cd202109LC = read_csv('crimeData/2021-09/2021-09-leicestershire-street.csv', show_col_types = FALSE)
cd202110LC = read_csv('crimeData/2021-10/2021-10-leicestershire-street.csv', show_col_types = FALSE)
cd202111LC = read_csv('crimeData/2021-11/2021-11-leicestershire-street.csv', show_col_types = FALSE)
cd202112LC = read_csv('crimeData/2021-12/2021-12-leicestershire-street.csv', show_col_types = FALSE)

cd202201LC = read_csv('crimeData/2022-01/2022-01-leicestershire-street.csv', show_col_types = FALSE)
cd202202LC = read_csv('crimeData/2022-02/2022-02-leicestershire-street.csv', show_col_types = FALSE)
cd202203LC = read_csv('crimeData/2022-03/2022-03-leicestershire-street.csv', show_col_types = FALSE)
cd202204LC = read_csv('crimeData/2022-04/2022-04-leicestershire-street.csv', show_col_types = FALSE)
cd202205LC = read_csv('crimeData/2022-05/2022-05-leicestershire-street.csv', show_col_types = FALSE)
cd202206LC = read_csv('crimeData/2022-06/2022-06-leicestershire-street.csv', show_col_types = FALSE)
cd202207LC = read_csv('crimeData/2022-07/2022-07-leicestershire-street.csv', show_col_types = FALSE)
cd202208LC = read_csv('crimeData/2022-08/2022-08-leicestershire-street.csv', show_col_types = FALSE)
cd202209LC = read_csv('crimeData/2022-09/2022-09-leicestershire-street.csv', show_col_types = FALSE)
cd202210LC = read_csv('crimeData/2022-10/2022-10-leicestershire-street.csv', show_col_types = FALSE)


crimedata = cd201911L %>% 
  add_row(cd201912L) %>%   add_row(cd202001L) %>%   add_row(cd202002L) %>%   add_row(cd202003L) %>%   add_row(cd202004L) %>%
  add_row(cd202005L) %>%   add_row(cd202006L) %>%   add_row(cd202007L) %>%   add_row(cd202008L) %>%   add_row(cd202009L) %>%
  add_row(cd202010L) %>%   add_row(cd202011L) %>%   add_row(cd202012L) %>%   add_row(cd202101L) %>%   add_row(cd202102L) %>% 
  add_row(cd202103L) %>%   add_row(cd202104L) %>%   add_row(cd202105L) %>%   add_row(cd202106L) %>%   add_row(cd202107L) %>%
  add_row(cd202108L) %>%   add_row(cd202109L) %>%   add_row(cd202110L) %>%  add_row(cd202111L) %>%   add_row(cd202112L) %>% 
  add_row(cd202201L) %>%   add_row(cd202202L) %>%   add_row(cd202203L) %>%  add_row(cd202204L) %>%   add_row(cd202205L) %>% 
  add_row(cd202206L) %>%   add_row(cd202207L) %>%   add_row(cd202208L) %>%  add_row(cd202209L) %>%   add_row(cd202210L) %>% 
  add_row(cd201911LC) %>% 
  add_row(cd201912LC) %>%   add_row(cd202001LC) %>%   add_row(cd202002LC) %>%   add_row(cd202003LC) %>%   add_row(cd202004LC) %>%
  add_row(cd202005LC) %>%   add_row(cd202006LC) %>%   add_row(cd202007LC) %>%   add_row(cd202008LC) %>%   add_row(cd202009LC) %>%
  add_row(cd202010LC) %>%   add_row(cd202011LC) %>%   add_row(cd202012LC) %>%   add_row(cd202101LC) %>%   add_row(cd202102LC) %>% 
  add_row(cd202103LC) %>%   add_row(cd202104LC) %>%   add_row(cd202105LC) %>%   add_row(cd202106LC) %>%   add_row(cd202107LC) %>%
  add_row(cd202108LC) %>%   add_row(cd202109LC) %>%   add_row(cd202110LC) %>%  add_row(cd202111LC) %>%   add_row(cd202112LC) %>% 
  add_row(cd202201LC) %>%   add_row(cd202202LC) %>%   add_row(cd202203LC) %>%  add_row(cd202204LC) %>%   add_row(cd202205LC) %>% 
  add_row(cd202206LC) %>%   add_row(cd202207LC) %>%   add_row(cd202208LC) %>%  add_row(cd202209LC) %>%   add_row(cd202210LC)
  
  
# Cleaning

crimedata = read_csv('CrimeData/MergedCrimeData.csv') %>% 
  select(Month, `LSOA code`, `Crime type`)

colnames(crimedata) = c("Year", "lsoa11cd", "CrimeType")

LsoaToPostcode = read_csv('PostCodeLSOA/Postcode to LSOA 2.csv')
Towns = read_csv("CleanedData/Towns.csv")


LsoaToPostcode  = LsoaToPostcode %>%
  mutate(shortPostcode = str_trim(substring(pcds, 1,4))) %>%
  left_join(Towns,by="shortPostcode") %>%
  filter(Country=="LANCASHIRE"|Country=="LEICESTERSHIRE") %>%
  group_by(lsoa11cd) %>%
  filter(row_number()==1) %>%
  select(lsoa11cd,shortPostcode,Town,District,Country)

crimedataCleaned = crimedata %>%
  left_join(LsoaToPostcode,by="lsoa11cd")%>%
  group_by(shortPostcode,CrimeType,Year)  %>%
  select(shortPostcode,CrimeType,Year) %>%
  na.omit() %>% 
  tally()


write.csv(crimedataCleaned, "CleanedData/Crime.csv")

