
# imports -----------------------------------------------------------------


library(remotes)#install.packages('remotes')
library(tdor)# install_github("CaRdiffR/tdor")
library(dplyr)#install.packages('dplyr')

library(readxl)#install.packages('readxl')
library(openxlsx)#install.packages('openxlsx')
library(here)#install.packages('here')

library(maps)#install.packages('maps')
library(stringr)#install.packages('stringr')
library(lubridate)#install.packages('lubridate')

# library(janitor)#install.packages('janitor')


# functions ---------------------------------------------------------------


# this is to extract current tdor data and write it to an excel
fake_the_excel <- function(path_to_excel){
    # get the tdor package data with names clean.
    # tdor_data <- janitor::clean_names(tdor::tdor)
    tdor_data <- tdor::tdor

    # head(tdor_data)
    spread_sheet_list <- list(tdor = tdor_data,other_random_sheet=mtcars)

    #save it as exsample excel
    openxlsx::write.xlsx(spread_sheet_list, file = path_to_excel)
}


# get the original dataframe and add the country code column
# parameters:
# original_data_frame: the tdor dataframe
# return: 
# the dataframe with country values
data_with_country_codes <- function(original_data_frame){
    # original_data_frame <- tdor_data
    country_names <- maps::iso3166 %>%  
        mutate(mapname_l=str_to_lower(mapname),
               ISOname_l=str_to_lower(ISOname)) %>% 
        count(mapname_l,ISOname_l,a3) %>% select(-n) %>% 
        mutate(std_iso=mapname_l) %>% 
        mutate(std_map=ISOname_l)
    
    # original_data_frame %>% 
        
    original_data_frame_with_a3 <- original_data_frame %>% 
        mutate(country_l=str_to_lower(Country)) %>% 
        mutate(country_l=if_else(country_l=="united kingdom",
                                 "united kingdom of great britain and northern ireland",
                                 country_l)) %>% 
        left_join(country_names,by=c("country_l"="ISOname_l"),suffix = c("_tdor","_iso")) %>% 
        left_join(country_names,by=c("country_l"="mapname_l"),suffix = c("_tdor_iso","_map")) %>% 
        mutate(iso_a3=if_else(is.na(a3_map),a3_tdor_iso,a3_map)) %>% 
        select(-country_l, -mapname_l, 
               -a3_tdor_iso,-std_iso_tdor_iso,-std_map_tdor_iso,
               -ISOname_l,-a3_map,-std_iso_map,-std_map_map,-std_map_map) %>% 
        group_by_all() %>% tally() %>% select(-n) %>%  # to delete duplicated rows
        arrange(Year,TDoR,Date)
    # original_data_frame_with_a3 %>% filter(n>1)
    
    original_data_frame_with_a3
}

# it replicates the data into a new sheet with a new column and saves it on a new excel file
# added a new excel file as dont want to overwrite previous version, just for safety reason.
# parameters: 
# origin_xslx_path: excel path
# spreadsheet_name: name of the spreadsheet with the data
# RETURN:
# writes a new file with the date and time generated.
save_new_excel_with_original_data_and_sheet_with_codes <- function(origin_xslx_path,spreadsheet_name){
    # origin_xslx_path <- path_to_excel_1
    # spreadsheet_name <- "tdor"
    # read the orginal 
    origin_xslx <- readxl::read_xlsx(origin_xslx_path,sheet = spreadsheet_name)
    
    # add the countrycodes
    country_with_codes <- data_with_country_codes(origin_xslx)
    
    # make the new excel
    spread_sheet_list_new <- list(tdor_original = origin_xslx,tdor_country_code=country_with_codes)

    
    # adding timestamp to filename
    current_time <- lubridate::now() %>% str_replace_all(" ","_") %>% str_replace_all(":","-")
    new_version_xlsx <- str_replace(origin_xslx_path,".xlsx",paste0(current_time,".xlsx"))
    
    #save it in new file
    openxlsx::write.xlsx(spread_sheet_list_new, file = new_version_xlsx)
}


# implementation ----------------------------------------------------------

# this is where is the excel
path_to_excel_1 <- here::here("jas1_weeks","2018-11-20","tdor_dataset.xlsx")

# this is to fake the excel from original package data
# this step is not needed if you already got other excel
fake_the_excel(path_to_excel_1)

# this is where is the excel + sheet name
# will write a new excel
save_new_excel_with_original_data_and_sheet_with_codes(path_to_excel_1,"tdor") 