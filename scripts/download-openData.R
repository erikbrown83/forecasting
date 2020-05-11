#function for downloading data from the nhs open data portal using
#the ckan api and the ckanr package

#note to see the available packages on the opendata website you use:
#package_list(as = "table") #this function lists the available data tables
#package_show("gp-practice-populations", as = "table") #this function lists the available information (including urls for downloading data files)


download_nhsOpenData <- function(url = "https://www.opendata.nhs.scot/", 
                                 package.id){
  
  require(ckanr)
  require(tidyverse)
  
  #create connection to open data portal:
  ckanr_setup(url)
  
  #now extract the list of urls for download:
  file_links <- package_show(package.id, as = "table")$resources$url
  
  #load all files and add together 
  #use url as id column:
  data <- file_links %>% map_df(read_csv, .id = "file") %>% 
    mutate(Date = parse_date(as.character(Date), format = "%Y%m%d"))
  
  return(data)
  
}
