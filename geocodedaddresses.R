
# Set working directory
"/Users/rashaelnimeiry/Library/Mobile Documents/com~apple~CloudDocs/geotruth"-> working 

# Read the link to survey from the Excel file
read_xlsx(glue('{working}/keys/tokens.xlsx')) %>% 
  filter(Program == 'supportsudanform') %>%
  pull(key) -> surveyform 

# Read the link to survey responses from the Excel file
read_xlsx(glue('{working}/keys/tokens.xlsx')) %>% 
  filter(Program == 'supportsudanresponses') %>%
  pull(key)-> surveyresponsesurl 

# Authenticate with Google Sheets
# gs4_auth()  # Uncomment if you need to authenticate

# Read the Google Sheet
read_sheet(surveyresponsesurl) %>% 
  # as.data.frame() %>% 
  janitor::clean_names() ->responses 



responses %>%
  head() %>%
  mutate(
    postal_code = as.numeric(postal_code),
    inputgeocodeaddress = case_when(
      is.na(complete_physical_address_of_business_project)  ~ 
        glue("{city_location_of_business_project}, {state_region_location_of_business_project}, {country_of_business_project}, {postal_code}"),
      nchar(complete_physical_address_of_business_project) < 10  ~ 
        glue("{city_location_of_business_project}, {state_region_location_of_business_project}, {country_of_business_project}"),
      TRUE ~ complete_physical_address_of_business_project
    )
  ) %>% 
  geocode(address = inputgeocodeaddress, method = 'osm', lat = latitude, long = longitude, return_addresses = TRUE) ->geocoded_responses
