library(tidyverse)
library(httr)

###issue with Sri Lankan restaurants was that the number
###is so small that the offset isn't needed

client_id <- "cjes0snrWY5LhSumtIFZug"
client_secret <- "jR_1bpodltEK_vlRwPIXL_vm9fnXBrbN5dAJAvgYQUNq4q7qXrAV-_kEGI55v7Zq34m-4r3xD83SiKPdYQ8kUTqYaZJ5qeTiW_ldQpQa9szkI9JJQVwEB-7UBEqEXXYx"

res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))

#token <- content(res)$access_token




yelp <- "https://api.yelp.com"
term <- "Sri Lankan"
#category <- "srilankan"
location <- "New York, NY"
#categories <- "srilankan"
limit <- 50
radius <- 30000

offset = seq(50,1000,50)


urls = c() 

for (i in offset){
  url = modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location, 
                               limit = limit,
                               categories=categories,
                               radius = radius
                               #,offset=0+i
                               ))
  urls = c(urls, url)}


###TROUBLESHOOTING
new_results = c()

for (i in urls){
  res <- GET(i, add_headers('Authorization' = paste("bearer", client_secret)))
  results <- content(res)
  new_results = c(new_results, results)
}




yelp_httr_parse <- function(x) {
  
 parse_list <- list(id = x$id, 
                     name = x$name, 
                     rating = x$rating, 
                     review_count = x$review_count, 
                     latitude = x$coordinates$latitude, 
                     longitude = x$coordinates$longitude, 
                     address1 = x$location$address1, 
                     city = x$location$city, 
                     state = x$location$state, 
                     distance = x$distance, 
                    categories = x$categories)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- data_frame(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   city = parse_list$city, 
                   state = parse_list$state, 
                   distance= parse_list$distance,
                   categories_1 = parse_list$categories[[1]][[1]],
                   categories_2 = parse_list$categories[[1]][[2]]
                   )
  df
}

results_list = c() 
for (j in 1:length(new_results)){
    result = lapply(new_results[j]$businesses, FUN = yelp_httr_parse)
    results_list = c(results_list, result)
  }


library(data.table)
business_data <- unique(setDT(do.call("rbind", results_list)))#[state=="PA" & city=="Philadelphia"]

###write out to csv
write.csv(business_data, paste0("C:/Users/argun/Documents/", "indian_food.csv"))
library(sf)
DT_sf = st_as_sf(business_data, coords = c("longitude", "latitude"), 
                 crs = 4326)
###write out to shapefile
st_write(DT_sf, paste0("C:/Users/argun/Documents/Bubble Tea Map/", "boba_nyc.shp"))
library(tmap)
tmap_mode("view")
tm_shape(DT_sf)+
  tm_symbols(shape=21, col="red", size = .0005,
             popup.vars=c("Restaurant Name"="name", "address"="address1"))

