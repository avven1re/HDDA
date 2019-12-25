library(httr)
library(jsonlite)
library(magrittr)

photo <- GET("https://api.nasa.gov/planetary/apod?api_key=4Su05tp6OP8LqNbLyI5QsdGgNeWLsOSar12N8DMx", query = list(hd = "True", date = "2019-12-01")) %>% content()

browseURL(photo$url)


ntpu <- GET("https://api.nasa.gov/planetary/earth/imagery/?lon=24.9362479&lat=121.3614785&cloud_score=True&api_key=4Su05tp6OP8LqNbLyI5QsdGgNeWLsOSar12N8DMx") %>% content()

browseURL(ntpu$url)
