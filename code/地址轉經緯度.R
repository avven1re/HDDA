#Import library
library(jsonlite)

#Google map 地址轉經緯度
geoPoint = function(address, key, verbose=FALSE) {
  #若未輸入地址, return錯誤
  if(verbose) cat(address,"\n")
  root = "https://maps.googleapis.com/maps/api/place/findplacefromtext/"
  #Google編碼為UTF8, 中文要轉碼後帶入URL才會正確
  address = iconv(address,"big5","utf8")
  #POI API型態(XML or JSON)
  return.call = "json"
  sensor = "false"
  #產生URL
  url_gen = paste(root, return.call, "?input=", address, "&inputtype=textquery&fields=geometry&key=", key, sep = "")
  #擷取網頁原始碼
  html_code = fromJSON(url_gen)
  #若status為OK抓取資料, 若不為OK return status
  if(html_code$status=="OK") {
    return(cbind(html_code$candidates$geometry$location, address))
  } else {
    return(paste("Status:", html_code$status, sep = " "))
  }
}

t <- geoPoint(rps02[22], "AIzaSyD7ExFH62Ni2cYE5Vp4939DVO6DeUKxupk" )
t[, 1]
sum(t[1] == "Status: ZERO_RESULTS")