library(httr)
library(jsonlite)
library(glue)
key <- "TyBY7HHxBgoUkBGxhzndB6EDWTML164GIHzkbyPyyk%2BaKp0uM8k33Dr1zvtM4aswGwClooAYyD3yTY%2B5D1TT1w%3D%3D"
df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(df) <- c("response.header.resultCode", "response.header.resultMsg", "response.body.items.item.dateKind", "response.body.items.item.dateName", "response.body.items.item.isHoliday", "response.body.items.item.locdate", "response.body.items.item.seq", "response.body.numOfRows", "response.body.pageNo", "response.body.totalCount")

for(year in 2015:2024) {
  for(month in 1:12) {
    if(month<10) {month_str = glue("0{month}")} else {month_str = glue("{month}")}
    url <- glue('http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo?serviceKey={key}&pageNo=1&numOfRows=10&solYear={year}&solMonth={month_str}')
    response <- GET(url)
    print(status_code(response))
    data <- content(response, "text")  
    json_data <- fromJSON(data)
    print(json_data)
    add <- as.data.frame(json_data)
    if(add$response.body.totalCount[1] != 0) {df <- rbind(df, add)}
  }
}

write.csv(df, "holidays.csv")
