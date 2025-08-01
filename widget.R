library(leaflet)
library(sf)
library(tidyverse)
library(htmltools)
library(htmlwidgets)
library(openxlsx)

clean_location <-  function(variable){
  case_match(
    str_to_lower(str_squish(variable)),
    c("region i","ilocos","region 1","ilocos region", "region i (ilocos region)","region 1 (ilocos region)") ~ "Region I (Ilocos Region)",
    c("region ii","region 2","cagayan valley", "region ii (cagayan valley)","region 2 (cagayan valley)") ~ "Region II (Cagayan Valley)",
    c("region iii","region 3","central luzon", "region iii (central luzon)","region 3 (central luzon)") ~ "Region III (Central Luzon)",
    c("region iv-a","southern tagalog","region 4a","calabarzon", "region iv-a (calabarzon)","region 4-a (calabarzon)") ~ "Region IV-A (CALABARZON)",
    c("region v","bicol","region 5","bicol region", "region v (bicol region)","region 5 (bicol region)" ) ~ "Region V (Bicol Region)",
    c("region vi","region 6","western visayas", "region vi (western visayas)", "region 6 (western visayas)") ~ "Region VI (Western Visayas)",
    c("region vii","region 7","central visayas", "region vii (central visayas)", "region 7 (central visayas)") ~ "Region VII (Central Visayas)",
    c("region viii","region 8","eastern visayas", "region viii (eastern visayas)","region 8 (eastern visayas)") ~ "Region VIII (Eastern Visayas)",
    c("region ix","region 9","zamboanga peninsula", "region ix (zamboanga peninsula)", "region 9 (zamboanga peninsula)") ~ "Region IX (Zamboanga Peninsula)",
    c("region x","region 10","northern mindanao", "region x (northern mindanao)","region 10 (northern mindanao)") ~ "Region X (Northern Mindanao)",
    c("region xi","region 11","davao region", "region xi (davao region)","region 11 (davao region)") ~ "Region XI (Davao Region)",
    c("region xii","region 12","soccsksargen", "region xii (soccsksargen)","region 12 (soccsksargen)") ~ "Region XII (SOCCSKSARGEN)",
    c("region xiii","region 13","caraga", "region xiii (caraga)","region 13 (caraga region)","REGION XIII (Caraga) ") ~ "Region XIII (Caraga)",
    c("region iv-b","mimaropa", "region iv-b (mimaropa)","mimaropa region","region 4-b (mimaropa region)" ) ~ "MIMAROPA Region",
    c("cordillera administrative region (car)", "car","cordillera administrative region") ~ "Cordillera Administrative Region (CAR)",
    c("autonomous region in muslim mindanao (armm)","armm","barmm","bangsamoro autonomous region in muslim mindanao (barmm)", "bangsamoro autonomous region in muslim mindanao") ~ "Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)",
    c("city of davao","davao city")~"davao city",
    c("cotabato (north cotabato)","north cotabato")~"north cotabato",
    c("compostela valley","davao de oro","davao de oro (compostela valley)")~"davao de oro (compostela valley)",
    c("western samar","samar","samar (western samar")~"samar (western samar)",
    c("maguindanao (excluding cotabato city)") ~ "maguindanao",
    c("basilan (excluding city of isabela)")~"basilan",
    c("ncr")~"national capital region (ncr)",
    c("agusan norte")~"agusan del norte",
    c("agusan sur")~"agusan del sur",
    c("davao norte")~"davao del norte",
    c("davao sur")~"davao del sur",
    c("lanao norte")~"lanao del norte",
    c("lanao sur")~"lanao del sur",
    c("mt. province")~"mountain province",
    c("mindoro occidental") ~"occidental mindoro",
    c("mindoro oriental") ~"oriental mindoro",
    c("surigao norte")~"surigao del norte",
    c("surigao sur")~"surigao del sur",
    c("zamboanga norte")~"zamboanga del norte",
    c("zamboanga sur")~"zamboanga del sur",
    .default = str_to_lower(variable)
  )
}

sua_new5 <- readRDS('sua_new5.rds')

provincial_map <- readRDS("province_map.rds")

suff_rat <- 
  sua_new5$sua |> 
  filter(eco=="alleco") |> 
  select(type,location,year,SuffRatio) |> 
  pivot_wider(names_from = year,values_from = SuffRatio) |>
  mutate(location2 = clean_location(location),
         location3 = str_to_title(location) |> 
           str_replace(pattern="Del ","del ") |> 
           str_replace(pattern="De", "de"),
         location3 = if_else(location3=="North Cotabato","Cotabato (North Cotabato)",location3)) |> 
  # ungroup() |> 
  select(-c(location,location2)) |> 
  rename(location = location3)

write_csv(suff_rat,"suff-rat.csv")


suff_rat |> 
  mutate(province = str_to_lower(location)) |> 
  left_join(reg_prov) |> 
  mutate(region = if_else(is.na(region),location,region),
         location = if_else(type!="reg",str_to_title(location),location)) |> 
  mutate(region = if_else(location == "Cotabato (North Cotabato)","Region XII (SOCCSKSARGEN)",region)) |>
  mutate(region = factor(region,
                         levels = c(
                           "Philippines",
                           "Cordillera Administrative Region (CAR)",
                           "Region I (Ilocos Region)",
                           "Region II (Cagayan Valley)",
                           "Region III (Central Luzon)",
                           "Region IV-A (CALABARZON)",
                           "MIMAROPA Region",
                           "national capital region (ncr)",
                           "Region V (Bicol Region)",
                           "Region VI (Western Visayas)",
                           "Region VII (Central Visayas)",
                           "Region VIII (Eastern Visayas)",
                           "Region IX (Zamboanga Peninsula)",
                           "Region X (Northern Mindanao)",
                           "Region XI (Davao Region)",
                           "Region XII (SOCCSKSARGEN)",
                           "Region XIII (Caraga)",
                           "Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)"
                         ),
                         ordered = T)
  ) |> 
  filter(type%in% c("natl","prov")) |> 
  mutate(ssr_group = cut(`2024`, breaks = c(0,50,100,125,Inf),
                         labels = c("Highly Deficient","Deficient","Marginally Sufficient","Sufficient"))) |> 
  select(type,region,province=location,SSR=`2024`,ssr_group) |>
  filter(!is.na(region)) |> 
  arrange(region,province) |> 
  write.xlsx("ssr2024v2.xlsx")

provdta <- provincial_map |> 
  # distinct(province) |> 
  left_join(suff_rat |> select(province = location,sratio=`2024`))

numPal <-  colorBin("RdYlBu",
                    domain = provdta$sratio,
                    bins =c(0,50,100,125,Inf)
)


ssr <- 
  leaflet() |>
  addProviderTiles("CartoDB.VoyagerNoLabels",
                   options = providerTileOptions(minZoom =6)) |>
  setView(lat = 12.8797, lng = 121.774, zoom=6) |> 
  addPolygons(stroke=TRUE,
              fillOpacity = 0.9,
              fillColor = ~numPal(sratio),
              color='white',
              weight=0.5,
              label = ~paste("<b> Province:</b>",str_to_title(location),"<br>",
                             "<b> Rice Sufficiency Index </b>", round(sratio,2)) |>
                lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              ),
              data = provdta
  ) |>
  addLegend(colors =  c("#2C7BB6","#ABD9E9","#FDAE61","#D7191C"),
            labels = c("Sufficient (SSR>=125%)", "Marginally Sufficient (100%<=SSR<125%)", "Deficient (50%<=SSR<100)", "Highly Deficient (SSR<50%)"),
            opacity=0.9,
            title = 'Rice sufficiency Index',
            position = "bottomleft",
            data = provdta) |> 
  addControl(
    html = tags$div(
      style =  "
      background: transparent !important;
      padding: 0 !important;
      border: none !important;
    ",
      tags$img(src = "logo.png", height = "100px")
    ),
    className = "customControl",
    position = "bottomright"
  )

ssr
browsable(ssr)


saveWidget(ssr, "ssr.html", selfcontained = TRUE)
browseURL("ssr.html")
