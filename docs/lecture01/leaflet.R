library(leaflet)

leaflet() %>% 
  addProviderTiles(provider = "Esri.WorldImagery",
                   options = tileOptions(minZoom = 2,
                                         noWrap = TRUE)) %>% 
  addPopups(lng = c(-89.405032, -89.407260, 11.324524), 
            lat = c(43.074805, 43.074241, 55.257877), 
            popup = c("We're here.", "MSC", "Denmark!"),
            options = popupOptions(closeButton = FALSE,
                                   closeOnClick = FALSE)) %>% 
  setView(lng = -89.405032, lat = 43.074805, zoom = 20)