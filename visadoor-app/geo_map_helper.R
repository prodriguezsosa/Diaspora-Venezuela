
geo_map <- function(data){
  # define plot margins
  m <- list(
    l = 25,
    r = 25,
    b = 25,
    t = 25,
    pad = 5
  )
  # plot styling parameters
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    showlakes = TRUE,
    lakecolor = toRGB('white'),
    landcolor = toRGB("gray85"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white")
  )
  
  plot_geo(data, locationmode = 'USA-states') %>%
    add_markers(
      x = ~Longitude, y = ~Latitude, hoverinfo = "text", 
      text = ~hover
                ) %>%
    #layout(title = 'Visa Applicants \n (2008 - 2017)', geo = g, margin = m)
    layout(geo = g, margin = m)
  
}