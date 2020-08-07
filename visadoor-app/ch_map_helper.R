
ch_map <- function(data, pallete, title){
  # specify column with text to be displayed
  hover <- unlist(data[,4])
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
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  plot_geo(data, locationmode = 'USA-states') %>%
    add_trace(z = ~Decile, text = ~hover, locations = ~Code, colors = pallete, showscale=TRUE) %>%
    colorbar(title = "Decile") %>%
    layout(title = title, geo = g, margin = m)
}