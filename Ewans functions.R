## theme map ----
theme_map <- function(leg.tit.size = 8,
                      legend.position = "bottom",...) {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5),
      plot.subtitle = element_text(size = 8, hjust = 0.5),
      legend.title = element_text(size = leg.tit.size),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "transparent", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA), 
      panel.background = element_rect(fill = "transparent", color = NA), 
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.position = legend.position,
      panel.border = element_blank(),
      ...
    )
}
## thememap no title ----
theme_map.notitle <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      legend.title = element_text(size = 8),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "transparent", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA), 
      panel.background = element_rect(fill = "transparent", color = NA), 
      legend.background = element_rect(fill = "transparent", color = NA),
      panel.border = element_blank(),
      ...
    )
}

## colour breaks and label makers ----
colour.brks <- function (lims, n = 5){pretty(seq(from = lims[1] , to = lims[2] , length = 10), n = n)}
colour.lable <- function(x,lims, n = 5, dividor = 1 ){
  breaks = colour.brks(lims, n = n)
  if( max(lims, na.rm = T)< max(x, na.rm = T)){ paste(breaks/dividor , c(rep("", times = length(colour.brks(lims, n = n))-1),"+"))} 
  else{paste(breaks)}}
## map plotter ----

map.ploter.ident <- function( fill.scale.title , main.title , sub.title ,
                              #   background = countries,
                              fillground = COUNTRY.ATI.shp,
                              fillground2 = COUNTRY.ATI.shp, 
                              pltly.text = NULL,
                              transformation = "identity",
                              col.limits = c(0,max(variable)) , 
                              to.plot = variable,
                              clr.breaks = colour.brks(lims = colour.limits),
                              clr.labels = colour.lable(x = variable ,
                                                        lims = colour.limits , 
                                                        breaks = colour.brks(colour.limits ),
                                                        dividor = 1)){
  
  ggplot() +
    #   geom_sf(data = background, size = 0.2) +
    geom_sf(data = fillground, mapping = aes(fill = to.plot, text = pltly.text  ), colour = NA) +
    geom_sf(data = fillground, fill = NA, size = 0.05, colour = "grey90") +
    scale_fill_viridis_c( trans = transformation, 
                          name = fill.scale.title,
                          limits = col.limits, 
                          oob = scales::squish, 
                          breaks = clr.breaks,
                          labels = clr.labels,
                          #option = "magma",direction = -1 
                          guide = guide_colorbar(
                            direction = "horizontal", barheight = unit(2, units = "mm"),
                            barwidth = unit(50, units = "mm"), draw.ulim = F,
                            title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
    labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
    )+
    theme_map() +
    theme(legend.position = "bottom") 
}




map.ploter.ident.col.def <- function( fill.scale.title , main.title , sub.title ,
                              #   background = countries,
                              fillground = COUNTRY.ATI.shp,
                              fillground2 = COUNTRY.ATI.shp, 
                              pltly.text = NULL,
                              #  transformation = "identity",
                              col.limits = c(0,max(variable)) , 
                              to.plot = variable,
                              clr.breaks = colour.brks(lims = colour.limits),
                              clr.labels = colour.lable(x = variable ,
                                                        lims = colour.limits , 
                                                        breaks = colour.brks(colour.limits ),
                                                        dividor = 1),
                              low.col = "white", high.col = "red"){
  
  ggplot() +
    #   geom_sf(data = background, size = 0.2) +
    geom_sf(data = fillground, mapping = aes(fill = to.plot, text = pltly.text   ), colour = NA) +
    geom_sf(data = fillground, fill = NA, size = 0.05, colour = "grey10") +
    scale_fill_gradient( low = low.col, high = high.col,
                      # trans = transformation, 
                          name = fill.scale.title,
                          limits = col.limits, 
                          oob = scales::squish, 
                          breaks = clr.breaks,
                          labels = clr.labels,
                          #option = "magma",direction = -1 
                          guide = guide_colorbar(
                            direction = "horizontal", barheight = unit(2, units = "mm"),
                            barwidth = unit(50, units = "mm"), draw.ulim = F,
                            title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
    labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
    )+
    theme_map() +
    theme(legend.position = "bottom") 
}


## hexgrid over landscape ----

Umake.target.hexgrid = function(master.grid.mask = countries,
                         grid.size = c(hexdist.h, hexdist.v),
                         target){
  hex.grid0 = st_make_grid(master.grid.mask, grid.size, what = "polygons", square = F)
  hex.grid = st_sf(hex.grid0) %>%
    # add grid ID
    mutate(grid_id = 1:length(lengths(hex.grid0)))
  
  # intersect with landscape - note that grid id is from original UK-wide grid, allows easy cross-ID

  target.hexgrid <- st_intersection(hex.grid, target) %>% 
    st_make_valid() %>%  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% 
    dplyr::select(grid_id) 
  target.hexgrid$hex.ha = st_area(target.hexgrid) %>% 
    set_units(value = "ha") %>%
    as.numeric()
  
  target.hexgrid
}

find.lims = function(var, quant.weights = rep(1, length(var)) , consider = rep(T, length(var)), quant.prob = 0.98, sd.mult = 3){
  # used to find upper limit to squish variable scales to etc
  min(mean(var[consider]) + sd.mult* sd(var[consider]),
      wtd.quantile (var[consider], q = quant.prob, na.rm = FALSE, weight= quant.weights[consider]))
}
  
keep.only.letters = function(x){
  str_replace_all(x, regex("[^a-zA-Z]"), "")
}

pad.lim = function (x, map.pad = 0.05){
  # function to add % padding to a range of two numbers
  c(x[1] - diff(range(x)*map.pad),x[2] + diff(range(x)*map.pad) )
}


# A helper function that erases all of y from x: 
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))





###########################################################################
# spatial curation --------------------------------------------------------
###########################################################################

st_first.spatial.curation = function (x, tolerance = 10, tiny.buff = 0.0001, smallest.hole = 5000){
  x %>% st_transform( 27700) %>% 
    st_simplify( preserveTopology = T, dTolerance = tolerance) %>% 
    st_buffer( dist = tiny.buff) %>% 
    st_simplify( preserveTopology = T, dTolerance = tolerance) %>% 
    st_make_valid()#%>% 
  #filter(!st_is_empty(.))
  
} 


###########################################################################
# add rows that total all english regions and UK CARs ---------------------
###########################################################################

add_eng_uk_tots <- function(df, of_colmn, by_var = NULL, cars.areas = NULL, cars.names = NULL){
  
  print.order = data.frame(Name = c( "Central", "North", "South East", "South West","England Total", "Northern Ireland","Scotland", "Wales", "UK Total"))
  print.order$order <- 1: length(print.order$Name)
  
  if("ha.cars" %in% names (df)){
    df <- 
      bind_rows(df %>% as.data.frame(),
        data.frame(Name = cars.names[!cars.names %in% df$Name],
                   ha.car_rainfor = 0,
                   ha.cars = cars.areas[!cars.names %in% df$Name]
                   )
      )
  }

  uk.dat <- df %>%
    group_by(across({{ by_var }})) %>%
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>% 
    mutate(Name = "UK Total")
  names(uk.dat) <- names(uk.dat) %>% str_remove("_sum")
  
  eng.regions.dat <- df %>%
    filter(Name %in% c("Central", "North", "South East", "South West")) %>% 
    group_by(across({{ by_var }})) %>% 
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>% 
    mutate(Name = "England Total")
  names(eng.regions.dat) <- names(eng.regions.dat) %>% str_remove("_sum")

  if("ha.cars" %in% names (df)){
    eng.regions.dat$ha.cars <- sum(cars.areas[!duplicated(cars.names) & (cars.names %in% c("Central", "North", "South East", "South West"))] )
    uk.dat$ha.cars <- sum(cars.areas[!duplicated(cars.names)] )
  }
  
nu.df <- bind_rows(df, eng.regions.dat,uk.dat) %>% 
    full_join(print.order, by = "Name") %>% 
    arrange(across({{ by_var }})) %>% 
    arrange(order) %>% 
    subset(., select=c(1:(dim(df)[2]+1)))

# commetned this out and it seems to still be doing the right job... not htought about it much 2023-07-14
# if("ha.cars" %in% names (df) & sum(is.na(nu.df$ha.car_rainfor))>0){
#   for ( i in 1:sum(is.na(nu.df$ha.car_rainfor))){
#     name <- nu.df$CAR[is.na(nu.df$ha.car_rainfor)][i]
#     nu.df$ha.cars[is.na(nu.df$ha.car_rainfor)][i] <- cars.areas[cars.names == name]
#   }
# }

nu.df 
}


## Just sum cars by country ----
sum_group_by_country <- function(df, of_colmn, by_var = NULL, cars.areas = NULL, cars.names = NULL){
  print.order = data.frame(Name = c( countries.names %>% sort(), "UK Total"))
  print.order$order <- 1: length(print.order$Name)
  
  not.eng <- df %>% 
    filter(Name %in% countries.names)
  
  eng <- df %>% 
    filter(Name %in% england.cars) %>% 
    group_by(across({{ by_var }})) %>%
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>% 
    mutate(Name = "England")
  names(uk.dat) <- names(uk.dat) %>% str_remove("_sum")
    
  uk.dat <- df %>%
    group_by(across({{ by_var }})) %>%
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>% 
    mutate(Name = "UK Total")
  names(uk.dat) <- names(uk.dat) %>% str_remove("_sum")
  
  
  if("ha.cars" %in% names (df)){
    eng$ha.cars <- sum(cars.areas[!duplicated(cars.names) & (cars.names %in% england.cars)] )
    uk.dat$ha.cars <- sum(cars.areas[!duplicated(cars.names)] )
  }
  
  nu.df <- bind_rows(not.eng, eng, uk.dat) %>% 
    full_join(print.order, by = "Name") %>% 
    arrange(across({{ by_var }})) %>% 
    arrange(order) %>% 
    subset(., select=c(1:(dim(df)[2]+1)))
  
  nu.df 
}



# Print with commas -------------------------------------------------------


print_with_commas <- function(vec) {
  n <- length(vec)
  if (n == 0) {
    return("")
  } else if (n == 1) {
    return(vec[1])
  } else if (n == 2) {
    return(paste(vec, collapse = " and "))
  } else {
    return(paste(vec[-n], collapse = ", ") %>% paste(vec[n], sep = ", and "))
  }
}

