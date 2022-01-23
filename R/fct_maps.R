#' Maps 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

NULL 



#' Base information for states map 
#'
#' @param cumulative_states cumulative states numbers
#' @param method Either "state" for COVID cases and deaths or "vaccinations" 
#' for vaccinations 
#'
#' @return
#' @export
#'
map_data_states <- function(cumulative_states, method){
  map_data_states <- cumulative_states
  map_state <- ggplot2::map_data("state")
  if (method == "outcomes") {
    map_data_states$state.x <- tolower(map_data_states$state.x)
    colnames(map_data_states)[colnames(map_data_states) == "state.x"] <- "region"
    states_data <- dplyr::inner_join(map_state, map_data_states, by = "region")
    return(
      list(
        map_state = map_state, 
        states_data = states_data
      )
    )
  }
  if (method == "vaccinations") {
    map_data_states$State <- tolower(map_data_states$State)
    colnames(map_data_states)[colnames(map_data_states) == "State"] <- "region"
    states_data <- dplyr::inner_join(map_state, map_data_states, by = "region")
    return(
      list(
        map_state = map_state, 
        states_data = states_data
      )
    )
  }
}


#' Map Base
#'
#' @param cumulative_states 
#'
#' @return ggplot polygon of states 
#' @export
#'
map_base <- function(state){
  map_base <- ggplot2::ggplot(
    data = state,
    mapping = ggplot2::aes(x = long, y = lat, group = group)) + 
      ggplot2::coord_fixed(1.3) + 
      ggplot2::geom_polygon(color = "black", fill = "gray") + 
      ggplot2::ylab("") + 
      ggplot2::xlab("") + 
      ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(), 
          legend.position = 'none')
  return(map_base)
}


#' United states maps 
#'
#' @param states_data COVID-19 data for states 
#' @param map_state Base map data from previous function 
#' @param method Select method for type of map 
#'
#' @return
#' @export
#'
map_united_states <- function(states_data, map_state, method = "cases"){
  map_base <- map_base(map_state)
  state <- ggplot2::map_data("state")
  if (method == "cases") {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = cases),
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#aec3b0")
  }
  if (method == "deaths") {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = deaths),
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 1) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = `Doses Delivered per 100K`), 
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 2) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = `Doses Administered per 100K`), 
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 3) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = `At least One Shot per 100K (All Types)`), 
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 4) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data,
        ggplot2::aes(fill = `Fully Vaccinated per 100K (All Types)`),
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 5) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = `Fully Vaccinated per 100K (Moderna)`), 
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 6) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = `Fully Vaccinated per 100K (Pfizer)`), 
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 7) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = `Fully Vaccinated per 100K (Janssen)`), 
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  if (method == 8) {
    mapState <- map_base + 
      ggplot2::geom_polygon(
        data = states_data, 
        ggplot2::aes(fill = `Fully Vaccinated per 100K (Other)`), 
        color = "white"
      ) + 
      ggplot2::scale_fill_gradient(low = "white", high = "#6F6E81")
  }
  return(mapState)
}

