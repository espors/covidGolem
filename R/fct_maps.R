#' Maps 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

NULL 


#' Map Base
#'
#' @param cumulative_states 
#'
#' @return
#' @export
#'
#' @examples
map_base <- function(cumulative_states){
  map_data_states <- cumulative_states
  state <- ggplot2::map_data("state")
  
  map_data_states$state.x <- tolower(map_data_states$state.x)
  colnames(map_data_states)[colnames(map_data_states) == "state.x"] <- "region"
  states <- dplyr::inner_join(state, map_data_states, by = "region")
  
  map_base <- ggplot(data = state, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = "gray") + 
    ylab('') + 
    xlab('') + 
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          legend.position = 'none')
  
  return(map_base)
}

