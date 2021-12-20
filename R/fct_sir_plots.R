#' sir_plots 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

NULL 

#' SIR Plots 
#'
#' @param sir_data Data frame: SIR values for given population level 
#' @param outcome Boolean value: Indicator for cases or deaths 
#' @param pop_level String: Indicator for population level 
#'
#' @return
#' @export
#'

sir_plot <- function(sir_data, outcome, pop_level) {
  dodge <- ggplot2::position_dodge(width = 0.9)
  custom <- c("#aec3b0", "#6F6E81")
  if (pop_level == "states") {
    if (outcome == 1){
      return(
        plotly::ggplotly(ggplot2::ggplot(
          data = sir_data, 
          ggplot2::aes(
            y = (sir - 1), 
            x = reorder(state.x, sir), 
            fill = typeC
          )
        ) + 
          ggplot2::geom_bar(stat = "identity", position = dodge) + 
          ggplot2::geom_errorbar(
            ggplot2::aes(ymax = (usir - 1), ymin = (lsir - 1 )),
            position = dodge, 
            width = 0.25
            ) + 
          ggplot2::xlab("State") + 
          ggplot2::ylab("1-SIR for cases") + 
          ggplot2::labs(fill = "Type") + 
          ggplot2::theme_minimal() + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 45, 
            hjust = 1
            )
          ) + 
          ggplot2::scale_fill_manual(values = custom) + 
          ggplot2::theme(legend.position = "none")
        )
      )
    }
    if (outcome == 0) {
      return(
        plotly::ggplotly(ggplot2::ggplot(
          data = sir_data, 
          ggplot2::aes(
            y = (sdr - 1), 
            x = reorder(state.x, sdr),
            fill = typeD
            )
          ) + 
            ggplot2::geom_bar(stat = "identity", position = dodge) + 
            ggplot2::geom_errorbar(
              ggplot2::aes(ymax = (usdr - 1), ymin = (lsdr - 1 )),
              position = dodge,
              width = 0.25
              ) +
            ggplot2::xlab("State") + 
            ggplot2::ylab("1-SIR for deaths") + 
            ggplot2::labs(fill = "Type") + 
            ggplot2::theme_minimal() + 
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            ) + 
            ggplot2::scale_fill_manual(values = custom) + 
            ggplot2::theme(legend.position = "none")
        )
      )
    }
  }
    if (pop_level == "counties") {
      if (outcome == 1) {
        return(
          plotly::ggplotly(ggplot2::ggplot(
            data = sir_data, 
            ggplot2::aes(
              y = (sir - 1), 
              x = reorder(county.x, sir), 
              fill = typeC
            )
          ) + 
            ggplot2::geom_bar(stat = "identity", position = dodge) + 
            ggplot2::geom_errorbar(
              ggplot2::aes(ymax = (usir - 1), ymin = (lsir - 1 )), 
              position = dodge, 
              width = 0.25
            ) + 
            ggplot2::xlab("Counties") + 
            ggplot2::ylab("1-SIR for cases") + 
            ggplot2::labs(fill = "Type") + 
            ggplot2::theme_minimal() + 
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            ) + 
            ggplot2::scale_fill_manual(values = custom) + 
            ggplot2::theme(legend.position = "none") + 
            ggplot2::theme(text = ggplot2::element_text(family = "Montserrat"))
          )
        )
      }
      if (outcome == 0) {
        return(
          plotly::ggplotly(ggplot2::ggplot(
            data = sir_data, 
            ggplot2::aes(
              y = (sdr - 1), 
              x = reorder(county.x, sdr), 
              fill = typeD
            )
          ) + 
            ggplot2::geom_bar(stat = "identity", position = dodge) + 
            ggplot2::geom_errorbar(
              ggplot2::aes(ymax = (usdr - 1), ymin = (lsdr - 1 )), 
              position = dodge, 
              width = 0.25
            ) + 
            ggplot2::xlab("Counties") + 
            ggplot2::ylab("1-SIR for deaths") + 
            ggplot2::labs(fill = "Type") + 
            ggplot2::theme_minimal() + 
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            ) + 
            ggplot2::scale_fill_manual(values = custom) + 
            ggplot2::theme(legend.position = "none")
          )
        )
      }
    }
  }