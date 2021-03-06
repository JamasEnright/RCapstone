#' Label points on plot - Base package
#'
#' @usage NULL
#'
#' @importFrom dplyr group_by_
#' @importFrom dplyr top_n
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_blank
#' @importFrom grid textGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid unit
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom ggplot2 ggproto
GeomTIMELINELABEL <- ggplot2::ggproto("GeomTIMELINELABEL", ggplot2::Geom,
                                      required_aes = c("x"),
                                      default_aes = ggplot2::aes(size=1,y=0.5,group=NULL,n_max=5,label=NULL),
                                      draw_key = ggplot2::draw_key_blank,
                                      draw_panel= function(data, panel_scales, coord) {

                                        #sort out the data
                                        data <- dplyr::group_by_(data,"group")
                                        data <- dplyr::top_n(data,data$n_max[1], size)
                                        data <- dplyr::ungroup(data)

                                        #Set y from group
                                        if(!is.null(data[1,]$group)){
                                          numg=length(unique(data$group))
                                          data$y=(as.numeric(as.factor(data$group))/(numg+1))
                                        }

                                        ## Transform the data first
                                        coords <- coord$transform(data, panel_scales)

                                        #create the chart
                                        textg<-grid::textGrob(label = coords$label, x = coords$x, y = coords$y+0.06,
                                                              just = c("left", "bottom"),rot=45)
                                        lineg<-grid::segmentsGrob(x0 = grid::unit(coords$x, "npc"),x1 = grid::unit(coords$x, "npc"),
                                                                  y0 = grid::unit(coords$y, "npc"),y1 = grid::unit(coords$y+0.05, "npc"))
                                        grid::gTree(children = grid::gList(lineg,textg))
                                      })

#' Label points on plot
#'
#' This method enchances the plot timeline function by labelling points.
#' By default, it will label the top 5 points. This is specified in the \code{n_max} parameter.
#' The column to label the points is in the \code{label} parameter.
#'
#' @inheritParams ggplot2::geom_label
#'
#' @return This function returns a graphic object
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(readr)
#' raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
#' eq_clean_data(raw_data) %>% eq_location_clean() %>%
#' dplyr::filter(!is.na(EQ_PRIMARY), !is.na(DEATHS)) %>%
#' ggplot2::ggplot() +
#'  ggplot2::aes(
#'    x = DATE,
#'    size = EQ_PRIMARY,
#'    colour = DEATHS,
#'    label=LOCATION_NAME
#'  ) +
#'  geom_timeline() +
#'  geom_timeline_label()
#'
#' @importFrom ggplot2 layer
#' @importFrom readr read_tsv
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTIMELINELABEL, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
