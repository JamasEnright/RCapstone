#' Plot Timeline - base packagae
#'
#' @usage NULL
#'
#' @importFrom grid textGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid pointsGrob
#' @importFrom grid xaxisGrob
#' @importFrom grid unit
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 .stroke
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point

GeomTIMELINE <- ggplot2::ggproto("GeomTIMELINE", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(colour="black", size=1, alpha=1,y=0.5,stroke = 0.5,fill="blue",shape=21,group=NULL),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel= function(data, panel_scales, coord) {

                                   #sort out the data
                                   xmin=min(data$x)
                                   xmax=max(data$x)

                                   #Set y from group
                                   if(!is.null(data[1,]$group)){
                                     numg=length(unique(data$group))
                                     data$y=as.numeric(as.factor(data$group))/(numg+1)
                                   }

                                   ## Transform the data first
                                   coords <- coord$transform(data, panel_scales)

                                   # label line
                                   labdat<-data.frame(label=unique(as.character(data$group)),y=unique(coords$y)+0.03,stringsAsFactors = FALSE)

                                   #create the chart
                                   labg<-grid::textGrob(label = labdat$label, x = (min(coords$x))-0.01, y = labdat$y)
                                   lg<-grid::segmentsGrob(x0 = grid::unit(xmin, "npc"),x1 = grid::unit(xmax, "npc"),
                                                          y0 = grid::unit(coords$y, "npc"),y1 = grid::unit(coords$y, "npc"))
                                   pg<-grid::pointsGrob(x = coords$x,y=coords$y,pch = coords$shape,size=grid::unit(coords$size,"mm"),gp=grid::gpar(col=coords$colour,fill=coords$fill,alpha = coords$alpha,fontsize=coords$size * ggplot2:::.pt + coords$stroke * ggplot2:::.stroke / 2))
                                   grid::gTree(children = grid::gList(lg,pg,grid::xaxisGrob(),labg))
                                 })

#' Plot Timeline
#'
#' This is a method of creating a graph that maps earthquake data along the timeline.
#'
#' @param x Date of earthquakes
#' @param size [optional] Size of earthquake
#' @param color [optional] Number of Deaths
#' @param group [optional] Column indicating how the data could be grouped.
#'
#' @return This function returns a graphic object
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
#' eq_clean_data(raw_data) %>% eq_location_clean() %>%
#' dplyr::filter(!is.na(EQ_PRIMARY), !is.na(DEATHS)) %>%
#' ggplot2::ggplot() +
#'  ggplot2::aes(
#'    x = DATE,
#'    size = EQ_PRIMARY,
#'    colour = DEATHS
#'  ) +
#'  geom_timeline()
#'
#' @importFrom ggplot2 layer
#'
#' @export


geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTIMELINE, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
