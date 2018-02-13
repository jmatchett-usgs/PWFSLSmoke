#' @export
#' @import ggplot2
#' @title Theme for Plotting Multiple Monitors
#' @param title_size size of plot title
#' @param axis_title_size size of axis titles
#' @param axis_label_size size of axis labels
#' @param facet_title_size size of facet titles
#' @param legend_title_size size of legend titles
#' @param legend_label_size size of legend item labels
#' @param legend_position legend position
#' @description A plotting \code{\link[ggplot2]{theme}} used by \code{\link[PWFSLSmoke]{monitorPlot_dailyBarplotFacets}}.
#' @return ggplot2 \code{\link[ggplot2]{theme}}

theme_monitors <- function(
    title_size=14,
    axis_title_size=12,
    axis_label_size=10,
    facet_title_size=12,
    legend_title_size=12,
    legend_label_size=10,
    legend_position='bottom') {

    theme(
        plot.title=element_text(size=title_size),

        axis.title=element_text(size=axis_title_size),
        axis.text.x=element_text(angle=45, hjust=1, size=axis_label_size),
        axis.text.y=element_text(size=axis_label_size),

        panel.background=element_rect(fill='white', color=NA),
        panel.border=element_rect(fill=NA, color='black'),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_line(color='lightgray', size=0.25, linetype=2),
        panel.grid.major.y=element_line(color='lightgray', size=0.25, linetype=2),
        panel.grid.minor.y=element_blank(),

        strip.text=element_text(size=facet_title_size),
        strip.background=element_rect(fill='grey85', color='black'),

        legend.box='horizontal',
        legend.position=legend_position,
        legend.direction='vertical',
        legend.justification=c(1,1),
        legend.box.background=element_rect(fill='white', color='black', size=0.25),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_label_size)
    )
}
