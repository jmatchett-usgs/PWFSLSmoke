#' @keywords ws_monitor
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @title Create Daily AQI Barplots of Multiple Monitors
#' @param ws_monitor \code{ws_monitor} object
#' @param monitorIDs vector of monitor IDs in \code{ws_monitor} to subset and plot (if \code{NULL}, all monitors are plotted)
#' @param title plot title
#' @param smooth_func \code{PWFSLSmoke} smoothing function (set to \code{NULL} to exclude smoothed values)
#' @param smooth_args list of additional arguments to the smoothing function
#' @param smooth_style how smoothed values are plotted
#' @param smooth_name name of smoothing method used in legend
#' @param theme ggplot2 \code{\link[ggplot2]{theme}} controlling plotting options (see \code{\link[PWFSLSmoke]{theme_monitors}} for default)
#' @param date_format formatting code for x-axis dates (see \code{\link[base]{strftime}} for codes)
#' @param ... additional arguments, such as \code{tlim}, passed to \code{\link[PWFSLSmoke]{monitor_subset}}
#' @return \code{\link[ggplot2]{ggplot}} object
#' @description Plots a facetted set of daily-averaged air quality data for multiple monitoring stations.

monitorPlot_dailyBarplotFacets <- function(
    ws_monitor, monitorIDs=NULL, title='Daily AQI',
    smooth_func=monitor_nowcast, smooth_args=list(), smooth_style=c('bars', 'points'),
    smooth_name='Hourly NowCast', theme=theme_monitors(), date_format='%b %d', ...) {

    # AQI colors and legend names
    aqi_colors <- AQI$colors
    aqi_names <- c('Good', 'Moderate', 'Unhealthy for Sensitive Groups',
        'Unhealthy', 'Very Unhealthy', 'Hazardous')
    names(aqi_colors) <- aqi_names
    aqi_actions <- c(
        'None.',
        'Unusually sensitive individuals should consider limiting prolonged or heavy exertion.',
        'People within Sensitive Groups should reduce prolonged or heavy outdoor exertion.',
        'People within Sensitive Groups should avoid all physical outdoor activity.',
        'Everyone should avoid prolonged or heavy exertion.',
        'Everyone should avoid any outdoor activity.'
    )
    # subset monitoring IDs
    if(!is.null(monitorIDs))
        ws_sub <- monitor_subset(ws_monitor, monitorIDs=monitorIDs, ...)
    else
        ws_sub <- ws_monitor
    site_names <- ws_sub$meta %>% select(monitorID, siteName)
    # AQI daily average
    ws_sub_daily <- monitor_dailyStatistic(ws_sub)
    data_daily <- ws_sub_daily$data %>%
        gather(key='monitorID', value='pm25', -datetime) %>%
        mutate(
            aqi=cut(pm25, breaks=AQI$breaks_24, labels=aqi_names),
            time_start=datetime,
            time_end=datetime + 86400,
            datetime=datetime + 86400/2
        ) %>%
        inner_join(site_names, by='monitorID')

    ws_plot <- ggplot(data_daily, aes(x=datetime, y=pm25, fill=aqi)) +
        geom_col(alpha=0.5, width=86400) +
        facet_wrap(~siteName, ncol=1) +
        scale_fill_manual(name='Daily Air Quality Index (24 hr AQI)', values=aqi_colors, drop=FALSE,
            guide=guide_legend(order=1, override.aes=list(shape=NA))) +
        labs(x='Date (midnight to midnight)', y=expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")")),
             title=title) +
        scale_x_datetime(breaks=ws_sub_daily$data$datetime + 86400/2,
                         minor_breaks=ws_sub_daily$data$datetime,
                         date_labels=date_format, expand=c(0,0)) +
        theme
    # Smoothed AQI
    if(!is.null(smooth_func)) {
        smooth_args$ws_monitor <- ws_sub
        ws_smooth <- do.call(smooth_func, smooth_args)
        data_smooth <- ws_smooth$data %>%
            gather(key='monitorID', value='pm25', -datetime) %>%
            mutate(aqi=cut(pm25, breaks=AQI$breaks_24, labels=aqi_names)) %>%
            filter(!is.na(pm25)) %>%
            inner_join(site_names, by='monitorID')
        smooth_style <- match.arg(smooth_style)
        if(identical(smooth_style, 'bars')) {
            ws_plot <- ws_plot +
                geom_linerange(mapping=aes(ymax=pm25, color=aqi), ymin=0, data=data_smooth)
        }
        else if(identical(smooth_style, 'points')) {
            ws_plot <- ws_plot +
                geom_linerange(mapping=aes(ymax=pm25), data=data_smooth, ymin=0, color='gray', size=0.1, show.legend=FALSE) +
                geom_point(mapping=aes(color=aqi), size=0.5, data=data_smooth)
        }
        ws_plot <- ws_plot +
            geom_col(mapping=aes(fill=NULL), alpha=0, color='black', size=0.25, width=86400) +
            scale_color_manual(name=sprintf('%s (actions to protect yourself)', smooth_name),
                values=aqi_colors, drop=FALSE, labels=aqi_actions,
                guide=guide_legend(order=0, override.aes=list(size=2))
            )
    }

    return(ws_plot)
}
