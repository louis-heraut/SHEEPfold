# Copyright 2022-2024 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#                     Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
#
# *1   INRAE, UR RiverLy, Villeurbanne, France
#
# This file is part of SHEEPfold R toolbox.
#
# SHEEPfold R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# SHEEPfold R toolbox is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SHEEPfold R toolbox.
# If not, see <https://www.gnu.org/licenses/>.


#' @title Info panel
#' @export
panel_axis = function(X,
                      subX=NULL,
                      X_color=IPCCgrey60,
                      X_tick_color=IPCCgrey75,
                      X_label_color=IPCCgrey40,
                      subX_color=IPCCgrey25,
                      axis.text.x_size=3.1,
                      date_labels="%Y",
                      breaks="10 years",
                      minor_breaks="2 years",
                      d_breaks=0,
                      break_round=-1,
                      add_breaks=NULL,
                      axis_margin=margin(t=0, r=0, b=0, l=, "mm")) {

    # Labels = c("1976", "2005",
    #            paste0("<b style='color:", IPCCgrey35, "'>2020</b>"),
    #            "2040",
    #            paste0("<b style='color:", IPCCgrey35, "'>2050</b>"),
    #            "2070",
    #            paste0("<b style='color:", IPCCgrey35, "'>2099</b>"))
    # majorDate = as.Date(c("1976-01-01", "2005-01-01", "2020-01-01",
    #                       "2040-01-01", "2050-01-01",
    #                       "2070-01-01", "2099-01-01"))
    # minorDate = seq.Date(as.Date("1980-01-01"),
    #                      as.Date("2090-01-01"),
    #                      "10 years")
    # minorDate = minorDate[!(minorDate %in% majorDate)]

    # fix_color = function (X) {
    #     in_sub = subX[1] <= X & X <= subX[2]
    #     out_sub = !in_sub
    #     X[in_sub] = paste0("<span style='color:", subX_color, "'>",
    #                        X[in_sub], "</span>")
    #     X[out_sub] = paste0("<span>", X[out_sub], "</span>")
    #     return (X)
    # }

    nX = length(X)
    
    get_breaks = function(X) {
        if (lubridate::is.Date(X)) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")) + d_breaks,
                           to=as.Date(paste0(Xmax, "-01-01")) + d_breaks,
                           by=breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=breaks)
        }

        if (!is.null(add_breaks)) {
            res = sort(c(res, add_breaks))
        }
        return (res)
    }

    get_minor_breaks = function(X) {
        if (lubridate::is.Date(X)) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(
                               as.Date(paste0(Xmin, "-01-01")) -
                               lubridate::duration(breaks)),
                           to=as.Date(
                               as.Date(paste0(Xmax, "-01-01")) +
                               lubridate::duration(breaks)),
                           by=minor_breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks - breaks,
                      to=Xmax + d_breaks + breaks,
                      by=minor_breaks)
        }
        return (res)
    }

    y_line = 0.95
    dy_tick = 0.26
    y_text = 0.1
    lw_line = 0.55
    lw_tick = 0.4
    minor_rel = 0.6
        
    Ticks_major = get_breaks(X)
    nTicks_major = length(Ticks_major)

    Ticks_minor = get_minor_breaks(X)
    OK_minor =
        min(X, na.rm=TRUE) <= Ticks_minor &
        Ticks_minor <= max(X, na.rm=TRUE)
    Ticks_minor = Ticks_minor[OK_minor]
    nTicks_minor = length(Ticks_minor)


    
    axis = ggplot2::ggplot() +  theme_void() +
        coord_cartesian(clip="off") +
        ggplot2::theme(plot.margin=axis_margin,
                       axis.ticks.y=element_blank(),
                       axis.ticks.length.y=unit(1.5, 'mm')) +
        # theme_IPCC(isGridY=FALSE,
        #            is_axis.ticks.y=FALSE,
        #            is_axis.text.y=FALSE,
        #            is_axis.ticks.x=FALSE,
        #            is_axis.text.x=FALSE,
        #            axis.text.x_size=axis.text.x_size,
        #            is_axis.line.x=FALSE) +
        theme(legend.position="none") +
        
        ggplot2::annotate("point",
                          x=X, y=y_line,
                          color=NA, fill=NA) +
        ggplot2::annotate("line",
                          x=X, y=y_line,
                          color=X_color,
                          linewidth=lw_line,
                          lineend="square")
    if (!is.null(subX)) {
        axis = axis + 
            ggplot2::annotate("line",
                              x=subX, y=y_line,
                              color=subX_color,
                              linewidth=lw_line,
                              lineend="square")
        OK_in_major = subX[1] <= Ticks_major & Ticks_major <= subX[2]
        OK_in_minor = subX[1] <= Ticks_minor & Ticks_minor <= subX[2]
    } else {
        OK_in_major = rep(TRUE, nTicks_major)
        OK_in_minor = rep(TRUE, nTicks_minor)
    }

    
    Color_label = rep(X_label_color, nTicks_major)
    Color_label[OK_in_major] = subX_color

    Color_major = rep(X_tick_color, nTicks_major)
    Color_major[OK_in_major] = subX_color
    
    Color_minor = rep(X_tick_color, nTicks_minor)
    Color_minor[OK_in_minor] = subX_color

    tmp_major = dplyr::tibble(x=rep(Ticks_major, each=2),
                              y=rep(c(0, -dy_tick)+y_line, nTicks_major),
                              color=rep(Color_major, each=2),
                              group=rep(1:nTicks_major, each=2))

    tmp_minor = dplyr::tibble(x=rep(Ticks_minor, each=2),
                              y=rep(c(0, -dy_tick*minor_rel)+y_line,
                                    nTicks_minor),
                              color=rep(Color_minor, each=2),
                              group=rep(1:nTicks_minor, each=2))
    
    axis = axis +
        geom_line(data=tmp_major,
                  aes(x=x, y=y,
                      group=group),
                  color=tmp_major$color,
                  linewidth=lw_tick,
                  lineend="square") +
        geom_line(data=tmp_minor,
                  aes(x=x, y=y,
                      group=group),
                  color=tmp_minor$color,
                  linewidth=lw_tick,
                  lineend="square") + 
                 
        annotate("text",
                 x=Ticks_major, y=y_text,
                 label=format(Ticks_major, date_labels),
                 size=axis.text.x_size,
                 color=Color_label,
                 family="Lato",
                 vjust=0, hjust=0.5)

    
    # ggplot2::scale_x_date(
    #              breaks=majorDate,
    #              minor_breaks=minorDate, 
    #              labels=Labels,
    #              guide="axis_minor",
    #              expand=c(0, 0)) +
    # ggplot2::scale_x_date(
    #              date_breaks="10 years",
    #              date_minor_breaks="5 years", 
    #              date_labels="%Y",
    #              guide="axis_minor",
    #              expand=c(0, 0)) +
    axis = axis +
        ggplot2::scale_y_continuous(limits=c(0, 1),
                                    expand=c(0, 0))


    # if (lubridate::is.Date(X)) {
        axis = axis +
            ggplot2::scale_x_date(
                         # breaks=get_breaks,
                         # minor_breaks=get_minor_breaks,
                         # guide='axis_minor',
                         # date_labels=date_labels,
                         expand=c(0, 0))
    # } else {
        # axis = axis +
            # ggplot2::scale_x_continuous(
                         # breaks=get_breaks,
                         # minor_breaks=get_minor_breaks,
                         # guide='axis_minor',
                         # expand=c(0, 0))
    # }
    
    return (axis)
}  
