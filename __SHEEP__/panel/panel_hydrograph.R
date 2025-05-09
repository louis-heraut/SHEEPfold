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


#' @title Hydrograph panel
#' @export
panel_hydrograph = function (QM_code, regimeLight, period=NULL,
                             legend="QM ($m^{3}.s^{-1}$)",
                             ratio_title=1/4,
                             variableAbove=FALSE,
                             margin_title=margin(t=0, r=0,
                                                 b=0, l=0, unit="mm"),
                             margin_hyd=margin(t=0, r=0,
                                               b=0, l=0, unit="mm"),
                             verbose=FALSE) {


    title = ggplot() + theme_void_Lato() +
        theme(plot.margin=margin_title)

    if (variableAbove) {
        title = title +
            annotate("text",
                     x=0,
                     y=1,
                     label=TeX(legend),
                     size=3, hjust=0, vjust=1,
                     color=IPCCgrey40) +
            annotate("text",
                     x=0,
                     y=0,
                     label=regimeLight,
                     size=2.5, hjust=0, vjust=0,
                     color=IPCCgrey40)
    } else {
        title = title +
            annotate("text",
                     x=1,
                     y=1,
                     label=regimeLight,
                     size=2.5, hjust=1, vjust=1,
                     color=IPCCgrey40)
    }
    


    title = title +
        scale_x_continuous(limits=c(0, 1),
                           expand=c(0, 0)) +
        scale_y_continuous(limits=c(0, 1),
                           expand=c(0, 0))
    
    
    # Vector of month index
    monthNum = 1:12
    # Vector of month name abbreviation
    monthName = c("J", "F", "M", "A", "M", "J",
                  "J", "A", "S", "O", "N", "D")

    # Open a new plot with the personalise theme
    hyd = ggplot() +
        theme_IPCC(is_axis.text.x=FALSE,
                   is_axis.text.y=FALSE) +
        # Theme modification
        theme(
            panel.background=element_rect(fill="white"),
            panel.border=element_blank(),
            axis.text.x=element_text(margin=unit(c(0, 0, 0, 0), "mm"),
                                     vjust=1, hjust=0.5),
            axis.text.y=element_text(size=8),
            axis.ticks.x=element_blank(),
            axis.line.y=element_line(color=IPCCgrey85, size=0.3),
            plot.title=element_text(size=8, vjust=-1, 
                                    hjust=-0.01, color=IPCCgrey40),
            axis.title.y=element_blank())
    
    hyd = hyd + 
        theme(plot.margin=margin_hyd)

    
    hyd = hyd +
        # Plots the bar
        geom_bar(aes(x=monthNum, y=QM_code), 
                 stat='identity',
                 fill=IPCCgrey67,
                 width=0.75, size=0.2)

    if (!variableAbove) {
        hyd = hyd +
            theme(axis.title.y=element_text(size=7.2,
                                            vjust=0, hjust=0.5,
                                            color=IPCCgrey40)) + 
            ylab(TeX(legend))
    }
    
    hyd = hyd +
        # X axis
        scale_x_continuous(breaks=monthNum,
                           labels=monthName,
                           limits=c(0, max(monthNum)+0.5),
                           expand=c(0, 0)) + 
        # Y axis
        scale_y_continuous(limits=c(0, max(QM_code)),
                           n.breaks=4,
                           expand=c(0, 0))
    

    plan = matrix(c("title",
                    "hyd"),
                  nrow=2, 
                  byrow=TRUE)
    
    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan,
                        verbose=verbose)
    
    herd = add_sheep(herd,
                     sheep=title,
                     id="title",
                     height=ratio_title,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=hyd,
                     id="hyd",
                     height=1,
                     verbose=verbose)

    # herd = shear_sheeps(herd)
    
    return (herd)
} 
