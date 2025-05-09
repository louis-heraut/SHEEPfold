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


panel_colorbar = function (min, max, palette_name,
                           colorStep=256, include=FALSE,
                           label=NULL, asFrac=FALSE,
                           reverse=FALSE,
                           round=TRUE,
                           size_color=1,
                           dx_color=0.5,
                           dy_color=0.5,
                           margin=margin(0, 0, 0, 0),
                           WIP=FALSE) {
    
    Palette = get_IPCC_Palette(palette_name, colorStep=colorStep, reverse=reverse)
    res = compute_colorBin(min, max,
                           colorStep=colorStep,
                           center=0,
                           include=include,
                           round=round)
    bin = res$bin
    upBin = res$upBin
    lowBin = res$lowBin
    
    if (is.null(label)) {
        if (asFrac) {
            label = float2frac(bin, round(colorStep/2))
        } else {
            label = round_label(bin, direction="H", ncharLim=4)
        }
    }
    
    nBin = length(bin)-1
    maxBin = max(bin, na.rm=TRUE)
    minBin = min(bin, na.rm=TRUE)
    bin = (bin - minBin) / (maxBin - minBin)
    bin = bin + seq(0, nBin*(dx_color+size_color), dx_color+size_color)
    midBin = zoo::rollmean(bin, 2)
    
    
    plot = ggplot() + theme_void_Lato() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=margin)

    if (WIP) {
        plot = plot + theme_WIP()
    }

    plot = plot +
        annotate("rect",
                 xmin=midBin-size_color/2,
                 xmax=midBin+size_color/2,
                 ymin=dy_color+size_color*2/3-size_color/2,
                 ymax=dy_color+size_color*2/3+size_color/2,
                 fill=Palette)
    
    for (i in 1:(nBin+1)) {
        b = bin[i]
        plot = plot +
            annotate("line",
                     x=c(b, b),
                     y=dy_color+size_color*2/3+
                         c(-size_color*2/3, size_color*2/3),
                     linewidth=0.6, color=IPCCgrey85)
    }
    
    plot = plot +
        annotate("text",
                 x=bin,
                 y=rep(0, nBin+1),
                 label=label, size=3.5,
                 hjust=0.5, vjust=0,
                 fontface="bold", color=IPCCgrey50)

    plot = plot +
        scale_x_continuous(expand=c(0, 0)) + 
        scale_y_continuous(expand=c(0, 0))
    

    return (plot)
}
