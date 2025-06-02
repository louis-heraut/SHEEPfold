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


panel_colorbar_circle = function (bin,
                                  Palette,
                                  size_circle=1,
                                  linewidth=0.2,
                                  text_size=2,
                                  d_left_shift=0.05,
                                  d_line=0.5,
                                  d_space=0.5,
                                  d_text=0.5,
                                  label=NULL,
                                  ncharLim=4,
                                  stroke=NULL,
                                  color=NULL,
                                  shape=21,
                                  colorText=IPCCgrey50,
                                  colorLine=IPCCgrey50,
                                  on_circle=FALSE,
                                  margin=margin(t=0, r=0, b=0, l=0,
                                                "cm")) {
    

    nColor = length(Palette)
    nBin = length(bin)
    
    plot = ggplot() + theme_void_Lato() +
        theme(plot.margin=margin)


    if (is.null(label)) {
        Label = round_label(bin, direction="V", ncharLim=ncharLim)
    } else {
        Label = label
    }

    if (is.null(stroke)) {
        stroke = NA
    }

    if (is.null(color)) {
        color = "transparent"
    }
    
    plot = plot +
        geom_point(aes(x=rep((d_line+d_left_shift)/2, nColor),
                       y=seq(0, nColor-1, by=1)),
                   fill=Palette, color=color, shape=shape,
                   stroke=stroke,
                   size=size_circle)
    
    for (i in 1:(nBin+1)) {
        b = bin[i]
        if (!is.finite(b)) {
            next
        }

        if (!on_circle) {
            plot = plot +
                annotate("line",
                         x=c(d_left_shift, d_line),
                         y=c((i-1)-1/2,
                         (i-1)-1/2),
                         lineend="round",
                         linewidth=linewidth, color=colorLine)
            plot = plot +
                annotate("text",
                         x=d_left_shift+d_line+d_space,
                         y=(i-1)-1/2,
                         label=TeX(Label[i]),
                         size=text_size,
                         hjust=0, vjust=0.5,
                         # fontface=text_fontface,
                         family="Lato",
                         color=colorText)

        } else {
            plot = plot +
                annotate("text",
                         x=d_left_shift+d_line+d_space,
                         y=(i-1),
                         label=TeX(Label[i]),
                         size=text_size,
                         hjust=0, vjust=0.5,
                         # fontface=text_fontface,
                         family="Lato",
                         color=colorText)
        }
    }

    plot = plot +
        scale_x_continuous(expand=c(0, 0),
                           limits=c(0,
                                    d_line+d_space+d_text)) + 
        scale_y_continuous(expand=c(0, 0),
                           limits=c(-1/2-1,
                                    nBin+1/2))
    

    return (plot)
}
