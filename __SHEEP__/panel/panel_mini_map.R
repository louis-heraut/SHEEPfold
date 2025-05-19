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


## 1. MAP PANEL ______________________________________________________
# Generates a map plot of the tendancy of a hydrological variable
#' @title Mini map panel
#' @export
panel_mini_map = function (meta, Shapefiles,
                           codeLight=NULL,
                           regionLight=NULL,
                           secteurLight=NULL,
                           regimeCodeLight=NULL,
                           coucheLight=NULL,
                           zoom=NULL,
                           map_limits=NULL,
                           x_echelle_pct=62,
                           y_echelle_pct=5,
                           echelle=c(0, 50, 100, 250),
                           echelle_size=2.6,
                           echelle_km_size=2.5,
                           echelle_tick_height=1.3,
                           size_codeLight=1.4,
                           stroke_codeLight=0.3,
                           margin_map=margin(t=0, r=0, b=0, l=0, unit="mm"),
                           verbose=FALSE) {
    
    # Extract shapefiles
    france = Shapefiles$france
    bassinHydro = Shapefiles$bassinHydro
    secteurHydro = Shapefiles$secteurHydro
    regionHydro = Shapefiles$regionHydro
    entiteHydro = Shapefiles$entiteHydro
    entitePiezo = Shapefiles$entitePiezo
    river = Shapefiles$river

    # Stores the coordonate system 
    cf = coord_fixed()
    # Makes it the default one to remove useless warning
    cf$default = TRUE

    # Open a new plot with the personalise theme
    map = ggplot() + theme_void_Lato() +
        # Fixed coordinate system (remove useless warning)
        cf +
        
        theme(plot.margin=margin_map) +
        
        # Plot the background of France
        geom_sf(data=france,
                color=NA,
                fill=IPCCgrey97)

    map = map +
        # Plot the hydrological basin
        geom_sf(data=bassinHydro,
                color=IPCCgrey85,
                fill=NA,
                size=0.25)
    
    # If the river shapefile exists
    if (!is.null(river)) {
        # Plot the river
        map = map +
            geom_sf(data=river,
                    color="white",
                    alpha=1,
                    fill=NA,
                    linewidth=0.7,
                    na.rm=TRUE)
        map = map +
            geom_sf(data=river,
                    color=INRAElightcyan,
                    alpha=1,
                    fill=NA,
                    linewidth=0.35,
                    na.rm=TRUE)
    }

    # Plot the white back of boundaries
    # map = map +
    #     geom_sf(data=france,
    #             color="white",
    #             fill=NA,
    #             linewidth=0.7)
    map = map +
        geom_sf(data=france,
                color=IPCCgrey40,
                fill=NA,
                linewidth=0.35)

    if (!is.null(zoom)) {
        xlim = c(min(meta$XL93_m)*(1-zoom[1]), max(meta$XL93_m)*(1+zoom[2]))
        ylim = c(min(meta$YL93_m)*(1-zoom[3]), max(meta$YL93_m)*(1+zoom[4]))
    } else if (!is.null(map_limits)) {
        xlim = map_limits[1:2]
        ylim = map_limits[3:4]
    } else {
        xlim = c(90000, 1250000)
        ylim = c(6040000, 7120000)
    }
    
    xmin = gpct(x_echelle_pct, xlim, shift=TRUE)
    xint = echelle*1E3
    ymin = gpct(y_echelle_pct, ylim, shift=TRUE)
    ymin_km = gpct(max(c(y_echelle_pct-5, 0)), ylim, shift=TRUE)
    ymax = ymin + gpct(echelle_tick_height, ylim)
    # size = 2.6
    # sizekm = 2.5
    linewidth = 0.4

    map = map +
        # Adds the base line of the scale
        geom_line(aes(x=c(xmin, max(xint)+xmin),
                      y=c(ymin, ymin)),
                  color=IPCCgrey40, size=0.2) +
        # Adds the 'km' unit
        annotate("text",
                 x=max(xint)+xmin+gpct(1, xlim), y=ymin_km,
                 vjust=0, hjust=0, label="km",
                 family="Lato",
                 color=IPCCgrey40, size=echelle_km_size)
    # For all graduations
    for (x in xint) {
        map = map +
            # Draws the tick
            annotate("segment",
                     x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                     color=IPCCgrey40, size=0.2) +
            # Adds the value
            annotate("text",
                     x=x+xmin, y=ymax+gpct(0.5, ylim),
                     vjust=0, hjust=0.5, label=x/1E3,
                     family="Lato",
                     color=IPCCgrey40, size=echelle_size)
    }

    if (!is.null(codeLight)) {
        Code = levels(factor(meta$code))
        L93X = meta$XL93_m[match(meta$code, Code)]           
        L93Y = meta$YL93_m[match(meta$code, Code)]
        
        # Creates a tibble to stores all the data to plot
        plot_map = tibble(L93X=L93X, L93Y=L93Y, code=Code)
        # Extract data of all stations not to highlight
        plot_map_codeNo = plot_map[plot_map$code != codeLight,]
        # Extract data of the station to highlight
        plot_map_code = plot_map[plot_map$code == codeLight,]
        # Plots only the localisation
        
        map = map +
            geom_sf(data=entiteHydro[entiteHydro$code == codeLight,],
                    color="white",
                    fill=NA,
                    linewidth=1.1) +
            geom_sf(data=entiteHydro[entiteHydro$code == codeLight,],
                    color=INRAEred,
                    fill=NA,
                    linewidth=0.4)

        map = map +
            geom_point(data=plot_map_code,
                       aes(x=L93X, y=L93Y),
                       shape=21, size=size_codeLight, stroke=stroke_codeLight,
                       color="white",
                       fill=INRAEred)
    }

    if (!is.null(regionLight)) {
        map = map +
            geom_sf(data=regionHydro[regionHydro$CdRegionHy == regionLight,],
                    color="white",
                    fill=NA,
                    linewidth=1.1) +
            geom_sf(data=regionHydro[regionHydro$CdRegionHy == regionLight,],
                    color=INRAEdarkcyan,
                    fill=NA,
                    linewidth=0.5)
    }


    if (!is.null(secteurLight)) {
        map = map +
            geom_sf(data=secteurLight[secteurLight$CdRegionHy == regionLight,],
                    color="white",
                    fill=NA,
                    linewidth=1.1) +
            geom_sf(data=secteurLight[secteurLight$CdRegionHy == regionLight,],
                    color=INRAEdarkcyan,
                    fill=NA,
                    linewidth=0.5)
    }
    

    if (!is.null(coucheLight)) {
        map = map +
            geom_sf(data=entitePiezo[entitePiezo$codeeh == coucheLight,],
                    color="white",
                    fill=NA,
                    linewidth=1.1) +
            geom_sf(data=entitePiezo[entitePiezo$codeeh == coucheLight,],
                    color=INRAEdarkcyan,
                    fill=NA,
                    linewidth=0.5)
    }

    if (!is.null(regimeCodeLight)) {
        matchCode = match(meta$code, regimeCodeLight)
        matchCode = matchCode[!is.na(matchCode)]
        L93X = meta$XL93_m[matchCode]           
        L93Y = meta$YL93_m[matchCode]
        
        # Creates a tibble to stores all the data to plot
        plot_map = tibble(L93X=L93X, L93Y=L93Y, code=regimeCodeLight)

        map = map +
            geom_point(data=plot_map,
                       aes(x=L93X, y=L93Y),
                       shape=21, size=1, stroke=0.3,
                       color="white",
                       fill=INRAEdarkcyan)
    }

    map = map +
        # Allows to crop shapefile without graphical problem
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)
    
    return (map)
}

