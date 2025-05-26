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


sheet_projection_secteur = function (Stations,
                                     Secteur,
                                     dataEX_serie,
                                     metaEX_serie,
                                     dataEX_criteria,
                                     metaEX_criteria,
                                     WL,
                                     NarraTRACC,
                                     delta_prob=0, 
                                     icons=NULL,
                                     logo_info="",
                                     Pages=NULL,
                                     Shapefiles=NULL,
                                     figdir="",
                                     verbose=FALSE) {

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    height = 29.7 - page_margin["t"] - page_margin["b"]
    width = 21 - page_margin["l"] - page_margin["r"]
    
    info_height = 4
    climate_height = 6
    hydroMap_height = 6
    hydroQM_height = 6
    foot_height = 1
    hydroTable_height = height - info_height - climate_height - hydroMap_height - hydroQM_height - foot_height
    
    plan = matrix(c(
        "info",
        "climate", 
        "hydroMap",
        "hydroQM",
        "hydroTable",
        "foot"
    ), ncol=1, byrow=TRUE)


    is_numeric = names(dataEX_criteria)[sapply(dataEX_criteria, is.numeric)]
    
    dataEX_criteria_stat =
        dplyr::summarise(dplyr::group_by(dataEX_criteria,
                                         SH, GWL),
                         dplyr::across(is_numeric,
                                       ~quantile(.x, delta_prob,
                                                 na.rm=TRUE),
                                       .names="min_{.col}"),
                         dplyr::across(is_numeric,
                                       ~quantile(.x, 0.5,
                                                 na.rm=TRUE),
                                       .names="median_{.col}"),
                         dplyr::across(is_numeric,
                                       ~quantile(.x, 1-delta_prob,
                                                 na.rm=TRUE),
                                       .names="max_{.col}"))

    SH = unique(substr(Stations$code, 1, 2))
    nSH = length(SH)

    nWL = length(WL)

    for (i in 1:nSH) {
        sh = SH[i]

        sh = "A3"
        
        print(paste0(i, "/", nSH, " so ", round(i/nSH*100, 1), "% done -> ", sh))

        Secteur = Secteurs[Secteurs$id_secteur == sh,]
        
        Stations_sh = filter(Stations, substr(code, 1, 2) == sh)
        dataEX_criteria_sh = filter(dataEX_criteria, SH == sh)

        secteurHydro_shp = Shapefiles$secteurHydro[Shapefiles$secteurHydro$CdSecteurH == sh,]
        
        Chain = unique(Projections$Chain)
        nChain = length(Chain)


        for (j in 1:nWL) {
            id_letter = 1
            
            wl = WL[[j]]
            print(paste0(j, "/", nWL, " so ", round(j/nWL*100, 1), "% done -> ", wl["RWL"]))

            
            dataEX_criteria_sl_wl = dplyr::filter(dataEX_criteria, SH==sh, GWL==wl["GWLclean"])
            dataEX_criteria_stat_sl_wl = dplyr::filter(dataEX_criteria_stat, SH==sh, GWL==wl["GWLclean"])
    
            
            ## PAGE _________________________________________________________
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan,
                                verbose=verbose)

            ### Info ________________________________________________________
            info_plan = matrix(c(
                "map", "title", "logo",
                "map", "legend", "logo"
            ), ncol=3, byrow=TRUE)

            info_title_height = 1.5
            info_legend_height = 1
            
            info_map_width = 1
            info_title_width = 2
            info_logo_width = 1

            info_herd = bring_grass(verbose=verbose)
            info_herd = plan_of_herd(info_herd, info_plan,
                                     verbose=verbose)

            xlim = c(90000, 1250000)
            ylim = c(6040000, 7120000)

            margin_map = margin(t=0, r=0, b=2, l=0, unit="mm")
            
            echelle = c(0, 100, 250)
            x_echelle_pct = 2
            y_echelle_pct = 4
            echelle_line_size = 0.3
            echelle_size = 2.1
            echelle_km_size = 2
            echelle_tick_height = 2

            ymin_km_shift = 2
            ymin_km_value_shift = 1
            
            cf = coord_fixed()
            cf$default = TRUE
            map = ggplot() + theme_void() + cf +
                theme(plot.margin=margin_map) +
                geom_sf(data=Shapefiles$france,
                        color=NA,
                        fill=IPCCgrey97) +
                geom_sf(data=Shapefiles$river,
                        color="white",
                        alpha=1,
                        fill=NA,
                        linewidth=0.7,
                        na.rm=TRUE) +
                geom_sf(data=Shapefiles$river,
                        color=INRAElightcyan,
                        alpha=1,
                        fill=NA,
                        linewidth=0.35,
                        na.rm=TRUE) +
                geom_sf(data=Shapefiles$bassinHydro,
                        color=IPCCgrey85,
                        fill=NA,
                        size=0.25) +
                geom_sf(data=Shapefiles$france,
                        color=IPCCgrey40,
                        fill=NA,
                        linewidth=0.35)
            
            xmin = gpct(x_echelle_pct, xlim, shift=TRUE)
            xint = echelle*1E3
            ymin = gpct(y_echelle_pct, ylim, shift=TRUE)
            ymin_km = gpct(max(c(y_echelle_pct-ymin_km_shift, 0)), ylim, shift=TRUE)
            ymax = ymin + gpct(echelle_tick_height, ylim)

            map = map +
                geom_line(aes(x=c(xmin, max(xint)+xmin),
                              y=c(ymin, ymin)),
                          color=IPCCgrey40, size=echelle_line_size,
                          lineend="round") +
                annotate("text",
                         x=max(xint)+xmin+gpct(1, xlim), y=ymin_km,
                         vjust=0, hjust=0, label="km",
                         family="Lato",
                         color=IPCCgrey40, size=echelle_km_size)

            for (x in xint) {
                map = map +
                    annotate("segment",
                             x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                             color=IPCCgrey40, size=echelle_line_size,
                             lineend="round") +
                    annotate("text",
                             x=x+xmin, y=ymax+gpct(ymin_km_value_shift, ylim),
                             vjust=0, hjust=0.5, label=x/1E3,
                             family="Lato",
                             color=IPCCgrey40, size=echelle_size)
            }

            map = map +
                geom_sf(data=secteurHydro_shp,
                        color="white",
                        fill=NA,
                        linewidth=1.1) +
                geom_sf(data=secteurHydro_shp,
                        color=wl["color"],
                        fill=NA,
                        linewidth=0.5)

            map = map +
                coord_sf(xlim=xlim, ylim=ylim,
                         expand=FALSE)

            info_herd = add_sheep(info_herd,
                                  sheep=map,
                                  id="map",
                                  width=info_map_width,
                                  verbose=verbose)

            #### Info text _______________________________________________________
            dy_newline = 0.24
            dy_region = 0.29
            dy_basin = 0.15
            
            title_text = strwrap(paste0(sh, " - ", Secteur$secteur), width=42)
            nLine = length(title_text)
            title_text[1] = sub("[-] ", "- \\\\textbf{", title_text[1])
            title_text[1] = paste0(title_text[1], "}")
            if (nLine > 1) {
                title_text[2:nLine] = sapply(title_text[2:nLine], function (x) {paste0("\\textbf{", x, "}")})
            }
            
            title = ggplot() +
                theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm"))

            for (l in 1:nLine) {
                title = title +
                    annotate("text",
                             x=0, y=0.98-dy_newline*(l-1),
                             label=latex2exp::TeX(title_text[l]),
                             size=5.4, hjust=0, vjust=1,
                             family="Raleway",
                             color=TRACCblue)
            }

            region = paste0("\\textbf{Region hydro.} \\; ", Secteur$region)
            basin = paste0("\\textbf{Bassin de gestion} \\; ", Secteur$bassin)
            
            title = title +
                annotate("text",
                         x=0, y=0.98-dy_newline*(nLine-1) - dy_region,
                         label=latex2exp::TeX(region),
                         size=3, hjust=0, vjust=1,
                         family="Lato",
                         color=TRACCblue) +
                annotate("text",
                         x=0, y=0.98-dy_newline*(nLine-1) - dy_region - dy_basin,
                         label=latex2exp::TeX(basin),
                         size=3, hjust=0, vjust=1,
                         family="Lato",
                         color=TRACCblue)

            title = title +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            

            info_herd = add_sheep(info_herd,
                                  sheep=title,
                                  id="title",
                                  height=info_title_height,
                                  width=info_title_width,
                                  verbose=verbose)


            narratracc = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm"))
            dy0 = 0.98
            dy_title = 0.27
            dy_newline = 0.17
            
            dx0 = 0.02
            dx_narratracc = 0.01
            dx_line = 0.05
            p_line = 0.75
            dx_text = 0.03

            linewidth = 1.4
            
            narratracc = narratracc +
                annotate("text",
                         x=dx0,
                         y=dy0,
                         label=latex2exp::TeX("\\textbf{NarraTRACC}"),
                         size=2.4, hjust=0, vjust=1,
                         family="Lato",
                         color=IPCCgrey35)

            nNarraTRACC = length(NarraTRACC)
            
            for (k in 1:nNarraTRACC) {
                y = dy0 - dy_title - dy_newline*(k-1)
                label = paste0(NarraTRACC[[k]]["name"], " : ", NarraTRACC[[k]]["description"])
                narratracc = narratracc +
                    annotate("line",
                             x=dx0 + dx_narratracc + c(0, dx_line),
                             y=y,
                             color=NarraTRACC[[k]]["color_light"],
                             linewidth=linewidth,
                             lineend="round") +
                    annotate("line",
                             x=dx0 + dx_narratracc + c(0, dx_line*p_line),
                             y=y,
                             color=NarraTRACC[[k]]["color"],
                             linewidth=linewidth,
                             lineend="round") +
                    annotate("text",
                             x=dx0 + dx_narratracc + dx_line + dx_text,
                             y=y,
                             label=label,
                             size=2.4, hjust=0, vjust=0.55,
                             family="Lato",
                             color=IPCCgrey35)
            }

            narratracc = narratracc +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            info_herd = add_sheep(info_herd,
                                  sheep=narratracc,
                                  id="legend",
                                  height=info_legend_height,
                                  verbose=verbose)
            
            #### Info logo _______________________________________________________
            logo_plan = matrix(c("Explore2",
                                 "TRACC",
                                 "RWL"),
                               ncol=1,
                               byrow=TRUE)

            logo_Explore2_height = 1
            logo_TRACC_height = 1
            logo_RWL_height = 1.2
            
            logo_herd = bring_grass(verbose=verbose)
            logo_herd = plan_of_herd(logo_herd, logo_plan,
                                     verbose=verbose)
            
            Explore2_grob = grid::rasterGrob(png::readPNG(logo_info[["Explore2"]]["path"]),
                                             vjust=0.6,
                                             hjust=0.5,
                                             height=0.8)
            logo_herd = add_sheep(logo_herd,
                                  sheep=Explore2_grob,
                                  id="Explore2",
                                  height=logo_Explore2_height,
                                  verbose=verbose)
            
            TRACC_grob = grid::rasterGrob(png::readPNG(logo_info[["TRACC"]]["path"]),
                                          vjust=0.5,
                                          hjust=0.5,
                                          height=0.7)        
            logo_herd = add_sheep(logo_herd,
                                  sheep=TRACC_grob,
                                  id="TRACC",
                                  height=logo_TRACC_height,
                                  verbose=verbose)

            if (wl["RWLclean"] == "RWL-40") {
                shift_C = 0.6
                dx_shift = 0
            } else if (wl["RWLclean"] == "RWL-27") {
                shift_C = 1.1
                dx_shift = 0.2
            }
            
            RWL = ggplot() +
                theme_void() +
                coord_fixed() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                annotate("text",
                         x=1.5 - dx_shift, y=0.57,
                         label="vivre à",
                         size=3, hjust=0, vjust=0.5,
                         family="Lato",
                         color=wl["color"]) +
                annotate("text",
                         x=2.5 - dx_shift, y=0.55,
                         label=latex2exp::TeX(paste0("\\textbf{+", wl["RWL"], "}")),
                         size=6, hjust=0, vjust=0.5,
                         family="Raleway",
                         color=wl["color"]) +
                annotate("text",
                         x=2.5+shift_C - dx_shift, y=0.55,
                         label=latex2exp::TeX("\\textbf{\\small{°C}}"),
                         size=6, hjust=0, vjust=0.5,
                         family="Raleway",
                         color=wl["color"]) +
                scale_x_continuous(limits=c(0, 5),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            logo_herd = add_sheep(logo_herd,
                                  sheep=RWL,
                                  id="RWL",
                                  height=logo_RWL_height,
                                  verbose=verbose)
            
            info_herd = add_sheep(info_herd,
                                  sheep=logo_herd,
                                  id="logo",
                                  height=1,
                                  width=info_logo_width,
                                  verbose=verbose)

            
            herd = add_sheep(herd,
                             sheep=info_herd,
                             id="info",
                             height=info_height,
                             width=width,
                             verbose=verbose)


### Climate __________________________________________________________
            climate_plan = matrix(c(
                "title", "title", 
                "climate_delta", "climate_table"
            ), ncol=2, byrow=TRUE)
            
            climate_herd = bring_grass(verbose=verbose)
            climate_herd = plan_of_herd(climate_herd, climate_plan,
                                        verbose=verbose)

            climate_title_height = 0.1
            climate_delta_height = 1
            
            climate_delta_width = 1
            climate_table_width = 1

#### Title __________________________________________________________
            title_text = paste0("(", letters[id_letter], ") Changements de précipitation (%) en fonction des changements de température (°C)")
            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=0, y=0.5,
                         label=latex2exp::TeX(title_text),
                         size=3, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey23)
            
            climate_herd = add_sheep(climate_herd,
                                     sheep=title,
                                     id="title",
                                     height=climate_title_height,
                                     verbose=verbose)
            
#### Climate plot ____________________________________________________
            climate_delta_plan = matrix(c(
                "dR", "hiver_title", "space", "ete_title",
                "dR",   "hiver",       "space", "ete",
                "void", "dT",          "space", "dT" 
            ), ncol=4, byrow=TRUE)

            climate_delta_variable_title_height = 0.1
            climate_delta_variable_height = 1
            climate_delta_dT_height = 0.1
            
            climate_delta_variable_width = 1
            climate_delta_space_width = 0.03
            climate_delta_dR_width = 0.1
            
            climate_delta_herd = bring_grass(verbose=verbose)
            climate_delta_herd = plan_of_herd(climate_delta_herd, climate_delta_plan,
                                              verbose=verbose)

            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=void(),
                                           id="void",
                                           verbose=verbose)
            
            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=void(),
                                           id="space",
                                           width=climate_delta_space_width,
                                           verbose=verbose)

##### axis __________________________________________________________
            dx_dR = 0.06
            dx_dT = 0.2
            
            axis_dR = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=0.5, y=dx_dR,
                         label="Δ",
                         size=2.4, hjust=0, vjust=0.42,
                         angle=90,
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.5, y=dx_dR,
                         label=latex2exp::TeX("\\; $\\Delta$R : Changements de précipitations (%)"),
                         size=2.4, hjust=0, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35)
            
            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=axis_dR,
                                           id="dR",
                                           width=climate_delta_dR_width,
                                           verbose=verbose)
            
            axis_dT = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=dx_dT, y=0.5,
                         label="Δ",
                         size=2.4, hjust=0, vjust=0.42,
                         family="Noto Sans",
                         color=IPCCgrey35) +
                annotate("text",
                         x=dx_dT, y=0.5,
                         label=latex2exp::TeX("\\; T : Changements de température (°C)"),
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey35)

            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=axis_dT,
                                           id="dT",
                                           height=climate_delta_dT_height,
                                           verbose=verbose)

##### hiver __________________________________________________________
            dx_title = 0.46 
            dy_title = 0.25
            
            delta_title = ggplot() + theme_void() +
                theme(plot.background=element_rect(fill=IPCCgrey97,
                                                   color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=dx_title, y=dy_title,
                         label="mode_cool",
                         size=2.4, hjust=0, vjust=0.15,
                         family="Material Symbols Outlined",
                         color=IPCCgrey40) +
                annotate("text",
                         x=dx_title, y=dy_title,
                         label=latex2exp::TeX("\\;\\; \\textbf{HIVER}"),
                         size=2.4, hjust=0, vjust=0,
                         family="Lato",
                         color=IPCCgrey40)
            
            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=delta_title,
                                           id="hiver_title",
                                           height=climate_delta_variable_title_height,
                                           verbose=verbose)


            panel_delta_variable = function (dataEX_criteria_sl_wl,
                                             season) {

                expand = function (X, fact=0.05) {
                    range_X = max(X)-min(X)
                    padding = range_X*fact
                    X = c(min(X)-padding, max(X)+padding)
                    return (X)
                }


                                
                get_labels = function(x, unit, is_unit_plurial, add_unit_space, Palette, dColor) {
                    nColor = length(Palette)
                    unit_suffixed = ifelse(!is_unit_plurial, unit,
                                    ifelse(x != 0, paste0(unit, "s"), unit))
                    unitHTML = ifelse(add_unit_space,
                                      paste0("<span style='font-size:4pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"),
                                      paste0("<span style='font-size:6pt'>", unit_suffixed, "</span>"))

                    color = ifelse(x < 0, Palette[1 + dColor],
                            ifelse(x > 0, Palette[nColor - dColor], ""))

                    x_text = ifelse(x > 0, paste0("+", x), as.character(x))

                    result = ifelse(x < 0 | x > 0,
                                    paste0("<span style='color:", color, "'><b>", x_text, "</b>", unitHTML, "</span>"),
                                    paste0("<span style=''><b>", x_text, "</b></span>"))

                    return (result)
                }

                get_labels_deltaR = function(x) {
                    result = get_labels(x, unit="%",
                                        is_unit_plurial=FALSE,
                                        add_unit_space=TRUE,
                                        Palette=get_IPCC_Palette("hydro_10"),
                                        dColor=1)
                    return (result)
                }
                
                get_labels_deltaT = function(x) {
                    result = get_labels(x, unit="°C",
                                        is_unit_plurial=FALSE,
                                        add_unit_space=TRUE,
                                        Palette=get_IPCC_Palette("temperature_10"),
                                        dColor=1)
                    return (result)
                }

                TMm_range = c(min(dataEX_criteria_sl_wl[[paste0("delta_TMm_", season)]]),
                              max(dataEX_criteria_sl_wl[[paste0("delta_TMm_", season)]]))
                TMm_range = expand(TMm_range)
                
                RR_range = c(min(dataEX_criteria_sl_wl[[paste0("delta_RR_", season)]]),
                             max(dataEX_criteria_sl_wl[[paste0("delta_RR_", season)]]))
                RR_range = expand(RR_range)
                
                delta_variable = ggplot() +
                    theme(plot.margin=margin(t=0, r=2,
                                             b=0, l=1, "mm"))
                if (RR_range[1] <= 0 & 0 <= RR_range[2]) {
                    delta_variable = delta_variable +
                        annotate("line",
                                 x=TMm_range, y=0,
                                 color=IPCCgrey60,
                                 linewidth=0.33)
                }
                if (RR_range[1] <= 0 & 0 <= RR_range[2]) {
                    delta_variable = delta_variable +
                        annotate("line",
                                 x=0, y=RR_range,
                                 color=IPCCgrey60,
                                 linewidth=0.33)
                }
                
                delta_variable = delta_variable +    
                    theme_IPCC(is_plot.background=TRUE,
                               is_panel.grid.major.x=TRUE,
                               is_panel.grid.major.y=TRUE,
                               is_axis.line.x=FALSE,
                               is_axis.ticks.x=FALSE,
                               is_axis.ticks.y=FALSE,
                               axis.ticks.length.x=0.8) +
                    geom_point(data=dataEX_criteria_sl_wl,
                               aes(x=get(paste0("delta_TMm_", season)),
                                   y=get(paste0("delta_RR_", season))),
                               size=1, color=IPCCgrey50) + 
                    scale_x_continuous(limits=TMm_range,
                                       labels=get_labels_deltaT,
                                       expand=c(0, 0)) +
                    
                    scale_y_continuous(limits=RR_range,
                                       labels=get_labels_deltaR,
                                       expand=c(0, 0))

                return (delta_variable)
            }

            delta_variable = panel_delta_variable(dataEX_criteria_sl_wl, "DJF")
    
            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=delta_variable,
                                           id="hiver",
                                           height=climate_delta_variable_height,
                                           width=climate_delta_variable_width,
                                           verbose=verbose)

##### ete __________________________________________________________
            delta_title = ggplot() + theme_void() +
                theme(plot.background=element_rect(fill=IPCCgrey97,
                                                   color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=dx_title, y=dy_title,
                         label="sunny",
                         size=2.4, hjust=0, vjust=0.15,
                         family="Material Symbols Rounded",
                         color=IPCCgrey40) +
                annotate("text",
                         x=dx_title, y=dy_title,
                         label=latex2exp::TeX("\\;\\; \\textbf{ÉTÉ}"),
                         size=2.4, hjust=0, vjust=0,
                         family="Lato",
                         color=IPCCgrey40)
            
            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=delta_title,
                                           id="ete_title",
                                           height=climate_delta_variable_title_height,
                                           verbose=verbose)


            delta_variable = panel_delta_variable(dataEX_criteria_sl_wl, "JJA")
            
            climate_delta_herd = add_sheep(climate_delta_herd,
                                           sheep=delta_variable,
                                           id="ete",
                                           height=climate_delta_variable_height,
                                           width=climate_delta_variable_width,
                                           verbose=verbose)

            climate_herd = add_sheep(climate_herd,
                                     sheep=climate_delta_herd,
                                     id="climate_delta",
                                     height=1,
                                     width=climate_delta_width,
                                     verbose=verbose)
            
            #### Climate table ___________________________________________________
            climate_table_plan = matrix(c(
                "climate_table_content",
                "climate_table_info"
            ), ncol=1, byrow=TRUE)

            climate_table_content_height = 1
            climate_table_info_height = 0.2

            climate_table_herd = bring_grass(verbose=verbose)
            climate_table_herd = plan_of_herd(climate_table_herd, climate_table_plan,
                                              verbose=verbose)

##### table ___________________________________________________________

            column_id = c("delta_TMm_DJF", "delta_RR_DJF", "delta_TMm_JJA", "delta_RR_JJA")
            column_name = c("T", "R", "T", "R")
            column_icon = c("mode_cool", "mode_cool", "sunny", "sunny")
            column_icon_font = c("Material Symbols Outlined", "Material Symbols Outlined", "Material Symbols Rounded", "Material Symbols Rounded")
            column_unit = c("°C", "%", "°C", "%")
            
            row_id = c("min",
                       "median",
                       "max",
                       sapply(NarraTRACC, "[", "Chain"))
            row_name = c("Minimum",
                         "Médiane",
                         "Maximum",
                         sapply(NarraTRACC, "[", "name"))
            row_color = c(rep(IPCCgrey35, 3),
                          sapply(NarraTRACC, "[", "color"))

            
            dy_column_title = 2
            dx_column = 1

            dx_row_title = 2
            dy_row = 1

            nCol = length(column_id) 
            nRow = length(row_id)            

            print(dataEX_criteria_stat)
            
            table = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 10),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 10),
                                   expand=c(0, 0))



            
            # for (rr in 1:nRow) {
            #     for (cc in 1:nCol) {
                    


                    
            #     }
            # }
            
            

            
            
            climate_table_herd = add_sheep(climate_table_herd,
                                           sheep=table,
                                           id="climate_table_content",
                                           height=climate_table_content_height,
                                           verbose=verbose)

##### info ___________________________________________________________
            dy_newline = 0.27
            info_text = c("Changements projetés en température et précipitations (référence : 1976-2005)",
                          "Les points correspondent aux 17 projections à partir desquelles sont calculées",
                          "les statistiques (minimum, médiane, maximum)")
            
            info = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            for (k in 1:length(info_text)) {
                info = info +             
                    annotate("text",
                             x=0.5,
                             y=0.9 - dy_newline*(k-1),
                             label=info_text[k],
                             size=2, hjust=0.5, vjust=1,
                             family="Lato",
                             color=IPCCgrey50)
            }
            
            climate_table_herd = add_sheep(climate_table_herd,
                                           sheep=info,
                                           id="climate_table_info",
                                           height=climate_table_info_height,
                                           verbose=verbose)
            
            climate_herd = add_sheep(climate_herd,
                                     sheep=climate_table_herd,
                                     id="climate_table",
                                     width=climate_table_width,
                                     height=1,
                                     verbose=verbose)
            
            herd = add_sheep(herd,
                             sheep=climate_herd,
                             id="climate",
                             height=climate_height,
                             width=width,
                             verbose=verbose)

            ### Hydro map ________________________________________________________
            hydroMap_plan = matrix(c(
                "etiage_title", "recharge_title", "crue_title", "colorbar",
                "etiage_map",   "recharge_map",   "crue_map",   "colorbar",
                "legend",       "legend",         "legend",     "colorbar"
            ), ncol=4, byrow=TRUE)

            hydroMap_variable_title_height = 0.2
            hydroMap_variable_map_height = 1
            hydroMap_legend_height = 0.15

            hydroMap_variable_width = 1
            hydroMap_colorbar_width = 0.3
            
            hydroMap_herd = bring_grass(verbose=verbose)
            hydroMap_herd = plan_of_herd(hydroMap_herd, hydroMap_plan,
                                         verbose=verbose)

            #### etiage __________________________________________________________
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="etiage_title",
                                      height=hydroMap_variable_title_height,
                                      verbose=verbose)

            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="etiage_map",
                                      height=hydroMap_variable_map_height,
                                      width=hydroMap_variable_width,
                                      verbose=verbose)

            #### recharge ________________________________________________________
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="recharge_title",
                                      height=hydroMap_variable_title_height,
                                      verbose=verbose)

            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="recharge_map",
                                      height=hydroMap_variable_map_height,
                                      width=hydroMap_variable_width,
                                      verbose=verbose)

            #### crue ____________________________________________________________
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="crue_title",
                                      height=hydroMap_variable_title_height,
                                      verbose=verbose)

            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="crue_map",
                                      height=hydroMap_variable_map_height,
                                      width=hydroMap_variable_width,
                                      verbose=verbose)
            
            #### other ___________________________________________________________        
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="legend",
                                      height=hydroMap_legend_height,
                                      verbose=verbose)

            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=contour(),
                                      id="colorbar",
                                      width=hydroMap_colorbar_width,
                                      verbose=verbose)
            
            herd = add_sheep(herd,
                             sheep=hydroMap_herd,
                             id="hydroMap",
                             height=hydroMap_height,
                             width=width,
                             verbose=verbose)


            ### Hydro QM _________________________________________________________
            hydroQM_plan = matrix(c(
                "title",      "title",         "title",         "title",
                "void",       "river_name_1",  "river_name_2",  "river_name_3", 
                "ref_info",   "river_ref_1",   "river_ref_2",   "river_ref_3",   
                "delta_info", "river_delta_1", "river_delta_2", "river_delta_3"
            ), ncol=4, byrow=TRUE)


            hydroQM_title_height = 0.15
            hydroQM_river_name_height = 0.2
            hydroQM_river_graph_height = 1
            
            hydroQM_river_width = 1
            hydroQM_info_width = 0.2  
            
            hydroQM_herd = bring_grass(verbose=verbose)
            hydroQM_herd = plan_of_herd(hydroQM_herd, hydroQM_plan,
                                        verbose=verbose)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=contour(),
                                     id="title",
                                     height=hydroQM_title_height,
                                     verbose=verbose)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=void(),
                                     id="void",
                                     verbose=verbose)
            
            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=contour(),
                                     id="ref_info",
                                     width=hydroQM_info_width,
                                     verbose=verbose)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=contour(),
                                     id="delta_info",
                                     width=hydroQM_info_width,
                                     verbose=verbose)
            
            Rivers = 1:3
            nRiver = length(Rivers)
            for (k in 1:nRiver) {
                hydroQM_herd = add_sheep(hydroQM_herd,
                                         sheep=contour(),
                                         id=paste0("river_name_", k),
                                         height=hydroQM_river_name_height,
                                         width=hydroQM_river_width,
                                         verbose=verbose)

                hydroQM_herd = add_sheep(hydroQM_herd,
                                         sheep=contour(),
                                         id=paste0("river_ref_", k),
                                         height=hydroQM_river_graph_height,
                                         width=hydroQM_river_width,
                                         verbose=verbose)
                
                hydroQM_herd = add_sheep(hydroQM_herd,
                                         sheep=contour(),
                                         id=paste0("river_delta_", k),
                                         height=hydroQM_river_graph_height,
                                         width=hydroQM_river_width,
                                         verbose=verbose)
            }
            

            herd = add_sheep(herd,
                             sheep=hydroQM_herd,
                             id="hydroQM",
                             height=hydroQM_height,
                             width=width,
                             verbose=verbose)

            ### Hydro table ______________________________________________________
            hydroTable_plan = matrix(c(
                "title",
                "content"
            ), ncol=1, byrow=TRUE)

            hydroTable_title_height = 0.2
            hydroTable_content_height = 1
            
            hydroTable_herd = bring_grass(verbose=verbose)
            hydroTable_herd = plan_of_herd(hydroTable_herd, hydroTable_plan,
                                           verbose=verbose)

            hydroTable_herd = add_sheep(hydroTable_herd,
                                        sheep=contour(),
                                        id="title",
                                        height=hydroTable_title_height,
                                        verbose=verbose)
            
            hydroTable_herd = add_sheep(hydroTable_herd,
                                        sheep=contour(),
                                        id="content",
                                        height=hydroTable_content_height,
                                        verbose=verbose)

            herd = add_sheep(herd,
                             sheep=hydroTable_herd,
                             id="hydroTable",
                             height=hydroTable_height,
                             width=width,
                             verbose=verbose)


            ### Foot _____________________________________________________________
            herd = add_sheep(herd,
                             sheep=contour(),
                             id="foot",
                             height=foot_height,
                             width=width,
                             verbose=verbose)



        # aaaa = ggplot() + theme_void_Lato() +
        #     theme(plot.margin=margin(t=0, r=0,
        #                              b=0, l=0, "mm")) +                      
        #     annotate("text",
        #              x=0,
        #              y=0.5,
        #              label=TeX("Changements futurs relativement peu marqués"),
        #                  # "Argousier : Débits réduits et étiages sévères",
        #              size=2.4, hjust=0, vjust=0,
        #              family="Lato",
        #              color=IPCCgrey35)+
        #         scale_x_continuous(limits=c(0, 1),
        #                            expand=c(0, 0)) +
        #         scale_y_continuous(limits=c(0, 1),
        #                            expand=c(0, 0))
            
        #     plan = matrix(c(
        #         "aaaa",
        #         "void"
        #     ), ncol=1, byrow=TRUE)


        #     herd = bring_grass(verbose=verbose)
        #     herd = plan_of_herd(herd, plan,
        #                         verbose=verbose)

        #     herd = add_sheep(herd,
        #                      sheep=aaaa,
        #                      id="aaaa",
        #                      height=10,
        #                      width=width,
        #                      verbose=verbose)

        #     herd = add_sheep(herd,
        #                      sheep=contour(),
        #                      id="void",
        #                      height=height-10,
        #                      width=width,
        #                      verbose=verbose)

            
            
            ## Plot ______________________________________________________________
            res = return_to_sheepfold(herd,
                                      page_margin=page_margin,
                                      paper_size="A4",
                                      hjust=0, vjust=1,
                                      verbose=verbose)        
            plot = res$plot
            paper_size = res$paper_size

            filename = paste0(sh, "_", wl["RWLclean"], "_projection_datasheet.pdf")

            if (!(file.exists(figdir))) {
                dir.create(figdir, recursive=TRUE)
            }

            # width_in = paper_size[1] / 2.54
            # height_in = paper_size[2] / 2.54
            # full_path = file.path(figdir, filename)

            # cairo_pdf(filename=full_path,
            #           width=width_in,
            #           height=height_in)
            
            # showtext::showtext_begin()
            # grid::grid.draw(plot)
            # showtext::showtext_end()
            # dev.off()



            # showtext::showtext_auto()
            # showtext::showtext_opts(dpi = 300)
            # sysfonts::font_add_google("Lato", "Lato")
            
            # ggplot2::ggsave(plot=plot,
            #                 path=figdir,
            #                 filename=filename,
            #                 width=paper_size[1],
            #                 height=paper_size[2], units='cm',
            #                 dpi=300,
            #                 device=cairo_pdf)


            
            Cairo::CairoPDF(file=file.path(figdir, filename),
                            width=paper_size[1]/2.54,
                            height=paper_size[2]/2.54,
                            title="")
            grid::grid.draw(plot)
            dev.off()


            # grDevices::cairo_pdf(file=file.path(figdir, filename),
            #                      width=paper_size[1]/2.54,
            #                      height=paper_size[2]/2.54)
            # grid::grid.newpage()
            # grid::grid.draw(plot)
            # dev.off()
            
            stop()
        }
    }
    return (NULL)
}
