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
                                     Secteurs,
                                     dataEX_criteria_climate,
                                     dataEX_criteria_hydro,
                                     dataEX_serie_hydro,
                                     # metaEX_criteria_climate,
                                     # metaEX_criteria_hydro,
                                     # metaEX_serie_hydro,
                                     WL,
                                     NarraTRACC,
                                     delta_prob=0,
                                     limit_conf_pct=80,
                                     icons=NULL,
                                     logo_info="",
                                     Pages=NULL,
                                     Shapefiles=NULL,
                                     Shapefiles_mini=NULL,
                                     figdir="",
                                     verbose=FALSE) {

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    height = 29.7 - page_margin["t"] - page_margin["b"]
    width = 21 - page_margin["l"] - page_margin["r"]
    
    info_height = 4
    climate_height = 6
    hydroMap_height = 6
    hydroQM_height = 7.1
    foot_height = 0.7
    hydroTable_height = height - info_height - climate_height - hydroMap_height - hydroQM_height - foot_height
    
    plan = matrix(c(
        "info",
        "climate", 
        "hydroMap",
        "hydroQM",
        "hydroTable",
        "foot"
    ), ncol=1, byrow=TRUE)

    Palette_hydro = get_IPCC_Palette("hydro_10")
    Palette_temperature = get_IPCC_Palette("temperature_10")
    dColor = 1
    nColor = length(Palette_temperature)
    
    is_numeric = names(dataEX_criteria_climate)[sapply(dataEX_criteria_climate, is.numeric)]
    
    dataEX_criteria_climate_stat =
        dplyr::summarise(dplyr::group_by(dataEX_criteria_climate,
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

    dataEX_criteria_hydro = tidyr::unite(dataEX_criteria_hydro,
                                         "Chain",
                                         "EXP", "GCM", "RCM",
                                         "BC", "HM",
                                         sep="_", remove=FALSE)



    dataEX_criteria_hydro_conf =
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro,
                                         code, GWL, EXP),
                         dplyr::across(.cols=where(is.numeric),
                                       .fns=~sum(.x>0)/dplyr::n()*100,
                                       .names="{.col}_above_pct"),
                         dplyr::across(.cols=where(is.numeric),
                                       .fns=~sum(.x<0)/dplyr::n()*100,
                                       .names="{.col}_below_pct"),
                         .groups="drop")

    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro,
                                         code, SH, GWL, EXP, GCM, RCM, BC),
                         dplyr::across(.cols=where(is.numeric),
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro_mean,
                                         code, SH, GWL, EXP, GCM, RCM),
                         dplyr::across(.cols=where(is.numeric),
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro_mean,
                                         code, SH, GWL, EXP, GCM),
                         dplyr::across(.cols=where(is.numeric),
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro_mean,
                                         code, SH, GWL, EXP),
                         dplyr::across(.cols=where(is.numeric),
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")  
    
    SH = unique(substr(Stations$code, 1, 2))
    nSH = length(SH) 
    
    nWL = length(WL)

    for (i in 1:nSH) {
        sh = SH[i]
        if (verbose) {
            print(paste0(i, "/", nSH, " so ", round(i/nSH*100, 1), "% done -> ", sh))
        }
        
        secteur = Secteurs[Secteurs$id_secteur == sh,]
        
        Stations_sh = dplyr::filter(Stations, substr(code, 1, 2) == sh)
        
        secteurHydro_shp_mini = Shapefiles_mini$secteurHydro[Shapefiles_mini$secteurHydro$CdSecteurH == sh,]
        secteurHydro_shp = Shapefiles$secteurHydro[Shapefiles$secteurHydro$CdSecteurH == sh,]
        
        # Chain = unique(Projections$Chain)
        # nChain = length(Chain)

        for (j in 1:nWL) {
            id_letter = 1
            
            wl = WL[[j]]
            print(paste0(j, "/", nWL, " so ", round(j/nWL*100, 1), "% done -> ", wl["RWL"]))
            
            dataEX_criteria_climate_sh_wl = dplyr::filter(dataEX_criteria_climate,
                                                          SH==sh, GWL == wl["GWLclean"])
            dataEX_criteria_hydro_sh_wl = dplyr::filter(dataEX_criteria_hydro,
                                                        SH==sh, GWL == wl["GWLclean"])

            dataEX_serie_hydro_sh_wl = dataEX_serie_hydro
            for (j in 1:length(dataEX_serie_hydro_sh_wl)) {
                dataEX_serie_hydro_sh_wl[[j]] = dplyr::filter(dataEX_serie_hydro_sh_wl[[j]],
                                                              SH==sh, GWL == wl["GWLclean"])
            }

            dataEX_criteria_climate_stat_sh_wl = dplyr::filter(dataEX_criteria_climate_stat,
                                                               SH==sh, GWL==wl["GWLclean"])
            dataEX_criteria_hydro_conf_sh_wl = dplyr::filter(dataEX_criteria_hydro_conf,
                                                             SH == sh, GWL==wl["GWLclean"])
            dataEX_criteria_hydro_mean_sh_wl = dplyr::filter(dataEX_criteria_hydro_mean,
                                                             SH == sh, GWL==wl["GWLclean"])
            
            
            ## PAGE _________________________________________________________
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan,
                                verbose=verbose)

            ### Info ________________________________________________________
            info_plan = matrix(c(
                "minimap", "title", "logo",
                "minimap", "legend", "logo"
            ), ncol=3, byrow=TRUE)

            info_title_height = 1.5
            info_legend_height = 1
            
            info_minimap_width = 1
            info_title_width = 2
            info_logo_width = 1

            info_herd = bring_grass(verbose=verbose)
            info_herd = plan_of_herd(info_herd, info_plan,
                                     verbose=verbose)

            xlim = c(90000, 1250000)
            ylim = c(6040000, 7120000)

            margin_minimap = margin(t=0, r=0, b=2, l=0, unit="mm")
            
            echelle = c(0, 100, 250)
            x_echelle_pct = 2
            y_echelle_pct = 4
            echelle_line_size = 0.3
            echelle_size = 2
            echelle_km_size = 1.8
            echelle_tick_height = 2

            ymin_km_shift = 2
            ymin_km_value_shift = 1
            
            cf = coord_fixed()
            cf$default = TRUE
            minimap = ggplot() + theme_void() + cf +
                theme(plot.margin=margin_minimap) +
                geom_sf(data=Shapefiles_mini$france,
                        color=NA,
                        fill=IPCCgrey97) +
                geom_sf(data=Shapefiles_mini$river,
                        color="white",
                        alpha=1,
                        fill=NA,
                        linewidth=0.65,
                        na.rm=TRUE) +
                geom_sf(data=Shapefiles_mini$river,
                        color=INRAElightcyan,
                        alpha=1,
                        fill=NA,
                        linewidth=0.3,
                        na.rm=TRUE) +
                geom_sf(data=Shapefiles_mini$bassinHydro,
                        color=IPCCgrey85,
                        fill=NA,
                        size=0.25) +
                geom_sf(data=Shapefiles_mini$france,
                        color=IPCCgrey40,
                        fill=NA,
                        linewidth=0.3)
            
            xmin = gpct(x_echelle_pct, xlim, shift=TRUE)
            xint = echelle*1E3
            ymin = gpct(y_echelle_pct, ylim, shift=TRUE)
            ymin_km = gpct(max(c(y_echelle_pct-ymin_km_shift, 0)), ylim, shift=TRUE)
            ymax = ymin + gpct(echelle_tick_height, ylim)

            minimap = minimap +
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
                minimap = minimap +
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

            minimap = minimap +
                geom_sf(data=secteurHydro_shp_mini,
                        color="white",
                        fill=NA,
                        linewidth=1.05) +
                geom_sf(data=secteurHydro_shp_mini,
                        color=wl["color"],
                        fill=NA,
                        linewidth=0.45)

            minimap = minimap +
                coord_sf(xlim=xlim, ylim=ylim,
                         expand=FALSE)

            info_herd = add_sheep(info_herd,
                                  sheep=minimap,
                                  id="minimap",
                                  width=info_minimap_width,
                                  verbose=verbose)

            #### Info text _______________________________________________________
            dy_newline = 0.24
            dy_region = 0.29
            dy_basin = 0.15
            
            title_text = strwrap(paste0(sh, " - ", secteur$secteur), width=42)
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

            region = paste0("\\textbf{Region hydro.} \\; ", secteur$region)
            basin = paste0("\\textbf{Bassin de gestion} \\; ", secteur$bassin)
            
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

            linewidth = 1.1
            
            narratracc = narratracc +
                annotate("text",
                         x=dx0,
                         y=dy0,
                         label=latex2exp::TeX("\\textbf{NarraTRACC}"),
                         size=2.2, hjust=0, vjust=1,
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
                             size=2.2, hjust=0, vjust=0.55,
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
            title_text = paste0("(", letters[id_letter], ") Changements relatifs de précipitation (%) en fonction des changements de température (°C)")
            id_letter = id_letter + 1
            
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
            climate_delta_dT_height = 0.13
            
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
                         label=latex2exp::TeX("\\; R : Changements de précipitations (%)"),
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
                         x=dx_dT, y=0.55,
                         label="Δ",
                         size=2.4, hjust=0, vjust=0.42,
                         family="Noto Sans",
                         color=IPCCgrey35) +
                annotate("text",
                         x=dx_dT, y=0.55,
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

            get_labels_HTML = function(x, unit, is_unit_plurial, add_unit_space, Palette, dColor) {
                nColor = length(Palette)
                unit_suffixed = ifelse(!is_unit_plurial, unit,
                                ifelse(x != 0, paste0(unit, "s"), unit))
                result = ifelse(add_unit_space,
                                  paste0("<span style='font-size:4pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"),
                                  paste0("<span style='font-size:6pt'>", unit_suffixed, "</span>"))

                color = ifelse(x < 0, Palette[1 + dColor],
                        ifelse(x > 0, Palette[nColor - dColor], ""))

                x_text = ifelse(x > 0, paste0("+", x), as.character(x))

                result = ifelse(x < 0 | x > 0,
                                paste0("<span style='color:", color, "'><b>", x_text, "</b>", result, "</span>"),
                                paste0("<span style=''><b>", x_text, "</b></span>"))

                return (result)
            }
            
            panel_delta_variable = function (dataEX_criteria_climate_sh_wl,
                                             season) {

                expand = function (X, fact=0.05) {
                    range_X = max(X)-min(X)
                    padding = range_X*fact
                    X = c(min(X)-padding, max(X)+padding)
                    return (X)
                }

                get_labels_deltaR = function(x) {
                    result = get_labels_HTML(x, unit="%",
                                             is_unit_plurial=FALSE,
                                             add_unit_space=TRUE,
                                             Palette=Palette_hydro,
                                             dColor=dColor)
                    return (result)
                }
                
                get_labels_deltaT = function(x) {
                    result = get_labels_HTML(x, unit="°C",
                                             is_unit_plurial=FALSE,
                                             add_unit_space=TRUE,
                                             Palette=Palette_temperature,
                                             dColor=dColor)
                    return (result)
                }

                TMm_range = c(min(dataEX_criteria_climate_sh_wl[[paste0("delta_TMm_", season)]]),
                              max(dataEX_criteria_climate_sh_wl[[paste0("delta_TMm_", season)]]))
                TMm_range = expand(TMm_range)
                
                RR_range = c(min(dataEX_criteria_climate_sh_wl[[paste0("delta_RR_", season)]]),
                             max(dataEX_criteria_climate_sh_wl[[paste0("delta_RR_", season)]]))
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
                    scale_x_continuous(limits=TMm_range,
                                       labels=get_labels_deltaT,
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=RR_range,
                                       labels=get_labels_deltaR,
                                       expand=c(0, 0))

                NarraTRACC_climateChains = sapply(NarraTRACC, "[", "climateChain")
                dataEX_criteria_climate_sh_wl_NO_narratrac =
                    dplyr::filter(dataEX_criteria_climate_sh_wl,
                                  !(climateChain %in% NarraTRACC_climateChains))

                delta_variable = delta_variable +
                    geom_point(data=dataEX_criteria_climate_sh_wl_NO_narratrac,
                               aes(x=get(paste0("delta_TMm_", season)),
                                   y=get(paste0("delta_RR_", season))),
                               size=1, color=IPCCgrey67)

                for (k in 1:nNarraTRACC) {
                    narratrac = NarraTRACC[[k]]
                    dataEX_criteria_climate_sh_wl_narratrac = dplyr::filter(dataEX_criteria_climate_sh_wl, climateChain == narratrac["climateChain"])
                    delta_variable = delta_variable +
                        geom_point(data=dataEX_criteria_climate_sh_wl_narratrac,
                                   aes(x=get(paste0("delta_TMm_", season)),
                                       y=get(paste0("delta_RR_", season))),
                                   size=1.4, color=IPCCgrey97) + 
                    geom_point(data=dataEX_criteria_climate_sh_wl_narratrac,
                                   aes(x=get(paste0("delta_TMm_", season)),
                                       y=get(paste0("delta_RR_", season))),
                                   size=1, color=narratrac["color"])
                }
    
                return (delta_variable)
            }

            delta_variable = panel_delta_variable(dataEX_criteria_climate_sh_wl, "DJF")
    
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


            delta_variable = panel_delta_variable(dataEX_criteria_climate_sh_wl, "JJA")
            
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
            climate_table_info_height = 0.3

            climate_table_herd = bring_grass(verbose=verbose)
            climate_table_herd = plan_of_herd(climate_table_herd, climate_table_plan,
                                              verbose=verbose)

##### table ___________________________________________________________
            column_id = c("", "delta_TMm_DJF", "delta_RR_DJF", "delta_TMm_JJA", "delta_RR_JJA")
            column_name = c("", "T", "R", "T", "R")
            column_icon = c("", "mode_cool", "mode_cool", "sunny", "sunny")
            column_icon_font = c("", "Material Symbols Outlined", "Material Symbols Outlined", "Material Symbols Rounded", "Material Symbols Rounded")
            column_unit = c("", "°C", "%", "°C", "%")
            
            row_id = c("",
                       "min",
                       "median",
                       "max",
                       sapply(NarraTRACC, "[", "climateChain"))
            row_name = c("",
                         "minimum",
                         "médiane",
                         "maximum",
                         sapply(NarraTRACC, "[", "name"))
            row_color = c("",
                          rep(IPCCgrey35, 3),
                          sapply(NarraTRACC, "[", "color"))


            get_labels_TeX = function(x, unit, is_unit_plurial, add_unit_space) {
                unit_suffixed = ifelse(!is_unit_plurial, unit,
                                ifelse(x != 0, paste0(unit, "s"), unit))
                result = ifelse(add_unit_space,
                                paste0("\\small{ ", unit_suffixed, "}"),
                                paste0(unit_suffixed))
                x_text = ifelse(x > 0, paste0("+", x), as.character(x))
                result = ifelse(x < 0 | x > 0,
                                paste0("\\textbf{", x_text, "}", result, ""),
                                paste0("\\textbf{", x_text, "}"))

                return (result)
            }
            
            xmax = 10
            ymax = 10
    
            dx_left_line = 0.1
            dx_left_text = 0.1
            dx_right_text = 0.4
            dx_left_narratrac_text = 0.35
            dx_left_narratrac_circle = 0.15
            
            dy_column_title = 1.5
            dx_row_title = 2.6

            dx_delta = 0.22
            dx_seas = 0.3
            
            nCol = length(column_id) 
            nRow = length(row_id)
            dx_column = (xmax-dx_row_title-dx_right_text)/(nCol-1)
            dy_row = (ymax-dy_column_title)/(nRow-1)
           
            
            table = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=2, l=4, "mm")) +
                scale_x_continuous(limits=c(0, xmax),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, ymax),
                                   expand=c(0, 0)) +
                annotate("rect",
                         xmin=dx_left_line, xmax=xmax,
                         ymin=ymax-dy_column_title, ymax=ymax,
                         color=NA, fill=IPCCgrey97)

            for (rr in 1:nRow) {
                ytmp = ymax - dy_column_title - dy_row*(rr-1)

                if (rr > 1) {
                    table = table +
                        annotate("line",
                                 x=c(dx_left_line, xmax),
                                 y=rep(ytmp, 2),
                                 color=IPCCgrey80,
                                 linewidth=0.25, lineend="round")
                }
                if (rr > 1) {
                    if (rr <= 4) {
                        xtmp = dx_left_line + dx_left_text
                    } else {
                        table = table +
                            annotate("point",
                                     x=dx_left_line + dx_left_narratrac_circle,
                                     y=ytmp + dy_row/2,
                                     color=row_color[rr], shape=16)
                        xtmp = dx_left_line + dx_left_narratrac_text
                    }
                    table = table +
                        annotate("text",
                                 x=xtmp,
                                 y=ytmp + dy_row/2,
                                 label=row_name[rr],
                                 vjust=0.5, hjust=0,
                                 size=3, family="Lato",
                                 color=IPCCgrey23)
                }
                for (cc in 2:nCol) {
                    if (cc == 3) {
                        dx_shift = -dx_column*0.1
                    } else if (cc == 4) {
                        dx_shift = dx_column*0.1
                    } else {
                        dx_shift = 0
                    }
                    
                    xtmp = dx_row_title + (cc-1)*dx_column + dx_shift
                    if (rr == 1) {
                        table = table +
                            annotate("text",
                                     x=xtmp - dx_column/2 - dx_delta,
                                     y=ytmp + dy_column_title*0.5,
                                     label="Δ",
                                     size=3, hjust=0.5, vjust=0.5,
                                     family="Noto Sans",
                                     color=IPCCgrey23) +
                            annotate("text",
                                     x=xtmp - dx_column/2,
                                     y=ytmp + dy_column_title*0.5,
                                     label=column_name[cc],
                                     vjust=0.5, hjust=0.5,
                                     size=3, family="Lato",
                                     color=IPCCgrey23) +
                            annotate("text",
                                     x=xtmp - dx_column/2 + dx_seas,
                                     y=ytmp + dy_column_title*0.5,
                                     label=column_icon[cc],
                                     size=3, hjust=0.5, vjust=0.7,
                                     family=column_icon_font[cc],
                                     color=IPCCgrey40)
                    } else {
                        if (rr <= 4) {
                            id = paste0(row_id[rr], "_", column_id[cc])
                            value = dataEX_criteria_climate_stat_sh_wl[[id]]
                        } else {
                            value = dplyr::filter(dataEX_criteria_climate_sh_wl,
                                                  climateChain == row_id[rr])[[column_id[cc]]]
                        }

                        format_value = function(x) {
                            value = signif(x, 2)
                            if (abs(value) < 0.001) {
                                return (round(x, 3))
                            } else {
                                return (value)
                            }
                        }
                        
                        value = format_value(value)
                        value = get_labels_TeX(value, unit=column_unit[cc],
                                               is_unit_plurial=FALSE,
                                               add_unit_space=TRUE)

                        if (column_name[cc] == "T") {
                            Palette_tmp = Palette_temperature
                        } else if (column_name[cc] == "R") {
                            Palette_tmp = Palette_hydro
                        }
                        color = ifelse(grepl("-", value), Palette_tmp[1 + dColor],
                                ifelse(grepl("+", value), Palette_tmp[nColor - dColor], ""))

                        table = table +
                            annotate("text",
                                     x=xtmp - dx_column*0.75,
                                     y=ytmp + dy_row/2,
                                     label=latex2exp::TeX(value),
                                     vjust=0.5, hjust=0,
                                     size=3, family="Lato",
                                     color=color)
                    }                
                }
            }

            # table = table +
                # annotate("line",
                         # x=rep(dx_row_title, 2),
                         # y=c(ymax, 0),
                         # linewidth=0.7, color="white") +
                # annotate("line",
                         # x=rep(dx_row_title + 2*dx_column + dx_column*0.1, 2),
                         # y=c(ymax, 0),
                         # linewidth=0.7, color="white")
            
            
            climate_table_herd = add_sheep(climate_table_herd,
                                           sheep=table,
                                           id="climate_table_content",
                                           height=climate_table_content_height,
                                           verbose=verbose)

##### info ___________________________________________________________
            dy_newline = 0.21
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
                             y=0.95 - dy_newline*(k-1),
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
                "title_etiage", "title_recharge", "title_crue", "colorbar",
                "map_etiage",   "map_recharge",   "map_crue",   "colorbar",
                "legend",       "legend",         "legend",     "colorbar"
            ), ncol=4, byrow=TRUE)

            hydroMap_variable_title_height = 0.2
            hydroMap_variable_map_height = 1
            hydroMap_legend_height = 0.15

            hydroMap_variable_width = 1
            hydroMap_colorbar_width = 0.3

            lim_nchar_newline = 50
            dy_title_newline = 0.4
            
            alpha = 0.6
            
            hydroMap_herd = bring_grass(verbose=verbose)
            hydroMap_herd = plan_of_herd(hydroMap_herd, hydroMap_plan,
                                         verbose=verbose)

            margin_map = margin(t=0, r=0, b=0, l=0, unit="mm")

            secteur_union = secteurHydro_shp |> sf::st_union()
            rivers_in_secteur = sf::st_intersection(Shapefiles$river, secteur_union)
            
            cf = coord_fixed()
            cf$default = TRUE
            map = ggplot() + theme_void() + cf +
                theme(plot.margin=margin_map) +
                
                geom_sf(data=Shapefiles$france,
                        color=NA, alpha=0.35,
                        fill=IPCCgrey97) +
                
                geom_sf(data=secteurHydro_shp,
                        color=NA,
                        fill=IPCCgrey97) +
                
                geom_sf(data=Shapefiles$river,
                        color=INRAElightcyan,
                        alpha=0.5,
                        fill=NA,
                        linewidth=0.28,
                        na.rm=TRUE) +
                geom_sf(data=rivers_in_secteur,
                        color=INRAElightcyan,
                        fill=NA,
                        linewidth=0.28,
                        na.rm=TRUE) +
                geom_sf(data=Shapefiles$bassinHydro,
                        color=IPCCgrey85,
                        fill=NA, lineend="round",
                        size=0.25) +
                
                geom_sf(data=Shapefiles$france,
                        color=IPCCgrey50,
                        fill=NA, lineend="round",
                        linewidth=0.3) +
                
                geom_sf(data=secteurHydro_shp,
                        color=IPCCgrey40,
                        fill=NA, lineend="round",
                        linewidth=0.48)
            
            bbox = sf::st_bbox(secteurHydro_shp)
            aspect_plot = 6.5/8.5
            margin_factor = 0.05
            
            x_range = bbox["xmax"] - bbox["xmin"]
            y_range = bbox["ymax"] - bbox["ymin"]
            aspect_bbox = as.numeric(y_range/x_range)

            if (aspect_bbox > aspect_plot) {
                target_x_range = as.numeric(y_range/aspect_plot)
                x_center = (bbox["xmin"] + bbox["xmax"])/2
                xlim = c(x_center - target_x_range/2, x_center + target_x_range/2)
                ylim = c(bbox["ymin"], bbox["ymax"])
            } else {
                target_y_range = as.numeric(x_range * aspect_plot)
                y_center = (bbox["ymin"] + bbox["ymax"])/2
                ylim = c(y_center - target_y_range/2, y_center + target_y_range/2)
                xlim = c(bbox["xmin"], bbox["xmax"])
            }

            xlim_center = mean(xlim)
            ylim_center = mean(ylim)

            xlim_range = diff(xlim)
            ylim_range = diff(ylim)

            xlim = c(xlim_center - xlim_range/2 * (1 + margin_factor),
                      xlim_center + xlim_range/2 * (1 + margin_factor))
            ylim = c(ylim_center - ylim_range/2 * (1 + margin_factor),
                      ylim_center + ylim_range/2 * (1 + margin_factor))

            map = map + coord_sf(xlim=xlim, ylim=ylim, expand=FALSE)

            

#### etiage __________________________________________________________
            title_text = paste0("(", letters[id_letter], ") \\textbf{VCN10} - Changements relatifs de l'intensité des étiages (%)")
            id_letter = id_letter + 1

            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            Lines = strwrap(title_text, lim_nchar_newline)
            nLines = length(Lines)
            
            for (ll in 1:nLines) {
                title = title + 
                    annotate("text",
                             x=0,
                             y=0.9-(ll-1)*dy_title_newline,
                             label=latex2exp::TeX(Lines[ll]),
                             size=3, hjust=0, vjust=1,
                             family="Lato",
                             color=IPCCgrey23)
            }
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=title,
                                      id="title_etiage",
                                      height=hydroMap_variable_title_height,
                                      verbose=verbose)


            dataEX_criteria_hydro_plot_sh_wl =
                dplyr::left_join(dplyr::select(dataEX_criteria_hydro_mean_sh_wl,
                                               code, deltaVCN10),
                                 dplyr::select(dataEX_criteria_hydro_conf_sh_wl,
                                               code, above_pct_deltaVCN10,
                                               below_pct_deltaVCN10),
                                 by="code")
            dataEX_criteria_hydro_plot_sh_wl =
                dplyr::left_join(dataEX_criteria_hydro_plot_sh_wl,
                                 dplyr::select(Stations, code, XL93_m, YL93_m),
                                 by="code")

            
            dataEX_criteria_hydro_plot_sh_wl$shape = 21 #o

            Ok_above = dataEX_criteria_hydro_plot_sh_wl$above_pct_deltaVCN10 >= limit_conf_pct
            dataEX_criteria_hydro_plot_sh_wl[Ok_above]$shape = 24 #^
            
            Ok_below = dataEX_criteria_hydro_plot_sh_wl$below_pct_deltaVCN10 >= limit_conf_pct
            dataEX_criteria_hydro_plot_sh_wl[Ok_below]$shape = 25 #v
            
            map_etiage = map
            # + 
                # geom_point()
            
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=map_etiage,
                                      id="map_etiage",
                                      height=hydroMap_variable_map_height,
                                      width=hydroMap_variable_width,
                                      verbose=verbose)

#### recharge ________________________________________________________
            title_text = paste0("(", letters[id_letter], ") \\textbf{Recharge} - Changements relatifs de la recharge annuelle (%)")
            id_letter = id_letter + 1

            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            Lines = strwrap(title_text, lim_nchar_newline)
            nLines = length(Lines)
            
            for (ll in 1:nLines) {
                title = title + 
                    annotate("text",
                             x=0,
                             y=0.9-(ll-1)*dy_title_newline,
                             label=latex2exp::TeX(Lines[ll]),
                             size=3, hjust=0, vjust=1,
                             family="Lato",
                             color=IPCCgrey23)
            }
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=title,
                                      id="title_recharge",
                                      height=hydroMap_variable_title_height,
                                      verbose=verbose)

            map_recharge = map
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=map_recharge,
                                      id="map_recharge",
                                      height=hydroMap_variable_map_height,
                                      width=hydroMap_variable_width,
                                      verbose=verbose)

#### crue ____________________________________________________________
            title_text = paste0("(", letters[id_letter], ") \\textbf{QJXA} - Changements relatifs de l'intensité des crues (%)")
            id_letter = id_letter + 1

            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            Lines = strwrap(title_text, lim_nchar_newline)
            nLines = length(Lines)
            
            for (ll in 1:nLines) {
                title = title + 
                    annotate("text",
                             x=0,
                             y=0.9-(ll-1)*dy_title_newline,
                             label=latex2exp::TeX(Lines[ll]),
                             size=3, hjust=0, vjust=1,
                             family="Lato",
                             color=IPCCgrey23)
            }
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=title,
                                      id="title_crue",
                                      height=hydroMap_variable_title_height,
                                      verbose=verbose)

            map_crue = map
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=map_crue,
                                      id="map_crue",
                                      height=hydroMap_variable_map_height,
                                      width=hydroMap_variable_width,
                                      verbose=verbose)
            
            #### other ___________________________________________________________        
            dx_shape = 0.15
            stroke = 0.4
            dy = 0.5

            legend = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 10),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=0.1, y=dy,
                         label=latex2exp::TeX("\\textbf{Consensus} du signe pour 80% des projections :"),
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) +
                
                annotate("point",
                         x=3.3-0.13, y=dy-0.05,
                         shape=24,
                         size=2, stroke=stroke, fill=NA,
                         color=IPCCgrey50) +
                annotate("text",
                         x=3.3, y=dy,
                         label="à la hausse,",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) +
                
                annotate("point",
                         x=4.3-0.13, y=dy+0.05,
                         shape=25,
                         size=2, stroke=stroke, fill=NA,
                         color=IPCCgrey50) +
                annotate("text",
                         x=4.3, y=dy,
                         label="à la baisse,",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) +
                
                annotate("point",
                         x=5.25-0.12, y=dy,
                         shape=21,
                         size=2, stroke=stroke, fill=NA,
                         color=IPCCgrey50) +
                annotate("text",
                         x=5.25, y=dy,
                         label="sans consensus.",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) + 
    
                annotate("point",
                         x=7.5-0.13, y=dy,
                         shape=15,
                         size=2, color=IPCCgrey92) +
                annotate("text",
                         x=7.5, y=dy,
                         label="Absence d'aquifère",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50)             
            

            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=legend,
                                      id="legend",
                                      height=hydroMap_legend_height,
                                      verbose=verbose)


            res = dataSHEEP::compute_colorBin(-60, 60, 10, center=0)
            label = get_labels_TeX(res$bin, unit="%",
                                   is_unit_plurial=FALSE,
                                   add_unit_space=TRUE)
            
            colorbar = panel_colorbar_circle(res$bin,
                                             Palette_hydro,
                                             size_circle=2.5,
                                             d_line=0.5,
                                             linewidth=0.3,
                                             d_space=0.2,
                                             d_text=1,
                                             text_size=2.5,
                                             label=label,
                                             ncharLim=4,
                                             stroke=NULL,
                                             color=NULL,
                                             shape=21,
                                             colorText=IPCCgrey50,
                                             colorLine=IPCCgrey50,
                                             on_circle=FALSE,
                                             margin=margin(t=0, r=2, b=0, l=3, "mm"))
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=colorbar,
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


            hydroQM_title_height = 0.2
            hydroQM_river_name_height = 0.2
            hydroQM_river_graph_height = 1
            
            hydroQM_river_width = 1
            hydroQM_info_width = 0.2  
            
            hydroQM_herd = bring_grass(verbose=verbose)
            hydroQM_herd = plan_of_herd(hydroQM_herd, hydroQM_plan,
                                        verbose=verbose)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=void(),
                                     id="void",
                                     verbose=verbose)

#### title ___________________________________________________________
            title_text = paste0("(", letters[id_letter], ") \\textbf{QM} - Altération du régime hydrologique (débit mensuel) pour trois bassins de surface différente et avec une bonne qualité de simulation")
            id_letter = id_letter + 1
            
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
            
            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=title,
                                     id="title",
                                     height=hydroQM_title_height,
                                     verbose=verbose)

#### informations référence ___________________________________________________________
            reference = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=0.2, y=0.5,
                         label=latex2exp::TeX("\\textbf{Période de RÉFÉRENCE}"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.48, y=0.5,
                         label=latex2exp::TeX("\\textbf{1976-2005}"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.8, y=0.5,
                         label=latex2exp::TeX("Débit mensuel (m$^3$/s)"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35)
            
            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=reference,
                                     id="ref_info",
                                     width=hydroQM_info_width,
                                     verbose=verbose)

#### informations changements ___________________________________________________________
            delta = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=0.2, y=0.5,
                         label=latex2exp::TeX("\\textbf{FRANCE +4\\small{°C}}"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.55, y=0.5,
                         label=latex2exp::TeX("Changements du"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.8, y=0.5,
                         label=latex2exp::TeX("débit mensuel (%)"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=delta,
                                     id="delta_info",
                                     width=hydroQM_info_width,
                                     verbose=verbose)

            
#### rivières ___________________________________________________________
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

            hydroTable_title_height = 0.15
            hydroTable_content_height = 1
            
            hydroTable_herd = bring_grass(verbose=verbose)
            hydroTable_herd = plan_of_herd(hydroTable_herd, hydroTable_plan,
                                           verbose=verbose)

#### title ___________________________________________________________
            title_text = paste0("(", letters[id_letter], ") Changements relatifs projetés en débits et recharge (référence : 1976-2005, statistiques spatiales sur les écarts relatifs médians)")
            id_letter = id_letter + 1
            
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
            
            hydroTable_herd = add_sheep(hydroTable_herd,
                                        sheep=title,
                                        id="title",
                                        height=hydroTable_title_height,
                                        verbose=verbose)

#### table ___________________________________________________________            
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
            foot_text = c("\\textbf{Avertissement} : Il ne s’agit pas de prévisions mais d’indications d’évolutions possibles. Ces fiches sont volontairement synthétiques et",
                          "une notice d’accompagnement fournit des informations pour la lecture et l’interprétation des graphiques de cette fiche.")
            dy_newline = 0.36
            
            foot = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 10),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            for (k in 1:length(foot_text)) {
                foot = foot +             
                    annotate("text",
                             x=0.01,
                             y=0.7 - dy_newline*(k-1),
                             label=latex2exp::TeX(foot_text[k]),
                             size=2, hjust=0, vjust=1,
                             family="Raleway",
                             color=TRACCblue)
            }

            foot_info = paste0("\\small{", format(Sys.Date(), "%B %Y"), "\\; - \\;}\\textbf{\\small{n°}", secteur$id, "}")
            
            foot = foot +
                annotate("text",
                         x=9.98,
                         y=0.5,
                         label=latex2exp::TeX(foot_info),
                         size=3, hjust=1, vjust=0.5,
                         family="Raleway",
                         color=TRACCblue)            

            herd = add_sheep(herd,
                             sheep=foot,
                             id="foot",
                             height=foot_height,
                             width=width,
                             verbose=verbose)
            
            
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
            #                 device=pdf)


            
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
