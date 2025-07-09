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
                                     dataEX_criteria_climate_secteur,
                                     dataEX_criteria_hydro,
                                     dataEX_serie_hydro,
                                     dataEX_criteria_recharge_MESO,
                                     dataEX_criteria_recharge_secteur,
                                     # metaEX_criteria_climate,
                                     # metaEX_criteria_hydro,
                                     # metaEX_serie_hydro,
                                     WL,
                                     NarraTRACC_selection,
                                     NarraTRACC_order,
                                     # delta_prob=0,
                                     limit_conf_pct=80,
                                     icons=NULL,
                                     logo_info="",
                                     Pages=NULL,
                                     Shapefiles=NULL,
                                     Shapefiles_mini=NULL,
                                     figdir="",
                                     is_MPI=FALSE,
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


    colorStep = 10
    Palette_hydro = get_IPCC_Palette("hydro_10", colorStep=colorStep)
    Palette_temperature = get_IPCC_Palette("temperature_10")
    dColor = 1
    nColor_hydro = length(Palette_hydro)
    nColor_temperature = length(Palette_temperature)
    Palette_bin = dataSHEEP::compute_colorBin(-50, 50, colorStep=colorStep, center=0, round=FALSE)
    # Palette_layers = c(4, 3, 2, 1, 1, 2, 3, 4)
    Palette_layers = c(5, 4, 3, 2, 1, 1, 2, 3, 4, 5)
    # Palette_layers = c(3, 2, 1, 1, 2, 3)
    
    delta_stat_prob = 0
    
    climate_criteria_cols = names(dataEX_criteria_climate_secteur)[sapply(dataEX_criteria_climate_secteur, is.numeric)]
    dataEX_criteria_climate_secteur_stat =
        dplyr::summarise(dplyr::group_by(dataEX_criteria_climate_secteur,
                                         SH, GWL),
                         dplyr::across(climate_criteria_cols,
                                       ~quantile(.x, delta_stat_prob,
                                                 na.rm=TRUE),
                                       .names="min_{.col}"),
                         dplyr::across(climate_criteria_cols,
                                       ~quantile(.x, 0.5,
                                                 na.rm=TRUE),
                                       .names="median_{.col}"),
                         dplyr::across(climate_criteria_cols,
                                       ~quantile(.x, 1-delta_stat_prob,
                                                 na.rm=TRUE),
                                       .names="max_{.col}"))

    
    dataEX_criteria_hydro = tidyr::unite(dataEX_criteria_hydro,
                                         "Chain",
                                         "EXP", "GCM", "RCM",
                                         "BC", "HM",
                                         sep="_", remove=FALSE)

    hydro_criteria_cols = names(dataEX_criteria_hydro)[sapply(dataEX_criteria_hydro, is.numeric)]
    dataEX_criteria_hydro_secteur =
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro,
                                         GWL,
                                         Chain, EXP, GCM, RCM, BC, HM,
                                         SH),
                         dplyr::across(.cols=hydro_criteria_cols,
                                       .fns=~median(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_hydro_secteur_stat =
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro_secteur,
                                         SH, GWL),
                         dplyr::across(hydro_criteria_cols,
                                       ~quantile(.x, delta_stat_prob,
                                                 na.rm=TRUE),
                                       .names="min_{.col}"),
                         dplyr::across(hydro_criteria_cols,
                                       ~quantile(.x, 0.5,
                                                 na.rm=TRUE),
                                       .names="median_{.col}"),
                         dplyr::across(hydro_criteria_cols,
                                       ~quantile(.x, 1-delta_stat_prob,
                                                 na.rm=TRUE),
                                       .names="max_{.col}"))

    recharge_criteria_cols = names(dataEX_criteria_recharge_secteur)[sapply(dataEX_criteria_recharge_secteur, is.numeric)]
    dataEX_criteria_recharge_secteur_stat =
        dplyr::summarise(dplyr::group_by(dataEX_criteria_recharge_secteur,
                                         SH, GWL),
                         dplyr::across(recharge_criteria_cols,
                                       ~quantile(.x, delta_stat_prob,
                                                 na.rm=TRUE),
                                       .names="min_{.col}"),
                         dplyr::across(recharge_criteria_cols,
                                       ~quantile(.x, 0.5,
                                                 na.rm=TRUE),
                                       .names="median_{.col}"),
                         dplyr::across(recharge_criteria_cols,
                                       ~quantile(.x, 1-delta_stat_prob,
                                                 na.rm=TRUE),
                                       .names="max_{.col}"))

    
    hydro_criteria_cols = names(dataEX_criteria_hydro)[sapply(dataEX_criteria_hydro, is.numeric)]
    dataEX_criteria_hydro_conf =
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro,
                                         code, GWL, EXP),
                         dplyr::across(.cols=hydro_criteria_cols,
                                       .fns=~sum(.x>0)/dplyr::n()*100,
                                       .names="{.col}_above_pct"),
                         dplyr::across(.cols=hydro_criteria_cols,
                                       .fns=~sum(.x<0)/dplyr::n()*100,
                                       .names="{.col}_below_pct"),
                         .groups="drop")
    
    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro,
                                         code, SH, GWL, EXP, GCM, RCM, BC),
                         dplyr::across(.cols=hydro_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro_mean,
                                         code, SH, GWL, EXP, GCM, RCM),
                         dplyr::across(.cols=hydro_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro_mean,
                                         code, SH, GWL, EXP, GCM),
                         dplyr::across(.cols=hydro_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_hydro_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_hydro_mean,
                                         code, SH, GWL, EXP),
                         dplyr::across(.cols=hydro_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")


    recharge_criteria_cols = names(dataEX_criteria_recharge_MESO)[sapply(dataEX_criteria_recharge_MESO, is.numeric)]
    dataEX_criteria_recharge_MESO_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_recharge_MESO,
                                         code_MESO, SH, GWL, EXP, GCM, RCM, BC),
                         dplyr::across(.cols=recharge_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_recharge_MESO_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_recharge_MESO_mean,
                                         code_MESO, SH, GWL, EXP, GCM, RCM),
                         dplyr::across(.cols=recharge_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_recharge_MESO_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_recharge_MESO_mean,
                                         code_MESO, SH, GWL, EXP, GCM),
                         dplyr::across(.cols=recharge_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")
    dataEX_criteria_recharge_MESO_mean = 
        dplyr::summarise(dplyr::group_by(dataEX_criteria_recharge_MESO_mean,
                                         code_MESO, SH, GWL, EXP),
                         dplyr::across(.cols=recharge_criteria_cols,
                                       .fns=~mean(.x, na.rm=TRUE)),
                         .groups="drop")


    for (cc in 1:length(dataEX_serie_hydro)) {
        dataEX_serie_hydro[[cc]] = tidyr::unite(dataEX_serie_hydro[[cc]],
                                                "Chain",
                                                "EXP", "GCM", "RCM",
                                                "BC", "HM",
                                                sep="_", remove=FALSE)
    }

    delta_deltaQMA_prob = 0.05

    hydro_serie_cols = names(dataEX_serie_hydro$deltaQMA)[sapply(dataEX_serie_hydro$deltaQMA, is.numeric)]
    dataEX_serie_hydro_deltaQMA_prob =
        dplyr::summarise(dplyr::group_by(dataEX_serie_hydro$deltaQMA,
                                         GWL, EXP, code, date),
                         dplyr::across(hydro_serie_cols,
                                       ~quantile(.x, delta_deltaQMA_prob,
                                                 na.rm=TRUE),
                                       .names="min_{.col}"),
                         dplyr::across(hydro_serie_cols,
                                       ~quantile(.x, 1-delta_deltaQMA_prob,
                                                 na.rm=TRUE),
                                       .names="max_{.col}"))

    
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
        

        for (j in 1:nWL) {
            id_letter = 1
            
            wl = WL[[j]]
            rwl = as.numeric(wl["RWL"])
            if (verbose) {
                print(paste0(j, "/", nWL, " so ", round(j/nWL*100, 1), "% done -> ", wl["RWL"]))
            }
            
            NarraTRACC = NarraTRACC_selection[[wl["RWLclean"]]]
            NarraTRACC_sh = dplyr::filter(NarraTRACC, SH == sh)
            NarraTRACC_sh_Chain = unlist(NarraTRACC_sh[paste0("Chain_", 1:4)])
            NarraTRACC_sh_climateChain = unlist(NarraTRACC_sh[paste0("climateChain_", 1:4)])
            NarraTRACC_sh_name = gsub("RWL", "", unlist(NarraTRACC_sh[paste0("name_", 1:4)]))
            NarraTRACC_sh_description = unlist(NarraTRACC_sh[paste0("description_", 1:4)])
            NarraTRACC_sh_color = unlist(NarraTRACC_sh[paste0("color_", 1:4)])


            NarraTRACC_sh_name_cut = gsub(".*[-]", "", NarraTRACC_sh_name)
            NarraTRACC_order_id = order(factor(NarraTRACC_sh_name_cut, levels=NarraTRACC_order))
            # NarraTRACC_order_id = order(match(, NarraTRACC_order))

            NarraTRACC_sh_Chain = NarraTRACC_sh_Chain[NarraTRACC_order_id]
            NarraTRACC_sh_climateChain = NarraTRACC_sh_climateChain[NarraTRACC_order_id]
            NarraTRACC_sh_name = NarraTRACC_sh_name[NarraTRACC_order_id]
            NarraTRACC_sh_description = NarraTRACC_sh_description[NarraTRACC_order_id]
            NarraTRACC_sh_color = NarraTRACC_sh_color[NarraTRACC_order_id]
                

            
            dataEX_criteria_climate_secteur_sh_wl = dplyr::filter(dataEX_criteria_climate_secteur,
                                                               SH==sh, GWL == wl["GWLclean"])

            dataEX_criteria_climate_secteur_stat_sh_wl = dplyr::filter(dataEX_criteria_climate_secteur_stat,
                                                                    SH==sh, GWL==wl["GWLclean"])
            
            dataEX_criteria_hydro_secteur_sh_wl = dplyr::filter(dataEX_criteria_hydro_secteur,
                                                             SH==sh, GWL == wl["GWLclean"])
            # dataEX_criteria_hydro_sh_wl = dplyr::filter(dataEX_criteria_hydro,
                                                        # SH==sh, GWL == wl["GWLclean"])

            dataEX_criteria_hydro_secteur_stat_sh_wl = dplyr::filter(dataEX_criteria_hydro_secteur_stat,
                                                                  SH==sh, GWL==wl["GWLclean"])
            dataEX_criteria_hydro_conf_sh_wl = dplyr::filter(dataEX_criteria_hydro_conf,
                                                             SH == sh, GWL==wl["GWLclean"])
            dataEX_criteria_hydro_mean_sh_wl = dplyr::filter(dataEX_criteria_hydro_mean,
                                                             SH == sh, GWL==wl["GWLclean"])

            dataEX_criteria_recharge_secteur_sh_wl = dplyr::filter(dataEX_criteria_recharge_secteur,
                                                                   SH==sh, GWL == wl["GWLclean"])
            dataEX_criteria_recharge_MESO_mean_wl = dplyr::filter(dataEX_criteria_recharge_MESO_mean,
                                                                  GWL==wl["GWLclean"])
            dataEX_criteria_recharge_secteur_stat_sh_wl = dplyr::filter(dataEX_criteria_recharge_secteur_stat,
                                                                        SH==sh, GWL==wl["GWLclean"])


            NarraTRACC_sh_Chain_not_in = NarraTRACC_sh_Chain %in% dataEX_criteria_hydro_secteur_sh_wl$Chain
            NarraTRACC_sh_name[!NarraTRACC_sh_Chain_not_in] = paste0(NarraTRACC_sh_name[!NarraTRACC_sh_Chain_not_in], "\\textbf{*}")
            
            
            dataEX_serie_hydro_sh_wl = dataEX_serie_hydro
            for (cc in 1:length(dataEX_serie_hydro_sh_wl)) {
                dataEX_serie_hydro_sh_wl[[cc]] = dplyr::filter(dataEX_serie_hydro_sh_wl[[cc]],
                                                               SH==sh, GWL == wl["GWLclean"])
            }
            
            dataEX_serie_hydro_deltaQMA_prob_wl =
                dplyr::filter(dataEX_serie_hydro_deltaQMA_prob,
                              GWL==wl["GWLclean"])
            
            ## PAGE _________________________________________________________
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan,
                                verbose=verbose)

            ### Info ________________________________________________________
            info_plan = matrix(c(
                "minimap", "title", "logo",
                "minimap", "legend", "legend"
            ), ncol=3, byrow=TRUE)

            info_title_height = 1.3
            info_legend_height = 1
            
            info_minimap_width = 1.1
            info_title_width = 2.4
            info_logo_width = 1

            info_herd = bring_grass(verbose=verbose)
            info_herd = plan_of_herd(info_herd, info_plan,
                                     verbose=verbose)

            xlim_map = c(90000, 1250000)
            ylim_minimap = c(6040000, 7120000)

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
            
            xmin_ech = gpct(x_echelle_pct, xlim_map, shift=TRUE)
            xint_ech = echelle*1E3
            ymin_ech = gpct(y_echelle_pct, ylim_minimap, shift=TRUE)
            ymin_km = gpct(max(c(y_echelle_pct-ymin_km_shift, 0)), ylim_minimap, shift=TRUE)
            ymax_ech = ymin_ech + gpct(echelle_tick_height, ylim_minimap)

            minimap = minimap +
                annotate("line",
                         x=c(xmin_ech, max(xint_ech)+xmin_ech),
                         y=c(ymin_ech, ymin_ech),
                         color=IPCCgrey40, size=echelle_line_size,
                         lineend="round") +
                annotate("text",
                         x=max(xint_ech)+xmin_ech+gpct(1, xlim_map), y=ymin_km,
                         vjust=0, hjust=0, label="km",
                         family="Lato",
                         color=IPCCgrey40, size=echelle_km_size)

            for (x_ech in xint_ech) {
                minimap = minimap +
                    annotate("segment",
                             x=x_ech+xmin_ech, xend=x_ech+xmin_ech, y=ymin_ech, yend=ymax_ech,
                             color=IPCCgrey40, size=echelle_line_size,
                             lineend="round") +
                    annotate("text",
                             x=x_ech+xmin_ech, y=ymax_ech+gpct(ymin_km_value_shift, ylim_minimap),
                             vjust=0, hjust=0.5, label=x_ech/1E3,
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
                coord_sf(xlim=xlim_map, ylim=ylim_minimap,
                         expand=FALSE)

            info_herd = add_sheep(info_herd,
                                  sheep=minimap,
                                  id="minimap",
                                  width=info_minimap_width,
                                  verbose=verbose)

            #### Info text _______________________________________________________
            dy_newline = 0.255
            dy_region = 0.31
            dy_basin = 0.16
            
            title_text = unlist(strsplit(paste0(sh, " - ", secteur$secteur_cut), "[ ][|][ ]"))
            nLine = length(title_text)
            title_text[1] = sub("[-] ", "- \\\\textbf{", title_text[1])
            title_text[1] = paste0(title_text[1], "}")
            if (nLine > 1) {
                title_text[2:nLine] = sapply(title_text[2:nLine], function (x) {paste0("\\textbf{", x, "}")})
                y0 = 0.98
            } else {
                y0 = 0.9
            }
            
            title = ggplot() +
                theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm"))

            for (l in 1:nLine) {
                title = title +
                    annotate("text",
                             x=0, y=y0-dy_newline*(l-1),
                             label=latex2exp::TeX(title_text[l]),
                             size=5.4, hjust=0, vjust=1,
                             family="Raleway",
                             color=TRACCblue)
            }

            region = paste0("\\textbf{Région hydrographique} \\; ", secteur$region)
            basin = paste0("\\textbf{Bassin de gestion} \\; ", secteur$bassin)
            
            title = title +
                annotate("text",
                         x=0, y=y0-dy_newline*(nLine-1) - dy_region,
                         label=latex2exp::TeX(region),
                         size=3, hjust=0, vjust=1,
                         family="Lato",
                         color=TRACCblue) +
                annotate("text",
                         x=0, y=y0-dy_newline*(nLine-1) - dy_region - dy_basin,
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
            dy_title = 0.25
            dy_newline = 0.16
            dy_star = 0.11
            
            dx0 = 0
            dx_narratracc = 0.01
            dx_line = 0.03
            p_line = 0.75
            dx_text = 0.02

            linewidth = 1.1

            label = "\\textbf{Narratifs hydrologiques (narraTRACCs)}"
            narratracc = narratracc +
                annotate("text",
                         x=dx0,
                         y=dy0,
                         label=latex2exp::TeX(label),
                         size=2.2, hjust=0, vjust=1,
                         family="Lato",
                         color=IPCCgrey35)

            nNarraTRACC = length(NarraTRACC_sh_Chain)
            
            for (k in 1:nNarraTRACC) {
                y = dy0 - dy_title - dy_newline*(k-1)
                label = paste0(NarraTRACC_sh_name[k], " : ", NarraTRACC_sh_description[k])
                narratracc = narratracc +
                    annotate("line",
                             x=dx0 + dx_narratracc + c(0, dx_line),
                             y=y,
                             color=NarraTRACC_sh_color[k],
                             linewidth=linewidth,
                             lineend="round") +
                    annotate("text",
                             x=dx0 + dx_narratracc + dx_line + dx_text,
                             y=y,
                             label=latex2exp::TeX(label),
                             size=2.2, hjust=0, vjust=0.55,
                             family="Lato",
                             color=IPCCgrey35)
            }

            if (any(!NarraTRACC_sh_Chain_not_in)) {
                label = "\\textbf{$_{$*}} \\tiny{: narratif non présent sur ce secteur hydrographique}"
                narratracc = narratracc +
                    annotate("text",
                             x=dx0,
                             y=dy0 - dy_title - dy_newline*(nNarraTRACC-1) - dy_star,
                             label=latex2exp::TeX(label),
                             size=3.7, hjust=0, vjust=1,
                             family="Lato",
                             color=IPCCgrey48)
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
            logo_plan = matrix(c("TRACC",
                                 "RWL"),
                               ncol=1,
                               byrow=TRUE)

            # logo_Explore2_height = 1
            logo_TRACC_height = 1
            logo_RWL_height = 1.2
            
            logo_herd = bring_grass(verbose=verbose)
            logo_herd = plan_of_herd(logo_herd, logo_plan,
                                     verbose=verbose)
            
            # Explore2_grob = grid::rasterGrob(png::readPNG(logo_info[["Explore2"]]["path"]),
            #                                  vjust=0.6,
            #                                  hjust=0.5,
            #                                  height=0.8)
            # logo_herd = add_sheep(logo_herd,
            #                       sheep=Explore2_grob,
            #                       id="Explore2",
            #                       height=logo_Explore2_height,
            #                       verbose=verbose)
            
            TRACC_grob = grid::rasterGrob(png::readPNG(logo_info[["TRACC"]]["path"]),
                                          vjust=0.83,
                                          hjust=0.5,
                                          height=0.89)        
            logo_herd = add_sheep(logo_herd,
                                  sheep=TRACC_grob,
                                  id="TRACC",
                                  height=logo_TRACC_height,
                                  verbose=verbose)

            if (wl["RWLclean"] == "RWL-40") {
                shift_C = 0.67
                dx_shift = 0
            } else if (wl["RWLclean"] == "RWL-27") {
                shift_C = 1.22
                dx_shift = 0.2
            }
            
            RWL = ggplot() +
                theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                annotate("text",
                         x=1.35 - dx_shift,
                         y=0.36,
                         label="vivre à",
                         size=3, hjust=0, vjust=0.5,
                         family="Lato",
                         color=wl["color"]) +
                annotate("text",
                         x=2.5 - dx_shift,
                         y=0.36,
                         label=latex2exp::TeX(paste0("\\textbf{+", wl["RWL"], "}")),
                         size=6, hjust=0, vjust=0.5,
                         family="Raleway",
                         color=wl["color"]) +
                annotate("text",
                         x=2.5+shift_C - dx_shift,
                         y=0.36,
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


            # herd = add_sheep(herd,
            #                  sheep=contour(),
            #                  id="climate",
            #                  height=climate_height,
            #                  width=width,
            #                  verbose=verbose)
            # herd = add_sheep(herd,
            #                  sheep=contour(),
            #                  id="hydroMap",
            #                  height=hydroMap_height,
            #                  width=width,
            #                  verbose=verbose)
            # herd = add_sheep(herd,
            #                  sheep=contour(),
            #                  id="hydroQM",
            #                  height=hydroQM_height,
            #                  width=width,
            #                  verbose=verbose)
            # herd = add_sheep(herd,
            #                  sheep=contour(),
            #                  id="hydroTable",
            #                  height=hydroTable_height,
            #                  width=width,
            #                  verbose=verbose)
            # herd = add_sheep(herd,
            #                  sheep=contour(),
            #                  id="foot",
            #                  height=foot_height,
            #                  width=width,
            #                  verbose=verbose)

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
            climate_table_width = 0.9

#### Title __________________________________________________________
            title_text = paste0("(", letters[id_letter], ") \\; \\; \\; \\;\\;- Changements relatifs de précipitation (%) en fonction des changements de température (°C) \\;\\small{référence 1991-2020}")
            id_letter = id_letter + 1
            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                
                annotate("text",
                         x=0.021, y=0.5,
                         label="Δ",
                         fontface="bold",
                         size=3, hjust=0, vjust=0.42,
                         color=IPCCgrey23) +
                annotate("text",
                         x=0.033, y=0.5,
                         label=latex2exp::TeX("\\textbf{P/}"),
                         size=3, hjust=0, vjust=0.43,
                         family="Lato",
                         color=IPCCgrey23) +
                annotate("text",
                         x=0.047, y=0.5,
                         label="Δ",
                         fontface="bold",
                         size=3, hjust=0, vjust=0.42,
                         color=IPCCgrey23) +
                annotate("text",
                         x=0.058, y=0.5,
                         label=latex2exp::TeX("\\textbf{T}"),
                         size=3, hjust=0, vjust=0.43,
                         family="Lato",
                         color=IPCCgrey23) + 

    
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
                "void", "dT",          "dT", "dT" 
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
            dx_dT = 0.25
            
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
                         label=latex2exp::TeX("\\; P : Changements de précipitations (%)"),
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

            get_labels_HTML = function(x, unit, is_unit_plurial, add_unit_space, Palette, dColor, correct_space=FALSE) {
                nColor = length(Palette)
                unit_suffixed = ifelse(!is_unit_plurial, unit,
                                ifelse(x != 0, paste0(unit, "s"), unit))

                if (is_MPI) {
                    if (correct_space) {
                        result = ifelse(x > 0,
                                        paste0("<span style='font-size:4pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"),
                                        paste0("<span style='font-size:2pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"))
                    } else {
                        if (any(grepl("[.]", x))) {
                            result = ifelse(grepl("[.]", x),
                                            paste0("<span style='font-size:1pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"),
                                            paste0("<span style='font-size:3pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"))
                        } else {
                            result = ifelse(add_unit_space,
                                            paste0("<span style='font-size:3pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"),
                                            paste0("<span style='font-size:6pt'>", unit_suffixed, "</span>"))
                        }
                    }
                    
                } else {
                    result = ifelse(add_unit_space,
                                    paste0("<span style='font-size:6pt'> </span><span style='font-size:6pt'>", unit_suffixed, "</span>"),
                                    paste0("<span style='font-size:6pt'>", unit_suffixed, "</span>"))
                }
                
                color = ifelse(x < 0, Palette[1 + dColor],
                        ifelse(x > 0, Palette[nColor - dColor], ""))

                x_text = ifelse(x > 0, paste0("+", x), as.character(x))

                result = ifelse(x < 0 | x > 0,
                                paste0("<span style='color:", color, "'><b>", x_text, "</b>", result, "</span>"),
                                paste0("<span style=''><b>", x_text, "</b></span>"))

                return (result)
            }

            panel_delta_variable = function (dataEX_criteria_climate_secteur_sh_wl,
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
                                             dColor=dColor,
                                             correct_space=TRUE)
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

                Ttick = 0.5
                TMm_range = c(min(dataEX_criteria_climate_secteur_sh_wl[[paste0("deltaTMm_", season)]]),
                              max(dataEX_criteria_climate_secteur_sh_wl[[paste0("deltaTMm_", season)]]))
                TMm_range =  c(floor(TMm_range[1] / Ttick) * Ttick, ceiling(TMm_range[2] / Ttick) * Ttick)
                TMm_range = expand(TMm_range)

                RR_range = c(min(dataEX_criteria_climate_secteur_sh_wl[[paste0("deltaRR_", season)]]),
                             max(dataEX_criteria_climate_secteur_sh_wl[[paste0("deltaRR_", season)]]))
                RR_range =  c(floor(RR_range[1] / 5) * 5, ceiling(RR_range[2] / 5) * 5)
                RR_range = expand(RR_range)

                delta_variable = ggplot() +
                    theme(plot.margin=margin(t=0, r=3.5,
                                             b=0, l=1, "mm"))
                if (RR_range[1] <= 0 & 0 <= RR_range[2]) {
                    delta_variable = delta_variable +
                        annotate("line",
                                 x=TMm_range, y=0,
                                 color=IPCCgrey60,
                                 linewidth=0.33)
                }
                # if (TMm_range[1] <= rwl & rwl <= TMm_range[2]) {
                    # delta_variable = delta_variable +
                        # annotate("line",
                                 # x=rwl, y=RR_range,
                                 # color=IPCCgrey60,
                                 # linewidth=0.33)
                # }

                if (is_MPI) {
                    delta_variable = delta_variable +    
                        theme_IPCC(is_plot.background=TRUE,
                                   is_panel.grid.major.x=TRUE,
                                   is_panel.grid.major.y=TRUE,
                                   is_axis.line.x=FALSE,
                                   is_axis.ticks.x=FALSE,
                                   axis.text.x_size=8,
                                   is_axis.ticks.y=FALSE,
                                   axis.ticks.length.x=0.8,
                                   
                                   axis.text.x_vjust=0,
                                   axis.text.x_margin=margin(t=0, b=0.5, unit="mm"),
                                   
                                   axis.text.y_vjust=0.4,
                                   axis.text.y_margin=margin(r=1, unit="mm"))
                } else {
                    delta_variable = delta_variable +    
                        theme_IPCC(is_plot.background=TRUE,
                                   is_panel.grid.major.x=TRUE,
                                   is_panel.grid.major.y=TRUE,
                                   is_axis.line.x=FALSE,
                                           is_axis.ticks.x=FALSE,
                                   axis.text.x_size=8,
                                   is_axis.ticks.y=FALSE,
                                   axis.ticks.length.x=0.8)
                }

                delta_variable = delta_variable +                    
                    scale_x_continuous(limits=TMm_range,
                                       n.breaks=4,
                                       labels=get_labels_deltaT,
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=RR_range,
                                       labels=get_labels_deltaR,
                                       expand=c(0, 0))

                dataEX_criteria_climate_secteur_sh_wl_NO_narratrac =
                    dplyr::filter(dataEX_criteria_climate_secteur_sh_wl,
                                  !(climateChain %in% NarraTRACC_sh_climateChain))

                delta_variable = delta_variable +
                    geom_point(data=dataEX_criteria_climate_secteur_sh_wl_NO_narratrac,
                               aes(x=get(paste0("deltaTMm_", season)),
                                   y=get(paste0("deltaRR_", season))),
                               size=1, color=IPCCgrey67)

                for (k in 1:nNarraTRACC) {
                    dataEX_criteria_climate_secteur_sh_wl_narratrac =
                        dplyr::filter(dataEX_criteria_climate_secteur_sh_wl,
                                      climateChain == NarraTRACC_sh_climateChain[k])
                    delta_variable = delta_variable +
                        geom_point(data=dataEX_criteria_climate_secteur_sh_wl_narratrac,
                                   aes(x=get(paste0("deltaTMm_", season)),
                                       y=get(paste0("deltaRR_", season))),
                                   size=1.4, color=IPCCgrey97) + 
                        geom_point(data=dataEX_criteria_climate_secteur_sh_wl_narratrac,
                                   aes(x=get(paste0("deltaTMm_", season)),
                                       y=get(paste0("deltaRR_", season))),
                                   size=1, color=NarraTRACC_sh_color[k])
                }
    
                return (delta_variable)
            }

            delta_variable = panel_delta_variable(dataEX_criteria_climate_secteur_sh_wl, "DJF")
    
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


            delta_variable = panel_delta_variable(dataEX_criteria_climate_secteur_sh_wl, "JJA")
            
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
            column_id = c("", "deltaTMm_DJF", "deltaRR_DJF", "deltaTMm_JJA", "deltaRR_JJA")
            column_name = c("", "T_Hiver", "P_Hiver", "T_Été", "P_Été")
            column_icon = c("", "mode_cool", "mode_cool", "sunny", "sunny")
            column_icon_font = c("", "Material Symbols Outlined", "Material Symbols Outlined", "Material Symbols Rounded", "Material Symbols Rounded")
            column_icon_shift = c(0, 0.6, 0.6, 0.5, 0.5)
            column_unit = c("", "°C", "%", "°C", "%")
            
            row_id = c("",
                       "max",
                       "median",
                       "min",
                       NarraTRACC_sh_climateChain)
            row_name = c("",
                         "maximum",
                         "médiane",
                         "minimum",
                         NarraTRACC_sh_name)
            row_icon = c("",
                         "expand_circle_up",
                         "do_not_disturb_on",
                         "expand_circle_down",
                         rep("", nNarraTRACC))
            row_color = c("",
                          rep(IPCCgrey35, 3),
                          NarraTRACC_sh_color)


            get_labels_TeX = function(x, unit, is_unit_plurial, add_unit_space, is_unit_zero=FALSE,
                                      digits=NULL) {
                unit_suffixed = ifelse(!is_unit_plurial, unit,
                                ifelse(x != 0, paste0(unit, "s"), unit))
                unit_formated = ifelse(add_unit_space,
                                paste0("\\small{ ", unit_suffixed, "}"),
                                paste0(unit_suffixed))
                if (!is.null(digits)) {
                    x_text = formatC(x, format="f", digits=digits)
                } else {
                    x_text = x
                }
                x_text = ifelse(x > 0, paste0("+", x_text), as.character(x_text))
                x_text = ifelse(x == 0, paste0(" ", x_text), as.character(x_text))
                result = ifelse((x < 0 | x > 0) | is_unit_zero,
                                paste0("\\textbf{", x_text, "}", unit_formated, ""),
                                paste0("\\textbf{", x_text, "}"))
                return (result)
            }
            
            xmax = 10
            ymax = 10
    
            dx_left_line = 0.1

            dx_left_icon = 0.22
            dx_left_text = 0.45

            dx_left_narratrac_circle = 0.22
            dx_left_narratrac_text = 0.45

            dx_right_text = 0.4

            
            dy_column_title = 1.5
            dx_row_title = 2.6

            # dx_delta = 0.4
            # dx_seas = 0.3
            
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
                        table = table +
                            annotate("text",
                                     x=dx_left_line + dx_left_icon,
                                     y=ytmp + dy_row/2,
                                     label=row_icon[rr],
                                     size=3, hjust=0.5, vjust=0.8,
                                     family="Material Symbols Outlined",
                                     color=IPCCgrey40) +
                            annotate("text",
                                     x=dx_left_line + dx_left_text,
                                     y=ytmp + dy_row/2,
                                     label=row_name[rr],
                                     vjust=0.5, hjust=0,
                                     size=3, family="Lato",
                                     color=IPCCgrey35)
                    } else {
                        table = table +
                            annotate("point",
                                     x=dx_left_line + dx_left_narratrac_circle,
                                     y=ytmp + dy_row/2,
                                     color=row_color[rr], shape=16) +
                            annotate("text",
                                     x=dx_left_line + dx_left_narratrac_text,
                                     y=ytmp + dy_row/2,
                                     label=latex2exp::TeX(row_name[rr]),
                                     vjust=0.5, hjust=0,
                                     size=3, family="Lato",
                                     color=IPCCgrey23)
                    }
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
                        label = TeX(convert2TeX(column_name[cc], bold=FALSE))
                        table = table +
                            annotate("text",
                                     x=xtmp - dx_column/2 - column_icon_shift[cc],
                                     y=ytmp + dy_column_title*0.5,
                                     label=column_icon[cc],
                                     size=3, hjust=0.5, vjust=0.7,
                                     family=column_icon_font[cc],
                                     color=IPCCgrey40) + 
                            annotate("text",
                                     x=xtmp - dx_column/2,
                                     y=ytmp + dy_column_title*0.5,
                                     label=label,
                                     vjust=0.5, hjust=0.5,
                                     size=3, family="Lato",
                                     color=IPCCgrey23) 
                    } else {
                        if (rr <= 4) {
                            id = paste0(row_id[rr], "_", column_id[cc])
                            value = dataEX_criteria_climate_secteur_stat_sh_wl[[id]]
                        } else {
                            value = dplyr::filter(dataEX_criteria_climate_secteur_sh_wl,
                                                  climateChain == row_id[rr])[[column_id[cc]]]
                        }

                        format_value = function(x, n=0) {
                            if (abs(x) >= 100) {
                                value = round(x, n-1)
                            } else {
                                value = round(x, n)                                
                            }
                            return (value)
                        }

                        # print("aa")
                        # print(value)                        
                        # print(row_id[rr])
                        # print(column_id[cc])
                        # print(dataEX_criteria_climate_secteur_sh_wl)
                        # print("bb")
                        
                        if (grepl("T", column_name[cc])) {
                            value = format_value(value, 1)
                            Palette_tmp = Palette_temperature
                            nColor_tmp = nColor_temperature
                            digits = 1
                        } else if (grepl("P", column_name[cc])) {
                            value = format_value(value, 0)
                            Palette_tmp = Palette_hydro
                            nColor_tmp = nColor_hydro
                            digits = NULL
                        }
                        valueC = get_labels_TeX(value, unit=column_unit[cc],
                                                is_unit_plurial=FALSE,
                                                add_unit_space=TRUE,
                                                is_unit_zero=TRUE,
                                                digits=digits)

                        if (value < 0) {
                            color = Palette_tmp[1 + dColor]
                        } else if (value > 0) {
                            color = Palette_tmp[nColor_tmp - dColor]
                        } else {
                            color = IPCCgrey50
                        }

                        table = table +
                            annotate("text",
                                     x=xtmp - dx_column*0.3,
                                     y=ytmp + dy_row/2,
                                     label=latex2exp::TeX(valueC),
                                     vjust=0.5, hjust=1,
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
            info_text = c("Changements projetés en température et précipitations (référence : 1991-2020)",
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
                "legend",       "legend",         "map_legend",     "colorbar"
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

            margin_map = margin(t=0, r=2, b=0, l=0, unit="mm")

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
                        color="#d1ecec",
                        fill=NA,
                        linewidth=0.22,
                        na.rm=TRUE) +
                geom_sf(data=rivers_in_secteur,
                        color=INRAElightcyan,
                        fill=NA,
                        linewidth=0.28,
                        na.rm=TRUE) +
                
                geom_sf(data=Shapefiles$secteurHydro,
                        color=IPCCgrey80,
                        fill=NA, lineend="round",
                        linewidth=0.28) +
                
                geom_sf(data=Shapefiles$bassinHydro,
                        color=IPCCgrey67,
                        fill=NA, lineend="round",
                        linewidth=0.29) +
                
                geom_sf(data=Shapefiles$france,
                        color=IPCCgrey48,
                        fill=NA, lineend="round",
                        linewidth=0.30) +
                
                geom_sf(data=secteurHydro_shp,
                        color=IPCCgrey50,
                        fill=NA, lineend="round",
                        linewidth=0.45)

            bbox = sf::st_bbox(secteurHydro_shp)
            aspect_plot = 6.3/8.5
            margin_factor = 0.05
            
            x_range = bbox["xmax"] - bbox["xmin"]
            y_range = bbox["ymax"] - bbox["ymin"]
            aspect_bbox = as.numeric(y_range/x_range)

            if (aspect_bbox > aspect_plot) {
                target_x_range = as.numeric(y_range/aspect_plot)
                x_center = (bbox["xmin"] + bbox["xmax"])/2
                xlim_map = c(x_center - target_x_range/2, x_center + target_x_range/2)
                ylim_map = c(bbox["ymin"], bbox["ymax"])
            } else {
                target_y_range = as.numeric(x_range * aspect_plot)
                y_center = (bbox["ymin"] + bbox["ymax"])/2
                ylim_map = c(y_center - target_y_range/2, y_center + target_y_range/2)
                xlim_map = c(bbox["xmin"], bbox["xmax"])
            }

            xlim_center = mean(xlim_map)
            ylim_center = mean(ylim_map)

            xlim_range = diff(xlim_map)
            ylim_range = diff(ylim_map)

            xlim_map = c(xlim_center - xlim_range/2 * (1 + margin_factor),
                         xlim_center + xlim_range/2 * (1 + margin_factor))
            ylim_map = c(ylim_center - ylim_range/2 * (1 + margin_factor),
                         ylim_center + ylim_range/2 * (1 + margin_factor))

            map = map + coord_sf(xlim=xlim_map, ylim=ylim_map, expand=FALSE)


            nlim = 9
            Stations_sh_selection = dplyr::filter(Stations_sh, n_rcp85 >= nlim)
            while (nrow(Stations_sh_selection) < 3 & nrow(Stations_sh_selection) < nrow(Stations_sh)) {
                nlim = nlim - 1
                Stations_sh_selection = dplyr::filter(Stations_sh, n_rcp85 >= nlim)
            }

            Stations_sh_selection = dplyr::filter(Stations_sh_selection,
                                                  surface_km2 == max(surface_km2, na.rm=TRUE) |
                                                  row_number() == which.min(abs(surface_km2-median(surface_km2,
                                                                                                   na.rm=TRUE))) |
                                                  surface_km2 == min(surface_km2, na.rm=TRUE))
            Stations_sh_selection = dplyr::arrange(Stations_sh_selection, dplyr::desc(surface_km2))



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
                                               code, deltaVCN10_above_pct,
                                               deltaVCN10_below_pct),
                                 by="code")
            dataEX_criteria_hydro_plot_sh_wl =
                dplyr::left_join(dataEX_criteria_hydro_plot_sh_wl,
                                 dplyr::select(Stations, code, XL93_m, YL93_m),
                                 by="code")

            
            dataEX_criteria_hydro_plot_sh_wl$shape = 21 #o
            dataEX_criteria_hydro_plot_sh_wl$size = 1.4

            Ok_above = dataEX_criteria_hydro_plot_sh_wl$deltaVCN10_above_pct >= limit_conf_pct
            dataEX_criteria_hydro_plot_sh_wl$shape[Ok_above] = 24 #^
            dataEX_criteria_hydro_plot_sh_wl$size[Ok_above] = 2
            
            Ok_below = dataEX_criteria_hydro_plot_sh_wl$deltaVCN10_below_pct >= limit_conf_pct
            dataEX_criteria_hydro_plot_sh_wl$shape[Ok_below] = 25 #v
            dataEX_criteria_hydro_plot_sh_wl$size[Ok_below] = 2
            
            res = get_colors(dataEX_criteria_hydro_plot_sh_wl$deltaVCN10,
                              upBin=Palette_bin$upBin,
                              lowBin=Palette_bin$lowBin,
                              Palette=Palette_hydro,
                              Palette_layers=Palette_layers)
            dataEX_criteria_hydro_plot_sh_wl$fill = res$colors
            dataEX_criteria_hydro_plot_sh_wl$layer = res$layers

            layers = as.numeric(levels(factor(dataEX_criteria_hydro_plot_sh_wl$layer)))


            # dataEX_criteria_hydro_plot_sh_wl_light = dplyr::filter(dataEX_criteria_hydro_plot_sh_wl,
                                                                   # code %in% Stations_sh_selection$code)
            # dataEX_criteria_hydro_plot_sh_wl_NOlight = dplyr::filter(dataEX_criteria_hydro_plot_sh_wl,
                                                                   # !(code %in% Stations_sh_selection$code))
            map_etiage = map
            for (l in layers) {
                dataEX_plot_tmp = dplyr::filter(dataEX_criteria_hydro_plot_sh_wl, layer==l)
                if (nrow(dataEX_plot_tmp) == 0) {
                    next
                }
                map_etiage = map_etiage +
                    annotate("point",
                             x=dataEX_plot_tmp$XL93_m,
                             y=dataEX_plot_tmp$YL93_m,
                             fill=dataEX_plot_tmp$fill,
                             color=IPCCgrey23,
                             stroke=0.35,
                             size=dataEX_plot_tmp$size,
                             shape=dataEX_plot_tmp$shape)
            }
            # map_etiage = map_etiage +
            #     annotate("point",
            #              x=dataEX_criteria_hydro_plot_sh_wl_light$XL93_m,
            #              y=dataEX_criteria_hydro_plot_sh_wl_light$YL93_m,
            #              fill=dataEX_criteria_hydro_plot_sh_wl_light$fill,
            #              color=IPCCgold,
            #              stroke=0.35,
            #              size=dataEX_criteria_hydro_plot_sh_wl_light$size,
            #              shape=dataEX_criteria_hydro_plot_sh_wl_light$shape)
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=map_etiage,
                                      id="map_etiage",
                                      height=hydroMap_variable_map_height,
                                      width=hydroMap_variable_width,
                                      verbose=verbose)

#### recharge ________________________________________________________
            title_text = paste0("(", letters[id_letter], ") \\textbf{Recharge$_{$annuelle}} - Changements relatifs de la recharge annuelle (%)")
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


            
            MESO = Shapefiles$MESO
            MESO = dplyr::filter(MESO, CdEuMasseD %in% dataEX_criteria_recharge_MESO_mean_wl$code_MESO)
            colors = get_colors(dataEX_criteria_recharge_MESO_mean_wl$deltaRecharge,
                                upBin=Palette_bin$upBin,
                                lowBin=Palette_bin$lowBin,
                                Palette=Palette_hydro)
            dataEX_criteria_recharge_MESO_mean_wl$color = colors
            MESO = dplyr::full_join(MESO,
                                    dplyr::select(dataEX_criteria_recharge_MESO_mean_wl,
                                                  CdEuMasseD=code_MESO,
                                                  color),
                                    by="CdEuMasseD")
            MESO$color[MESO$Non_Aqfr > 0.5] = "#FFD700"
            
            
            map_recharge = ggplot() + theme_void() + cf +
                theme(plot.margin=margin_map) +
                
                geom_sf(data=Shapefiles$france,
                        color=NA, alpha=0.35,
                        fill=IPCCgrey97) +
                
                
                geom_sf(data=MESO,
                        color=NA,
                        fill=MESO$color) +
    
    
                geom_sf(data=Shapefiles$river,
                        color=INRAElightcyan,#"#d1ecec",
                        fill=NA,
                        linewidth=0.22,
                        na.rm=TRUE) +
                geom_sf(data=rivers_in_secteur,
                        color=INRAElightcyan,
                        fill=NA,
                        linewidth=0.28,
                        na.rm=TRUE) +
                
                # geom_sf(data=Shapefiles$secteurHydro,
                        # color=IPCCgrey80,
                        # fill=NA, lineend="round",
                        # linewidth=0.28) +
                
                geom_sf(data=Shapefiles$bassinHydro,
                        color=IPCCgrey67,
                        fill=NA, lineend="round",
                        linewidth=0.29) +
                
                geom_sf(data=Shapefiles$france,
                        color=IPCCgrey48,
                        fill=NA, lineend="round",
                        linewidth=0.30) +
                
                geom_sf(data=secteurHydro_shp,
                        color=IPCCgrey50,
                        fill=NA, lineend="round",
                        linewidth=0.45)


            dataEX_criteria_hydro_plot_sh_wl_light = dplyr::filter(dataEX_criteria_hydro_plot_sh_wl,
                                                                   code %in% Stations_sh_selection$code)
            dataEX_criteria_hydro_plot_sh_wl_light = dplyr::left_join(dataEX_criteria_hydro_plot_sh_wl_light,
                                                                      dplyr::select(Stations_sh_selection, code,
                                                                                    surface_km2),
                                                                      by="code")
            dataEX_criteria_hydro_plot_sh_wl_light = dplyr::arrange(dataEX_criteria_hydro_plot_sh_wl_light,
                                                                    dplyr::desc(surface_km2))
            
            dataEX_criteria_hydro_plot_sh_wl_light$label = 1:nrow(dataEX_criteria_hydro_plot_sh_wl_light)
            
            map_recharge = map_recharge + coord_sf(xlim=xlim_map, ylim=ylim_map, expand=FALSE) +
                shadowtext::geom_shadowtext(data=dataEX_criteria_hydro_plot_sh_wl_light,
                                            aes(x=XL93_m,
                                                y=YL93_m,
                                                label=label),
                                            family="Lato",
                                            fontface="bold",
                                            vjust=0.5, hjust=0.5,
                                            color=IPCCgrey35, size=2,
                                            bg.color=IPCCgrey97, bg.r=0.15)
            
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

            dataEX_criteria_hydro_plot_sh_wl =
                dplyr::left_join(dplyr::select(dataEX_criteria_hydro_mean_sh_wl,
                                               code, deltaQJXA),
                                 dplyr::select(dataEX_criteria_hydro_conf_sh_wl,
                                               code, deltaQJXA_above_pct,
                                               deltaQJXA_below_pct),
                                 by="code")
            dataEX_criteria_hydro_plot_sh_wl =
                dplyr::left_join(dataEX_criteria_hydro_plot_sh_wl,
                                 dplyr::select(Stations, code, XL93_m, YL93_m),
                                 by="code")

            
            dataEX_criteria_hydro_plot_sh_wl$shape = 21 #o
            dataEX_criteria_hydro_plot_sh_wl$size = 1.4

            Ok_above = dataEX_criteria_hydro_plot_sh_wl$deltaQJXA_above_pct >= limit_conf_pct
            dataEX_criteria_hydro_plot_sh_wl$shape[Ok_above] = 24 #^
            dataEX_criteria_hydro_plot_sh_wl$size[Ok_above] = 2
            
            Ok_below = dataEX_criteria_hydro_plot_sh_wl$deltaQJXA_below_pct >= limit_conf_pct
            dataEX_criteria_hydro_plot_sh_wl$shape[Ok_below] = 25 #v
            dataEX_criteria_hydro_plot_sh_wl$size[Ok_below] = 2
            
            res = get_colors(dataEX_criteria_hydro_plot_sh_wl$deltaQJXA,
                              upBin=Palette_bin$upBin,
                              lowBin=Palette_bin$lowBin,
                              Palette=Palette_hydro,
                              Palette_layers=Palette_layers)
            dataEX_criteria_hydro_plot_sh_wl$fill = res$colors
            dataEX_criteria_hydro_plot_sh_wl$layer = res$layers

            layers = as.numeric(levels(factor(dataEX_criteria_hydro_plot_sh_wl$layer)))
            
            # dataEX_criteria_hydro_plot_sh_wl_light = dplyr::filter(dataEX_criteria_hydro_plot_sh_wl,
                                                                   # code %in% Stations_sh_selection$code)
            # dataEX_criteria_hydro_plot_sh_wl_NOlight = dplyr::filter(dataEX_criteria_hydro_plot_sh_wl,
                                                                     # !(code %in% Stations_sh_selection$code))

            map_crue = map
            for (l in layers) {
                dataEX_plot_tmp = dplyr::filter(dataEX_criteria_hydro_plot_sh_wl, layer==l)
                if (nrow(dataEX_plot_tmp) == 0) {
                    next
                }
                map_crue = map_crue +
                    annotate("point",
                             x=dataEX_plot_tmp$XL93_m,
                             y=dataEX_plot_tmp$YL93_m,
                             fill=dataEX_plot_tmp$fill,
                             color=IPCCgrey23,
                             stroke=0.35,
                             size=dataEX_plot_tmp$size,
                             shape=dataEX_plot_tmp$shape)
            }
            # map_crue = map_crue +
            #     annotate("point",
            #              x=dataEX_criteria_hydro_plot_sh_wl_light$XL93_m,
            #              y=dataEX_criteria_hydro_plot_sh_wl_light$YL93_m,
            #              fill=dataEX_criteria_hydro_plot_sh_wl_light$fill,
            #              color=IPCCgold,
            #              stroke=0.35,
            #              size=dataEX_criteria_hydro_plot_sh_wl_light$size,
            #              shape=dataEX_criteria_hydro_plot_sh_wl_light$shape)


            # width_map = xlim_map[2]-xlim_map[1]
            # target_width = width_map/5
            # echelle = c(0,
            #             max(pretty(c(0, target_width^0.9), n=1)),
            #             max(pretty(c(0, target_width), n=1)))
            
            # echelle_line_size = 0.3
            # echelle_size = 2
            # echelle_km_size = 1.8
            # echelle_tick_height = 2

            # ymin_km_shift = 2
            # ymin_km_value_shift = 1

            # x_echelle_pct = 3
            # y_echelle_pct = 4
            # xmin_ech = gpct(x_echelle_pct, xlim_map, shift=TRUE)
            # ymin_ech = gpct(y_echelle_pct, ylim_map, shift=TRUE)
            # ymin_km = gpct(max(c(y_echelle_pct-ymin_km_shift, 0)), ylim_map, shift=TRUE)
            # ymax_ech = ymin_ech + gpct(echelle_tick_height, ylim_map)

            # map_crue = map_crue +
            #     annotate("line",
            #              x=c(xmin_ech, max(echelle)+xmin_ech),
            #              y=c(ymin_ech, ymin_ech),
            #               color=IPCCgrey40, size=echelle_line_size,
            #              lineend="round") +
            #     annotate("text",
            #              x=max(echelle)+xmin_ech+gpct(1, xlim_map), y=ymin_km,
            #              vjust=0, hjust=0, label="km",
            #              family="Lato",
            #              color=IPCCgrey40, size=echelle_km_size)

            # for (x_ech in echelle) {
            #     map_crue = map_crue +
            #         annotate("segment",
            #                  x=x_ech+xmin_ech, xend=x_ech+xmin_ech, y=ymin_ech, yend=ymax_ech,
            #                  color=IPCCgrey40, size=echelle_line_size,
            #                  lineend="round") +
            #         annotate("text",
            #                  x=x_ech+xmin_ech, y=ymax_ech+gpct(ymin_km_value_shift, ylim_map),
            #                  vjust=0, hjust=0.5, label=x_ech/1E3,
            #                  family="Lato",
            #                  color=IPCCgrey40, size=echelle_size)
            # }
            
            
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
                         x=0.15, y=dy,
                         label=latex2exp::TeX("\\textbf{Consensus} du signe pour 80% des projections :"),
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) +
                
                annotate("point",
                         x=4.95-0.19, y=dy-0.05,
                         shape=24,
                         size=2, stroke=stroke, fill=NA,
                         color=IPCCgrey50) +
                annotate("text",
                         x=4.95, y=dy,
                         label="à la hausse,",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) +
                
                annotate("point",
                         x=6.45-0.19, y=dy+0.05,
                         shape=25,
                         size=2, stroke=stroke, fill=NA,
                         color=IPCCgrey50) +
                annotate("text",
                         x=6.45, y=dy,
                         label="à la baisse,",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) +
                
                annotate("point",
                         x=7.9-0.16, y=dy,
                         shape=21,
                         size=2, stroke=stroke, fill=NA,
                         color=IPCCgrey50) +
                annotate("text",
                         x=7.9, y=dy,
                         label="sans consensus.",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50)

            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=legend,
                                      id="legend",
                                      height=hydroMap_legend_height,
                                      verbose=verbose)



            aspect_legend = 80/1040
            ylim_legend = c(0, (xlim_map[2]-xlim_map[1])*aspect_legend)
            
            map_legend = ggplot() + theme_void() + cf +
                theme(plot.margin=margin_map) +
                coord_sf(xlim=xlim_map, ylim=ylim_legend, expand=FALSE)
            

            width_map = xlim_map[2]-xlim_map[1]
            target_width = width_map/5
            echelle = c(0,
                        max(pretty(c(0, target_width^0.9), n=1)),
                        max(pretty(c(0, target_width), n=1)))
            
            echelle_line_size = 0.3
            echelle_size = 2
            echelle_km_size = 1.8
            echelle_tick_height = 16
            echelle_text_shift = 10
            
            ymin_km_shift = 10
            ymin_km_value_shift = 10

            x_echelle_pct = 93
            y_echelle_pct = 30
            xmax_ech = gpct(x_echelle_pct, xlim_map, shift=TRUE)
            ymin_ech = gpct(y_echelle_pct, ylim_legend, shift=TRUE)
            ymin_km = gpct(max(c(y_echelle_pct-ymin_km_shift, 0)), ylim_legend, shift=TRUE)
            ymax_ech = ymin_ech + gpct(echelle_tick_height, ylim_legend)

            x_symbol = gpct(2, xlim_map, shift=TRUE)
            x_symbol_text = gpct(6, xlim_map, shift=TRUE)
            
            map_legend = map_legend +
                annotate("point",
                         x=x_symbol, y=mean(ylim_legend),
                         shape=15,
                         size=2, color="#FFD700") +
                annotate("text",
                         x=x_symbol_text, y=mean(ylim_legend),
                         label="Absence d'aquifère",
                         size=2.4, hjust=0, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey50) +
                
                annotate("line",
                         x=c(xmax_ech, xmax_ech-max(echelle)),
                         y=c(ymin_ech, ymin_ech),
                          color=IPCCgrey50, size=echelle_line_size,
                         lineend="round") +
                annotate("text",
                         x=xmax_ech+gpct(1, xlim_map), y=ymin_km,
                         vjust=0, hjust=0, label="km",
                         family="Lato",
                         color=IPCCgrey50, size=echelle_km_size)

            for (x_ech in echelle) {
                map_legend = map_legend +
                    annotate("segment",
                             x=xmax_ech-max(echelle)+x_ech, xend=xmax_ech-max(echelle)+x_ech,
                             y=ymin_ech, yend=ymax_ech,
                             color=IPCCgrey50, size=echelle_line_size,
                             lineend="round") +
                    annotate("text",
                             x=xmax_ech-max(echelle)+x_ech,
                             y=ymax_ech+gpct(ymin_km_value_shift, ylim_legend),
                             vjust=0, hjust=0.5, label=x_ech/1E3,
                             family="Lato",
                             color=IPCCgrey50, size=echelle_size)
            }
            
            hydroMap_herd = add_sheep(hydroMap_herd,
                                      sheep=map_legend,
                                      id="map_legend",
                                      height=hydroMap_legend_height,
                                      verbose=verbose)

            

            label = get_labels_TeX(Palette_bin$bin, unit="%",
                                   is_unit_plurial=FALSE,
                                   add_unit_space=TRUE)
            
            colorbar = panel_colorbar_circle(Palette_bin$bin,
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
                                             colorText=IPCCgrey40,
                                             colorLine=IPCCgrey40,
                                             on_circle=FALSE,
                                             margin=margin(t=-1, r=1, b=2, l=2, "mm"))

            colorbar = colorbar +
                annotate("text",
                         x=0.7, y=-1,
                         label=latex2exp::TeX("\\small{référence}"),
                         family="Lato",
                         color=IPCCgrey40, 
                         vjust=0, hjust=0.5,
                         size=2.5) + 
                annotate("text",
                         x=0.7, y=-1.5,
                         label=latex2exp::TeX("\\small{1991-2020}"),
                         family="Lato",
                         color=IPCCgrey40,
                         vjust=0, hjust=0.5,
                         size=2.5)
            
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
                "river_void", "river_void",    "river_void",    "river_void",
                "delta_info", "river_delta_1", "river_delta_2", "river_delta_3",
                "void",       "axis_1",        "axis_2",         "axis_3"
            ), ncol=4, byrow=TRUE)


            hydroQM_title_height = 0.2
            hydroQM_river_name_height = 0.2
            hydroQM_river_graph_height = 1
            hydroQM_river_void_height = 0.00
            hydroQM_axis_height = 0.15
            
            
            hydroQM_river_width = 1
            hydroQM_info_width = 0.2  
            
            hydroQM_herd = bring_grass(verbose=verbose)
            hydroQM_herd = plan_of_herd(hydroQM_herd, hydroQM_plan,
                                        verbose=verbose)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=void(panel.background_fill=IPCCgrey97),
                                     id="void",
                                     verbose=verbose)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=void(panel.background_fill=IPCCgrey97),
                                     id="river_void",
                                     height=hydroQM_river_void_height,
                                     verbose=verbose)

#### title ___________________________________________________________
            title_text = paste0("(", letters[id_letter], ") \\textbf{QM} - Régime hydrologique et changements relatifs des débits mensuels moyens pour trois bassins versants \\;\\small{référence 1991-2020}")
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
                theme(plot.background=element_rect(fill=IPCCgrey97,
                                                   color=NA),
                      plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("text",
                         x=0.2, y=0.52,
                         label=latex2exp::TeX("\\textbf{Période de RÉFÉRENCE}"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.48, y=0.52,
                         label=latex2exp::TeX("\\textbf{1991-2020}"),
                         size=2.3, hjust=0.5, vjust=0.5,
                         angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.8, y=0.52,
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
                theme(plot.background=element_rect(fill=IPCCgrey97,
                                                   color=NA),
                      plot.margin=margin(t=0, r=0,
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
            days_span_mean = 10
            days_span_delta = 6
            alpha_mean = 0.23
            alpha_delta = 0.07
            size_mean = 0.35
            size_delta = 0.3
            alpha_delta_rect = 0.2

            loli_fact = 2
            
            get_labels_deltaQ = function(x) {
                result = get_labels_HTML(x, unit="%",
                                         is_unit_plurial=FALSE,
                                         add_unit_space=TRUE,
                                         Palette=Palette_hydro,
                                         dColor=dColor,
                                         correct_space=TRUE)
                return (result)
            }

            color_minus = Palette_hydro[1 + dColor]
            color_plus = Palette_hydro[nColor_hydro - dColor]

            nRiver = nrow(Stations_sh_selection)
            for (k in 1:3) {
                if (k > nRiver) {
                    plot = ggplot() + theme_void() +
                        theme(plot.background=element_rect(fill=IPCCgrey97,
                                                           color=NA))
                    hydroQM_herd = add_sheep(hydroQM_herd,
                                             sheep=plot,
                                             id=paste0("river_name_", k),
                                             height=hydroQM_river_name_height,
                                             width=hydroQM_river_width,
                                             label=paste0("align_", k),
                                             verbose=verbose)
                    hydroQM_herd = add_sheep(hydroQM_herd,
                                             sheep=plot,
                                             id=paste0("river_ref_", k),
                                             height=hydroQM_river_graph_height,
                                             width=hydroQM_river_width,
                                             label=paste0("align_", k),
                                             verbose=verbose)
                    hydroQM_herd = add_sheep(hydroQM_herd,
                                             sheep=plot,
                                             id=paste0("river_delta_", k),
                                             height=hydroQM_river_graph_height,
                                             width=hydroQM_river_width,
                                             label=paste0("align_", k),
                                             verbose=verbose)
                    hydroQM_herd = add_sheep(hydroQM_herd,
                                             sheep=plot,
                                             id=paste0("axis_", k),
                                             height=hydroQM_axis_height,
                                             label=paste0("align_", k),
                                             verbose=verbose)
                    next
                }
                
                river_code = Stations_sh_selection$code[k]
                river_name = Stations_sh_selection$river_name[k]

                dataEX_serie_hydro_sh_wl_code = dataEX_serie_hydro_sh_wl
                for (cc in 1:length(dataEX_serie_hydro_sh_wl_code)) {
                    dataEX_serie_hydro_sh_wl_code[[cc]] =
                        dplyr::filter(dataEX_serie_hydro_sh_wl_code[[cc]],
                                      code == river_code)
                }
                dataEX_serie_hydro_deltaQMA_prob_wl_code =
                    dplyr::filter(dataEX_serie_hydro_deltaQMA_prob_wl,
                                  code == river_code)
                
                river_surface = Stations$surface_km2[Stations$code == river_code]
                text = paste0(k, ". \\textbf{", river_code, " - ", river_name, "} \\small{",
                              " ", signif(river_surface, 3), " km$^2$}")
                river_name = ggplot() + theme_void() +
                    theme(plot.background=element_rect(fill=IPCCgrey97,
                                                       color=NA),
                          plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) +
                    scale_x_continuous(limits=c(0, 1),
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=c(0, 1),
                                       expand=c(0, 0)) +
                    annotate("text",
                             x=0, y=0.5,
                             label=latex2exp::TeX(text),
                             size=2.4, hjust=0, vjust=0.5,
                             family="Lato",
                             color=IPCCgrey35)
                
                hydroQM_herd = add_sheep(hydroQM_herd,
                                         sheep=river_name,
                                         id=paste0("river_name_", k),
                                         height=hydroQM_river_name_height,
                                         width=hydroQM_river_width,
                                         label=paste0("align_", k),
                                         verbose=verbose)


                if (is_MPI) {
                    river_ref = ggplot() + coord_cartesian(clip="off") + 
                        theme_IPCC(is_plot.background=TRUE,
                                   is_panel.grid.major.x=FALSE,
                                   is_panel.grid.major.y=TRUE,
                                   is_axis.line.x=FALSE,
                                   # axis.line.x_size=0.33,
                                   is_axis.ticks.x=FALSE,
                                   is_axis.ticks.y=FALSE,
                                   is_axis.text.x=FALSE,
                                   axis.ticks.length.x=0.8,
                                   
                                   axis.text.y_vjust=0.4,
                                   axis.text.y_margin=margin(r=1, unit="mm"))
                } else {
                    river_ref = ggplot() + coord_cartesian(clip="off") + 
                        theme_IPCC(is_plot.background=TRUE,
                                   is_panel.grid.major.x=FALSE,
                                   is_panel.grid.major.y=TRUE,
                                   is_axis.line.x=FALSE,
                                   # axis.line.x_size=0.33,
                                   is_axis.ticks.x=FALSE,
                                   is_axis.ticks.y=FALSE,
                                   is_axis.text.x=FALSE,
                                   axis.ticks.length.x=0.8)
                }
               
                river_ref = river_ref +
                    theme(plot.margin=margin(t=1, r=2, b=4, l=1, "mm")) +
                    scale_x_date(expand=c(0, 0)) +
                    scale_y_continuous(limits=c(0, NA),
                                       expand=c(0, 0)) + 
                    geom_segment(data=dataEX_serie_hydro_sh_wl_code$meanQMA,
                                 aes(x=date-days_span_mean, xend=date+days_span_mean,
                                     y=meanQMA, yend=meanQMA, group=Chain),
                                 color=IPCCgrey67,
                                 alpha=alpha_mean, size=size_mean)
                
                hydroQM_herd = add_sheep(hydroQM_herd,
                                         sheep=river_ref,
                                         id=paste0("river_ref_", k),
                                         height=hydroQM_river_graph_height,
                                         width=hydroQM_river_width,
                                         label=paste0("align_", k),
                                         verbose=verbose)

                date_axis = unique(dataEX_serie_hydro_sh_wl_code$deltaQMA$date)
                date_axis_lim = c(min(date_axis)-days_span_mean, max(date_axis)+days_span_mean)

                dataEX_serie_hydro_sh_wl_code_deltaQMAplus =
                    dplyr::filter(dataEX_serie_hydro_sh_wl_code$deltaQMA,
                                  deltaQMA > 0)
                dataEX_serie_hydro_sh_wl_code_deltaQMAminus =
                    dplyr::filter(dataEX_serie_hydro_sh_wl_code$deltaQMA,
                                  deltaQMA <= 0)

                dataEX_serie_hydro_deltaQMA_prob_wl_code_minBelow =
                    dplyr::filter(dataEX_serie_hydro_deltaQMA_prob_wl_code,
                                  min_deltaQMA <= 0)
                dataEX_serie_hydro_deltaQMA_prob_wl_code_minBelow =
                    dplyr::mutate(dplyr::group_by(dataEX_serie_hydro_deltaQMA_prob_wl_code_minBelow,
                                                  date),
                                  max_deltaQMA=min(c(0, max_deltaQMA)))
                dataEX_serie_hydro_deltaQMA_prob_wl_code_maxAbove = 
                    dplyr::filter(dataEX_serie_hydro_deltaQMA_prob_wl_code,
                                  0 < max_deltaQMA)
                dataEX_serie_hydro_deltaQMA_prob_wl_code_maxAbove =
                    dplyr::mutate(dplyr::group_by(dataEX_serie_hydro_deltaQMA_prob_wl_code_maxAbove,
                                                  date),
                                  min_deltaQMA=max(c(0, min_deltaQMA)))

                dataEX_serie_hydro_sh_wl_code_narratracc =
                    dplyr::filter(dataEX_serie_hydro_sh_wl_code$deltaQMA,
                                  Chain %in% NarraTRACC_sh_Chain)

                dataEX_serie_hydro_sh_wl_code_narratracc$color = NA
                for (nt in 1:nNarraTRACC) {
                    Ok = dataEX_serie_hydro_sh_wl_code_narratracc$Chain == NarraTRACC_sh_Chain[nt]
                    dataEX_serie_hydro_sh_wl_code_narratracc$color[Ok] = NarraTRACC_sh_color[nt]
                }

                if (is_MPI) {
                    river_delta = ggplot() + coord_cartesian(clip="off") + 
                        theme_IPCC(is_plot.background=TRUE,
                                   is_panel.grid.major.x=FALSE,
                                   is_panel.grid.major.y=TRUE,
                                   is_axis.line.x=FALSE,
                                   is_axis.ticks.x=FALSE,
                                   is_axis.ticks.y=FALSE,
                                   is_axis.text.x=FALSE,
                                   is_axis.text.y=k==1,
                                   axis.ticks.length.x=0.8,
                                   
                                   axis.text.y_vjust=0.4,
                                   axis.text.y_margin=margin(r=1, unit="mm"))
                    
                } else {
                    river_delta = ggplot() + coord_cartesian(clip="off") + 
                        theme_IPCC(is_plot.background=TRUE,
                                   is_panel.grid.major.x=FALSE,
                                   is_panel.grid.major.y=TRUE,
                                   is_axis.line.x=FALSE,
                                   is_axis.ticks.x=FALSE,
                                   is_axis.ticks.y=FALSE,
                                   is_axis.text.x=FALSE,
                                   is_axis.text.y=k==1,
                                   axis.ticks.length.x=0.8)
                    
                }
                
                river_delta = river_delta +
                    theme(plot.margin=margin(t=4, r=2, b=1, l=1, "mm")) +
                    scale_x_date(expand=c(0, 0)) +
                    coord_cartesian(ylim=c(-100, 100)) +
                    scale_y_continuous(labels=get_labels_deltaQ,
                                       expand=c(0, 0)) +
                    
                    geom_rect(data=dataEX_serie_hydro_deltaQMA_prob_wl_code,
                              aes(xmin=date-days_span_delta,
                                  xmax=date+days_span_delta,
                                  ymin=min_deltaQMA,
                                  ymax=max_deltaQMA,
                                  goups=date),
                             fill=IPCCgrey97,
                             color=NA) +
                    geom_rect(data=dataEX_serie_hydro_deltaQMA_prob_wl_code_minBelow,
                              aes(xmin=date-days_span_delta,
                                  xmax=date+days_span_delta,
                                  ymin=min_deltaQMA,
                                  ymax=max_deltaQMA,
                                  goups=date),
                              alpha=alpha_delta_rect,
                              fill=color_minus,
                              color=NA) +
                    geom_rect(data=dataEX_serie_hydro_deltaQMA_prob_wl_code_maxAbove,
                              aes(xmin=date-days_span_delta,
                                  xmax=date+days_span_delta,
                                  ymin=min_deltaQMA,
                                  ymax=max_deltaQMA,
                                  goups=date),
                              alpha=alpha_delta_rect,
                              fill=color_plus,
                              color=NA) +
                    
                    # geom_segment(data=dataEX_serie_hydro_sh_wl_code_deltaQMAplus,
                    #              aes(x=date-days_span_delta, xend=date+days_span_delta,
                    #                  y=deltaQMA, yend=deltaQMA, group=Chain),
                    #              color=color_plus, lineend="round",
                    #              alpha=alpha_delta, size=size_delta) +
                    # geom_segment(data=dataEX_serie_hydro_sh_wl_code_deltaQMAminus,
                    #              aes(x=date-days_span_delta, xend=date+days_span_delta,
                    #                  y=deltaQMA, yend=deltaQMA, group=Chain),
                    #              color=color_minus, lineend="round",
                    #              alpha=alpha_delta, size=size_delta) +
                    annotate("line",
                             x=date_axis_lim, 
                             y=0,
                             color=IPCCgrey60,
                             linewidth=0.33) +
                    
                    
                    # geom_segment(data=dataEX_serie_hydro_sh_wl_code_narratracc,
                                 # aes(x=date-days_span_delta,
                                     # xend=date+days_span_delta*loli_fact,
                                     # y=deltaQMA, yend=deltaQMA, group=Chain),
                             # color=IPCCgrey97,
                             # linewidth=0.3, lineend="round") +
                    annotate("point",
                             x=dataEX_serie_hydro_sh_wl_code_narratracc$date,#+days_span_delta*loli_fact,
                             y=dataEX_serie_hydro_sh_wl_code_narratracc$deltaQMA,
                             color=IPCCgrey97,
                             size=1,
                             shape=20) +
                    geom_line(data=dataEX_serie_hydro_sh_wl_code_narratracc,
                              aes(x=date,#+days_span_delta*loli_fact,
                                  y=deltaQMA,
                                  group=Chain),
                              color=IPCCgrey97,
                              linewidth=0.5) +
                    
                    # geom_segment(data=dataEX_serie_hydro_sh_wl_code_narratracc,
                    #              aes(x=date-days_span_delta,
                    #                  xend=date+days_span_delta,#*loli_fact,
                    #                  y=deltaQMA, yend=deltaQMA, group=Chain),
                    #              color=dataEX_serie_hydro_sh_wl_code_narratracc$color,
                    #              linewidth=0.3,
                    #              alpha=0.5, lineend="round") +
                    
                    
                    geom_line(data=dataEX_serie_hydro_sh_wl_code_narratracc,
                              aes(x=date,#+days_span_delta*loli_fact,
                                  y=deltaQMA,
                                  group=Chain,
                                  color=color),
                              linewidth=0.3) +
                    annotate("point",
                             x=dataEX_serie_hydro_sh_wl_code_narratracc$date,#+days_span_delta*loli_fact,
                             y=dataEX_serie_hydro_sh_wl_code_narratracc$deltaQMA,
                             color=dataEX_serie_hydro_sh_wl_code_narratracc$color,
                             size=0.4,
                             shape=20) +
                    
                    scale_color_identity()
                
                        # annotate("line",
                                 # x=c(limits_bar[1],
                                     # limits_bar[2]+dx_sL/2),
                                 # y=plot_y[s],
                                 # color=plot_color[s],
                                 # linewidth=0.4,
                                 # alpha=0.5,
                # lineend="round")
                
                hydroQM_herd = add_sheep(hydroQM_herd,
                                         sheep=river_delta,
                                         id=paste0("river_delta_", k),
                                         height=hydroQM_river_graph_height,
                                         width=hydroQM_river_width,
                                         label=paste0("align_", k),
                                         verbose=verbose)

                
                axis = ggplot() + theme_void() +
                    theme(plot.background=element_rect(fill=IPCCgrey97,
                                                       color=NA),
                          text=element_text(family="Lato"),
                          plot.margin=margin(t=0, r=0,
                                             b=1, l=0, "mm"),
                          axis.text.x=element_text(color=IPCCgrey40,
                                                   # face="bold",
                                                   size=7)) +
                    annotate("point",
                             x=date_axis, y=0.5,
                             color=NA) +
                    scale_x_date(date_breaks="1 month",
                                 limits=date_axis_lim,
                                 labels = function(x) substr(month.name[as.numeric(format(x, "%m"))], 1, 1),
                                 expand=c(0, 0))
                
                hydroQM_herd = add_sheep(hydroQM_herd,
                                         sheep=axis,
                                         id=paste0("axis_", k),
                                         height=hydroQM_axis_height,
                                         label=paste0("align_", k),
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
            title_text = paste0("(", letters[id_letter], ") Changements relatifs projetés en débits et recharge (statistiques sur l'ensemble des médianes spatiales des écarts relatifs) \\;\\small{référence 1991-2020}")
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
            column_id = c("", "deltaVCN10-5", "deltaQA", "deltaRecharge", "deltaQSA_DJF",
                          "deltaQSA_MAM", "deltaQSA_JJA", "deltaQSA_SON", "deltaQJXA-10")
            column_name = c("", "VCN10-5_ans", "QA", "Recharge_ann.", "QS_Hiver",
                            "QS_Printemps", "QS_Été", "QS_Automne", "QJXA-10_ans")
            column_icon = c("", "", "", "",
                            "mode_cool", "emoji_nature",
                            "sunny", "temp_preferences_eco",
                            "")
            column_icon_font = c("", "", "", "",
                                 "Material Symbols Outlined", "Material Symbols Outlined",
                                 "Material Symbols Rounded", "Material Symbols Outlined")
            column_icon_shift = c(0, 0, 0, 0,
                                  0.34, 0.45, 0.29, 0.44,
                                  0)
            column_unit = c("", "%", "%", "%", "%", "%", "%", "%", "%")

            row_id = c("",
                       "max",
                       "median",
                       "min",
                       NarraTRACC_sh_Chain)
            row_id_climateChain = c("",
                                    "max",
                                    "median",
                                    "min",
                                    NarraTRACC_sh_climateChain)
            row_name = c("",
                         "maximum",
                         "médiane",
                         "minimum",
                         NarraTRACC_sh_name)
            row_icon = c("",
                         "expand_circle_up",
                         "do_not_disturb_on",
                         "expand_circle_down",
                         rep("", nNarraTRACC))
            row_color = c("",
                          rep(IPCCgrey35, 3),
                          NarraTRACC_sh_color)

            xmax = 10
            ymax = 10
    
            dx_left_line = 0

            dx_left_icon = 0.12
            dx_left_text = 0.25

            dx_left_narratrac_circle = 0.12
            dx_left_narratrac_text = 0.25

            dx_right_text = 0.1
            
            dy_column_title = 1.5
            dx_row_title = 1.1
            
            nCol = length(column_id) 
            nRow = length(row_id)
            dx_column = (xmax-dx_row_title-dx_right_text)/(nCol-1)
            dy_row = (ymax-dy_column_title)/(nRow-1)
           

            table = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=2, l=0, "mm")) +
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
                        table = table +
                            annotate("text",
                                     x=dx_left_line + dx_left_icon,
                                     y=ytmp + dy_row/2,
                                     label=row_icon[rr],
                                     size=3, hjust=0.5, vjust=0.8,
                                     family="Material Symbols Outlined",
                                     color=IPCCgrey40) +
                            annotate("text",
                                     x=dx_left_line + dx_left_text,
                                     y=ytmp + dy_row/2,
                                     label=row_name[rr],
                                     vjust=0.5, hjust=0,
                                     size=3, family="Lato",
                                     color=IPCCgrey35)
                    } else {
                        table = table +
                            annotate("point",
                                     x=dx_left_line + dx_left_narratrac_circle,
                                     y=ytmp + dy_row/2,
                                     color=row_color[rr], shape=16) +
                            annotate("text",
                                     x=dx_left_line + dx_left_narratrac_text,
                                     y=ytmp + dy_row/2,
                                     label=latex2exp::TeX(row_name[rr]),
                                     vjust=0.5, hjust=0,
                                     size=3, family="Lato",
                                     color=IPCCgrey23)
                    }
                }
                for (cc in 2:nCol) {
                    # if (cc == 3) {
                    #     dx_shift = -dx_column*0.1
                    # } else if (cc == 4) {
                    #     dx_shift = dx_column*0.1
                    # } else {
                    #     dx_shift = 0
                    # }
                    
                    xtmp = dx_row_title + (cc-1)*dx_column #+ dx_shift
                    if (rr == 1) {
                        label = TeX(convert2TeX(column_name[cc], bold=FALSE))
                        table = table +
                            annotate("text",
                                     x=xtmp - dx_column/2 - column_icon_shift[cc],
                                     y=ytmp + dy_column_title*0.5,
                                     label=column_icon[cc],
                                     size=3, hjust=0.5, vjust=0.7,
                                     family=column_icon_font[cc],
                                     color=IPCCgrey40) + 
                            annotate("text",
                                     x=xtmp - dx_column/2,
                                     y=ytmp + dy_column_title*0.5,
                                     label=label,
                                     vjust=0.5, hjust=0.5,
                                     size=3, family="Lato",
                                     color=IPCCgrey23)
                    } else {
                        if (grepl("Recharge", column_id[cc])) {
                            if (rr <= 4) {
                                id = paste0(row_id_climateChain[rr], "_", column_id[cc])
                                value = dataEX_criteria_recharge_secteur_stat_sh_wl[[id]]
                            } else {
                                value = dplyr::filter(dataEX_criteria_recharge_secteur_sh_wl,
                                                      climateChain == row_id_climateChain[rr])[[column_id[cc]]]
                            }
                        } else {
                            if (rr <= 4) {
                                id = paste0(row_id[rr], "_", column_id[cc])
                                value = dataEX_criteria_hydro_secteur_stat_sh_wl[[id]]
                            } else {
                                value = dplyr::filter(dataEX_criteria_hydro_secteur_sh_wl,
                                                      Chain == row_id[rr])[[column_id[cc]]]
                            }
                        }

                        # print("aa")
                        # print(value)                        
                        # print(row_id[rr])
                        # print(column_id[cc])
                        # print(dataEX_criteria_hydro_secteur_sh_wl$Chain)

                        if (!identical(value, numeric(0))) {
                            value = format_value(value, 0)
                            valueC = get_labels_TeX(value, unit=column_unit[cc],
                                                    is_unit_plurial=FALSE,
                                                    add_unit_space=TRUE,
                                                    is_unit_zero=TRUE)
                            
                            if (value < 0) {
                                color = Palette_hydro[1 + dColor]
                            } else if (value > 0) {
                                color = Palette_hydro[nColor_hydro - dColor]
                            } else {
                                color = IPCCgrey40
                            }
                            vjust = 0.5
                            hjust = 1
                        } else {
                            valueC = "—"
                            color = IPCCgrey40
                            vjust = 0.95
                            hjust = 2
                        }

                        table = table +
                            annotate("text",
                                     x=xtmp - dx_column*0.3,
                                     y=ytmp + dy_row/2,
                                     label=latex2exp::TeX(valueC),
                                     vjust=vjust, hjust=hjust,
                                     size=3, family="Lato",
                                     color=color)
                    }
                }
            }
            



            hydroTable_herd = add_sheep(hydroTable_herd,
                                        sheep=table,
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

            foot_plan = matrix(c(
                "Explore2", "content"
            ), ncol=2, byrow=TRUE)

            foot_Explore2_width = 0.12
            foot_content_width = 1
            
            foot_herd = bring_grass(verbose=verbose)
            foot_herd = plan_of_herd(foot_herd, foot_plan,
                                     verbose=verbose)

                        
            Explore2_grob = grid::rasterGrob(png::readPNG(logo_info[["Explore2"]]["path"]),
                                             vjust=0.57,
                                             hjust=0.5,
                                             height=0.9)
            foot_herd = add_sheep(foot_herd,
                                  sheep=Explore2_grob,
                                  id="Explore2",
                                  width=foot_Explore2_width,
                                  verbose=verbose)
            

            foot_text = c("\\textbf{Avertissement} : Il ne s’agit pas de prévisions mais d’indications d’évolutions possibles. Ces fiches sont volontairement synthétiques",
                          "et une notice d’accompagnement fournit des informations pour la lecture et l’interprétation des graphiques de cette fiche.")
            dy_newline = 0.36
            
            content = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 10),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                annotate("line",
                         x=c(0.15, 0.15), y=c(0, 0.62),
                         linewidth=1.5,
                         color=TRACClightblue)

            for (k in 1:length(foot_text)) {
                content = content +             
                    annotate("text",
                             x=0.25,
                             y=0.61 - dy_newline*(k-1),
                             label=latex2exp::TeX(foot_text[k]),
                             size=2, hjust=0, vjust=1,
                             family="Raleway",
                             color=TRACCblue)
            }

            foot_info = paste0("\\small{", format(Sys.Date(), "%B %Y"), "\\; - \\;}\\textbf{",
                               gsub(".*[-]", "", wl["RWLclean"]), "-", secteur$id_secteur, "}")
            
            content = content +
                annotate("text",
                         x=9.98,
                         y=0.15,
                         label=latex2exp::TeX(foot_info),
                         size=3, hjust=1, vjust=0,
                         family="Raleway",
                         color=TRACCblue)         

            foot_herd = add_sheep(foot_herd,
                                  sheep=content,
                                  id="content",
                                  width=foot_content_width,
                                  verbose=verbose)
            
            
            herd = add_sheep(herd,
                             sheep=foot_herd,
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
            #                  device=pdf)


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
            
            # stop()
        }
    }
    return (NULL)
}
