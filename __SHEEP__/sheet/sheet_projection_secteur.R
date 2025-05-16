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


sheet_projection_secteur = function (meta,
                                     dataEX_serie,
                                     metaEX_serie,
                                     dataEX_criteria,
                                     metaEX_criteria,
                                     Colors,
                                     Colors_light,
                                     Names,
                                     historical=c("1976-01-01", "2005-08-31"),
                                     delta_prob=0, 
                                     icon_path="",
                                     logo_info="",
                                     Pages=NULL,
                                     Shapefiles=NULL,
                                     figdir="",
                                     verbose=FALSE) {
    
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    height = 29.7 - page_margin["t"] - page_margin["b"]
    width = 21 - page_margin["l"] - page_margin["r"]
    
    info_height = 3
    climate_height = 6
    hydro_map_height = 7
    hydro_QM_height = 7
    foot_height = 1
    hydro_table_height = height - info_height - climate_height - hydro_map_height - hydro_QM_height - foot_height
    
    plan = matrix(c(
        "info",
        "climat", 
        "hydro_map",
        "hydro_QM",
        "hydro_table",
        "foot"
    ), ncol=1, byrow=TRUE)


    is_numeric = names(dataEX_criteria)[sapply(dataEX_criteria, is.numeric)]
    
    dataEX_criteria_prob =
        dplyr::summarise(dplyr::group_by(dataEX_criteria,
                                         SH),
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
    

    for (i in 1:nSH) {
        sh = SH[i]
        print(paste0(i, "/", nSH, " so ", round(i/nSH*100, 1), "% done -> ", sh))

        Stations_sh = filter(Stations, substr(code, 1, 2) == sh)
        dataEX_criteria_sh = filter(dataEX_criteria, SH == sh)

        Chain = unique(Projections$Chain)
        nChain = length(Chain)
        
        
## 1. PAGE 1 _________________________________________________________
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)

### 1.1. Info ________________________________________________________

        info = panel_info_secteur(
            meta=meta,
            Shapefiles=Shapefiles,
            codeLight=code,
            nProjections=nChain,
            projection_legend=narratif,
            to_do=c("title", "subtitle", "map", "spatial", "projection"),
            if_NA_unkowned=FALSE,
            subtitle_height=0.28,
            echelle=c(0, 250),
            x_echelle_pct=2,
            y_echelle_pct=3,
            echelle_size=2,
            echelle_km_size=1.9,
            echelle_tick_height=3,
            margin_map=margin(t=0, r=0, b=0, l=0, unit="mm"),
            verbose=verbose)
        # info = contour()
        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         width=width,
                         verbose=verbose)

        

### 1.2. Régime ______________________________________________________
        limits_ymax = quantile(c(dataEX_serie_code$medQJ_H0$medQJ_H0,
                                 dataEX_serie_code$medQJ_H2$medQJ_H2,
                                 dataEX_serie_code$medQJ_H3$medQJ_H3),
                               1,
                               na.rm=TRUE)

        hide_y_axis = FALSE

        medQJ_plan = matrix(c("medQJ_H0", "medQJ_H2", "medQJ_H3"),
                            ncol=3, byrow=TRUE)
        medQJ = bring_grass(verbose=verbose)
        medQJ = plan_of_herd(medQJ, medQJ_plan,
                             verbose=verbose)
        
        for (j in 1:nVariables_medQJ) {

            variable = Variables_medQJ[j]
            # print(variable)
            
            dataMOD = dataEX_serie_code[[variable]]
            dataMOD$date = as.Date("1970-01-01") + lubridate::yday(dataMOD$date)-1
            dataMOD_storylines_med =
                dplyr::summarise(
                           dplyr::group_by(
                                      dplyr::filter(dataMOD,
                                                    climateChain %in%
                                                    Storylines),
                                      date,
                                      climateChain),
                           Chain=paste0(climateChain[1], "|median"),
                           !!variable:=median(get(variable), na.rm=TRUE),
                           .groups="drop")

            dataMOD = dplyr::bind_rows(dataMOD, dataMOD_storylines_med)
            start_year = min(dataMOD$date)
                
            variable_SAFRAN = "medQJ_H0"
            dataMOD_SAFRAN = dataEX_serie_code[[variable_SAFRAN]]
            dataMOD_SAFRAN$date = as.Date("1970-01-01") +
                lubridate::yday(dataMOD_SAFRAN$date)-1
            dataMOD_SAFRAN_med =
                dplyr::summarise(
                           dplyr::group_by(
                                      dplyr::filter(dataMOD_SAFRAN,
                                                    EXP == "SAFRAN"),
                                      date,
                                      climateChain),
                           !!paste0(variable,
                                    "_SAFRAN"):=median(get(variable_SAFRAN),
                                                       na.rm=TRUE),
                           .groups="drop")

            
            dataMOD = dplyr::left_join(dataMOD,
                                       dplyr::select(dataMOD_SAFRAN_med,
                                                     -climateChain))

            dataMOD = dplyr::select(dataMOD,
                                    date,
                                    HM=Chain,
                                    Q_sim=variable,
                                    Q_obs=paste0(variable, "_SAFRAN"))
            if (j == 1) {
                margin_l = 0
                margin_r = 1
            } else if (j == 2) {
                margin_l = 4
                margin_r = 0
            } else {
                margin_l = 5
                margin_r = 0
            }
            
            Colors_tmp = Colors 
            names(Colors_tmp) = paste0(names(Colors_tmp), "|median")

            if (alt_config) {
                ratio_title = 1/6.6
                margin_title =
                    margin(t=2, r=margin_r,
                           b=1, l=margin_l, "mm")
            } else {
                ratio_title = 1/6.6
                margin_title =
                    margin(t=2, r=margin_r,
                           b=0, l=margin_l, "mm")
            }
            
            medQJ_H = panel_spaghetti(dataMOD,
                                    Colors_tmp,
                                    title=paste0("(", letters[id_letter+j],
                                                 ") Régime hydrologique"),
                                    unit="m^{3}$/$s",
                                    subtitle=Horizons_medQJ[j],
                                    alpha=0.85,
                                    alpha_non_color=0.1,
                                    isSqrt=TRUE,
                                    missRect=FALSE,
                                    isBack=FALSE,
                                    isTitle=TRUE,
                                    date_labels="%d %b",
                                    breaks="3 months",
                                    minor_breaks="1 months",
                                    # add_x_breaks=as.Date("1970-12-31"),
                                    Xlabel="",
                                    limits_ymin=0,
                                    limits_ymax=limits_ymax,
                                    isBackObsAbove=TRUE,
                                    lwObs=0.6,
                                    lwObs_back=1,
                                    lwSim=0.4,
                                    lwSim_back=0.7,
                                    lwSim_non_color=0.2,
                                    grid=TRUE,
                                    convertTeX=FALSE,
                                    dx0_title=0.03,
                                    dx0_subtitle=0.095,
                                    ratio_title=ratio_title,
                                    margin_title=margin_title,
                                    margin_spag=
                                        margin(t=0, r=margin_r,
                                               b=0, l=margin_l, "mm"),
                                    first=FALSE,
                                    last=TRUE,
                                    hide_y_axis=hide_y_axis,
                                    verbose=verbose)
            # medQJ = contour()
            medQJ = add_sheep(medQJ,
                              sheep=medQJ_H,
                              id=variable,
                              height=1,
                              width=1/3,
                              verbose=verbose)
            hide_y_axis = TRUE
        }

        herd = add_sheep(herd,
                         sheep=medQJ,
                         id="medQJ",
                         height=medQJ_height,
                         width=width,
                         verbose=verbose)
        
        id_letter = id_letter + nVariables_medQJ


### 1.3. Legend ______________________________________________________
        dx0 = 0.015

        x_palette = 0
        dx_palette = 0.07
        dx_palette_shape = 0.05
        dx_palette_text = 0.18
        y_palette = 9.3
        dy_palette = 0.2
        dy_palette_title = 0.2
        
        x_spread = 0
        dx_spread = 0.04
        dx_spread_line = 0.18
        dx_spread_rect = dx_spread_line
        dx_spread_text = 0.28
        y_spread = 7.94
        dy_spread = 0.32
        dy_spread_text_line = 0.12
        dy_spread_rect = 0.16
        dy_spread_title = 0.24
        
        x_signe = 0
        dx_signe = 0.07
        dx_signe_text = 0.14
        y_signe = 6.55
        dy_signe = 0.16
        dy_signe_title = 0.2
        dy_signe_title_line = 0.16

        x_stripe = 0
        dx_stripe = 0.04
        y_stripe = 4.6
        dx_stripe_palette = 0.06
        dx_stripe_palette_line = 0.03
        dx_stripe_palette_text = 0.03
        dy_stripe_palette = 0.12
        dy_stripe_text = 0.2
        dy_stripe_line = 0.12
        dy_stripe_title = 0.17
            
        dy_back_bottom = 0.2
        dy_back_top = 0.7

        legend = ggplot() + theme_void_Lato() +
            theme(plot.margin=margin(t=0, r=0,
                                     b=0, l=0, "mm")) +
            # annotate("rect",
            #          xmin=0, xmax=0.95,
            #          ymin=y_signe - dy_back_bottom,
            #          ymax=y_palette + dy_back_top,
            #          fill=NA, color=IPCCgrey85,
            #          linewidth=0.6, linejoin="round")

        # legend = legend +
            annotate("text",
                     x=dx0 + x_palette,
                     y=y_palette + dy_palette*1 + dy_palette_title,
                     label=TeX("\\textbf{Changements}"),
                     hjust=0, vjust=0.5, size=2.4,
                     family="Lato",
                     family="Lato",
                     color=IPCCgrey35)
        nColor = 6
        dColor = 1
        Palette = metaEX_serie$palette[metaEX_serie$variable_en ==
                                       Variables_serie[1]]
        Palette = unlist(strsplit(Palette, " "))
        PaletteLeg = colorRampPalette(Palette)(nColor)
        PaletteEX = Palette
        PaletteEX = c(PaletteEX[1+dColor], PaletteEX[length(PaletteEX)-dColor])
        PaletteEX_info = c("Moins d'eau",
                           "Plus d'eau")
        for (k in 1:2) {
            legend = legend +
                annotate("point",
                         x=dx0 + x_palette +
                             dx_palette + dx_palette_shape,
                         y=y_palette + dy_palette*(k-1),
                         color=PaletteEX[k], alpha=0.6,
                         size=2, shape=15) +
                # annotate("point",
                #          x=dx0 + x_palette +
                #              dx_palette,
                #          y=y_palette + dy_palette*(k-1),
                #          color="white", alpha=1,
                #          size=2, shape=15) +
                annotate("point",
                         x=dx0 + x_palette +
                             dx_palette,
                         y=y_palette + dy_palette*(k-1),
                         color=PaletteEX[k], alpha=0.3,
                         size=2, shape=15) +
                annotate("text",
                         x=dx0 + x_palette +
                             dx_palette + dx_palette_text,
                         y=y_palette + dy_palette*(k-1),
                         label=PaletteEX_info[k],
                         hjust=0, vjust=0.6, size=2.2,
                         family="Lato",
                         color=IPCCgrey35) 
        }
        
        legend = legend +
            annotate("text",
                     x=dx0 + x_spread,
                     y=y_spread + dy_spread*2 + dy_spread_title,
                     label=TeX("\\textbf{Évolution}"),
                     hjust=0, vjust=0.5, size=2.4,
                     family="Lato",
                     color=IPCCgrey35) + 
            
            annotate("line",
                     x=dx0 + x_spread + dx_spread + c(0, dx_spread_line),
                     y=y_spread + dy_spread*2,
                     color=IPCCgrey50, lineend="round",
                     linewidth=0.7) +
            annotate("text",
                     x=dx0 + x_spread + dx_spread + dx_spread_text,
                     y=y_spread + dy_spread*2 + dy_spread_text_line/2,
                     label="Moyenne",
                     hjust=0, vjust=0.6, size=2.2,
                     family="Lato",
                     color=IPCCgrey35) +
            annotate("text",
                     x=dx0 + x_spread + dx_spread + dx_spread_text,
                     y=y_spread + dy_spread*2 - dy_spread_text_line/2,
                     label="d'ensemble",
                     hjust=0, vjust=0.6, size=2.2,
                     family="Lato",
                     color=IPCCgrey35) +
            
            annotate("rect",
                     xmin=dx0 + x_spread + dx_spread,
                     xmax=dx0 + x_spread + dx_spread + dx_spread_rect,
                     ymin=y_spread + dy_spread - dy_spread_rect/2,
                     ymax=y_spread + dy_spread + dy_spread_rect/2,
                     fill=IPCCgrey50, alpha=0.5,
                     color=NA, linetype="solid",
                     linejoin="round",
                     linewidth=0.4) +
            annotate("text",
                     x=dx0 + x_spread + dx_spread + dx_spread_text,
                     y=y_spread + dy_spread + dy_spread_text_line/2,
                     label="Incertitude de",
                     hjust=0, vjust=0.6, size=2.2,
                     family="Lato",
                     color=IPCCgrey35) +
            annotate("text",
                     x=dx0 + x_spread + dx_spread + dx_spread_text,
                     y=y_spread + dy_spread - dy_spread_text_line/2,
                     label="modélisation",
                     hjust=0, vjust=0.6, size=2.2,
                     family="Lato",
                     color=IPCCgrey35) +

            annotate("rect",
                     xmin=dx0 + x_spread + dx_spread,
                     xmax=dx0 + x_spread + dx_spread + dx_spread_rect,
                     ymin=y_spread - dy_spread_rect/2,
                     ymax=y_spread + dy_spread_rect/2,
                     fill=IPCCgrey50, alpha=0.2,
                     color=IPCCgrey60, linetype="11",
                     linejoin="bevel",
                     linewidth=0.4) +
            annotate("text",
                     x=dx0 + x_spread + dx_spread + dx_spread_text,
                     y=y_spread + dy_spread_text_line/2,
                     label="Variabilité",
                     hjust=0, vjust=0.6, size=2.2,
                     family="Lato",
                     color=IPCCgrey35) +
            annotate("text",
                     x=dx0 + x_spread + dx_spread + dx_spread_text,
                     y=y_spread - dy_spread_text_line/2,
                     label="naturelle",
                     hjust=0, vjust=0.6, size=2.2,
                     family="Lato",
                     color=IPCCgrey35)

        
        legend = legend +
            annotate("text",
                     x=dx0 + x_signe,
                     y=y_signe + dy_signe*2 +
                         dy_signe_title + dy_signe_title_line*2,
                     label=TeX("\\textbf{Accord}"),
                     hjust=0, vjust=0.5, size=2.4,
                     family="Lato",
                     color=IPCCgrey35) +
            annotate("text",
                     x=dx0 + x_signe,
                     y=y_signe + dy_signe*2 +
                         dy_signe_title + dy_signe_title_line,
                     label=TeX("\\textbf{sur le signe}"),
                     hjust=0, vjust=0.5, size=2.4,
                     family="Lato",
                     color=IPCCgrey35) +
            annotate("text",
                     x=dx0 + x_signe,
                     y=y_signe + dy_signe*2 +
                         dy_signe_title,
                     label=TeX("\\textbf{de l'évolution}"),
                     hjust=0, vjust=0.5, size=2.4,
                     family="Lato",
                     color=IPCCgrey35)

        Signe_info = c("Diminution",
                       "Pas d'accord",
                       "Augmentation")
        Signe_shape = c(25, 21, 24)
        Signe_fill = c(IPCCgrey60, NA, IPCCgrey60)
        
        for (k in 1:3) {
            legend = legend +
                annotate("point",
                         x=dx0 + x_signe + dx_signe,
                         y=y_signe + dy_signe*(k-1),
                         color=IPCCgrey60,
                         fill=Signe_fill[k], size=1.5,
                         shape=Signe_shape[k]) +
                annotate("text",
                         x=dx0 + x_signe + dx_signe + dx_signe_text,
                         y=y_signe + dy_signe*(k-1),
                         label=Signe_info[k],
                         hjust=0, vjust=0.6, size=2.2,
                         family="Lato",
                         color=IPCCgrey35) 
        }

        Lines = c("Les palettes de couleur",
                  "sont communes aux",
                  "quatre narratifs pour", 
                  "chaque variable.")
        nLines = length(Lines)
        
        legend = legend +
            annotate("text",
                     x=dx0 + x_stripe,
                     y=y_stripe +
                         dy_stripe_palette*nColor +
                         dy_stripe_text + 
                         dy_stripe_line*(nLines-1) +
                         dy_stripe_title,
                     label=TeX("\\textbf{Stripes}"),
                     hjust=0, vjust=0.5, size=2.4,
                     family="Lato",
                     color=IPCCgrey35)

        for (k in 1:nLines) {
            legend = legend +
                annotate("text",
                         x=dx0 + x_stripe,
                         y=y_stripe +
                             dy_stripe_palette*nColor +
                             dy_stripe_text +
                             dy_stripe_line*(k-1),
                         label=rev(Lines)[k],
                         hjust=0, vjust=0.6, size=2.2,
                         family="Lato",
                         color=IPCCgrey35)
        }

        for (k in 1:nColor) {
            legend = legend +
                annotate("rect",
                         xmin=dx0 + x_stripe + dx_stripe,
                         xmax=dx0 + x_stripe + dx_stripe + dx_stripe_palette,
                         ymin=y_stripe + dy_stripe_palette*(k-1),
                         ymax=y_stripe + dy_stripe_palette*k,
                         linewidth=0,
                         fill=PaletteLeg[k], color=NA)
        }
        kLabels = c(0, round(nColor/2), nColor)
        Labels = c("min", "0", "max")
        nLabels = length(Labels)

        for (k in 1:nLabels) {
            legend = legend +
                annotate("line",
                         x=dx0 + x_stripe + dx_stripe + dx_stripe_palette +
                             c(0, dx_stripe_palette_line),
                         y=y_stripe + dy_stripe_palette*kLabels[k],
                         linewidth=0.25, lineend="square",
                         color=IPCCgrey35) +
                annotate("text",
                         x=dx0 + x_stripe + dx_stripe + dx_stripe_palette +
                             dx_stripe_palette_line +
                             dx_stripe_palette_text,
                         y=y_stripe + dy_stripe_palette*kLabels[k],
                         label=Labels[k],
                         hjust=0, vjust=0.5, size=1.9,
                         family="Lato",
                         color=IPCCgrey35)
        }
        
        dy_plus = 3/4 * dy_stripe_palette*nColor
        dy_moins = 1/4 * dy_stripe_palette*nColor
        legend = legend +
            annotate("text",
                     x=dx0 + x_stripe + dx_stripe + dx_stripe_palette +
                         dx_stripe_palette_line +
                         dx_stripe_palette_text*2,
                     y=y_stripe + dy_plus,
                     label="Plus d'eau",
                     hjust=0, vjust=0.5, size=2.1,
                     family="Lato",
                     color=IPCCgrey35) +
            annotate("text",
                     x=dx0 + x_stripe + dx_stripe + dx_stripe_palette +
                         dx_stripe_palette_line +
                         dx_stripe_palette_text*2,
                     y=y_stripe + dy_moins,
                     label="Moins d'eau",
                     hjust=0, vjust=0.5, size=2.1,
                     family="Lato",
                     color=IPCCgrey35)


        

        legend = legend + 
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            scale_y_continuous(limits=c(0, 10),
                               expand=c(0, 0))

        herd = add_sheep(herd,
                         sheep=legend,
                         id="legend",
                         height=block_height,
                         width=legend_width,
                         verbose=verbose)
        

### 1.4. Variable ____________________________________________________
#### 1.4.0. Initialisation ___________________________________________
        lim_date = as.Date(c('1975-01-01', '2100-12-31'))
        historical_mod = c(lim_date[1], historical[2])
        axis = panel_axis(lim_date,
                          subX=historical_mod,

                          X_color=IPCCgrey40,
                          X_tick_color=IPCCgrey48,
                          X_label_color=IPCCgrey35,
                          subX_color=IPCCgrey60,
                          
                          axis.text.x_size=3,
                          date_labels="%Y",
                          breaks="10 years",
                          minor_breaks="5 years")
        axis = axis +
            theme(axis.ticks.length.y=unit(0, 'mm'))
        
        for (j in 1:nVariables_serie) {
            variable = Variables_serie[j]
            # print(variable)
            
            variable_to_display =
                metaEX_serie$variable_fr[metaEX_serie$variable_en == variable]
            
            name_to_display =
                metaEX_serie$name_fr[metaEX_serie$variable_en == variable]
            
            block_plan = matrix(c("void", "title", 
                                  "void", "spread",
                                  "void", "signe",
                                  "info_stripes", "stripes",
                                  "void", "axis",
                                  "axis_void", "axis_void"),
                                ncol=2, byrow=TRUE)
            block = bring_grass(verbose=verbose)
            block = plan_of_herd(block, block_plan,
                                 verbose=verbose)

            titleTeX = TeX(paste0("(", letters[id_letter+j], ") ",
                              convert2TeX(variable_to_display, bold=TRUE),
                              " $-$ ", name_to_display))
            
            title = ggplot() + theme_void_Lato() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) + 
                annotate("text",
                         x=0,
                         y=1,
                         label=titleTeX,
                         size=3, hjust=0, vjust=1,
                         family="Lato",
                         color=IPCCgrey23) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            block = add_sheep(block,
                              sheep=title,
                              id="title",
                              height=variable_title_height,
                              width=variable_graph_width,
                              verbose=verbose)


#### 1.4.1. Spread __________________________________________________
            dataMOD = dataEX_serie_code[[variable]]
            dataMOD = dplyr::filter(dataMOD, EXP != "SAFRAN")
            dataMOD_historical =
                dplyr::summarise(
                           dplyr::group_by(
                                      dplyr::filter(dataMOD,
                                                    historical[1] <= date &
                                                    date <= historical[2]),
                                      Chain),
                           !!paste0("mean", variable):=
                               mean(get(variable), na.rm=TRUE))
            dataMOD = dplyr::left_join(dataMOD, dataMOD_historical,
                                       by="Chain")
            dataMOD$delta =
                (dataMOD[[variable]] - dataMOD[[paste0("mean", variable)]]) /
                dataMOD[[paste0("mean", variable)]] * 100
            dataMOD$date =
                as.Date(paste0(lubridate::year(dataMOD$date), "-01-01"))
            date_stat =
                dplyr::summarise(
                           dplyr::group_by(
                                      dplyr::filter(
                                                 dataMOD,
                                                 # climateChain %in% Storylines &
                                                 !is.na(get(variable))),
                                      Chain),
                           min_date=min(date, na.rm=TRUE),
                           max_date=max(date, na.rm=TRUE))
            min_date = max(date_stat$min_date, na.rm=TRUE)
            max_date = min(date_stat$max_date, na.rm=TRUE)
            
            dataMOD = dplyr::filter(dataMOD,
                                    min_date <= date &
                                    date <= max_date)
            dataMOD_delta = dataMOD
            
            dataMOD_QUALYPSO = data_QUALYPSO[[variable]]$spread
            for (k in 1:length(dataMOD_QUALYPSO)) {
                dataMOD_QUALYPSO[[k]] = dataMOD_QUALYPSO[[k]][dataMOD_QUALYPSO[[k]]$code == code,]
            }
            
            unitHTML = paste0("<span style='font-size:6pt'> %</span>")
            get_labels = function (X) {
                idNA = which(is.na(X))
                OK = X < 0
                OK[is.na(OK)] = FALSE
                X[OK] = paste0(paste0("<span style='color:", PaletteEX[1], "'><b>"),
                               X[OK],
                               paste0("</b>", unitHTML, "</span>"))
                OK = 0 < X
                OK[is.na(OK)] = FALSE
                X[OK] = paste0(paste0("<span style='color:", PaletteEX[2], "'><b>+"),
                               X[OK],
                               paste0("</b>", unitHTML, "</span>"))
                OK = X == 0
                OK[is.na(OK)] = FALSE
                X[OK] = paste0(paste0("<span style=''><b>"),
                               X[OK],
                               "</b></span>")
                X[idNA] = NA
                return (X)
            }


            dataMOD_delta_stat =
                dplyr::summarise(dplyr::group_by(dataMOD_delta, date),
                                 min=min(delta, na.rm=TRUE),
                                 max=max(delta, na.rm=TRUE))
            dataMOD_delta_stat$min[!is.finite(dataMOD_delta_stat$min)] = NA
            dataMOD_delta_stat$max[!is.finite(dataMOD_delta_stat$max)] = NA

            # dataMOD_delta_stat$zero = 0
            # dataMOD_delta_stat = dplyr::mutate(dataMOD_delta_stat,
            #                                    switch_max=dplyr::if_else(0 < max,
            #                                                            "+max", "-max"),
            #                                    switch_min=dplyr::if_else(0 < min,
            #                                                            "+min", "-min"),
            #                                    )

            min_all = min(dataMOD_delta_stat$min, na.rm=TRUE)
            max_all = max(dataMOD_delta_stat$max, na.rm=TRUE)
            
            # dataMOD_delta_stat =
            #     dplyr::mutate(dataMOD_delta_stat,
            #                   down_zero=
            #                       dplyr::if_else(max < 0,
            #                                      max, 0),
            #                   down_min=
            #                       dplyr::if_else(min > 0,
            #                                      0, min),
            
            #                   up_zero=
            #                       dplyr::if_else(min > 0,
            #                                      min, 0),
            #                   up_max=
            #                       dplyr::if_else(max < 0,
            #                                      0, max))

            
            # dataMOD_delta_stat$up_max = dataMOD_delta_stat$max
            
            # OK = dataMOD_delta_stat$max_up < 0
            # if (any(OK)) {
            #     dataMOD_delta_stat$max_up[OK] = 0 
            # }
            
            # dataMOD_delta_stat$min_down = dataMOD_delta_stat$min
            # OK = 0 < dataMOD_delta_stat$min_down
            # if (any(OK)) {
            #     dataMOD_delta_stat$min_down[OK] = 0
            # }

            crosses = function(x, y) {
                outx = x[1]
                outy = y[1]
                id = 1
                outid = id
                sn = sign(y[1])
                outsn = sn
                
                for (i in 2:length(x)) {
                    if (sign(y[i-1]) != sign(y[i])) {
                        x_sol = -y[i-1]*(x[i]-x[i-1])/(y[i]-y[i-1])+x[i-1]
                        outx = c(outx, rep(x_sol, 2))
                        outy = c(outy, rep(0, 2))
                        outid = c(outid, id, id+1)
                        id = id + 1
                        outsn = c(outsn,
                                  sign(y[i-1]),
                                  sign(y[i]))
                    }
                    outx <- c(outx, x[i])
                    outy <- c(outy, y[i])
                    outid = c(outid, id)
                    outsn = c(outsn, sign(y[i]))
                }
                tibble(x=outx, y=outy, id=outid, sign=outsn)
            }
            
            min_ribbon = with(dataMOD_delta_stat, crosses(date, min))
            max_ribbon = with(dataMOD_delta_stat, crosses(date, max))

            spread = ggplot() +
                theme_IPCC(is_axis.line.x=FALSE,
                           is_axis.ticks.x=FALSE,
                           is_axis.text.x=FALSE,
                           is_axis.ticks.y=FALSE,
                           isGridY=FALSE) + 
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm"),
                      legend.position="none",
                      axis.ticks.length.x=unit(1, 'mm'),
                      axis.ticks.length.y=unit(0, 'mm'))
            
            spread = spread +
                geom_ribbon(data=dataMOD_delta_stat,
                            aes(x=date,
                                ymax=max),
                            ymin=0,
                            fill=PaletteEX[2],
                            alpha=0.2, color=NA) +
                geom_ribbon(data=dataMOD_delta_stat,
                            aes(x=date,
                                ymin=min),
                            ymax=0,
                            fill=PaletteEX[1],
                            alpha=0.2, color=NA) +
                
                geom_ribbon(data=max_ribbon,
                            aes(x=x,
                                ymin=y),
                            ymax=max_all,
                            fill="white", color=NA) +
                geom_ribbon(data=min_ribbon,
                            aes(x=x,
                                ymax=y),
                            ymin=min_all,
                            fill="white", color=NA) +
                
                geom_line(data=dplyr::filter(max_ribbon, sign==1),
                          aes(x=x, y=y,
                              group=id),
                          color=PaletteEX[2],
                          linewidth=0.25,
                          linetype="11") +
                geom_line(data=dplyr::filter(max_ribbon, sign==-1),
                          aes(x=x, y=y,
                              group=id),
                          color=PaletteEX[1],
                          linewidth=0.25,
                          linetype="11") +
                
                geom_line(data=dplyr::filter(min_ribbon, sign==-1),
                          aes(x=x, y=y,
                              group=id),
                          color=PaletteEX[1],
                          linewidth=0.25,
                          linetype="11") +
                geom_line(data=dplyr::filter(min_ribbon, sign==1),
                          aes(x=x,y=y,
                              group=id),
                          color=PaletteEX[2],
                          linewidth=0.25,
                          linetype="11") 
            
            # tmp = dplyr::filter(dataMOD_delta_stat,
            #                     !is.na(min) & !is.na(max))
            # min_end = tmp$min[nrow(tmp)]
            # max_end = tmp$max[nrow(tmp)]

            # if (min_end <= 0 & max_end <= 0) {
            #     spread = spread +
            #         geom_ribbon(data=dataMOD_delta_stat,
            #                     aes(x=date,
            #                         ymin=min,
            #                         ymax=max),
            #                     fill=PaletteEX[1],
            #                     color=PaletteEX[1], 
            #                     alpha=0.2, linewidth=0.25,
            #                     linetype="11")
            # } else if (0 <= min_end & 0 <= max_end) {
            #     spread = spread +
            #         geom_ribbon(data=dataMOD_delta_stat,
            #                     aes(x=date,
            #                         ymin=min,
            #                         ymax=max),
            #                     fill=PaletteEX[2],
            #                     color=PaletteEX[2], 
            #                     alpha=0.2, linewidth=0.25,
            #                     linetype="11")
            # } else {
            #     spread = spread +
            #         geom_area(data=dataMOD_delta_stat,
            #                   aes(x=date,
            #                       y=min),
            #                   fill=PaletteEX[1],
            #                   color=PaletteEX[1], 
            #                   alpha=0.2, linewidth=0.25,
            #                   linetype="11") +
            #         geom_area(data=dataMOD_delta_stat,
            #                   aes(x=date,
            #                       y=max),
            #                   fill=PaletteEX[2],
            #                   color=PaletteEX[2], 
            #                   alpha=0.2, linewidth=0.25,
            #                   linetype="11")
            # }


            tmp = dplyr::filter(dataMOD_QUALYPSO$inside,
                                !is.na(q5) & !is.na(q95) &
                                !is.na(mean))
            q5_end = tmp$q5[nrow(tmp)]
            q95_end = tmp$q95[nrow(tmp)]
            mean_end = tmp$mean[nrow(tmp)]

            if (q5_end <= 0 & q95_end <= 0) {
                spread = spread +
                    geom_ribbon(data=dataMOD_QUALYPSO$inside,
                                aes(x=date,
                                    ymin=q5*100,
                                    ymax=q95*100),
                                fill=PaletteEX[1],
                                color=NA, 
                                alpha=0.4, linewidth=0.25,
                                linetype="solid")
            } else if (0 <= q5_end & 0 <= q95_end) {
                spread = spread +
                    geom_ribbon(data=dataMOD_QUALYPSO$inside,
                                aes(x=date,
                                    ymin=q5*100,
                                    ymax=q95*100),
                                fill=PaletteEX[2],
                                color=NA, 
                                alpha=0.4, linewidth=0.25,
                                linetype="solid")
            } else {
                spread = spread +
                    geom_ribbon(data=dataMOD_QUALYPSO$inside,
                                aes(x=date,
                                    ymin=q5*100),
                                ymax=0,
                                fill=PaletteEX[1],
                                color=NA, 
                                alpha=0.4, linewidth=0.25,
                                linetype="solid") +
                    geom_ribbon(data=dataMOD_QUALYPSO$inside,
                                aes(x=date,
                                    ymax=q95*100),
                                ymin=0,
                                fill=PaletteEX[2],
                                color=NA, 
                                alpha=0.4, linewidth=0.25,
                                linetype="solid")
            }

            spread = spread +
                annotate("line",
                         x=lim_date, y=0,
                         color=IPCCgrey48,
                         linewidth=0.3,
                         linetype="solid",
                         lineend="square")

            if (mean_end <= 0) {
                spread = spread +
                    geom_line(data=dataMOD_QUALYPSO$inside,
                              aes(x=date,
                                  y=mean*100),
                              color=PaletteEX[1], 
                              linewidth=0.5, lineend="round")
            } else {
                spread = spread +
                    geom_line(data=dataMOD_QUALYPSO$inside,
                              aes(x=date,
                                  y=mean*100),
                              color=PaletteEX[2], 
                              linewidth=0.5, lineend="round")
            }

            y_grid = pretty(c(min_all, max_all), n=4)
            n_y_grid = length(y_grid)
            tmp = dplyr::tibble(x=rep(lim_date, n_y_grid),
                                y=rep(y_grid, each=2),
                                id=rep(1:n_y_grid, each=2))

            spread = spread +
                geom_line(data=tmp,
                          aes(x=x, y=y, group=id),
                          color=IPCCgrey60,
                          alpha=0.4, linewidth=0.25)
            
            spread = spread +
                scale_x_date(limits=lim_date,
                             # breaks=get_breaks,
                             expand=c(0, 0)) +
                scale_y_continuous(limits=c(-100, max(y_grid)),
                                   breaks=y_grid,
                                   labels=get_labels(y_grid),
                                   position="right",
                                   expand=c(0, 0))
    
            block = add_sheep(block,
                              sheep=spread,
                              id="spread",
                              label="align",
                              height=variable_spread_height,
                              width=variable_graph_width,
                              verbose=verbose)

#### 1.4.2. Signe __________________________________________________
            dataMOD_QUALYPSO = data_QUALYPSO[[variable]]$signe
            dataMOD_QUALYPSO = dataMOD_QUALYPSO[dataMOD_QUALYPSO$code == code,]
            convert_shape = c("24"=1, "21"=0, "25"=-1)
            dataMOD_QUALYPSO$shape =
                names(convert_shape)[match(dataMOD_QUALYPSO$signe,
                                           convert_shape)]
            dataMOD_QUALYPSO$shape = as.numeric(dataMOD_QUALYPSO$shape)
            dataMOD_QUALYPSO$dy = -0.06 * dataMOD_QUALYPSO$signe
            dataMOD_QUALYPSO$color = IPCCgrey60
            dataMOD_QUALYPSO$fill = "white"
            dataMOD_QUALYPSO$color[dataMOD_QUALYPSO$signe == 1] = PaletteEX[2]
            dataMOD_QUALYPSO$fill[dataMOD_QUALYPSO$signe == 1] = PaletteEX[2]
            dataMOD_QUALYPSO$color[dataMOD_QUALYPSO$signe == -1] = PaletteEX[1]
            dataMOD_QUALYPSO$fill[dataMOD_QUALYPSO$signe == -1] = PaletteEX[1]
            
            get_breaks = function(X, breaks="10 years", break_round=-1) {
                Xmin = round(lubridate::year(min(X)), break_round)
                Xmax = round(lubridate::year(max(X)), break_round)
                if (Xmax-Xmin <= 1) {
                    Xmin = lubridate::year(X)[1]
                    Xmax = lubridate::year(X)[1] + 1
                }
                res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")),
                               to=as.Date(paste0(Xmax, "-01-01")),
                               by=breaks)
                return (res)
            }
            
            signe =  ggplot() + theme_void_Lato() + 
                theme(panel.grid.major.x=element_line(color=IPCCgrey85,
                                                      size=0.3),
                      plot.margin=margin(t=0, r=0,
                                         b=1.5, l=0, "mm"),
                      axis.ticks.length.y=unit(0, 'mm')) +
                annotate("point",
                         x=dataMOD_QUALYPSO$date,
                         y=0.5 + dataMOD_QUALYPSO$dy,
                         shape=dataMOD_QUALYPSO$shape,
                         color="white",
                         fill="white", size=2.4) +
                annotate("point",
                         x=dataMOD_QUALYPSO$date,
                         y=0.5 + dataMOD_QUALYPSO$dy,
                         shape=dataMOD_QUALYPSO$shape,
                         color=dataMOD_QUALYPSO$color,
                         fill=dataMOD_QUALYPSO$fill, size=1.5) +
                scale_x_date(limits=lim_date,
                             breaks=get_breaks,
                             expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            block = add_sheep(block,
                              sheep=signe,
                              id="signe",
                              label="align",
                              height=variable_signe_height,
                              width=variable_graph_width,
                              verbose=verbose)

            
#### 1.4.3. Stripes __________________________________________________
            info_stripes =  ggplot() + theme_void_Lato() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))

            dy0 = 0.02
            dy_space = 0.04
            dy = (1 - dy_space*(nStorylines - 1) - dy0*2) / nStorylines

            x_back = 0.5
            x_front = 0.8
            y_rel_arrow = 0.25
            lw_arrow = 1.1
                
            for (k in 1:nStorylines) {
                y = dy0 + dy_space*(k-1) + dy*(k-1)
                y_top = y + dy*y_rel_arrow
                y_mid = y + dy/2
                y_bottom = y + dy*(1-y_rel_arrow)
                
                info_stripes = info_stripes +
                    annotate("line",
                             x=c(x_back, x_front),
                             y=c(y_top, y_mid),
                             color=rev(Colors)[k],
                             linewidth=lw_arrow, lineend="round") +
                    annotate("line",
                             x=c(x_front, x_back),
                             y=c(y_mid, y_bottom),
                             color=rev(Colors)[k],
                             linewidth=lw_arrow, lineend="round")
            }
    
            block = add_sheep(block,
                              sheep=info_stripes,
                              id="info_stripes",
                              width=variable_info_width,
                              verbose=verbose)
            block = add_sheep(block,
                              sheep=void(),
                              id="void",
                              width=variable_info_width,
                              verbose=verbose)

            colorStep = 256
            Palette = metaEX_serie$palette[metaEX_serie$variable_en == variable]
            Palette = unlist(strsplit(Palette, " "))
            Palette = colorRampPalette(Palette)(colorStep)

            dataMOD_delta_storyline =
                dplyr::filter(dataMOD_delta,
                              climateChain %in% Storylines)

            ###
            dataMOD_delta_storyline =
                dplyr::summarise(dplyr::group_by(dataMOD_delta_storyline,
                                                 date, climateChain),
                                 delta=median(delta, na.rm=TRUE),
                                 .groups="drop")
            ###
            min_value = quantile(dataMOD_delta_storyline$delta,
                                 stripe_prob, na.rm=TRUE)
            max_value = quantile(dataMOD_delta_storyline$delta,
                                 1-stripe_prob, na.rm=TRUE)
            
            res = compute_colorBin(min_value, max_value,
                                   length(Palette),
                                   center=0,
                                   include=FALSE)
            dataMOD_delta_storyline$fill =
                get_colors(dataMOD_delta_storyline$delta,
                           res$upBin, res$lowBin, Palette)

            stripes_plan = matrix(1:nStorylines)
            stripes = bring_grass(verbose=verbose)
            stripes = plan_of_herd(stripes, stripes_plan,
                                   verbose=verbose)
            
            for (k in 1:nStorylines) {
                dataMOD_delta_sl = dplyr::filter(dataMOD_delta_storyline,
                                                 climateChain == Storylines[k])
                # dataMOD_delta_sl$y = factor(dataMOD_delta_sl$Chain)
                # dataMOD_delta_sl = dplyr::arrange(dataMOD_delta_sl, HM)
                
                stripes_k =
                    ggplot2::ggplot() + theme_void_Lato() + 
                    ggplot2::theme(plot.margin=margin(t=0.3, r=0,
                                                      b=0.3, l=0, "mm"),
                                   axis.ticks.length.y=unit(0, 'mm')) +
                    # ggplot2::annotate("tile",
                                      # x=dataMOD_delta_sl$date,
                                      # y=dataMOD_delta_sl$y,
                                      # fill=dataMOD_delta_sl$fill) +
                    ###
                    ggplot2::annotate("tile",
                                      x=dataMOD_delta_sl$date,
                                      y=1,
                                      fill=dataMOD_delta_sl$fill) +
                    ###
                    
                    ggplot2::scale_x_date(limits=lim_date,
                                          expand=c(0, 0)) +
                    ggplot2::scale_y_discrete(expand=c(0, 0))

                stripes = add_sheep(stripes,
                                    sheep=stripes_k,
                                    id=k,
                                    label="align",
                                    height=1,
                                    verbose=verbose)
            }

            block = add_sheep(block,
                              sheep=stripes,
                              id="stripes",
                              # label="align",
                              height=variable_stripes_height,
                              width=variable_graph_width,
                              verbose=verbose)

#### 1.4.4. Axis _____________________________________________________     
            block = add_sheep(block,
                              sheep=axis,
                              id="axis",
                              label="align",
                              height=variable_axis_height,
                              width=variable_graph_width,
                              verbose=verbose)

            block = add_sheep(block,
                              sheep=void(),
                              id="axis_void",
                              height=variable_axis_void_height,
                              width=1,
                              verbose=verbose)

            herd = add_sheep(herd,
                             sheep=block,
                             id=variable,
                             height=block_height,
                             width=block_width,
                             verbose=verbose)
        }
        id_letter = id_letter + nVariables_serie
        

### 1.5. Foot ________________________________________________________
        warning = ggplot() + theme_void_Lato() +
            theme(plot.margin=margin(t=0, r=0,
                                     b=0, l=0, "mm"))

        Lines = c("\\textbf{Avertissement} : Ces résultats comportent des incertitudes.",
                  "Ils sont donnés à titre indicatif. Il ne s’agit pas de prévisions mais d’indications",
                  "d’évolutions possibles. Ces fiches sont volontairement synthétiques et une notice",
                  "d’accompagnement fournit des informations pour la lecture et l’interprétation",
                  "des graphiques de cette fiche.")

        dy0 = 0.75
        dy = 0.15
        dx0 = 0
        for (k in 1:length(Lines)) {
            warning = warning +
                annotate("text",
                         x=dx0,
                         y=dy0 - dy*(k-1),
                         label=TeX(Lines[k]),
                         size=2, hjust=0, vjust=1,
                         family="Lato",
                         color=IPCCgrey50)
        }

        warning = warning +
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            scale_y_continuous(limits=c(0, 1),
                               expand=c(0, 0))


        footName = 'Synthèse des projections sous RCP 8.5'
        if (is.null(Pages)) {
            n_page = 1
        } else {
            if (nrow(Pages) == 0) {
                n_page = 1
            } else {
                n_page = Pages$n[nrow(Pages)] + 1
            }
            Pages = bind_rows(
                Pages,
                tibble(section=footName,
                       subsection=code,
                       n=n_page))
        }
        
        foot = panel_foot(footName, n_page,
                          foot_height, logo_info,
                          left_plot=warning,
                          verbose=verbose)
        herd = add_sheep(herd,
                         sheep=foot,
                         id="foot",
                         height=foot_height,
                         width=width,
                         verbose=verbose)

        
### 1.6. End _________________________________________________________
        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size="A4",
                                  hjust=0, vjust=1,
                                  verbose=verbose)
        plot = res$plot
        paper_size = res$paper_size
        # plot = void()
        # paper_size = c(21, 29.7)
        
        filename = paste0(code, "_projection_datasheet_1.pdf")

        if (!(file.exists(figdir))) {
            dir.create(figdir, recursive=TRUE)
        }
        
        ggplot2::ggsave(plot=plot,
                        path=figdir,
                        filename=filename,
                        width=paper_size[1],
                        height=paper_size[2], units='cm',
                        dpi=300,
                        device=cairo_pdf)

        
## 2. PAGE 2 _________________________________________________________
### 2.1. Initialisation ______________________________________________
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan_2,
                            verbose=verbose)

        
### 2.2. Delta ______________________________________________
        delta_plan = matrix(c("title", Types_short),
                            ncol=1, byrow=TRUE)
        delta = bring_grass(verbose=verbose)
        delta = plan_of_herd(delta, delta_plan,
                             verbose=verbose)

        text = paste0("(", letters[id_letter+1], ") ",
                      "Changements en ", Horizons_delta[2], " (H2) et ",
                      "en ", Horizons_delta[3], " (H3) par rapport à la ",
                      Horizons_delta[1])
        
        title = ggplot() + theme_void_Lato() +
            theme(plot.margin=margin(t=0, r=0,
                                     b=0, l=0, "mm")) + 
            annotate("text",
                     x=0,
                     y=1,
                     label=text,
                     size=3, hjust=0, vjust=1,
                     family="Lato",
                     color=IPCCgrey23) +
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            scale_y_continuous(limits=c(0, 1),
                               expand=c(0, 0))
        
        delta = add_sheep(delta,
                          sheep=title,
                          id="title",
                          height=delta_title_height,
                          verbose=verbose)

        dy_type = c(0.3, 0, 0.3)

        for (j in 1:nTypes) {
            type = Types[j]
            type_short = Types_short[j]

            # print(type)

            Variables_delta_type = Variables_delta[TypesALL == type]
            nVariables_delta_type = length(Variables_delta_type)

            grepl_first = function (pattern, table) {
                which(grepl(get_regexp(pattern), table))[1]
            }
            Ok = sapply(Variables_delta_type, grepl_first,
                        table=metaEX_criteria$variable_en)
            
            Units = metaEX_criteria$unit_fr[Ok]
            if (all(nchar(Units) == 1) | all(nchar(Units) != 1)) {
                delta_type_void_width = 0.09
            } else {
                delta_type_void_width = 0
            }
            Variables_delta_type =
                c(Variables_delta_type[nchar(Units) == 1],
                  Variables_delta_type[nchar(Units) > 1])

            grepl_first = function (pattern, table) {
                which(grepl(get_regexp(pattern), table))[1]
            }
            Ok = sapply(Variables_delta_type, grepl_first,
                        table=metaEX_criteria$variable_en)
            
            Variables_delta_type_display =
                gsub("(delta)|([{])|([}])|([_]H[[:digit:]])", "",
                     metaEX_criteria$variable_fr[Ok])
            
            delta_type_plan = matrix(c("title", Variables_delta_type_display, "void"),
                                     ncol=nVariables_delta_type+2, byrow=TRUE)
            delta_type = bring_grass(verbose=verbose)
            delta_type = plan_of_herd(delta_type, delta_type_plan,
                                      verbose=verbose)

            icon = svgparser::read_svg(file.path(icon_path,
                                                 paste0(gsub(" ", "_", type),
                                                        ".svg")))
            dx0 = 0.5
            y_title = 3.3
            y_icon = 2.4
            size_icon = 0.6
                
            titleTeX = TeX(paste0("\\textbf{", type, "}"))
            title = ggplot() + theme_void_Lato() + #coord_fixed(clip="off") +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) + 
                annotate("text",
                         x=dx0,
                         y=y_title + dy_type[j],
                         label=titleTeX,
                         size=3, hjust=0, vjust=0.35,
                         family="Lato",
                         color=IPCCgrey23, angle=90) +
                annotation_custom(icon,
                                  xmin=dx0 - size_icon,
                                  xmax=dx0 + size_icon,
                                  ymin=y_icon + dy_type[j] - size_icon,
                                  ymax=y_icon + dy_type[j] + size_icon) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 10),
                                   expand=c(0, 0))
            
            delta_type = add_sheep(delta_type,
                                   sheep=title,
                                   id="title",
                                   width=delta_type_title_width,
                                   verbose=verbose)

            delta_type = add_sheep(delta_type,
                                   sheep=void(),
                                   id="void",
                                   width=delta_type_void_width,
                                   verbose=verbose)

            unit_save = ""
            
            for (k in 1:nVariables_delta_type) {
                variable = Variables_delta_type[k]
                variable_display = Variables_delta_type_display[k]
                
                variable_4ElicuSenpai = variable_display
                variable_4ElicuSenpai = gsub("QSA", "QS", variable_4ElicuSenpai)
                variable_4ElicuSenpai = gsub("QMA", "QM", variable_4ElicuSenpai)
                variable_4ElicuSenpai = gsub("debut", "début", variable_4ElicuSenpai)
                if (grepl("Q[[:digit:]]+A", variable_4ElicuSenpai)) {
                    variable_4ElicuSenpai = gsub("A", "", variable_4ElicuSenpai)
                }
                
                dx_bar = 1.1
                dx_sL = 1.1
                dx_space = 1.4
                
                limits_x = c(-dx_bar*dx_space - dx_sL, (nDeltaHorizon-1)*dx_bar +
                                                       nDeltaHorizon*(1+dx_sL) +
                                                       dx_bar*dx_space)

                bar_plan = matrix(c("title",
                                    "plot"),
                                  ncol=1, byrow=TRUE)
                bar = bring_grass(verbose=verbose)
                bar = plan_of_herd(bar, bar_plan,
                                   verbose=verbose)

                titleTeX = TeX(convert2TeX(variable_4ElicuSenpai, bold=TRUE))
                
                title = ggplot() + theme_void_Lato() +
                    theme(plot.margin=margin(t=0, r=0,
                                             b=0, l=0, "mm")) + 
                    annotate("text",
                             x=0.5,
                             y=0.96,
                             label=titleTeX,
                             size=2.5, hjust=0.5, vjust=1,
                             family="Lato",
                             color=IPCCgrey40) +
                    annotate("line",
                             x=c(0.15, 0.85),
                             y=c(0.08, 0.08),
                             linewidth=0.4, color=IPCCgrey40,
                             lineend="round") +
                    scale_x_continuous(limits=c(0, 1),
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=c(0, 1),
                                       expand=c(0, 0))
                
                bar = add_sheep(bar,
                                sheep=title,
                                id="title",
                                label=paste0("align_", variable_display),
                                height=delta_type_bar_title_height, 
                                verbose=verbose)
                
                plot = ggplot()

                for (h in 1:nDeltaHorizon) {
                    H = deltaHorizon[h]
                    variable_H = paste0(variable, "_", H)
                    limits_bar = c((h-1)*(1+dx_bar+dx_sL),
                                   1+(h-1)*(1+dx_bar+dx_sL))

                    Ok = metaEX_criteria$variable_en == variable_H
                    Palette = metaEX_criteria$palette[Ok]
                    Palette = unlist(strsplit(Palette, " "))
                    nColor = length(Palette)

                    unit = metaEX_criteria$unit_fr[Ok]

                    if (nchar(unit) == 1) {
                        dColor = 1
                    } else {
                        dColor = 2
                    }
                    
                    Delta_variable_H = dataEX_criteria_code[[variable_H]]
                    Chain_variable_H = dataEX_criteria_code$Chain
                    
                    minDelta_variable_H =
                        dataEX_criteria_prob_code[[paste0("min_", variable_H)]]
                    maxDelta_variable_H =
                        dataEX_criteria_prob_code[[paste0("max_", variable_H)]]
                    nDelta_variable_H = length(Delta_variable_H)
                    
                    tmp = dplyr::tibble(Chain=rep(Chain_variable_H, each=2),
                                        x=rep(limits_bar, nDelta_variable_H),
                                        y=rep(Delta_variable_H, each=2))
                    
                    plot = plot +
                        annotate("rect",
                                 xmin=limits_bar[1], xmax=limits_bar[2],
                                 ymin=minDelta_variable_H,
                                 ymax=maxDelta_variable_H,
                                 fill="white",
                                 color=NA)

                    if (minDelta_variable_H <= 0) {
                        plot = plot +
                            annotate("rect",
                                     xmin=limits_bar[1], xmax=limits_bar[2],
                                     ymin=minDelta_variable_H,
                                     ymax=min(c(0, maxDelta_variable_H)),
                                     fill=Palette[1+dColor],
                                     alpha=0.2,
                                     color=NA) +
                            geom_line(data=dplyr::filter(tmp, y <= 0),
                                      aes(x=x, y=y, group=Chain),
                                      color=Palette[1+dColor],
                                      linewidth=0.3,
                                      alpha=0.07,
                                      lineend="round")
                    }
                    if (0 < maxDelta_variable_H) {
                        plot = plot +
                            annotate("rect",
                                     xmin=limits_bar[1], xmax=limits_bar[2],
                                     ymin=max(c(0, minDelta_variable_H)),
                                     ymax=maxDelta_variable_H,
                                     fill=Palette[nColor-dColor],
                                     alpha=0.2,
                                     color=NA) +
                            geom_line(data=dplyr::filter(tmp, 0 < y),
                                      aes(x=x, y=y, group=Chain),
                                      color=Palette[nColor-dColor],
                                      linewidth=0.3,
                                      alpha=0.07,
                                      lineend="round")
                    }
                }
                
                if (k == 1 | unit != unit_save) {
                    show_y_axis = TRUE
                    if (nchar(unit) == 1) {
                        times = 1.5
                    } else {
                        times = 1.65
                    }
                } else {
                    show_y_axis = FALSE
                    times = 1
                }
                unit_save = unit
                plot = plot +
                    theme_IPCC(is_panel.background=TRUE,
                               is_plot.title=TRUE,
                               plot.title_size=8,
                               is_axis.ticks.y=FALSE,
                               is_axis.text.y=show_y_axis,
                               axis.text.y_size=8,
                               axis.text.y_margin=
                                   margin(t=0, r=-0.6,
                                          b=0, l=0,
                                          unit="mm"),
                               axis.ticks.length.y=1.5*show_y_axis,
                               is_axis.line.x=FALSE,
                               is_axis.ticks.x=FALSE,
                               is_axis.text.x=FALSE) +
                    theme(plot.margin=margin(t=1, r=0,
                                             b=3, l=2*show_y_axis, "mm"))
                if (!show_y_axis) {
                    plot = plot +
                        theme(axis.line.y=element_line(color=IPCCgrey60,
                                                       size=0.3,
                                                       linetype="dotted"))
                }
    
                plot = plot +
                    annotate("line",
                             x=limits_x, y=c(0, 0),
                             color=IPCCgrey60, size=0.35,
                             lineend="square")

                for (h in 1:nDeltaHorizon) {
                    H = deltaHorizon[h]
                    variable_H = paste0(variable, "_", H)
                    limits_bar = c((h-1)*(1+dx_bar+dx_sL),
                                   1+(h-1)*(1+dx_bar+dx_sL))

                    plot_y = c()
                    plot_color = c()
                    
                    for (storyline in Storylines) {
                        color = Colors[names(Colors) == storyline]
                        Delta_variable_H =
                            dplyr::filter(dataEX_criteria_code,
                                          climateChain == storyline)[[variable_H]]
                        medDelta_variable_H = median(Delta_variable_H, na.rm=TRUE)
    
                        plot_y = c(plot_y, medDelta_variable_H)
                        plot_color = c(plot_color, color)
                    }
                    
                    n = length(plot_y)
                    # tmp = dplyr::tibble(id=rep(Storylines, each=2),
                    #                     x=rep(c(limits_bar[1],
                    #                             limits_bar[2]+dx_sL/2), n),
                    #                     y=rep(plot_y, each=2),
                    #                     color=rep(plot_color, each=2))
                    
                    # plot = plot +
                        # geom_line(data=dplyr::filter(tmp, y <= 0),
                        #           aes(x=x, y=y, group=id), 
                        #           color=Palette[1+dColor],
                        #           linewidth=0.4,
                        #           alpha=0.5,
                        #           lineend="round") +
                        # geom_line(data=dplyr::filter(tmp, 0 < y),
                        #           aes(x=x, y=y, group=id), 
                        #           color=Palette[nColor-dColor],
                        #           linewidth=0.4,
                        #           alpha=0.5,
                        #           lineend="round") +

                    plot = plot +
                        annotate("point",
                                 x=rep(limits_bar[2]+dx_sL/2, n),
                                 y=plot_y,
                                 color=IPCCgrey97,
                                 size=2.2,
                                 shape=20)
                    
                    for (s in 1:nStorylines) {
                        plot = plot +
                            annotate("line",
                                     x=c(limits_bar[1],
                                         limits_bar[2]+dx_sL/2),
                                     y=plot_y[s],
                                     color=plot_color[s],
                                     linewidth=0.4,
                                     alpha=0.5,
                                     lineend="round")
                    }
                    
                     plot = plot +
                        annotate("point",
                                 x=rep(limits_bar[2]+dx_sL/2, n),
                                 y=plot_y,
                                 color=plot_color,
                                 size=1.2,
                                 shape=20)
                }

                if (j == 1 & k == 1) {
                    for (h in 1:nDeltaHorizon) {
                        H = deltaHorizon[h]
                        limits_bar = c((h-1)*(1+dx_bar+dx_sL),
                                        1+(h-1)*(1+dx_bar+dx_sL))
                        limits_line = c(limits_bar[1]-0.4,
                                        limits_bar[2]+0.3)
                        plot = plot +
                            annotate("text",
                                     x=mean(limits_bar),
                                     y=84,
                                     label=H,
                                     fontface="bold",
                                     size=2.3, hjust=0.5, vjust=0,
                                     family="Lato",
                                     color=IPCCgrey60) +
                            annotate("line",
                                     x=limits_line,
                                     y=80,
                                     linewidth=0.45, color=IPCCgrey60,
                                     lineend="round")
                    }
                }

                limits_y = c(-100, 100) 
                if (nchar(unit) == 1) {
                    Labels = c(-75, -50, -25, 0, 25, 50, 75)
                } else {
                    Labels = c(-90, -60, -30, 0, 30, 60, 90)
                }
                Breaks = Labels
                                
                get_label = function (x) {
                    if (nchar(unit) == 1) {
                        unitHTML = paste0("<span style='font-size:6pt'> ",
                                          unit, "</span>")
                    } else {
                        if (x != 0) {
                            unit = paste0(unit, "s")
                        }
                        unitHTML = paste0("<span style='font-size:6pt'> ",
                                          unit, "</span>")
                    }
                    
                    if (x < 0) {
                        paste0("<span style='color:", Palette[1+dColor], "'>",
                               "<b>", x, "</b>",
                               unitHTML,
                               "</span>")
                    } else if (x > 0) {
                        paste0("<span style='color:", Palette[nColor-dColor], "'>",
                               "<b>+", x, "</b>",
                               unitHTML,
                               "</span>")
                    } else {
                        paste0("<span style=''>",
                               "<b>", x, "</b>",
                               "</span>")
                    }
                }
                Labels = sapply(Labels, get_label)
                
                plot = plot +
                    scale_x_continuous(limits=limits_x,
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=limits_y,
                                       labels=Labels,
                                       breaks=Breaks,
                                       expand=c(0, 0))

                bar = add_sheep(bar,
                                sheep=plot,
                                id="plot",
                                label=paste0("align_", variable_display),
                                height=delta_type_bar_plot_height, 
                                verbose=verbose)

                delta_type = add_sheep(delta_type,
                                  sheep=bar,
                                  id=variable_display,
                                  height=1,
                                  width=delta_type_bar_width*times,
                                  verbose=verbose)
            }
            delta = add_sheep(delta,
                              sheep=delta_type,
                              id=type_short,
                              height=delta_type_height,
                              width=width,
                              verbose=verbose)
        }
        id_letter = id_letter + 1
        
        ###
        herd = add_sheep(herd,
                         sheep=delta,
                         id="delta",
                         height=delta_height,
                         width=width,
                         verbose=verbose)
        # herd = add_sheep(herd,
        #                  sheep=contour(),
        #                  id="delta",
        #                  height=delta_height,
        #                  width=width,
        #                  verbose=verbose)
        ###

        
### 2.3. Extreme _____________________________________________________
#### 2.3.1. Initialisation ____________________________________________
        for (j in 1:nVariables_extreme) {
            variable = Variables_extreme[j]
            div = div_Variables_extreme[j]
            rp = rp_Variables_extreme[j]

            x_dot = get(paste0("x_", rp, "dot"))
            y_dot = get(paste0("y_", rp, "dot"))
            
            # print(variable)
            
            title_text = Titles_extreme[j]

            extreme_plan = matrix(c(
                "title", "title", "title", "title", "title", "title", "title",
                "Hleg", "H0", "void1", "H2", "void2", "H3", "void3"),
                ncol=7, byrow=TRUE)
            extreme = bring_grass(verbose=verbose)
            extreme = plan_of_herd(extreme, extreme_plan,
                                   verbose=verbose)
            
            titleTeX = TeX(paste0("(", letters[id_letter+j], ") ",
                                  convert2TeX(variable, bold=TRUE),
                                  " $-$ ", title_text))

            title = ggplot() + theme_void_Lato() +
                theme(plot.margin=margin(t=3, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0,
                         y=1,
                         label=titleTeX,
                         size=3, hjust=0, vjust=1,
                         family="Lato",
                         color=IPCCgrey23) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme = add_sheep(extreme,
                                sheep=title,
                                id="title",
                                height=extreme_title_height,
                                verbose=verbose)

            void_line = void(panel.background_fill=IPCCgrey97,
                             plot.margin=margin(t=0, r=0,
                                                b=0, l=0, "mm")) +
                annotate("line",
                         x=0.5,
                         y=c(0.1, 0.884),
                         linewidth=0.25, color=IPCCgrey85) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme = add_sheep(extreme,
                                sheep=void_line,
                                id="void1",
                                height=extreme_H_height,
                                width=extreme_void_width,
                                verbose=verbose)
            extreme = add_sheep(extreme,
                                sheep=void_line,
                                id="void2",
                                height=extreme_H_height,
                                width=extreme_void_width,
                                verbose=verbose)
            
            extreme = add_sheep(extreme,
                                sheep=
                                    void(panel.background_fill=IPCCgrey97,
                                         plot.margin=margin(t=0, r=0,
                                                            b=0, l=0, "mm")),
                                id="void3",
                                height=extreme_H_height,
                                width=extreme_voidex_width,
                                verbose=verbose)

            
#### 2.3.2. Légende __________________________________________________
            extreme_H_plan = matrix(c("title",
                                       "0"),
                                     ncol=1, byrow=TRUE)
            extreme_H = bring_grass(verbose=verbose)
            extreme_H = plan_of_herd(extreme_H, extreme_H_plan,
                                      verbose=verbose)
            
            extreme_H = add_sheep(extreme_H,
                                  sheep=void(panel.background_fill=IPCCgrey97,
                                             plot.margin=margin(t=0, r=0,
                                                                b=0, l=0, "mm")),
                                  id="title",
                                  height=extreme_H_title_height,
                                  verbose=verbose)

            extreme_sL_plan = matrix(c("n",
                                       "delta"),
                                     ncol=1, byrow=TRUE)
            extreme_sL = bring_grass(verbose=verbose)
            extreme_sL = plan_of_herd(extreme_sL, extreme_sL_plan,
                                      verbose=verbose)

            text = ggplot() + theme_void_Lato() +
                theme(panel.background=element_rect(fill=IPCCgrey97, color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.48,
                         y=0.52,
                         label=TeX(paste0("\\textbf{FRÉQUENCE}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.75,
                         y=0.52,
                         label=TeX(paste0("\\textbf{par ", rp, " ans}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("line",
                         x=0.98,
                         y=c(0.08, 0.95),
                         linewidth=0.3, color=IPCCgrey85) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=text,
                                   id="n",
                                   height=extreme_H_sL_n_height+
                                       extreme_H_sL_n_text_height,
                                   verbose=verbose)

            text = ggplot() + theme_void_Lato() +
                theme(panel.background=element_rect(fill=IPCCgrey97, color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.48,
                         y=0.57,
                         label=TeX(paste0("\\textbf{CHANGEMENT}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.75,
                         y=0.57,
                         label=TeX(paste0("\\textbf{de débit}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         family="Lato",
                         color=IPCCgrey35) +
                annotate("line",
                         x=0.98,
                         y=c(0.18, 0.95),
                         linewidth=0.3, color=IPCCgrey85) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=text,
                                   id="delta",
                                   height=extreme_H_sL_delta_height+
                                       extreme_H_sL_delta_text_height,
                                   verbose=verbose)
            
            extreme_H = add_sheep(extreme_H,
                                   sheep=extreme_sL,
                                   id=0,
                                   height=extreme_H_sL_height,
                                   verbose=verbose)
            extreme = add_sheep(extreme,
                                sheep=extreme_H,
                                id="Hleg",
                                height=extreme_H_height,
                                width=extreme_leg_width,
                                verbose=verbose)

            
#### 2.3.3. H0 _______________________________________________________
            extreme_H_plan = matrix(c("title",
                                       "0"),
                                     ncol=1, byrow=TRUE)
            extreme_H = bring_grass(verbose=verbose)
            extreme_H = plan_of_herd(extreme_H, extreme_H_plan,
                                      verbose=verbose)


            title = ggplot() + theme_void_Lato() +
                theme(panel.background=element_rect(fill=IPCCgrey97, color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.5,
                         y=0.35,
                         label=TeX(Horizons_extreme[1]),
                         size=3, hjust=0.5, vjust=0.5,
                         family="Lato",
                         color=IPCCgrey35) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme_H = add_sheep(extreme_H,
                                  sheep=title,
                                  id="title",
                                  height=extreme_H_title_height,
                                  verbose=verbose)


            extreme_sL_plan = matrix(c("n",
                                       "n_text",
                                       "axis",
                                       "void"),
                                     ncol=1, byrow=TRUE)
            extreme_sL = bring_grass(verbose=verbose)
            extreme_sL = plan_of_herd(extreme_sL, extreme_sL_plan,
                                      verbose=verbose)


            plot = ggplot() + theme_void_Lato() +
                theme(panel.background=element_rect(fill=IPCCgrey97,
                                                    color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm"))
            plot = plot +
                annotate("point",
                         x=x_dot[2:rp],
                         y=y_dot[2:rp],
                         size=1.35,
                         fill=IPCCgrey85,
                         color="transparent",
                         shape=21)
            plot = plot +
                annotate("point",
                         x=x_dot[1],
                         y=y_dot[1],
                         size=1.35,
                         fill=IPCCgrey50,
                         color="transparent",
                         shape=21) +
                scale_x_continuous(limits=c(-dp_x, 1+dp_x),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(-dp_y, 1+dp_y),
                                   expand=c(0, 0))
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=plot,
                                   id="n",
                                   height=extreme_H_sL_n_height,
                                   verbose=verbose)

            text = ggplot() + theme_void_Lato() + 
                theme(panel.background=element_rect(fill=IPCCgrey97,
                                                    color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.5,
                         y=0.75,
                         label=TeX(paste0("\\textbf{1 fois}")),
                         family="Lato",
                         size=3, hjust=0.5, vjust=0.5,
                         color=IPCCgrey50) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=text,
                                   id="n_text",
                                   height=extreme_H_sL_n_text_height,
                                   verbose=verbose)
            get_labels = function (X) {
                up = X > 0
                X[X != 0] = paste0(X[X != 0], " %")
                X[up] = paste0("+", X[up])
                return (X)
            }

            axis = ggplot() + theme_void_Lato() + 
                theme(plot.background=element_rect(fill=IPCCgrey97,
                                                   color=NA),
                      panel.grid.major.y=element_line(color=IPCCgrey85,
                                                      size=0.25),
                      axis.text.y=element_text(color=IPCCgrey35,
                                               hjust=1, size=7,
                                               margin=margin(t=0, r=1,
                                                             b=0, l=0, "mm")),
                      plot.margin=margin(t=3, r=0, b=1, l=6, "mm")) + 
                annotate("line",
                         x=c(0, 1), 
                         y=0,
                         color=IPCCgrey60,
                         linewidth=0.33) + 
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=limits_bar_y[[j]],
                                   labels=get_labels,
                                   expand=c(0, 0))
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=axis,
                                   id="axis",
                                   height=extreme_H_sL_delta_height,
                                   verbose=verbose)
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=void(panel.background_fill=IPCCgrey97),
                                   id="void",
                                   height=extreme_H_sL_delta_text_height,
                                   verbose=verbose)
            
            extreme_H = add_sheep(extreme_H,
                                   sheep=extreme_sL,
                                   id=0,
                                   height=extreme_H_sL_height,
                                   verbose=verbose)
            
            extreme = add_sheep(extreme,
                                sheep=extreme_H,
                                id="H0",
                                height=extreme_H_height,
                                width=1,
                                verbose=verbose)

            
#### 2.3.4. HX _______________________________________________________
            for (h in 1:nDeltaHorizon) {
                H = deltaHorizon[h]
                
                variable_H = paste0(variable, "_", H)

                variable_delta_H = paste0("delta", variable_H)
                Ok = metaEX_criteria$variable_en == variable_delta_H
                unit_delta = metaEX_criteria$unit_fr[Ok]

                variable_n_H = paste0("n", variable_H)
                Ok = metaEX_criteria$variable_en == variable_n_H
                unit_n = metaEX_criteria$unit_fr[Ok]
                

                extreme_H_plan = matrix(c(rep("title", nStorylines),
                                          1:nStorylines),
                                        ncol=nStorylines, byrow=TRUE)
                extreme_H = bring_grass(verbose=verbose)
                extreme_H = plan_of_herd(extreme_H, extreme_H_plan,
                                         verbose=verbose)

                title = ggplot() + theme_void_Lato() + 
                    theme(panel.background=element_rect(fill=IPCCgrey97,
                                                        color=NA),
                          plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                    annotate("text",
                             x=0.5,
                             y=0.3,
                             label=TeX(Horizons_extreme[h+1]),
                             size=3, hjust=0.5, vjust=0.5,
                             family="Lato",
                             color=IPCCgrey35) +
                    scale_x_continuous(limits=c(0, 1),
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=c(0, 1),
                                       expand=c(0, 0))
                
                extreme_H = add_sheep(extreme_H,
                                      sheep=title,
                                       id="title",
                                       height=extreme_H_title_height,
                                       verbose=verbose)
                
                
                for (k in 1:nStorylines) {
                    storyline = Storylines[k]
                    color = Colors[names(Colors) == storyline]
                    color_light = Colors_light[names(Colors_light) == storyline]

                    Delta_variable_n_H =
                        dplyr::filter(dataEX_criteria_code,
                                      climateChain == storyline)[[variable_n_H]]
                    medDelta_variable_n_H = median(Delta_variable_n_H, na.rm=TRUE)
                    minDelta_variable_n_H = min(Delta_variable_n_H, na.rm=TRUE)
                    maxDelta_variable_n_H = max(Delta_variable_n_H, na.rm=TRUE)
                    
                    nH_r0 = round(medDelta_variable_n_H / div, 0)
                    nH_r1 =
                        sprintf("%.1f", round(medDelta_variable_n_H / div, 1))
                    nH_min_r1 =
                        sprintf("%.1f", round(minDelta_variable_n_H / div, 1))
                    nH_max_r1 =
                        sprintf("%.1f", round(maxDelta_variable_n_H / div, 1))

                    Delta_variable_delta_H =
                        dplyr::filter(dataEX_criteria_code,
                                      climateChain ==
                                      storyline)[[variable_delta_H]]

                    medDelta_variable_delta_H =
                        median(Delta_variable_delta_H, na.rm=TRUE)
                    minDelta_variable_delta_H =
                        min(Delta_variable_delta_H, na.rm=TRUE)
                    maxDelta_variable_delta_H =
                        max(Delta_variable_delta_H, na.rm=TRUE)

                    if (medDelta_variable_delta_H > 0) {
                        deltaH_r1 =
                            paste0("+",
                                   sprintf("%.1f",
                                           round(medDelta_variable_delta_H, 1)))
                    } else {
                        deltaH_r1 =
                            sprintf("%.1f", round(medDelta_variable_delta_H, 1))
                    }
                    if (medDelta_variable_delta_H > 0) {
                        deltaH_r0 =
                            paste0("+", round(medDelta_variable_delta_H, 0))
                    } else {
                        deltaH_r0 = round(medDelta_variable_delta_H, 0)
                    }
                    
                    extreme_sL_plan = matrix(c("n",
                                               "n_text",
                                               "delta",
                                               "delta_text"),
                                             ncol=1, byrow=TRUE)
                    extreme_sL = bring_grass(verbose=verbose)
                    extreme_sL = plan_of_herd(extreme_sL, extreme_sL_plan,
                                              verbose=verbose)

                    plot = ggplot() + theme_void_Lato() +
                        theme(panel.background=element_rect(fill=IPCCgrey97,
                                                            color=NA),
                              plot.margin=margin(t=0, r=0, b=0, l=0, "mm"))

                    if (!is.na(nH_r0)) {
                        if (nH_r0 < rp) {
                            plot = plot +
                                annotate("point",
                                         x=x_dot[(nH_r0+1):rp],
                                         y=y_dot[(nH_r0+1):rp],
                                         size=1.35,
                                         fill=color_light,
                                         color="transparent",
                                         shape=21)
                        }
                        plot = plot +
                            annotate("point",
                                     x=x_dot[1:nH_r0],
                                     y=y_dot[1:nH_r0],
                                     size=1.35,
                                     fill=color,
                                     color="transparent",
                                     shape=21)
                    }
                    plot = plot +
                        scale_x_continuous(limits=c(-dp_x, 1+dp_x),
                                           expand=c(0, 0)) +
                        scale_y_continuous(limits=c(-dp_y, 1+dp_y),
                                           expand=c(0, 0))
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=plot,
                                           id="n",
                                           height=extreme_H_sL_n_height,
                                           verbose=verbose)

                    text = ggplot() + theme_void_Lato() + 
                        theme(panel.background=element_rect(fill=IPCCgrey97,
                                                            color=NA),
                              plot.margin=margin(t=0, r=0, b=0, l=0, "mm"))

                    if (!is.na(nH_r0)) {
                        text = text +
                            annotate("text",
                                     x=0.5,
                                     y=0.75,
                                     label=TeX(paste0("\\textbf{", nH_r1, " fois}")),
                                     size=3, hjust=0.5, vjust=0.5,
                                     family="Lato",
                                     color=color) +
                            annotate("text",
                                     x=0.5,
                                     y=0.3,
                                     label=paste0("(", nH_min_r1, " - ",
                                                  nH_max_r1, ")"),
                                     size=2, hjust=0.5, vjust=0.5,
                                     family="Lato",
                                     color=color)
                    }
                    text = text +
                        scale_x_continuous(limits=c(0, 1),
                                           expand=c(0, 0)) +
                        scale_y_continuous(limits=c(0, 1),
                                           expand=c(0, 0))
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=text,
                                           id="n_text",
                                           height=extreme_H_sL_n_text_height,
                                           verbose=verbose)


                    x_bar = 0.42
                    dx_bar = 0.05
                    x_bar_mini = 0.53
                    dx_bar_mini = 0.025
                    
                    Delta_variable_H = dataEX_criteria_code[[variable_delta_H]]
                    Chain_variable_H = dataEX_criteria_code$Chain
                    nDelta_variable_H = length(Delta_variable_H)
                    
                    tmp = dplyr::tibble(Chain=rep(Chain_variable_H, each=2),
                                        x=rep(c(x_bar-dx_bar,
                                                x_bar+dx_bar),
                                              nDelta_variable_H),
                                        y=rep(Delta_variable_H, each=2))
                    
                    plot = ggplot() + theme_void_Lato() + 
                        theme(plot.background=element_rect(fill=IPCCgrey97,
                                                            color=NA),
                              panel.grid.major.y=element_line(color=IPCCgrey85,
                                                              size=0.25),
                              plot.margin=margin(t=3, r=0, b=1, l=0, "mm"))
                        # annotate("rect",
                        #          xmin=0.5-dbar, xmax=0.5+dbar, 
                        #          ymin=0, ymax=medDelta_variable_delta_H,
                        #          color=NA, fill=color)


                    plot = plot +
                        geom_line(data=tmp,
                                  aes(x=x, y=y, group=Chain),
                                  color=IPCCgrey50,
                                  linewidth=0.3,
                                  alpha=0.07,
                                  lineend="round")

                    min_delta = max(c(minDelta_variable_delta_H,
                                      limits_bar_y[[j]][1]))
                    max_delta = min(c(maxDelta_variable_delta_H,
                                      limits_bar_y[[j]][2]))
                    
                    plot = plot +
                        annotate("line",
                                 x=c(0, 1), 
                                 y=0,
                                 color=IPCCgrey60,
                                 linewidth=0.33) +
                        annotate("rect",
                                 xmin=x_bar_mini-dx_bar_mini,
                                 xmax=x_bar_mini+dx_bar_mini,
                                 ymin=min_delta,
                                 ymax=max_delta,
                                 color=NA,
                                 fill=color) +
                        # annotate("line",
                        #          x=c(x_bar-dx_bar, x_bar+dx_bar), 
                        #          y=max_delta,
                        #          color=color,
                        #          linewidth=0.3) +
                        annotate("line",
                                 x=c(x_bar_mini-dx_bar_mini,
                                     x_bar_mini+dx_bar_mini), 
                                 y=medDelta_variable_delta_H,
                                 color=IPCCgrey97, lineend="square",
                                 linewidth=1) +
                        annotate("line",
                                 x=c(x_bar_mini-dx_bar_mini,
                                     x_bar_mini+dx_bar_mini), 
                                 y=medDelta_variable_delta_H,
                                 color=color, lineend="square",
                                 linewidth=0.4)
                        # annotate("line",
                        #          x=c(x_bar-dx_bar, x_bar+dx_bar), 
                        #          y=min_delta,
                        #          color=color,
                        #          linewidth=0.3)
                    
                    # plot = plot +
                    #     annotate("line",
                    #              x=c(0.5, 0.5), 
                    #              y=c(max(c(minDelta_variable_delta_H,
                    #                        limits_bar_y[[j]][1])),
                    #                  min(c(maxDelta_variable_delta_H,
                    #                        limits_bar_y[[j]][2]))),
                    #              color=color_light,
                    #              linewidth=0.4)
                    
                    # if (minDelta_variable_delta_H >= limits_bar_y[[j]][1]) {
                    #     plot = plot +
                    #         annotate("line",
                    #                  x=c(0.475, 0.525), 
                    #                  y=c(minDelta_variable_delta_H,
                    #                      minDelta_variable_delta_H),
                    #                  color=color_light,
                    #                  linewidth=0.4)
                    # }
                    # if (maxDelta_variable_delta_H <= limits_bar_y[[j]][2]) {
                    #     plot = plot +
                    #         annotate("line",
                    #                  x=c(0.475, 0.525), 
                    #                  y=c(maxDelta_variable_delta_H,
                    #                      maxDelta_variable_delta_H),
                    #                  color=color_light,
                    #                  linewidth=0.4)
                    # }
                    
                    plot = plot +
                        scale_x_continuous(limits=c(0, 1),
                                           expand=c(0, 0)) +
                        scale_y_continuous(limits=limits_bar_y[[j]],
                                           expand=c(0, 0))
                    
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=plot,
                                           id="delta",
                                           height=extreme_H_sL_delta_height,
                                           verbose=verbose)


                    # text = ggplot() + theme_void_Lato() + 
                    #     theme(panel.background=element_rect(fill=IPCCgrey97,
                    #                                         color=NA),
                    #           plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                    #     annotate("text",
                    #              x=0.5,
                    #              y=0.6,
                    #              label=TeX(paste0("\\textbf{", deltaH_r1, "} %")),
                    #              size=3, hjust=0.5, vjust=0.5,
                    #              color=color) +
                    #     scale_x_continuous(limits=c(0, 1),
                    #                        expand=c(0, 0)) +
                    #     scale_y_continuous(limits=c(0, 1),
                    #                        expand=c(0, 0))
                    text = void(panel.background_fill=IPCCgrey97)
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=text,
                                           id="delta_text",
                                           height=extreme_H_sL_delta_text_height,
                                           verbose=verbose)

                    extreme_H = add_sheep(extreme_H,
                                          sheep=extreme_sL,
                                          id=k,
                                          height=extreme_H_sL_height,
                                          verbose=verbose)
                }
                extreme = add_sheep(extreme,
                                    sheep=extreme_H,
                                    id=H,
                                    height=extreme_H_height,
                                    width=nStorylines,
                                    verbose=verbose)
            }
            ###
            herd = add_sheep(herd,
                             sheep=extreme,
                             id=variable,
                             height=extreme_height,
                             width=width,
                             verbose=verbose)
            # herd = add_sheep(herd,
                             # sheep=void(),
                             # id=variable,
                             # height=extreme_height,
                             # width=width,
                             # verbose=verbose)
            ###
        }
        id_letter = id_letter + nVariables_extreme


### 2.4. Foot ________________________________________________________
        if (is.null(Pages)) {
            n_page = 2
        } else {
            if (nrow(Pages) == 0) {
                n_page = 1
            } else {
                n_page = Pages$n[nrow(Pages)] + 1
            }
            Pages = bind_rows(
                Pages,
                tibble(section=footName,
                       subsection=code,
                       n=n_page))
        }

        foot = panel_foot(footName, n_page,
                          foot_height, logo_info,
                          left_plot=warning,
                          verbose=verbose)
        herd = add_sheep(herd,
                         sheep=foot,
                         id="foot",
                         height=foot_height,
                         width=width,
                         verbose=verbose)
        
        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size="A4",
                                  hjust=0, vjust=1,
                                  verbose=verbose)
        
        plot = res$plot
        paper_size = res$paper_size

        filename = paste0(code, "_projection_datasheet_2.pdf")

        if (!(file.exists(figdir))) {
            dir.create(figdir, recursive=TRUE)
        }
        ggplot2::ggsave(plot=plot,
                        path=figdir,
                        filename=filename,
                        width=paper_size[1],
                        height=paper_size[2], units='cm',
                        dpi=300,
                        device=cairo_pdf)


## 3. MERGING ________________________________________________________
        input = paste0(code, "_projection_datasheet_",
                       c(1, 2), ".pdf")
        input = file.path(figdir, input)
        output = paste0(code, "_projection_datasheet.pdf")
        output = file.path(figdir, output)
        qpdf::pdf_combine(input=input, output=output)
        unlink(input)
    }
    return (Pages)
}
