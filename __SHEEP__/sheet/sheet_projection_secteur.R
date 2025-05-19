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
                                     Colors_GWL,
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
    
    info_height = 5
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
            
## PAGE _________________________________________________________
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)

### Info ________________________________________________________
        info_plan = matrix(c(
            "map", "title", "logo",
            "map", "legend", "logo"
        ), ncol=3, byrow=TRUE)

        info_title_height = 1
        info_legend_height = 1
        
        info_map_width = 1
        info_title_width = 2
        info_logo_width = 1

        info_herd = bring_grass(verbose=verbose)
        info_herd = plan_of_herd(info_herd, info_plan,
                                 verbose=verbose)

        xlim = c(90000, 1250000)
        ylim = c(6040000, 7120000)

        margin_map = margin(t=0, r=0, b=0, l=0, unit="mm")
        
        echelle = c(0, 250)
        x_echelle_pct = 2
        y_echelle_pct = 3
        echelle_size = 2
        echelle_km_size = 1.9
        echelle_tick_height = 3
        
        cf = coord_fixed()
        cf$default = TRUE
        map = ggplot() + theme_void_Lato() + cf +
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
        ymin_km = gpct(max(c(y_echelle_pct-5, 0)), ylim, shift=TRUE)
        ymax = ymin + gpct(echelle_tick_height, ylim)

        map = map +
            geom_line(aes(x=c(xmin, max(xint)+xmin),
                          y=c(ymin, ymin)),
                      color=IPCCgrey40, size=0.2) +
            annotate("text",
                     x=max(xint)+xmin+gpct(1, xlim), y=ymin_km,
                     vjust=0, hjust=0, label="km",
                     family="Lato",
                     color=IPCCgrey40, size=echelle_km_size)

        for (x in xint) {
            map = map +
                annotate("segment",
                         x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                         color=IPCCgrey40, size=0.2) +
                annotate("text",
                         x=x+xmin, y=ymax+gpct(0.5, ylim),
                         vjust=0, hjust=0.5, label=x/1E3,
                         family="Lato",
                         color=IPCCgrey40, size=echelle_size)
        }

        secteurLight_shp = Shapefiles$secteurHydro[Shapefiles$secteurHydro$CdSecteurH == sh,]

        map = map +
            geom_sf(data=secteurLight_shp,
                    color="white",
                    fill=NA,
                    linewidth=1.1) +
            geom_sf(data=secteurLight_shp,
                    color=INRAEdarkcyan,
                    fill=NA,
                    linewidth=0.5)

        map = map +
            coord_sf(xlim=xlim, ylim=ylim,
                     expand=FALSE)

        info_herd = add_sheep(info_herd,
                              sheep=map,
                              id="map",
                              height=info_height,
                              width=info_map_width,
                              verbose=verbose)

#### Info text _______________________________________________________
        info_herd = add_sheep(info_herd,
                              sheep=contour(),
                              id="title",
                              height=info_title_height,
                              width=info_title_width,
                              verbose=verbose)

        info_herd = add_sheep(info_herd,
                              sheep=contour(),
                              id="legend",
                              height=info_legend_height,
                              verbose=verbose)
        
#### Info logo _______________________________________________________
        info_herd = add_sheep(info_herd,
                              sheep=contour(),
                              id="logo",
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
            "climate_delta", "climate_table"
        ), ncol=2, byrow=TRUE)
        
        climate_herd = bring_grass(verbose=verbose)
        climate_herd = plan_of_herd(climate_herd, climate_plan,
                                    verbose=verbose)
        
        climate_delta_width = 1
        climate_table_width = 1

#### Climate plot ____________________________________________________        
        climate_delta_plan = matrix(c(
            "title", "title", "title",
            "dR",    "hiver_title", "ete_title",
            "dR",    "hiver", "ete",
            "void",  "dT",    "dT" 
        ), ncol=3, byrow=TRUE)

        climate_delta_title_height = 0.1
        climate_delta_variable_title_height = 0.1
        climate_delta_variable_height = 1
        climate_delta_dT_height = 0.1
        
        climate_delta_variable_width = 1
        climate_delta_dR_width = 0.1
        
        climate_delta_herd = bring_grass(verbose=verbose)
        climate_delta_herd = plan_of_herd(climate_delta_herd, climate_delta_plan,
                                          verbose=verbose)
        
        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=contour(),
                                       id="title",
                                       height=climate_delta_title_height,
                                       verbose=verbose)

        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=void(),
                                       id="void",
                                       verbose=verbose)

        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=contour(),
                                       id="dR",
                                       width=climate_delta_dR_width,
                                       verbose=verbose)
        
        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=contour(),
                                       id="dT",
                                       height=climate_delta_dT_height,
                                       verbose=verbose)

        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=contour(),
                                       id="hiver_title",
                                       height=climate_delta_variable_title_height,
                                       verbose=verbose)
        
        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=contour(),
                                       id="hiver",
                                       height=climate_delta_variable_height,
                                       width=climate_delta_variable_width,
                                       verbose=verbose)

        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=contour(),
                                       id="ete_title",
                                       height=climate_delta_variable_title_height,
                                       verbose=verbose)
        
        climate_delta_herd = add_sheep(climate_delta_herd,
                                       sheep=contour(),
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
            "climate_table_title",
            "climate_table_content"
        ), ncol=1, byrow=TRUE)

        climate_table_title_height = 0.1
        climate_table_content_height = 1

        climate_table_herd = bring_grass(verbose=verbose)
        climate_table_herd = plan_of_herd(climate_table_herd, climate_table_plan,
                                          verbose=verbose)

        climate_table_herd = add_sheep(climate_table_herd,
                                       sheep=contour(),
                                       id="climate_table_title",
                                       height=climate_table_title_height,
                                       verbose=verbose)
        
        climate_table_herd = add_sheep(climate_table_herd,
                                       sheep=contour(),
                                       id="climate_table_content",
                                       height=climate_table_content_height,
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
        for (j in 1:nRiver) {
            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=contour(),
                                     id=paste0("river_name_", j),
                                     height=hydroQM_river_name_height,
                                     width=hydroQM_river_width,
                                     verbose=verbose)

            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=contour(),
                                     id=paste0("river_ref_", j),
                                     height=hydroQM_river_graph_height,
                                     width=hydroQM_river_width,
                                     verbose=verbose)
            
            hydroQM_herd = add_sheep(hydroQM_herd,
                                     sheep=contour(),
                                     id=paste0("river_delta_", j),
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

        
        
## Plot ______________________________________________________________
        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size="A4",
                                  hjust=0, vjust=1,
                                  verbose=verbose)        
        plot = res$plot
        paper_size = res$paper_size

        filename = paste0(sh, "_secteur_projection_datasheet.pdf")

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

        stop()
    }
    return (NULL)
}
