# Get the data to populate drop-downs. Runs every time this module is loaded.

# continuous plot and data modules ######
cont_data.plot_module_data <- function(con, env = .GlobalEnv) {
  get_cached("cont_data.plot_module_data",
             env = env,
             fetch_fun = function() {
               locs <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT loc.location_id, loc.name, loc.name_fr FROM locations AS loc INNER JOIN timeseries ON loc.location_id = timeseries.location_id ORDER BY loc.name ASC"
               )
               sub_locs <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT sub_location_id, sub_location_name, sub_location_name_fr FROM sub_locations WHERE location_id IN (SELECT DISTINCT location_id FROM timeseries) ORDER BY sub_location_name ASC;"
               )
               params <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT parameter_id, param_name, COALESCE(param_name_fr, param_name) AS param_name_fr, unit_default AS unit FROM parameters WHERE parameter_id IN (SELECT DISTINCT parameter_id FROM timeseries) ORDER BY param_name ASC;"
               )
               media <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT m.* FROM media_types as m WHERE EXISTS (SELECT 1 FROM timeseries AS t WHERE m.media_id = t.media_id);"
               )
               aggregation_types <- DBI::dbGetQuery(
                 con,
                 "SELECT * FROM aggregation_types WHERE aggregation_type_id IN (SELECT DISTINCT aggregation_type_id FROM timeseries);"
               )
               parameter_relationships <- DBI::dbGetQuery(
                 con,
                 "SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM timeseries AS t WHERE p.parameter_id = t.parameter_id) ;"
               )
               range <- DBI::dbGetQuery(
                 con,
                 "SELECT MIN(start_datetime) AS min_datetime, MAX(end_datetime) AS max_datetime FROM timeseries;"
               )
               timeseries <- DBI::dbGetQuery(
                 con,
                 "SELECT timeseries_id, location_id, sub_location_id, media_id, parameter_id, aggregation_type_id, EXTRACT(EPOCH FROM record_rate) AS record_rate, z, start_datetime, end_datetime FROM timeseries;"
               )
               
               rates <- data.frame(seconds = unique(timeseries$record_rate)[order(unique(timeseries$record_rate))])
               rates$period <- as.character(lubridate::seconds_to_period(rates$seconds))
               z <- unique(timeseries$z[!is.na(timeseries$z)])
               
               locations_projects <- DBI::dbGetQuery(con, paste0("SELECT * FROM locations_projects WHERE location_id IN (", paste(locs$location_id, collapse = ", "), ");"))
               if (nrow(locations_projects) > 0) {
                 projects <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT * FROM projects WHERE location_id IN (", paste(locations_projects$project_id, collapse = ", "), ");"))
               } else {
                 locations_projects <- data.frame(location_id = numeric(), project_id = numeric())
                 projects <- data.frame(project_id = numeric(), name = character(), ame_fr = character())
               }
               
               locations_networks <- DBI::dbGetQuery(con, paste0("SELECT * FROM locations_networks WHERE location_id IN (", paste(locs$location_id, collapse = ", "), ");"))
               if (nrow(locations_networks) > 0) {
                 networks <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT * FROM networks WHERE network_id IN (", paste(locations_networks$network_id, collapse = ", "), ");"))
               } else {
                 networks <- data.frame()
               }
               
               if (any(!is.na(parameter_relationships$group_id))) {
                 groups <- parameter_relationships$group_id[!is.na(parameter_relationships$group_id)]
                 param_groups <- DBI::dbGetQuery(con, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(groups, collapse = ", "), ");"))
               } else {
                 param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
               }
               if (any(!is.na(parameter_relationships$sub_group_id))) {
                 sub_groups <- parameter_relationships$sub_group_id[!is.na(parameter_relationships$sub_group_id)]
                 param_sub_groups <- DBI::dbGetQuery(con, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
               } else {
                 param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(), sub_group_name_fr = character(), description = character(), description_fr = character())
               }
               
               list(
                 locs = locs,
                 sub_locs = sub_locs,
                 params = params,
                 media = media,
                 aggregation_types = aggregation_types,
                 parameter_relationships = parameter_relationships,
                 range = range,
                 timeseries = timeseries,
                 rates = rates,
                 z = z,
                 locations_projects = locations_projects,
                 projects = projects,
                 locations_networks = locations_networks,
                 networks = networks,
                 param_groups = param_groups,
                 param_sub_groups = param_sub_groups
               )
             },
             ttl = 60 * 60 * 2) # Cache the data for 2 hours
}


# discrete data module #####
disc_data_module_data <- function(con, env = .GlobalEnv) {
  get_cached("disc_data_module_data",
             env = env,
             fetch_fun = function() {
               locs <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT loc.location_id, loc.name, loc.name_fr FROM locations AS loc INNER JOIN samples ON loc.location_id = samples.location_id ORDER BY loc.name ASC"
               )
               sub_locs <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT sub_location_id, sub_location_name, sub_location_name_fr FROM sub_locations WHERE location_id IN (SELECT DISTINCT location_id FROM samples) ORDER BY sub_location_name ASC;"
               )
               params <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT parameter_id, param_name, COALESCE(param_name_fr, param_name) AS param_name_fr, unit_default AS unit FROM parameters WHERE parameter_id IN (SELECT DISTINCT parameter_id FROM results) ORDER BY param_name ASC;"
               )
               media <- DBI::dbGetQuery(
                 con,
                 "SELECT DISTINCT m.* FROM media_types as m WHERE EXISTS (SELECT 1 FROM samples AS s WHERE m.media_id = s.media_id);"
               )
               parameter_relationships <- DBI::dbGetQuery(
                 con,
                 "SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id) ;"
               )
               range <- DBI::dbGetQuery(
                 con,
                 "SELECT MIN(datetime) AS min_date, MAX(datetime) AS max_date FROM samples;"
               )
               sample_types <- DBI::dbGetQuery(
                 con,
                 "SELECT st.sample_type_id, st.sample_type, COALESCE(st.sample_type_fr, st.sample_type) AS sample_type_fr FROM sample_types AS st WHERE EXISTS (SELECT 1 FROM samples AS s WHERE st.sample_type_id = s.sample_type);"
               )
               samples <- DBI::dbGetQuery(
                 con,
                 "SELECT sample_id, location_id, sub_location_id, media_id, datetime, sample_type FROM samples;"
               )
               
               locations_projects <- DBI::dbGetQuery(con, paste0("SELECT * FROM locations_projects WHERE location_id IN (", paste(locs$location_id, collapse = ", "), ");"))
               if (nrow(locations_projects) > 0) {
                 projects <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT * FROM projects WHERE location_id IN (", paste(locations_projects$project_id, collapse = ", "), ");"))
               } else {
                 locations_projects <- data.frame(location_id = numeric(), project_id = numeric())
                 projects <- data.frame(project_id = numeric(), name = character(), name_fr = character())
               }
               
               locations_networks <- DBI::dbGetQuery(con, paste0("SELECT * FROM locations_networks WHERE location_id IN (", paste(locs$location_id, collapse = ", "), ");"))
               if (nrow(locations_networks) > 0) {
                 networks <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT * FROM networks WHERE network_id IN (", paste(locations_networks$network_id, collapse = ", "), ");"))
               } else {
                 networks <- data.frame()
               }
               
               if (any(!is.na(parameter_relationships$group_id))) {
                 groups <- parameter_relationships$group_id[!is.na(parameter_relationships$group_id)]
                 param_groups <- DBI::dbGetQuery(con, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(groups, collapse = ", "), ");"))
               } else {
                 param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
               }
               if (any(!is.na(parameter_relationships$sub_group_id))) {
                 sub_groups <- parameter_relationships$sub_group_id[!is.na(parameter_relationships$sub_group_id)]
                 param_sub_groups <- DBI::dbGetQuery(con, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
               } else {
                 param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(), sub_group_name_fr = character(), description = character(), description_fr = character())
               }
               
               list(
                 locs = locs,
                 sub_locs = sub_locs,
                 params = params,
                 media = media,
                 parameter_relationships = parameter_relationships,
                 range = range,
                 sample_types = sample_types,
                 samples = samples,
                 locations_projects = locations_projects,
                 projects = projects,
                 locations_networks = locations_networks,
                 networks = networks,
                 param_groups = param_groups,
                 param_sub_groups = param_sub_groups
               )
             },
             ttl = 60 * 60 * 2) # Cache the data for 2 hours
}


# parameter map module #########
map_params_module_data <- function(con, env = .GlobalEnv) {
  get_cached("map_params_module_data",
             env = env,
             fetch_fun = function() {
               list(
                 locations = dbGetQueryDT(
                   con,
                   "SELECT location, name, name_fr, latitude, longitude, location_id, geom_id, visibility_public, location_type FROM locations"
                 ),
                 timeseries = dbGetQueryDT(
                   con,
                   "SELECT ts.timeseries_id, ts.location_id, p.param_name, p.param_name_fr, m.media_type, ts.media_id, ts.parameter_id, ts.aggregation_type_id, ts.start_datetime, ts.end_datetime, z FROM timeseries AS ts LEFT JOIN parameters AS p ON ts.parameter_id = p.parameter_id LEFT JOIN media_types AS m ON ts.media_id = m.media_id"
                 ),
                 parameters = dbGetQueryDT(
                   con,
                   "SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default, pr.group_id, pr.sub_group_id FROM parameters AS p RIGHT JOIN timeseries AS ts ON p.parameter_id = ts.parameter_id LEFT JOIN parameter_relationships AS pr ON p.parameter_id = pr.parameter_id;"
                 )
               )
             }, 
             ttl = 60 * 60 * 2)  # Cache for 2 hours
}


# locations map module #########
map_location_module_data <- function(con, env = .GlobalEnv) {
  get_cached("map_location_module_data",
             env = env,
             fetch_fun = function() {
               list(
                 locations = dbGetQueryDT(con, "SELECT location, name, name_fr, latitude, longitude, location_id, geom_id, visibility_public, location_type FROM locations"),
                 timeseries = dbGetQueryDT(
                   con,
                   "SELECT ts.timeseries_id, ts.location_id, p.param_name, p.param_name_fr, m.media_type, ts.media_id, ts.parameter_id, ts.aggregation_type_id, ts.start_datetime, ts.end_datetime, ts.z, 'continuous' AS data_type
             FROM continuous.timeseries AS ts
             LEFT JOIN public.parameters AS p ON ts.parameter_id = p.parameter_id
             LEFT JOIN public.media_types AS m ON ts.media_id = m.media_id
           UNION ALL
           SELECT MIN(r.result_id) AS timeseries_id, s.location_id, p.param_name, p.param_name_fr, m.media_type, s.media_id, r.parameter_id, NULL AS aggregation_type_id,
                  MIN(s.datetime) AS start_datetime, MAX(s.datetime) AS end_datetime, MIN(s.z) AS z, 'discrete' AS data_type
             FROM discrete.results r
             JOIN discrete.samples s ON r.sample_id = s.sample_id
             LEFT JOIN public.parameters p ON r.parameter_id = p.parameter_id
             LEFT JOIN public.media_types m ON s.media_id = m.media_id
            GROUP BY s.location_id, p.param_name, p.param_name_fr, m.media_type, s.media_id, r.parameter_id"),
                 projects = dbGetQueryDT(con, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);"),
                 networks =  dbGetQueryDT(con, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);"),
                 locations_projects = dbGetQueryDT(con, "SELECT * FROM locations_projects;"),
                 locations_networks = dbGetQueryDT(con, "SELECT * FROM locations_networks;"),
                 media_types = dbGetQueryDT(
                   con,
                   "SELECT mt.* FROM public.media_types mt WHERE mt.media_id IN (
              SELECT DISTINCT media_id FROM continuous.timeseries
              UNION
              SELECT DISTINCT media_id FROM discrete.samples)"),
                 parameters = dbGetQueryDT(
                   con,
                   "SELECT DISTINCT p.parameter_id, p.param_name, p.param_name_fr, p.unit_default, pr.group_id, pr.sub_group_id
             FROM public.parameters AS p
             LEFT JOIN public.parameter_relationships AS pr ON p.parameter_id = pr.parameter_id
            WHERE p.parameter_id IN (
              SELECT DISTINCT parameter_id FROM continuous.timeseries
              UNION
              SELECT DISTINCT parameter_id FROM discrete.results)"),
                 parameter_groups = dbGetQueryDT(
                   con,
                   "SELECT DISTINCT pg.group_id, pg.group_name, pg.group_name_fr
             FROM public.parameter_groups AS pg
             LEFT JOIN public.parameter_relationships AS pr ON pg.group_id = pr.group_id
            WHERE pr.parameter_id IN (
              SELECT DISTINCT parameter_id FROM continuous.timeseries
              UNION
              SELECT DISTINCT parameter_id FROM discrete.results)"),
                 parameter_sub_groups = dbGetQueryDT(
                   con,
                   "SELECT psg.sub_group_id, psg.sub_group_name, psg.sub_group_name_fr
             FROM public.parameter_sub_groups AS psg
             LEFT JOIN public.parameter_relationships AS pr ON psg.sub_group_id = pr.sub_group_id
            WHERE pr.parameter_id IN (
              SELECT DISTINCT parameter_id FROM continuous.timeseries
              UNION
              SELECT DISTINCT parameter_id FROM discrete.results)")
               )
             },
             ttl = 60 * 60 * 2)  # Cache for 2 hours
}
