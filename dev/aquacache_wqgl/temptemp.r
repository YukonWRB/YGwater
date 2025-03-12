
library(YGwater)

con <- AquaConnect(username="admin", password="SnowFa11ing")

statement <- readr::read_file("H:/esniede/github/YGwater/dev/aquacache_wqgl/guideline_pb.plpgsql")
DBI::dbExecute(conn=con, "DROP FUNCTION IF EXISTS discrete.guideline_pb(integer)")
DBI::dbExecute(conn=con, statement)
DBI::dbGetQuery(con, "SELECT * FROM discrete.guideline_pb(9404)")
DBI::dbDisconnect(con)



get_parameter_names <- function(con) {
    query <- "SELECT parameter_id, param_name FROM parameters"
    result <- DBI::dbGetQuery(con, query)
    return(result)
}

parameter_names <- get_parameter_names(con)

get_parameter_ids <- function(con) {
    query <- "SELECT param_name, parameter_id FROM parameters"
    result <- DBI::dbGetQuery(con, query)
    return(result)
}

search_parameter_id <- function(con, search_term) {
    query <- paste0("SELECT param_name, parameter_id FROM parameters WHERE param_name ILIKE '%", search_term, "%'")
    result <- DBI::dbGetQuery(con, query)
    return(result)
}

get_sample_fractions <- function(con) {
    query <- "SELECT sample_fraction, sample_fraction_id FROM sample_fractions"
    result <- DBI::dbGetQuery(con, query)
    return(result)
}


#get_sample_fractions(con)



* Contaminated Sites, Schedule 3 (water) Aquatic Life freshwater for Lead * converts from µg/L to mg/L  * Updated 2022/11/15 by Marie Ducharme #Z=1 #S=2 H = A H = [XHard] > 0 ? [XHard] H = [Hard-T] > 0 ? [Hard-T] H = [Ca-T] * [Mg-T] > 0 ? (2.497*[Ca-T])+(4.118 * [Mg-T])  H = [Hard-D] > 0 ? [Hard-D] H = [Ca-D] * [Mg-D] > 0 ? (2.497*[Ca-D])+(4.118 *[Mg-D])  X = H = A ? "Hard missing" X = H < 50 ? 0.04 X = H < 100 ? 0.05 X = H < 200 ? 0.06 X = H < 300 ? 0.11 X = H >= 300 ? 0.16
