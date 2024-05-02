upload_sqlite <- function(data, db_name = "dasc") {
  
  con <- odbc::dbConnect(RSQLite::SQLite(), glue::glue("{db_name}.db"))
  
  table_name <- deparse(substitute(data))
  
  dbWriteTable(con, name = table_name, value = data, overwrite = TRUE)
  
  odbc::dbDisconnect(con)
  
  return(db_name)
  
}

create_users <- function(n) {
  
  users <- tibble::tibble(user_id = 1:1000,
                          user_name = stringi::stri_rand_strings(1000, 10),
                          user_created_at = as.character(sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace = TRUE)))
  
  return(users)
  
}

create_vehicles <- function(n) {
  
  vehicles <- tibble::tibble(vehicle_id = 1:1000,
                             vehicle_name = stringi::stri_rand_strings(1000, 10),
                             vehicle_created_at = as.character(sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace = TRUE)),
                             vehicle_type = sample(c("vehicle", "bike", "car"), 1000, replace = TRUE))
  
  return(vehicles)
  
}

create_vehicle_sessions <- function(n, vehicle_ids, user_ids) {
  
  vehicle_sessions <- tibble::tibble(session_id = 1:n,
                                     session_user_id = sample(user_ids, n, replace = TRUE),
                                     session_vehicle_id = sample(vehicle_ids, n, replace = TRUE),
                                     session_start = as.character(sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), n, replace = TRUE)),
                                     session_end = as.character(sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), n, replace = TRUE)))
  
  return(vehicle_sessions)
  
}

create_vehicle_states <- function(n, vehicle_ids) {
  
  vehicle_states <- tibble::tibble(vs_id = 1:n,
                                   vs_vehicle_id = sample(vehicle_ids, n, replace = TRUE),
                                   vs_start = as.character(sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), n, replace = TRUE)),
                                   vs_end = as.character(sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), n, replace = TRUE)),
                                   vs_type = sample(c("active", "inactive", "maintenance"), n, replace = TRUE))
  
  return(vehicle_states)
  
}




create_database <- function(db_name, tables) {
  
  con <- odbc::dbConnect(RSQLite::SQLite(), glue::glue("{db_name}.db"))
  
  purrr::map(tables, ~dbWriteTable(con, name = deparse(substitute(.)), value = ., overwrite = TRUE))
  
  
  
}





import_to_sqlite <- function(con, path) {
  
  csvs <- list.files(path = path, full.names = TRUE, pattern = ".csv")
  
  table_names <- gsub("./","",gsub(".R","",list.files(path = path, full.names = FALSE, pattern = ".csv")))
  
  purrr::map(csvs, ~read.csv.sql(., sql = "create table main.iris as select * from file", 
                                 dbname = "testingdb"))
  
  
}

create_sessions <- function() {
  
  
  
}


# con <- odbc::dbConnect(duckdb::duckdb(), ":memory:")
# 
# dbSendQuery(con, "create table days as SELECT 
#   range as hour_start,
#   range + interval '1' HOUR as hour_end
# FROM range(DATE '2023-01-01', DATE '2023-12-31', INTERVAL '1' HOUR);
# ")

generate_sessions <- function(time_from, 
                              duration = c(0:120),
                              gap = c(0:600), 
                              n = 1) {
  
  
  session_duration <- sample(duration, size = n, replace = TRUE)
  session_offset <- cumsum(sample(gap, replace = TRUE, size = n))
  
  print(session_duration)
  print(session_offset)
  
  df <- tibble::tibble(start_time = time_from + minutes(cumsum(session_duration)) + minutes(session_offset),
                       end_time = start_time + minutes(session_duration))
  
  return(df)
  
}


add_keys <- function(db_name = "dasc") {
  
  # Add upload_sqlite parameters to define primary and foreign keys?
  
  con <- odbc::dbConnect(RSQLite::SQLite(), glue::glue("{db_name}.db"))
  
  dm <- dm::dm_from_con(con)
  
  dm <- dm %>% 
    dm_add_pk(users, user_id) %>% 
    dm_add_pk(profiles, profile_id) %>% 
    dm_add_pk(groups, group_id) %>% 
    dm_add_pk(user_profiles, user_profile_id) %>% 
    dm_add_fk(user_profiles, up_user_id, users) %>% 
    dm_add_fk(user_profiles, up_profile_id, profiles) %>% 
    dm_add_pk(vehicles, vehicle_id) %>% 
    dm_add_pk(vehicle_sessions, session_id) %>%
    dm_add_pk(vehicle_states, vehicle_state_id) %>% 
    dm_add_pk(subscriptions, subscription_id) %>% 
    dm_add_pk(user_subscriptions, user_subscription_id) %>% 
    dm_add_fk(user_subscriptions, us_user_id, users) %>% 
    dm_add_fk(user_subscriptions, us_subscription_id, subscriptions) %>% 
    dm_add_fk(vehicle_sessions, session_vehicle_id, vehicles) %>% 
    dm_add_fk(vehicle_sessions, session_user_id, users)
    
  
  dm_draw(dm) %>% export_svg %>% charToRaw %>% rsvg_png("database_schema.png")
  
  #Check its existence
  if (file.exists(glue::glue("{db_name}_keys.db"))) {
    #Delete file if it exists
    file.remove(glue::glue("{db_name}_keys.db"))
  }
  
  con_keys <- odbc::dbConnect(RSQLite::SQLite(), glue::glue("{db_name}_keys.db"))
  
  dbSendStatement(con_keys, "PRAGMA writable_schema = 1;")
  dbSendStatement(con_keys, "delete from sqlite_master where type in ('table', 'index', 'trigger');")
  dbSendStatement(con_keys, "PRAGMA writable_schema = 0;")
  dbSendStatement(con_keys, "VACUUM;")
  
  copy_dm_to(dest = con_keys, dm = dm, set_key_constraints = TRUE, temporary = FALSE)
  
  odbc::dbDisconnect(con_keys)
  
  
} 

