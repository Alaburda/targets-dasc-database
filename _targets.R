# _targets.R file
library(targets)
source("R/functions.R")
tar_option_set(packages = c("readr", "dplyr", "ggplot2","odbc","dm","rsvg","DiagrammeRsvg"))

list(
  tar_target(users, create_users(n = 1000)),
  tar_target(users_uploaded, upload_sqlite(users)),
  tar_target(vehicles, create_vehicles(n = 100)),
  tar_target(vehicles_uploaded, upload_sqlite(vehicles)),
  tar_target(vehicle_sessions, create_vehicle_sessions(n = 100, vehicle_ids = vehicles$vehicle_id, user_ids = users$user_id)),
  tar_target(vehicle_sessions_uploaded, upload_sqlite(vehicle_sessions)),
  tar_target(vehicle_states, create_vehicle_states(n = 100, vehicle_ids = vehicles$vehicle_id)),
  tar_target(vehicle_states_uploaded, upload_sqlite(vehicle_states)),
  tar_target(keys_added, add_keys(db_name = vehicle_states_uploaded))
)