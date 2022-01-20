load_db <- function(db_name){
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = db_name)
}