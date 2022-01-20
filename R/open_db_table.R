load_db <- function(db_name){
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = db_name)
}

open_db_table <- function(db_name,db_table){
  conn <- load_db(db_name)
  DBI::dbReadTable(conn, db_table)
}