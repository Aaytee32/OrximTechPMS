load_db <- function(db_name){
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = db_name)
}

write_to_db <- function(db_name,
                        filename, 
                        datasource,
                        add_condition){
  
  conn <- load_db(db_name)
  DBI::dbWriteTable(conn, 
                    filename, 
                    datasource, 
                    add_condition)
}