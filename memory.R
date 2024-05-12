library(DBI)
library(RSQLite)

# Establecer conexión con la base de datos SQLite
con <- dbConnect(RSQLite::SQLite(), dbname = "shinyEventsDB.sqlite")

# Crear la tabla de eventos si no existe
dbExecute(con, "CREATE TABLE IF NOT EXISTS events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT,
    start TEXT,
    end TEXT,
    color TEXT,
    allDay BOOLEAN
)")

# Cerrar conexión
dbDisconnect(con)



