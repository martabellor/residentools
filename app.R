
library(shiny)
library(shinyjs)
library(jsonlite)
library(DBI)
library(RSQLite)

# Colores de los eventos por tipo
event_colors <- c(
  "Guardia" = "#FF6347",   # Este color se redefinirá para las guardias en función del residente
  "Vacaciones" = "#4682B4",
  "LD" = "#32CD32",
  "Congreso" = "#FFD700",
  "Sesión" = "#DA70D6",
  "Curso" = "pink"
)

# Colores de las guardias por residente
guardia_colors <- c(
  "Marta" = "#006400",    # Verde oscuro
  "Pedro" = "#000000",    # Negro
  "Nuria" = "#808080",    # Gris
  "Raquel" = "#8B4513",   # Marrón
  "Cibrán" = "#00008B"    # Azul oscuro
)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/fullcalendar/3.10.2/fullcalendar.min.css"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/fullcalendar/3.10.2/fullcalendar.min.js"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/fullcalendar/3.10.2/locale/es.js"),  # Incluir el locale español
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('initCalendar', function(events) {
        $('#calendar').fullCalendar('destroy');
        $('#calendar').fullCalendar({
          header: {
            left: 'prev,next today',
            center: 'title',
            right: 'month,agendaWeek,agendaDay'
          },
          defaultView: 'month',
          editable: true,
          eventLimit: true,
          locale: 'es',
          firstDay: 1,
          events: events,
          eventClick: function(calEvent, jsEvent, view) {
            Shiny.setInputValue('clicked_event', calEvent.id, {priority: 'event'});
          }
        });
      });"
    ))
  ),
  selectInput("resident", "Residente:", choices = c("Marta", "Pedro", "Nuria", "Raquel", "Cibrán", "Todos", "Residente externo 1", "Residente externo 2")),
  selectInput("event_type", "Tipo de Evento:", choices = names(event_colors)),
  dateRangeInput("event_dates", "Fechas del Evento:", start = Sys.Date(), end = Sys.Date() + 1),
  textInput("event_desc", "Descripción del Evento:", ""),
  actionButton("add_event", "Agregar Evento"),
  div(id = "calendar")
)
server <- function(input, output, session) {
  events <- reactiveVal(list())  # Inicializa una variable reactiva para almacenar los eventos

  updateCalendar <- function() {
    con <- dbConnect(RSQLite::SQLite(), dbname = "shinyEventsDB.sqlite")
    stored_events <- dbGetQuery(con, "SELECT id, title, start, end, color, allDay FROM events")
    dbDisconnect(con)
    events(stored_events)  # Almacenar los eventos recuperados en la variable reactiva
    session$sendCustomMessage("initCalendar", toJSON(stored_events, auto_unbox = TRUE))
  }

  observeEvent(input$add_event, {
    if (nchar(input$event_desc) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Se requiere una descripción para el evento.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return()
    }

    event_color <- if (input$event_type == "Guardia" && input$resident %in% names(guardia_colors)) {
      guardia_colors[[input$resident]]
    } else {
      event_colors[[input$event_type]]
    }

    con <- dbConnect(RSQLite::SQLite(), dbname = "shinyEventsDB.sqlite")
    dbExecute(con, "INSERT INTO events (title, start, end, color, allDay) VALUES (?, ?, ?, ?, ?)",
              list(sprintf("%s: %s - %s", input$event_type, input$resident, input$event_desc),
                   as.character(input$event_dates[1]),
                   as.character(input$event_dates[2]),
                   event_color,
                   TRUE))
    dbDisconnect(con)
    updateCalendar()
  })

  observe({
    updateCalendar()
  })

  observeEvent(input$clicked_event, {
    event_to_edit <- events()[events()$id == input$clicked_event, ]
    if (nrow(event_to_edit) > 0) {
      showModal(modalDialog(
        title = "Editar o Eliminar Evento",
        textInput("edit_desc", "Editar descripción:", event_to_edit$title),
        dateRangeInput("edit_dates", "Editar Fechas:", start = event_to_edit$start, end = event_to_edit$end),
        actionButton("save_changes", "Guardar Cambios"),
        actionButton("delete_event", "Eliminar Evento"),
        footer = modalButton("Cerrar")
      ))
    }
  })

  observeEvent(input$save_changes, {
    if (!is.null(input$clicked_event)) {
      con <- dbConnect(RSQLite::SQLite(), dbname = "shinyEventsDB.sqlite")
      dbExecute(con, "UPDATE events SET title = ?, start = ?, end = ? WHERE id = ?",
                list(input$edit_desc, as.character(input$edit_dates[1]), as.character(input$edit_dates[2]), as.numeric(input$clicked_event)))
      dbDisconnect(con)
      updateCalendar()
    }
  })

  observeEvent(input$delete_event, {
    if (!is.null(input$clicked_event)) {
      con <- dbConnect(RSQLite::SQLite(), dbname = "shinyEventsDB.sqlite")
      dbExecute(con, "DELETE FROM events WHERE id = ?", list(as.numeric(input$clicked_event)))
      dbDisconnect(con)
      updateCalendar()
    }
  })
}

shinyApp(ui, server)




