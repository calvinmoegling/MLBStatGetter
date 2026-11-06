# install.packages(c("shiny", "shinyFiles", "baseballr", "httr2", "jsonlite", "dplyr", "stringr"))
# Run with: shiny::runApp("app.R")

library(shiny)
library(shinyFiles)
library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(baseballr)

UUID  <- "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
feeds <- new.env()   # game feeds already downloaded

get_json <- function(url) {
  request(url) |> req_user_agent("Mozilla/5.0") |> req_perform() |>
    resp_body_string() |> fromJSON(simplifyVector = FALSE)
}

# Savant's play page tells us which game the play is from
find_game_pk <- function(play_id) {
  html <- tryCatch(
    request(paste0("https://baseballsavant.mlb.com/sporty-videos?playId=", play_id)) |>
      req_user_agent("Mozilla/5.0") |> req_perform() |> resp_body_string(),
    error = function(e) "")
  as.integer(str_match(html, "(?i)game_?pk[\"']?\\s*[:=]\\s*[\"']?([0-9]{5,7})")[1, 2])
}

get_feed <- function(pk) {
  key <- as.character(pk)
  if (is.null(feeds[[key]])) {
    feeds[[key]] <- get_json(paste0("https://statsapi.mlb.com/api/v1.1/game/", pk, "/feed/live"))
  }
  feeds[[key]]
}

# named vector -> one-row, all-character data frame
one_row <- function(v) {
  as.data.frame(as.list(setNames(as.character(v), make.unique(names(v)))), check.names = FALSE)
}

# A blank column whose header acts as a divider between sections of the CSV
divider <- function(label) one_row(setNames("", label))

DIV_INFO    <- "|| PLAY INFO ||"
DIV_API     <- "|| STATS API ||"
DIV_STATCAST <- "|| STATCAST ||"

# Returns a data frame (one row per pitch) or stops with a message
pull_play <- function(play_id, source, whole_at_bat) {
  pk <- find_game_pk(play_id)
  if (is.na(pk)) stop("Couldn't find the game from the Savant page")
  feed <- get_feed(pk)
  
  # the plate appearance that contains this play
  at_bat <- NULL
  for (p in feed$liveData$plays$allPlays) {
    ids <- vapply(p$playEvents, function(ev) if (is.null(ev$playId)) "" else ev$playId, "")
    if (play_id %in% ids) { at_bat <- p; break }
  }
  if (is.null(at_bat)) stop("Play not found in that game")
  
  batter_id <- at_bat$matchup$batter$id
  date      <- feed$gameData$datetime$officialDate
  ab_num    <- at_bat$about$atBatIndex + 1   # Savant counts from 1
  
  # Only this batter's plate appearance: the requested play, or (if asked)
  # every actual pitch of this one plate appearance (no pickoffs, subs, etc.)
  evs <- Filter(function(ev) {
    !is.null(ev$playId) && (ev$playId == play_id || (whole_at_bat && isTRUE(ev$isPitch)))
  }, at_bat$playEvents)
  
  both <- source == "both"
  
  # Statcast: pulled by batter, then narrowed to this exact at bat
  sav <- NULL
  if (source != "statsapi") {
    sav <- tryCatch(
      scrape_statcast_savant(start_date = date, end_date = date,
                             playerid = batter_id, player_type = "batter"),
      error = function(e) NULL)
    if (!is.null(sav) && nrow(sav) > 0) {
      sav <- sav[which(sav$game_pk == pk &
                         sav$batter == batter_id &
                         sav$at_bat_number == ab_num), ]
    }
  }
  
  rows <- lapply(evs, function(ev) {
    info <- c(play_id = ev$playId, game_pk = pk, date = date,
              batter = at_bat$matchup$batter$fullName,
              pitcher = at_bat$matchup$pitcher$fullName,
              inning = at_bat$about$inning, at_bat_number = ab_num,
              pitch_number = ev$pitchNumber, result = at_bat$result$event,
              requested = ev$playId == play_id)
    parts <- list()
    if (both) parts <- c(parts, list(divider(DIV_INFO)))
    parts <- c(parts, list(one_row(info)))
    
    if (source != "statcast") {
      if (both) parts <- c(parts, list(divider(DIV_API)))
      api <- unlist(ev)
      names(api) <- paste0("api_", gsub("\\.", "_", names(api)))
      parts <- c(parts, list(one_row(api)))
    }
    
    if (source != "statsapi") {
      if (both) parts <- c(parts, list(divider(DIV_STATCAST)))
      if (!is.null(sav) && nrow(sav) > 0) {
        sc <- sav[which(sav$pitch_number == ev$pitchNumber), ]
        if (nrow(sc) == 1) {
          v <- unlist(lapply(as.list(sc), as.character))
          names(v) <- paste0("sc_", names(v))
          parts <- c(parts, list(one_row(v)))
        }
      }
    }
    do.call(cbind, parts)
  })
  out <- bind_rows(rows)
  if ("pitch_number" %in% names(out)) out <- out[order(as.numeric(out$pitch_number)), ]
  out
}

ui <- fluidPage(
  titlePanel("MLB play lookup"),
  textAreaInput("ids", "Play IDs (one per line)", rows = 8, width = "100%"),
  radioButtons("source", "Data", inline = TRUE,
               choices = c("Both" = "both", "Stats API" = "statsapi", "Statcast" = "statcast")),
  checkboxInput("whole_at_bat", "Include every pitch of the at bat (that batter only)"),
  shinySaveButton("pick", "Choose CSV...", "Choose or name the CSV", filetype = list(csv = "csv")),
  textInput("path", "CSV file (updated if it exists, created if not)", "plays.csv", width = "100%"),
  actionButton("run", "Pull plays", class = "btn-primary"),
  tableOutput("status"),
  
  hr(),
  h4("Help"),
  
  tags$details(
    tags$summary(strong("Data definitions (what each CSV column means)")),
    tags$ul(
      tags$li(strong("sc_ columns (Statcast): "),
              tags$a("Statcast Search CSV Documentation",
                     href = "https://baseballsavant.mlb.com/csv-docs", target = "_blank")),
      tags$li(strong("api_ columns (Stats API): "),
              tags$a("MLB Stats API endpoint guide (community wiki)",
                     href = "https://github.com/toddrob99/MLB-StatsAPI/wiki/Endpoints",
                     target = "_blank"),
              tags$br(),
              tags$small("MLB doesn't publish official public docs for the Stats API, so this community-maintained guide is the closest reference. The api_ columns come from the live game feed: ",
                         tags$code("statsapi.mlb.com/api/v1.1/game/{gamePk}/feed/live"),
                         ". Column names are the feed's field paths with dots turned into underscores, e.g. ",
                         tags$code("api_pitchData_startSpeed"), "."))
    )
  ),
  
  tags$details(
    tags$summary(strong("How to find a play ID (and see the play)")),
    tags$ol(
      tags$li("Go to ",
              tags$a("Baseball Savant Statcast Search",
                     href = "https://baseballsavant.mlb.com/statcast_search", target = "_blank"),
              " and filter by player and date (or season)."),
      tags$li("Scroll to the results table. Rows with video have a play/video link in them."),
      tags$li("Click it. The page that opens has this address: ",
              tags$code("https://baseballsavant.mlb.com/sporty-videos?playId=XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX")),
      tags$li("Copy the long ID after ", tags$code("playId="), " from the address bar and paste it into the box above, one per line."),
      tags$li("To watch any play later, paste its ID onto the end of ",
              tags$code("https://baseballsavant.mlb.com/sporty-videos?playId="),
              " and open it.")
    ),
    tags$small("Tip: with \"Include every pitch of the at bat\" checked, you only need the ID of one pitch to get the whole plate appearance.")
  )
)

server <- function(input, output, session) {
  
  roots <- c(Home = path.expand("~"), "Working directory" = getwd(), getVolumes()())
  shinyFileSave(input, "pick", roots = roots, filetypes = "csv", session = session)
  observeEvent(input$pick, {
    sel <- parseSavePath(roots, input$pick)
    if (nrow(sel)) updateTextInput(session, "path", value = as.character(sel$datapath))
  })
  
  observeEvent(input$run, {
    ids <- unique(tolower(unlist(str_extract_all(input$ids, UUID))))
    req(length(ids) > 0)
    
    path <- trimws(input$path)
    if (!grepl("\\.csv$", path, ignore.case = TRUE)) path <- paste0(path, ".csv")
    
    results <- list()
    status  <- list()
    withProgress(message = "Pulling plays", value = 0, {
      for (id in ids) {
        incProgress(1 / length(ids))
        out <- tryCatch(pull_play(id, input$source, input$whole_at_bat),
                        error = function(e) conditionMessage(e))
        if (is.character(out)) {
          status[[id]] <- data.frame(play_id = id, status = out)
        } else {
          results[[id]] <- out
          note <- if (input$source != "statsapi" && !any(startsWith(names(out), "sc_")))
            " (no Statcast match)" else ""
          status[[id]] <- data.frame(play_id = id,
                                     status = paste0(nrow(out), " row(s) pulled", note))
        }
      }
    })
    output$status <- renderTable(bind_rows(status))
    if (!length(results)) return()
    
    # keep each at bat's pitches together, in pitch order
    new <- bind_rows(results) |> distinct(play_id, .keep_all = TRUE)
    
    if (file.exists(path)) {
      old <- read.csv(path, colClasses = "character", na.strings = "", check.names = FALSE)
      if (!"play_id" %in% names(old)) {
        showNotification("That file has no play_id column. Choose another file.", type = "error")
        return()
      }
      new <- bind_rows(old[!old$play_id %in% new$play_id, ], new)   # refresh repeats
    }
    
    tryCatch({
      write.csv(new, path, row.names = FALSE, na = "")
      showNotification(paste("Saved", normalizePath(path)), type = "message")
    }, error = function(e) {
      showNotification("Couldn't write the file. Is it open in Excel?", type = "error")
    })
  })
}

shinyApp(ui, server)