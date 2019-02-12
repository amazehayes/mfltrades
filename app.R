library(shiny)
library(shinydashboard)
library(DT)
library(httr)
library(jsonlite)
library(Hmisc)
library(dplyr)
library(plotly)

options(stringsAsFactors = FALSE)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
players_import <- function(leagueID){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  leagueID <- leagueID
  r <- GET(paste0("http://www77.myfantasyleague.com/2019/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"))
  rules_content <- content(r)
  if(is.raw(rules_content)){
    r <- GET(paste0("http://www77.myfantasyleague.com/2018/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"))
    rules_content <- content(r)
  }
  starters <- rules_content$league$starters$position
  starters <- as.data.frame(do.call(rbind, starters))
  starters[,1:2] <- sapply(starters[,1:2], as.character)
  starterspos <- starters$name
  
  player_url_pref <- 'https://www71.myfantasyleague.com/2019/export?TYPE=players&DETAILS=0&SINCE=&PLAYERS=&JSON=1'
  r <- GET(player_url_pref)
  player_content <- content(r)
  player_data <- player_content$players$player
  name_vec <- c('status', 'position', 'name', 'id', 'team')
  player_data <- lapply(player_data, 
                        function(x) {
                          if('status' %nin% names(x)) {
                            x[['status']] <- 'NR'
                          } 
                          return(x[name_vec])
                        })
  
  player_data_df <- data.frame(do.call(rbind, player_data))
  player_data_df[,1:ncol(player_data_df)] <- sapply(player_data_df[,1:ncol(player_data_df)], as.character)
  player_data_df <- player_data_df %>% filter(position %in% starterspos)
  player_data_df$name <- paste(gsub("^.+\\,","",player_data_df$name),gsub("\\,.+$","",player_data_df$name))
  player_data_df$name <- trim(player_data_df$name)
  
  return(player_data_df)
  
}

trades_table <- function(leagueID,username,password,players){
  
  username <- username
  password <- password
  leagueID <- leagueID
  playersdf <- players_import(leagueID)
  player_ids <- playersdf$id
  player_names <- playersdf$name
  
  login_url <- paste0("https://api.myfantasyleague.com/2019/login?USERNAME=",username,"&PASSWORD=",password,"&XML=1")
  r <- GET(login_url)
  cookie_value <- r$cookies$value
  
  leagueID <- leagueID
  r <- GET(paste0("http://www77.myfantasyleague.com/2019/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"),add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
  rules_content <- content(r)
  if(is.raw(rules_content)){
    r <- GET(paste0("http://www77.myfantasyleague.com/2018/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"),add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    rules_content <- content(r)
  }
  history <- rules_content$league$history$league
  history <- as.data.frame(do.call(rbind, history))
  history[,1:2] <- sapply(history[,1:2], as.character)
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  history$ID <- substrRight(history$url,5)
  years <- history$year[history$ID == leagueID]
  years <- sort(as.numeric(years))
  years <- years[length(years)]:years[1]
  
  leagues_url <- paste0('http://www77.myfantasyleague.com/2019/export?TYPE=league&', 'L=', leagueID, '&APIKEY=&JSON=1')
  leagues_url2 <- paste0('http://www77.myfantasyleague.com/2018/export?TYPE=league&', 'L=', leagueID, '&APIKEY=&JSON=1')
  r <- GET(leagues_url,add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
  leagues_content <- content(r)
  if(is.raw(leagues_content)){
    r <- GET(leagues_url2,add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    leagues_content <- content(r)
  }
  leagues <- leagues_content$league$franchises$franchise
  cnames <- c()
  for(i in 1:length(leagues)){
    cnames <- c(cnames,names(leagues[[i]]))
  }
  cnames <- unique(cnames)
  leagues_df <- matrix(nrow = length(leagues), ncol = length(cnames))
  colnames(leagues_df) <- cnames
  for(i in 1:nrow(leagues_df)){
    for(j in 1:length(cnames)){
      v <- unlist(as.character(leagues[[i]][cnames[j]]))
      leagues_df[i,cnames[j]] <- v
    }
  }
  leagues_df <- as.data.frame(leagues_df)
  
  leagues_df[,1:ncol(leagues_df)] <- sapply(leagues_df[,1:ncol(leagues_df)], as.character)
  franchise_id <- leagues_df$id
  franchise_names <- leagues_df$name
  
  # TRADES LOG API CALL USING ABOVE INFO
  trans_url <- matrix(ncol = 1,nrow = length(years))
  colnames(trans_url) <- "URL"
  rownames(trans_url) <- years
  for(i in 1:nrow(trans_url)) {
    x <- paste0('http://www77.myfantasyleague.com/',years[i],'/export?TYPE=transactions&L=',leagueID,'&APIKEY=&W=&TRANS_TYPE=TRADE&FRANCHISE=&DAYS=99999&COUNT=&JSON=1')
    trans_url[i,] <- x
  }
  
  trans_final <- NULL 
  for(i in 1:nrow(trans_url)){
    r <- GET(trans_url[i],add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    
    trans_content <- content(r)
    
    if(is.raw(trans_content)){
      transactions <- NULL
    }
    else {
      transactions <- trans_content$transactions$transaction
    }
    
    if(is.null(transactions)){
      transactions_df <- NULL
    }
    if(!is.null(transactions)){
      transactions_df <- data.frame(do.call(rbind, transactions))
    }
    
    trans_final <- rbind(trans_final, transactions_df)
  }
  trans_final[,1:ncol(trans_final)] <- sapply(trans_final[,1:ncol(trans_final)], as.character)
  
  
  
  for(i in 1:length(franchise_id)){
    trans_final[,2] <- gsub(franchise_id[i], franchise_names[i], trans_final[,2], fixed = TRUE)
    trans_final[,5] <- gsub(franchise_id[i], franchise_names[i], trans_final[,5], fixed = TRUE)
    trans_final[,1] <- gsub(franchise_id[i], franchise_names[i], trans_final[,1], fixed = TRUE)
    trans_final[,4] <- gsub(franchise_id[i], franchise_names[i], trans_final[,4], fixed = TRUE)
  }
  trans_final <- data.frame(lapply(trans_final, function(x) {
    gsub("FP_", "", x)
  }))
  trans_final <- data.frame(lapply(trans_final, function(x) {
    gsub(",", ";", x)
  }))
  
  trans_final$timestamp <- as.POSIXct(as.numeric(as.character(trans_final$timestamp)),origin="1970-01-01")
  
  
  for(k in 50:0){
    for(i in 20:0){
      trans_final[,1] <- gsub(paste0("DP_",k,"_",i),paste0(k+1,".",i+1), trans_final[,1], fixed = TRUE)
      trans_final[,4] <- gsub(paste0("DP_",k,"_",i),paste0(k+1,".",i+1), trans_final[,4], fixed = TRUE)
    }
  }
  
  for(i in length(player_ids):1){
    trans_final[,1] <- gsub(player_ids[i], player_names[i], trans_final[,1], fixed = TRUE)
    trans_final[,4] <- gsub(player_ids[i], player_names[i], trans_final[,4], fixed = TRUE)
  }
  
  trans_finalA <- trans_final[,c(5,4,2,1,3)]
  trans_finalB <- trans_final[,c(2,1,5,4,3)]
  colnames(trans_finalA) <- c("Franchise1","Franchise1 Gave Up","Franchise2","Franchise2 Gave Up","Date")
  colnames(trans_finalB) <- c("Franchise1","Franchise1 Gave Up","Franchise2","Franchise2 Gave Up","Date")
  
  trans_final2 <- rbind(trans_finalA,trans_finalB)
  trans_final2 <- trans_final2[order(trans_final2$Date, decreasing = TRUE),]
  
  return(trans_final2)
}

add_drop_table <- function(leagueID,username,password,players){
  
  username <- username
  password <- password
  leagueID <- leagueID
  playersdf <- players_import(leagueID)
  player_ids <- playersdf$id
  player_names <- playersdf$name
  
  login_url <- paste0("https://api.myfantasyleague.com/2019/login?USERNAME=",username,"&PASSWORD=",password,"&XML=1")
  r <- GET(login_url)
  cookie_value <- r$cookies$value
  
  leagueID <- leagueID
  r <- GET(paste0("http://www77.myfantasyleague.com/2019/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"),add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
  rules_content <- content(r)
  if(is.raw(rules_content)){
    r <- GET(paste0("http://www77.myfantasyleague.com/2018/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"),add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    rules_content <- content(r)
  }
  history <- rules_content$league$history$league
  history <- as.data.frame(do.call(rbind, history))
  history[,1:2] <- sapply(history[,1:2], as.character)
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  history$ID <- substrRight(history$url,5)
  years <- history$year[history$ID == leagueID]
  years <- sort(as.numeric(years))
  years <- years[length(years)]:years[1]
  
  leagues_url <- paste0('http://www77.myfantasyleague.com/2019/export?TYPE=league&', 'L=', leagueID, '&APIKEY=&JSON=1')
  leagues_url2 <- paste0('http://www77.myfantasyleague.com/2018/export?TYPE=league&', 'L=', leagueID, '&APIKEY=&JSON=1')
  r <- GET(leagues_url,add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
  leagues_content <- content(r)
  if(is.raw(leagues_content)){
    r <- GET(leagues_url2,add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    leagues_content <- content(r)
  }
  leagues <- leagues_content$league$franchises$franchise
  cnames <- c()
  for(i in 1:length(leagues)){
    cnames <- c(cnames,names(leagues[[i]]))
  }
  cnames <- unique(cnames)
  leagues_df <- matrix(nrow = length(leagues), ncol = length(cnames))
  colnames(leagues_df) <- cnames
  for(i in 1:nrow(leagues_df)){
    for(j in 1:length(cnames)){
      v <- unlist(as.character(leagues[[i]][cnames[j]]))
      leagues_df[i,cnames[j]] <- v
    }
  }
  leagues_df <- as.data.frame(leagues_df)
  
  leagues_df[,1:ncol(leagues_df)] <- sapply(leagues_df[,1:ncol(leagues_df)], as.character)
  franchise_id <- leagues_df$id
  franchise_names <- leagues_df$name
  
  # TRADES LOG API CALL USING ABOVE INFO
  trans_url <- matrix(ncol = 1,nrow = length(years))
  colnames(trans_url) <- "URL"
  rownames(trans_url) <- years
  for(i in 1:nrow(trans_url)) {
    x <- paste0('http://www77.myfantasyleague.com/',years[i],'/export?TYPE=transactions&L=',leagueID,'&APIKEY=&W=&TRANS_TYPE=FREE_AGENT&FRANCHISE=&DAYS=99999&COUNT=&JSON=1')
    trans_url[i,] <- x
  }
  
  transdf <- NULL 
  for(i in 1:nrow(trans_url)){
    r <- GET(trans_url[i],add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    
    trans_content <- content(r)
    
    if(is.raw(trans_content)){
      transactions <- NULL
    }
    else {
      transactions <- trans_content$transactions$transaction
    }
    
    if(is.null(transactions)){
      transactions_df <- NULL
    }
    if(!is.null(transactions)){
      transactions_df <- data.frame(do.call(rbind, transactions))
    }
    
    transdf <- rbind(transdf, transactions_df)
  }
  transdf[,1:ncol(transdf)] <- sapply(transdf[,1:ncol(transdf)], as.character)

  for(i in length(player_ids):1){
    transdf[,3] <- gsub(player_ids[i], player_names[i], transdf[,3], fixed = TRUE)
  }
  for(i in 1:length(franchise_id)){
    transdf[,2] <- gsub(franchise_id[i], franchise_names[i], transdf[,2], fixed = TRUE)
  }
  transdf[,3] <- gsub(",",";",transdf[,3], fixed = TRUE)
  transdf$timestamp <- as.POSIXct(as.numeric(as.character(transdf$timestamp)),origin="1970-01-01")
  
  transdf <- transform(transdf, test=do.call(rbind, strsplit(transaction, '|', fixed=TRUE)), stringsAsFactors=FALSE)
  colnames(transdf) <- c("Timestamp","Franchise","transaction","Type","PlayersAdded","PlayersDropped")
  transdf <- transdf[,c(2,5,6,1)]
  
  p <- matrix(ncol = 1,nrow = nrow(transdf))
  for(i in 1:nrow(transdf)){
    
    if(transdf$PlayersAdded[i] == transdf$PlayersDropped[i]){
      v <- NA
    }
    else {
      v <- transdf$PlayersDropped[i]
    }
    p[i,] <- v
  }
  transdf <- cbind(transdf,p)
  transdf <- transdf[,c(1,2,5,4)]
  colnames(transdf) <- c("Franchise","PlayersAdded","PlayersDropped","Date")
  
  return(transdf)
  
}

draft_table <- function(leagueID,username,password,players){
  
  username <- username
  password <- password
  leagueID <- leagueID
  playersdf <- players_import(leagueID)
  player_ids <- playersdf$id
  player_names <- playersdf$name
  
  login_url <- paste0("https://api.myfantasyleague.com/2019/login?USERNAME=",username,"&PASSWORD=",password,"&XML=1")
  r <- GET(login_url)
  cookie_value <- r$cookies$value
  
  leagueID <- leagueID
  r <- GET(paste0("http://www77.myfantasyleague.com/2019/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"),add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
  rules_content <- content(r)
  if(is.raw(rules_content)){
    r <- GET(paste0("http://www77.myfantasyleague.com/2018/export?TYPE=league&L=",leagueID,"&APIKEY=&JSON=1"),add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    rules_content <- content(r)
  }
  history <- rules_content$league$history$league
  history <- as.data.frame(do.call(rbind, history))
  history[,1:2] <- sapply(history[,1:2], as.character)
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  history$ID <- substrRight(history$url,5)
  years <- history$year[history$ID == leagueID]
  years <- sort(as.numeric(years))
  years <- years[length(years)]:years[1]
  
  leagues_url <- paste0('http://www77.myfantasyleague.com/2019/export?TYPE=league&', 'L=', leagueID, '&APIKEY=&JSON=1')
  leagues_url2 <- paste0('http://www77.myfantasyleague.com/2018/export?TYPE=league&', 'L=', leagueID, '&APIKEY=&JSON=1')
  r <- GET(leagues_url,add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
  leagues_content <- content(r)
  if(is.raw(leagues_content)){
    r <- GET(leagues_url2,add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    leagues_content <- content(r)
  }
  leagues <- leagues_content$league$franchises$franchise
  cnames <- c()
  for(i in 1:length(leagues)){
    cnames <- c(cnames,names(leagues[[i]]))
  }
  cnames <- unique(cnames)
  leagues_df <- matrix(nrow = length(leagues), ncol = length(cnames))
  colnames(leagues_df) <- cnames
  for(i in 1:nrow(leagues_df)){
    for(j in 1:length(cnames)){
      v <- unlist(as.character(leagues[[i]][cnames[j]]))
      leagues_df[i,cnames[j]] <- v
    }
  }
  leagues_df <- as.data.frame(leagues_df)
  
  leagues_df[,1:ncol(leagues_df)] <- sapply(leagues_df[,1:ncol(leagues_df)], as.character)
  franchise_id <- leagues_df$id
  franchise_names <- leagues_df$name
  
  # TRADES LOG API CALL USING ABOVE INFO
  draft_url <- matrix(ncol = 1,nrow = length(years))
  colnames(draft_url) <- "URL"
  rownames(draft_url) <- years
  for(i in 1:nrow(draft_url)) {
    x <- paste0("http://www77.myfantasyleague.com/",years[i],"/export?TYPE=draftResults&L=",leagueID,"&APIKEY=&JSON=1")
    draft_url[i,] <- x
  }
  
  draftdf <- NULL 
  for(i in 1:(nrow(draft_url)-1)){
    r <- GET(draft_url[i],add_headers(paste0("Cookie: MFL_USER_ID=",cookie_value)))
    
    draft_content <- content(r)
    
    if(is.raw(draft_content)){
      draft <- NULL
    }
    else {
      draft <- draft_content$draftResults$draftUnit$draftPick
    }
    
    if(is.null(draft)){
      draft_df <- NULL
    }
    if(!is.null(draft)){
      draft_df <- data.frame(do.call(rbind, draft))
    }
    
    draftdf <- rbind(draftdf, draft_df)
  }
  draftdf[,3:5] <- sapply(draftdf[,3:5], as.numeric)
  
  for(i in 1:length(franchise_id)){
    draftdf[,2] <- gsub(franchise_id[i], franchise_names[i], draftdf[,2], fixed = TRUE)
  }
  for(i in length(player_ids):1){
    draftdf[,4] <- gsub(player_ids[i], player_names[i], draftdf[,4], fixed = TRUE)
  }
  draftdf$timestamp <- as.POSIXct(as.numeric(as.character(draftdf$timestamp)),origin="1970-01-01")
  
  draftdf <- draftdf[,c(3,5,4,2,1)]
  draftdf <- na.omit(draftdf)
  colnames(draftdf) <- c("Round","Pick","Player","Franchise","Date")
  
  return(draftdf)
}


bodyadmin <- dashboardBody(tags$head(tags$style(HTML('.sidebar {
                                                     color: #FFF;
                                                     position: fixed;
                                                     width: 230px;
                                                     white-space: nowrap;
                                                     overflow: visible;
                                                     }
                                                     .main-header {
                                                     position: fixed;
                                                     width:100%;
                                                     }
                                                     .content-wrapper, .right-side {
                                                     min-height: 100%;
                                                     background-color: #efefef;
                                                     z-index: 800;
                                                     }
                                                     table.dataTable thead th, table.dataTable thead td {
                                                     padding: 10px 18px;
                                                     border-bottom: 1px solid #111;
                                                     background: #ffffff;
                                                     }
                                                     .content {
                                                     padding-top: 120px!important;
                                                     }
                                                     
                                                     @media (min-width:768px) {
                                                     .content {
                                                     padding-top: 80px!important;
                                                     }
                                                     }
                                                     
                                                     @media (max-width:767px) {
                                                     .skin-black .main-header>.logo {
                                                     text-align: left;
                                                     }
                                                     }'))),
                           tabItems(
                           tabItem(tabName = "trades", class = "active",
                                   fluidRow(
                                     valueBoxOutput("favowner", width = 4),
                                     box(plotlyOutput("playersacquired"), width = 4),
                                     box(plotlyOutput("playerstraded"), width = 4)
                                     
                                   ),
                                   
                                   br(),
                                   
                                   fluidRow(
                                     box(plotlyOutput("tradesbymonth"), width = 12)
                                   ),
                                   
                                   br(),
                                   
                                   fluidRow(dataTableOutput("transactions")),
                                   br(),
                                   br(),
                                   br()
                                   ),  
                           
                           tabItem(tabName = "drafts",
                                   fluidRow(
                                     box(plotlyOutput("roundsdrafted"), width = 6),
                                     box(plotlyOutput("playersdrafted"), width = 6)
                                     
                                   ),
                                   
                                   br(),
                                   
                                   fluidRow(dataTableOutput("drafttable")),
                                   br(),
                                   br(),
                                   br()
                                   ),
                           
                           tabItem(tabName = "adds",
                                   fluidRow(
                                     box(plotlyOutput("playersadded"), width = 6),
                                     box(plotlyOutput("playersdropped"), width = 6)
                                     
                                   ),
                                   
                                   br(),
                                   
                                   fluidRow(dataTableOutput("addstable")),
                                   br(),
                                   br(),
                                   br()
                                   ),
                           
                           tabItem(tabName = "donate",
                             fluidRow(
                               column(4),
                               strong("Patreon:"),
                               tagList(a("Click Here to visit the Patreon site",href="patreon.com/amazehayes",target="_blank")),
                               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                             )
                           )
                           
                           )
)

    
                           

ui <- dashboardPage(
   
  dashboardHeader(title = "MFL Transactions"),
  
  dashboardSidebar(selectInput("owner","Select Owner:", choices = NULL),
                   textInput("time","Select Date (yyyy-mm-dd):", value = "2000-01-01"),
                   sidebarMenu(
                   menuItem("Trades", tabName = "trades", icon = icon("bar-chart")),
                   menuItem("Rookie Drafts", tabName = "drafts", icon = icon("bar-chart")),
                   menuItem("Free Agent Acquisitions", tabName = "adds", icon = icon("bar-chart")),
                   menuItem("Support the App", tabName = "donate", icon = icon("dollar")))),
  
  body = uiOutput("app")
   
)


server <- function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    Username <- isolate(input$.username)
    Password <- isolate(input$.password)
    leagueid <- isolate(input$leagueid)
    if (length(Username) > 0 & length(Password) > 0 & length(leagueid) > 0) {
        USER$Logged <- TRUE
    }
    else {
      show("message")
      output$message = renderText("Please enter MFL Username, Password and League ID")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app <- renderUI(
    if(USER$Logged==FALSE) {
      dashboardBody(
        fluidPage(
          fluidRow(
            h1("Enter MFL Username, Password, League ID and Years for League")
            ),
          fluidRow(
            column(width=4, offset = 4, div(style = "padding:0px"),
                   wellPanel(id = "login",
                             textInput(".username", "MFL Username:"),
                             passwordInput(".password", "MFL Password:"),
                             textInput("leagueid","MFL League ID:"),
                             div(actionButton(".login", "Go!"), style="text-align: center;"),
                             br(),
                             br(),
                             strong("Why do you need my username and password?"),
                             br(),
                             ("- This allows the app access to your league through the MFL API."),
                             br(),
                             ("- The usernames and passwords are not kept/stored/or utilized in any other way."),
                             br(),
                             br(),
                             strong("Where do I find my league ID?"),
                             br(),
                             ("- The league ID is the five digit number in your league's URL."),
                             br(),
                             ("- For example, your home page URL will look like this:"),
                             br(),
                             br(),
                             ("http://www77.myfantasyleague.com/2019/home/XXXXX#0"),
                             br(),
                             br(),
                             (" where the XXXXX at the end is your league ID. Do not include the #0.")),
                   textOutput("message"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
            )
          )
        )
      )
    }
    else {
      bodyadmin
    }
  )
  
   
  observeEvent(input$.login, {
    
    df <- trades_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    
    updateSelectInput(session, "owner",
                      label = "Select Owner:",
                      choices = unique(df$Franchise1))
  })
  
  
  output$favowner <- renderValueBox({
    
    df <- trades_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    
    owner <- input$owner
    time <- input$time
    owner_list <- df$Franchise1
    ownerdf <- df %>% filter(Franchise1 == owner, Date >= as.character(time))
    
    ntbo <- matrix(ncol = 2, nrow = length(owner_list))
    colnames(ntbo) <- c("Franchise","NoTrades")
    for(i in 1:nrow(ntbo)){
      
      loopdf <- ownerdf %>% filter(Franchise2 == owner_list[i])
      
      ntbo[i,] <- c(owner_list[i],nrow(loopdf))
      
    }
    ntbo <- as.data.frame(ntbo)
    ntbo$NoTrades <- as.numeric(ntbo$NoTrades)
    ntbo <- ntbo %>% arrange(-NoTrades)
    favowner <- ntbo$Franchise[1]
    favownertrades <- ntbo$NoTrades[1]
    
    valueBox(paste("Most Trades With:"),paste(favowner,":",favownertrades), icon = icon("exchange"))
    
  })
  
  
   output$playerstraded <- renderPlotly({
     
     df <- trades_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
     
     playersdf <- players_import(as.numeric(input$leagueid))
     player_ids <- playersdf$id
     player_names <- playersdf$name
     owner <- input$owner
     time <- input$time
     
     ownerdf <- df %>% filter(Franchise1 == owner, Date >= as.character(time))
     for(i in length(player_ids):1){
       ownerdf[,2] <- gsub(player_names[i], player_ids[i], ownerdf[,2], fixed = TRUE)
       ownerdf[,4] <- gsub(player_names[i], player_ids[i], ownerdf[,4], fixed = TRUE)
     }
     ownerdf[,2] <- gsub(11613, 12652, ownerdf[,2], fixed = TRUE)
     ownerdf[,4] <- gsub(11613, 12652, ownerdf[,4], fixed = TRUE)
     playerstraded <- c(ownerdf$`Franchise1 Gave Up`)
     playerstraded <- strsplit(playerstraded,';',fixed=TRUE)
     playerstraded <- as.data.frame(unlist(playerstraded))
     colnames(playerstraded) <- "id"
     playerstradeddf <- merge(playerstraded,playersdf, by = "id")
     
     piecestraded <- c(unique(as.character(playerstradeddf$position)))
     
     npta <- matrix(ncol = 2, nrow = length(piecestraded))
     colnames(npta) <- c("Piece","NoTrades")
     for(i in 1:nrow(npta)){
       
       loopdf <- playerstradeddf %>% filter(position == piecestraded[i])
       
       npta[i,] <- c(piecestraded[i],nrow(loopdf))
       
     }
     npta <- as.data.frame(npta)
     npta$NoTrades <- as.numeric(npta$NoTrades)
     
     rookiedraft <- draft_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
     maxrounds <- max(rookiedraft$Round)
     futurepicks <- as.data.frame(playerstraded[grepl("_",playerstraded$id),])
     picks <- as.data.frame(playerstraded[grepl("[.]",playerstraded$id),])
     colnames(futurepicks) <- "pick"
     colnames(picks) <- "pick"
     futurepicks$round <- as.numeric(substrRight(futurepicks$pick,1))
     picks$round <- as.numeric(substr(picks$pick,1,2))
     psummary <- matrix(ncol = 2, nrow = maxrounds)
     colnames(psummary) <- c("Piece","NoTrades")
     rownames(psummary) <- 1:maxrounds
     for(i in 1:maxrounds){
       dfloop <- picks %>% filter(round == i)
       dfloop2 <- futurepicks %>% filter(round == i)
       v <- nrow(dfloop) + nrow(dfloop2)
       psummary[i,] <- c(i,v)
     }
     npta <- rbind(npta,psummary)
     
     
     donut <- plot_ly(labels = ~Piece, values = ~NoTrades, data = npta) %>%
       add_pie(hole = 0.6) %>%
       layout(title = "Players Traded by Position",  showlegend = TRUE,
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
     donut
     
   })
  
   output$playersacquired <- renderPlotly({
     
     df <- trades_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
     
     playersdf <- players_import(as.numeric(input$leagueid))
     player_ids <- playersdf$id
     player_names <- playersdf$name
     owner <- input$owner
     time <- input$time
     
     ownerdf <- df %>% filter(Franchise1 == owner, Date >= as.character(time))
     for(i in length(player_ids):1){
       ownerdf[,2] <- gsub(player_names[i], player_ids[i], ownerdf[,2], fixed = TRUE)
       ownerdf[,4] <- gsub(player_names[i], player_ids[i], ownerdf[,4], fixed = TRUE)
     }
     ownerdf[,2] <- gsub(11613, 12652, ownerdf[,2], fixed = TRUE)
     ownerdf[,4] <- gsub(11613, 12652, ownerdf[,4], fixed = TRUE)
     playersacquired <- c(ownerdf$`Franchise2 Gave Up`)
     playersacquired <- strsplit(playersacquired,';',fixed=TRUE)
     playersacquired <- as.data.frame(unlist(playersacquired))
     colnames(playersacquired) <- "id"
     playersacquireddf <- merge(playersacquired,playersdf, by = "id")
     
     piecesacquired <- c(unique(as.character(playersacquireddf$position)))
     
     nptf <- matrix(ncol = 2, nrow = length(piecesacquired))
     colnames(nptf) <- c("Piece","NoTrades")
     for(i in 1:nrow(nptf)){
       
       loopdf <- playersacquireddf %>% filter(position == piecesacquired[i])
       
       nptf[i,] <- c(piecesacquired[i],nrow(loopdf))
       
     }
     nptf <- as.data.frame(nptf)
     nptf$NoTrades <- as.numeric(nptf$NoTrades)
     
     rookiedraft <- draft_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
     maxrounds <- max(rookiedraft$Round)
     futurepicks <- as.data.frame(playersacquired[grepl("_",playersacquired$id),])
     picks <- as.data.frame(playersacquired[grepl("[.]",playersacquired$id),])
     colnames(futurepicks) <- "pick"
     colnames(picks) <- "pick"
     futurepicks$round <- as.numeric(substrRight(futurepicks$pick,1))
     picks$round <- as.numeric(substr(picks$pick,1,2))
     psummary <- matrix(ncol = 2, nrow = maxrounds)
     colnames(psummary) <- c("Piece","NoTrades")
     rownames(psummary) <- 1:maxrounds
     for(i in 1:maxrounds){
       dfloop <- picks %>% filter(round == i)
       dfloop2 <- futurepicks %>% filter(round == i)
       v <- nrow(dfloop) + nrow(dfloop2)
       psummary[i,] <- c(i,v)
     }
     nptf <- rbind(nptf,psummary)
     
     donut <- plot_ly(labels = ~Piece, values = ~NoTrades, data = nptf) %>%
       add_pie(hole = 0.6) %>%
       layout(title = "Players Acquired by Position",  showlegend = TRUE,
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
     donut
     
   })
   
   output$tradesbymonth <- renderPlotly({
     
     df <- trades_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
     
     owner <- input$owner
     time <- input$time
     
     ownerdf <- df %>% filter(Franchise1 == owner, Date >= as.character(time))
     ownerdf <- ownerdf %>% mutate(date2 = format(as.Date(Date), "%m"))
     
     tbm <- matrix(ncol = 2, nrow = 12)
     colnames(tbm) <- c("Month","NoTrades")
     months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
     for(i in 1:12){
       
       if(i < 10){
         month <- paste0(0,i)
       }
       if(i >= 10){
         month <- i
       }
       
       dfloop <- ownerdf %>% filter(date2 == month)
       tbm[i,] <- c(months[i],nrow(dfloop))
       
     }
     tbm <- as.data.frame(tbm)
     tbm$NoTrades <- as.numeric(tbm$NoTrades)
     
     p <- plot_ly(tbm,
                  x = ~Month,
                  y = ~NoTrades,
                  type = "bar"
     )%>%
       layout(
         title = paste(owner,"Trades by Month"),
         xaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~Month),
         yaxis = list(title = "#Trades")
       )
     p
     
   })
   

  
  
  
  output$transactions <- renderDataTable({
     
     df <- trades_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
     
     df <- df %>% filter(Franchise1 == input$owner, Date >= as.character(input$time))
     
     df
     
   }, rownames = FALSE, filter = "top", options = list(pageLength = 25))
  
  output$addstable <- renderDataTable({
    
    df <- add_drop_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    df <- df %>% filter(Franchise == input$owner, Date >= as.character(input$time))
    
    df
    
  }, rownames = FALSE, filter = "top", options = list(pageLength = 25))
  
  output$playersdropped <- renderPlotly({
    
    df <- add_drop_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    
    playersdf <- players_import(as.numeric(input$leagueid))
    player_ids <- playersdf$id
    player_names <- playersdf$name
    owner <- input$owner
    time <- input$time
    
    ownerdf <- df %>% filter(Franchise == owner, Date >= as.character(time))
    for(i in length(player_ids):1){
      ownerdf[,2] <- gsub(player_names[i], player_ids[i], ownerdf[,2], fixed = TRUE)
      ownerdf[,3] <- gsub(player_names[i], player_ids[i], ownerdf[,3], fixed = TRUE)
    }
    ownerdf[,2] <- gsub(11613, 12652, ownerdf[,2], fixed = TRUE)
    ownerdf[,3] <- gsub(11613, 12652, ownerdf[,3], fixed = TRUE)
    playersdropped <- c(ownerdf$PlayersDropped)
    playersdropped <- strsplit(playersdropped,';',fixed=TRUE)
    playersdropped <- as.data.frame(unlist(playersdropped))
    colnames(playersdropped) <- "id"
    playersdroppeddf <- merge(playersdropped,playersdf, by = "id")
    
    piecesdropped <- c(unique(as.character(playersdroppeddf$position)))
    
    npta <- matrix(ncol = 2, nrow = length(piecesdropped))
    colnames(npta) <- c("Piece","NoTrades")
    for(i in 1:nrow(npta)){
      
      loopdf <- playersdroppeddf %>% filter(position == piecesdropped[i])
      
      npta[i,] <- c(piecesdropped[i],nrow(loopdf))
      
    }
    npta <- as.data.frame(npta)
    npta$NoTrades <- as.numeric(npta$NoTrades)
    
    donut <- plot_ly(labels = ~Piece, values = ~NoTrades, data = npta) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Players Dropped by Position",  showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    donut
    
  })
  
  output$playersadded <- renderPlotly({
    
    df <- add_drop_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    
    playersdf <- players_import(as.numeric(input$leagueid))
    player_ids <- playersdf$id
    player_names <- playersdf$name
    owner <- input$owner
    time <- input$time
    
    ownerdf <- df %>% filter(Franchise == owner, Date >= as.character(time))
    for(i in length(player_ids):1){
      ownerdf[,2] <- gsub(player_names[i], player_ids[i], ownerdf[,2], fixed = TRUE)
      ownerdf[,3] <- gsub(player_names[i], player_ids[i], ownerdf[,3], fixed = TRUE)
    }
    ownerdf[,2] <- gsub(11613, 12652, ownerdf[,2], fixed = TRUE)
    ownerdf[,3] <- gsub(11613, 12652, ownerdf[,3], fixed = TRUE)
    playersadded <- c(ownerdf$PlayersAdded)
    playersadded <- strsplit(playersadded,';',fixed=TRUE)
    playersadded <- as.data.frame(unlist(playersadded))
    colnames(playersadded) <- "id"
    playersaddeddf <- merge(playersadded,playersdf, by = "id")
    
    piecesadded <- c(unique(as.character(playersaddeddf$position)))
    
    npta <- matrix(ncol = 2, nrow = length(piecesadded))
    colnames(npta) <- c("Piece","NoTrades")
    for(i in 1:nrow(npta)){
      
      loopdf <- playersaddeddf %>% filter(position == piecesadded[i])
      
      npta[i,] <- c(piecesadded[i],nrow(loopdf))
      
    }
    npta <- as.data.frame(npta)
    npta$NoTrades <- as.numeric(npta$NoTrades)
    
    donut <- plot_ly(labels = ~Piece, values = ~NoTrades, data = npta) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Players Added by Position",  showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    donut
    
  })
  
  
  output$drafttable <- renderDataTable({
    
    df <- draft_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    df <- df %>% filter(Franchise == input$owner, Date >= as.character(input$time))
    
    df
    
  }, rownames = FALSE, filter = "top", options = list(pageLength = 25))
  
  output$roundsdrafted <- renderPlotly({
    
    df <- draft_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    df <- df %>% filter(Franchise == input$owner, Date >= as.character(input$time))
    rsummary <- matrix(ncol = 2, nrow = max(df$Round))
    colnames(rsummary) <- c("Piece","NoPicks")
    for(i in 1:nrow(rsummary)){
      dfloop <- df %>% filter(Round == i)
      rsummary[i,] <- c(i, nrow(dfloop))
    }
    rsummary <- as.data.frame(rsummary)
    
    donut <- plot_ly(labels = ~Piece, values = ~NoPicks, data = rsummary) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Number of Picks by Round",  showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    donut
  })
  
  output$playersdrafted <- renderPlotly({
    
    df <- draft_table(as.numeric(input$leagueid),input$.username,input$.password,players_import(as.numeric(input$leagueid)))
    df <- df %>% filter(Franchise == input$owner, Date >= as.character(input$time))
    playersdf <- players_import(as.numeric(input$leagueid))
    player_ids <- playersdf$id
    player_names <- playersdf$name
    owner <- input$owner
    time <- input$time
    
    for(i in length(player_ids):1){
      df[,3] <- gsub(player_names[i], player_ids[i], df[,3], fixed = TRUE)
    }
    df[,3] <- gsub(11613, 12652, df[,3], fixed = TRUE)
    playersdrafted <- c(df$Player)
    playersdrafted <- strsplit(playersdrafted,';',fixed=TRUE)
    playersdrafted <- as.data.frame(unlist(playersdrafted))
    colnames(playersdrafted) <- "id"
    playersdrafteddf <- merge(playersdrafted,playersdf, by = "id")
    piecesdrafted <- c(unique(as.character(playersdrafteddf$position)))
    
    npd <- matrix(ncol = 2, nrow = length(piecesdrafted))
    colnames(npd) <- c("Piece","NoDrafted")
    for(i in 1:nrow(npd)){
      
      loopdf <- playersdrafteddf %>% filter(position == piecesdrafted[i])
      
      npd[i,] <- c(piecesdrafted[i],nrow(loopdf))
      
    }
    npd <- as.data.frame(npd)
    npd$NoTrades <- as.numeric(npd$NoDrafted)
    
    donut <- plot_ly(labels = ~Piece, values = ~NoDrafted, data = npd) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Number of Picks by Round",  showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    donut
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

