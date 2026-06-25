## Setup ----
library(dplyr)
library(stringr)
library(leaflet)
data_path <- 'review_results_shiny_250626.csv'
# updated 25.06.2026: manuscript dataset, beneficial & adverse terms, updated color palette
# updated 16.06.2026: revised dataset (benthos added, categories consolidated)
# updated 24.04.2026: final dataset, leaflet color matches improved, filtering for overrepresented papers done after other filters applied
# updated 09.04.2026: new color scheme, final results spreadsheet
# updated 12.02.2026: seaweed + seagrass, explanatory text
# updated 13.02.2026: marine mammal and commercial fish supergroups, color pallette optimized for shiny

### data ----
meta <- read.csv(data_path, header = T, sep = ';')
commercial_fish <- c("cod (Gadus morhua)", "herring (Clupea harengus)", "Flatfish", "Sprat (Sprattus sprattus)", 
                     "mackerel (Scomber scombrus)", "haddock (Melanogrammus aeglefinus)", "saithe (Pollachius virens)")
endpoints_trophic <-  c("harbour porpoise (Phocoena phocoena)", "Seals", "Seabirds", 
                        "cod (Gadus morhua)", "Atlantic mackerel (Scomber scombrus)", "Sprat (Sprattus sprattus)", 
                        "whiting (Merlangius merlangus)", "saithe (Pollachius virens)", "Flatfish", "haddock (Melanogrammus aeglefinus)", "herring (Clupea harengus)",
                        "Zooplankton", "Benthos", "Phytoplankton", "Seaweed & Seagrass")
labs <- c("Insignificant", "Mixed", "Mixed-Adverse", "Mixed-Beneficial","Adverse", "Beneficial")
meta$Commercial <- lapply(meta$Endpoint, function(x) if(x %in% commercial_fish) "TRUE" else "FALSE")
meta$Include <- lapply(meta$Notes, function(x) if(grepl('Exclude', x, fixed = TRUE)) "Exclude" else "Include")
meta_include <- meta[meta$Include=='Include',]
names(meta_include)[1] <- "PaperID"
meta_include <- meta_include[, !grepl("^X.", names(meta_include))]
meta_include <- meta_include[!(meta_include$Effect=="" | meta_include$Endpoint=="" | meta_include$Stressor=="" | meta_include$Direction==""),]
meta_exclude <- meta_include %>%
  group_by(Endpoint) %>%
  summarise(freq=n())

excluded_endpoints <- meta_exclude[meta_exclude$freq < 5,1]

# remove endpoints with < 5 records for visualization
meta_include <- meta_include[!meta_include$Endpoint %in% excluded_endpoints,]
meta_include$Lat <- as.numeric(gsub(',', '.', meta_include$Lat))
meta_include$Long <- as.numeric(gsub(',', '.', meta_include$Long))

# meta <- read.csv(data_path, header = T, sep = ';')
# names(meta)[1] <- "PaperID"
# commercial_fish <- c("cod (Gadus morhua)", "herring (Clupea harengus)", "Flatfish", "Sprat (Sprattus sprattus)",
#                      "mackerel (Scomber scombrus)", "haddock (Melanogrammus aeglefinus)", "saithe (Pollachius virens)")
marine_mammals <- c("Seals", "harbour porpoise (Phocoena phocoena)")
meta_include$Commercial <- lapply(meta_include$Endpoint, function(x) if(x %in% commercial_fish) "TRUE" else "FALSE")
meta_include$Mammals <- lapply(meta_include$Endpoint, function(x) if(x %in% marine_mammals) "TRUE" else "FALSE")
# meta$Include <- lapply(meta$Notes, function(x) if(grepl('Exclude', x, ignore.case = TRUE)) "Exclude" else "Include")
# meta_include <- meta[meta$Include=='Include',]
# meta_include <- meta_include[, !grepl("^X", names(meta_include))]
# meta_plot <- meta_include %>% group_by(Stressor, Effect, Endpoint, Commercial, Mammals, Direction) %>% summarise(freq = n())

meta_plot <- meta_include %>% group_by(Stressor, Effect, Endpoint, Direction) %>% summarise(freq = n()) %>% arrange(Stressor, desc(Direction))


pal <- hcl.colors(6, palette = 'Spectral')
names(pal) <- rev(c("Positive", "Mixed-Positive", "Insignificant",  "Mixed", "Mixed-Negative", "Negative"))

### studies ----
meta_study_plot <- meta_include %>% group_by(Stressor, Commercial, Mammals, Effect, Endpoint, Direction, PaperID) %>% summarise(freq = n())

### leaflet ----
leaf_dat <- meta_include
leaf_dat <- leaf_dat[,colSums(is.na(leaf_dat))<nrow(leaf_dat)]
leaf_dat$lng <- meta_include$Long
leaf_dat$lat <- meta_include$Lat
leaf_dat$Species <- lapply(leaf_dat$Species, str_replace, pattern="sealB", replacement="seal")
marker_colors <- c("red", "darkred", "orange", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "lightred", "pink", "cadetblue", "gray", "black")
pal_endpoint <- marker_colors
names(pal_endpoint) <- unique(leaf_dat$Endpoint)
library(fontawesome)
icons <- c("book", "desktop", "file", "flask",  "anchor", "question") # https://rstudio.github.io/fontawesome/articles/icon-reference.html
names(icons) <- c("Review", "Model", "Survey", "Lab", "Field", "Other")

leaf_dat$fill <- pal_endpoint[leaf_dat$Endpoint]
leaf_dat$DOI_link <- paste0("https://doi.org/", leaf_dat$DOI)

Encoding(x = leaf_dat$PaperID) <- "UTF-8"

# replace all non UTF-8 character strings with an empty space
leaf_dat$PaperID <-
  iconv( x = leaf_dat$PaperID
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )

Encoding(x = leaf_dat$Title) <- "UTF-8"

# replace all non UTF-8 character strings with an empty space
leaf_dat$Title <-
  iconv( x = leaf_dat$Title
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )

Encoding(x = leaf_dat$DOI_link) <- "UTF-8"

# replace all non UTF-8 character strings with an empty space
leaf_dat$DOI_link <-
  iconv( x = leaf_dat$DOI_link
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )

assign_icon <- function(x) {
  if (x %in% names(icons)) {
    as.character(icons[x])
  } else {
    as.character(icons["Other"])
  }
}
leaf_dat$icon <- lapply(leaf_dat$Approach.category, FUN = assign_icon)

Encoding(x = leaf_dat$PaperID) <- "UTF-8"

icons_plot <- awesomeIcons(
  icon = leaf_dat$icon,
  iconColor = "white",
  library = "fa",
  markerColor = leaf_dat$fill
)

## Shiny app ----
library(shiny)
library(stringr)
library(networkD3)
library(plotly)
library(ggalluvial)
library(htmltools)
library(sp)

d.comp=meta_include
d.path=meta_plot
d.study=meta_study_plot
d.map=leaf_dat
d=NULL
count="not counted yet"
# Offset, in pixels, for location of tooltip relative to mouse cursor,
# in both x and y direction.
offset <- 1
# Width of node boxes
node_width <- 1/4
# Width of alluvia
alluvium_width <- 1/3

stressor.names=c("All",unique(d.path$Stressor))
endpoint.names=c("All","Commercial fish","Marine mammals",unique(d.path$Endpoint))
mode.names=c("Pathways","Overrepresented Studies","All Studies")

ui <- fluidPage(
  titlePanel("Stressors in the Skagerrak, Kattegat, Baltic, and North Seas"),
  fluidRow(
    column(3,
           p("This app summarizes a non-exhaustive Web of Science literature review of stressor relationships acting in the Skagerrak, Kattegat, Baltic, and North Seas. For questions and comments please contact griffin.hill@niva.no."),
           em("For best results, view in a fullscreen browser window", style = "color:blue"),
           selectInput("stressor", "Stressor", stressor.names),
           selectInput("endpoint", "Endpoint", endpoint.names),
           selectInput("mode", "Plot type", mode.names),
           textOutput("count")
    ),
    column(9,
           leafletOutput("leaf", width = "80%", height = "500px"),
           em("Click points on the map for metadata", style = "color:blue")
    )
  ),
  fluidRow(
    plotOutput("plot", height = "700px", hover = hoverOpts(id = "plot_hover")),
    htmlOutput("tooltip"),
    em("Hover your mouse over nodes or pathways within the plot for additional information", style = "color:blue")
  )
)



# conditionalPanel(
#   condition = "input.mode != 'Map'",
#   plotOutput("staticPlot")
# ),
# conditionalPanel(
#   condition = "input.mode == 'Map'",
#   leafletOutput("mapPlot") # Maps need leafletOutput
# )

server <- function(input, output, session) {
  
  #get subset of data according to inputs
  get.data<-reactive(
    {
      if (input$mode == "Pathways") {
        d <- d.path
      } else if (input$mode == "Overrepresented Studies" | input$mode == "All Studies") {
        d <- d.study
      } else {
        d <- d.path
      }
      if(input$stressor!="All") {d=d[d$Stressor==input$stressor,]} else {d=d}
      if(input$endpoint!="All") {if (input$endpoint=="Commercial fish") {d=d[d$Endpoint %in% commercial_fish,]} else if (input$endpoint=="Marine mammals") {d=d[d$Endpoint %in% marine_mammals,]} else {d=d[d$Endpoint==input$endpoint,]}} else {d=d}
      if(input$mode== "Overrepresented Studies") {
        d_over_studies <- d %>% group_by(PaperID) %>% summarise(freq = n())
        d_over_studies <- d_over_studies[d_over_studies$freq > 2,]
        d <- d[d$PaperID %in% d_over_studies$PaperID,]
      }
      d
    }
  )
  get.leafdata<-reactive(
    {
      d <- d.map
      if(input$stressor!="All") {d=d[d$Stressor==input$stressor,]} else {d=d}
      # if(input$endpoint!="All") {d=d[d$Endpoint==input$endpoint,]} else {d=d}
      if(input$endpoint!="All") {if (input$endpoint=="Commercial fish") {d=d[d$Endpoint %in% commercial_fish,]} else if (input$endpoint=="Marine mammals") {d=d[d$Endpoint %in% marine_mammals,]} else {d=d[d$Endpoint==input$endpoint,]}} else {d=d}
      d <- na.omit(d)
      d
    }
  )
  #count pathways or studies
  get.count<-reactive(
    {
      h=""
      if(input$mode=="Pathways") {h="pathways"} else {h="Overrepresented Studies"}
      if(input$mode=="Overrepresented Studies") {h="Overrepresented Studies"}
      if(input$mode=="All Studies") {h="All Studies"}
      # if(input$mode=="Map") {h="Overrepresented Studies"}
      d=get.data()
      if(input$mode=="Pathways") {count=paste(nrow(d), h, "found.")} else 
      {count=paste(length(unique(d$PaperID)), h, "found.")}
      count
    }
  )
  
  #create plot object
  get.plot<-reactive(
    {
      de=get.data()
      
      if(input$mode=="Pathways")
      {
        if(input$endpoint!="All") {
          if(input$endpoint=="Commercial fish" | input$endpoint=="Marine mammals") {
            if(input$stressor != "All") {
              g <- ggplot(data = de,
                          aes(axis1 = Stressor,   # First variable on the X-axis
                              axis2 = Effect, # Second variable on the X-axis
                              axis3 = Endpoint,   # Third variable on the X-axis
                              y = freq)) +
                stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
                stat_stratum() +
                geom_text(stat = "stratum",
                          aes(label = after_stat(stratum))) +
                geom_text(stat = "stratum",
                          aes(label = ..count..), vjust = 1.6) +
                scale_fill_manual(values = pal, labels = labs) +
                theme_void() +
                labs(title = paste(input$endpoint, input$stressor, "Stressor Linkages")) +
                theme(plot.title = element_text(size = 22, face = "bold"))
              g <- g + labs(caption = "Direction of a stressor's action represents whether it it positively or negatively affects the endpoint via the given mechanism. \nFor example, a negative pathway flowing from Eutrophication to Herring via Reproduction/Recruitment signifies a paper that found eutrophication was negatively impacting reproduction and/or recruitment in herring.") + theme(plot.caption = element_text(size = 14, hjust = 0))
              return(g)
            } else {
              g <- ggplot(data = de,
                          aes(axis1 = Stressor,   # First variable on the X-axis
                              axis2 = Effect, # Second variable on the X-axis
                              axis3 = Endpoint,   # Third variable on the X-axis
                              y = freq)) +
                stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
                stat_stratum() +
                geom_text(stat = "stratum",
                          aes(label = after_stat(stratum))) +
                geom_text(stat = "stratum",
                          aes(label = ..count..), vjust = 1.6) +
                scale_fill_manual(values = pal, labels = labs) +
                theme_void() +
                # geom_alluvium(aes(fill = Direction)) +
                # geom_stratum() +
                # geom_text(stat = "stratum",
                #           aes(label = after_stat(stratum))) +
                # scale_x_discrete(limits = c("Stressor", "Effect"),
                #                  expand = c(0.15, 0.05)) +
                # theme_void() +
                labs(title = paste(input$endpoint, "Stressor Linkages")) +
                theme(plot.title = element_text(size = 22, face = "bold"))
              g <- g + labs(caption = "Direction of a stressor's action represents whether it it positively or negatively affects the endpoint via the given mechanism. \nFor example, a negative pathway flowing from Eutrophication to Herring via Reproduction/Recruitment signifies a paper that found eutrophication was negatively impacting reproduction and/or recruitment in herring.") + theme(plot.caption = element_text(size = 14, hjust = 0))
              return(g)
            }
          } else {
            if(input$stressor != "All") {
              g <- ggplot(data = de,
                          aes(axis1 = Stressor,   # First variable on the X-axis
                              axis2 = Effect, # Second variable on the X-axis
                              # axis3 = Endpoint,   # Third variable on the X-axis
                              y = freq)) +
                # geom_alluvium(aes(fill = Direction)) +
                # geom_stratum() +
                # geom_text(stat = "stratum",
                #           aes(label = after_stat(stratum))) +
                # scale_x_discrete(limits = c("Stressor", "Effect"),
                #                  expand = c(0.15, 0.05)) +
                # theme_void() +
                stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
                stat_stratum() +
                geom_text(stat = "stratum",
                          aes(label = after_stat(stratum))) +
                geom_text(stat = "stratum",
                          aes(label = ..count..), vjust = 1.6) +
                scale_fill_manual(values = pal, labels = labs) +
                theme_void() +
                labs(title = paste(input$endpoint, input$stressor, "Stressor Linkages")) +
                theme(plot.title = element_text(size = 22, face = "bold"))
              g <- g + labs(caption = "Direction of a stressor's action represents whether it it positively or negatively affects the endpoint via the given mechanism. \nFor example, a negative pathway flowing from Eutrophication to Herring via Reproduction/Recruitment signifies a paper that found eutrophication was negatively impacting reproduction and/or recruitment in herring.") + theme(plot.caption = element_text(size = 14, hjust = 0))
              return(g)
            } else {
              g <- ggplot(data = de,
                          aes(axis1 = Stressor,   # First variable on the X-axis
                              axis2 = Effect, # Second variable on the X-axis
                              # axis3 = Endpoint,   # Third variable on the X-axis
                              y = freq)) +
                stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
                stat_stratum() +
                geom_text(stat = "stratum",
                          aes(label = after_stat(stratum))) +
                geom_text(stat = "stratum",
                          aes(label = ..count..), vjust = 1.6) +
                scale_fill_manual(values = pal, labels = labs) +
                # geom_alluvium(aes(fill = Direction)) +
                # geom_stratum() +
                # geom_text(stat = "stratum",
                #           aes(label = after_stat(stratum))) +
                # scale_x_discrete(limits = c("Stressor", "Effect"),
                #                  expand = c(0.15, 0.05)) +
                theme_void() +
                labs(title = paste(input$endpoint, "Stressor Linkages")) +
                theme(plot.title = element_text(size = 22, face = "bold"))
              g <- g + labs(caption = "Direction of a stressor's action represents whether it it positively or negatively affects the endpoint via the given mechanism. \nFor example, a negative pathway flowing from Eutrophication to Herring via Reproduction/Recruitment signifies a paper that found eutrophication was negatively impacting reproduction and/or recruitment in herring.") + theme(plot.caption = element_text(size = 14, hjust = 0))
              return(g)
            }
          }

        } else {
          if(input$stressor!="All") {
            g <- ggplot(data = de,
                        aes(axis1 = Stressor,   # First variable on the X-axis
                            axis2 = Effect, # Second variable on the X-axis
                            axis3 = Endpoint,   # Third variable on the X-axis
                            y = freq)) +
              stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
              stat_stratum() +
              geom_text(stat = "stratum",
                        aes(label = after_stat(stratum))) +
              geom_text(stat = "stratum",
                        aes(label = ..count..), vjust = 1.6) +
              scale_fill_manual(values = pal, labels = labs) +
              # 
              # geom_alluvium(aes(fill = Direction)) +
              # geom_stratum() +
              # geom_text(stat = "stratum",
              #           aes(label = after_stat(stratum))) +
              # scale_x_discrete(limits = c("Stressor", "Endpoint"),
              #                  expand = c(0.15, 0.05)) +
              theme_void() +
              labs(title = paste("All Species", input$stressor,"Stressor Linkages")) +
              theme(plot.title = element_text(size = 22, face = "bold"))
            g <- g + labs(caption = "Direction of a stressor's action represents whether it it positively or negatively affects the endpoint via the given mechanism. \nFor example, a negative pathway flowing from Eutrophication to Herring via Reproduction/Recruitment signifies a paper that found eutrophication was negatively impacting reproduction and/or recruitment in herring.") + theme(plot.caption = element_text(size = 14, hjust = 0))
            return(g)
          } else {
            g <- ggplot(data = de,
                        aes(axis1 = Stressor,   # First variable on the X-axis
                            axis2 = Effect, # Second variable on the X-axis
                            axis3 = Endpoint,   # Third variable on the X-axis
                            y = freq)) +
              stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
              stat_stratum() +
              geom_text(stat = "stratum",
                        aes(label = after_stat(stratum))) +
              geom_text(stat = "stratum",
                        aes(label = ..count..), vjust = 1.6) +
              scale_fill_manual(values = pal, labels = labs) +
              # 
              # geom_alluvium(aes(fill = Direction)) +
              # geom_stratum() +
              # geom_text(stat = "stratum",
              #           aes(label = after_stat(stratum))) +
              # scale_x_discrete(limits = c("Stressor", "Endpoint"),
              #                  expand = c(0.15, 0.05)) +
              theme_void() +
              labs(title = paste("All Stressor Linkages")) +
              theme(plot.title = element_text(size = 22, face = "bold"))
            g <- g + labs(caption = "Direction of a stressor's action represents whether it it positively or negatively affects the endpoint via the given mechanism. \nFor example, a negative pathway flowing from Eutrophication to Herring via Reproduction/Recruitment signifies a paper that found eutrophication was negatively impacting reproduction and/or recruitment in herring.") + theme(plot.caption = element_text(size = 14, hjust = 0))
            return(g)
          }

        }
        
      }
      if(input$mode=="All Studies") 
      {
        # plot based on individual papers
        g <- ggplot(data = de,
                    aes(axis1 = Stressor,   # First variable on the X-axis
                        axis2 = Effect, # Second variable on the X-axis
                        axis3 = Endpoint,   # Third variable on the X-axis
                        axis4 = PaperID,
                        y = freq)) +
          stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
          stat_stratum() +
          geom_text(stat = "stratum",
                    aes(label = after_stat(stratum))) +
          geom_text(stat = "stratum",
                    aes(label = ..count..), vjust = 1.6) +
          scale_fill_manual(values = pal, labels = labs) +
          # 
          # geom_alluvium(aes(fill = Direction)) +
          # geom_stratum() +
          # geom_text(stat = "stratum",
          #           aes(label = after_stat(stratum))) +
          # scale_x_discrete(limits = c("Stressor", "PaperID"),
          #                  expand = c(0.15, 0.05)) +
          theme_void()
        return(g)
        
      }
      
      if(input$mode=="Overrepresented Studies") 
      {
        # plot based on individual papers
        g <- ggplot(data = de,
                    aes(axis1 = Stressor,   # First variable on the X-axis
                        axis2 = Effect, # Second variable on the X-axis
                        axis3 = Endpoint,   # Third variable on the X-axis
                        axis4 = PaperID,
                        y = freq)) +
          stat_alluvium(aes(fill = Direction), ,aes.bind = 'alluvia', lode.guidance = 'frontback') +
          stat_stratum() +
          geom_text(stat = "stratum",
                    aes(label = after_stat(stratum))) +
          geom_text(stat = "stratum",
                    aes(label = ..count..), vjust = 1.6) +
          scale_fill_manual(values = pal, labels = labs) +
          # 
          # geom_alluvium(aes(fill = Direction)) +
          # geom_stratum() +
          # geom_text(stat = "stratum",
          #           aes(label = after_stat(stratum))) +
          # scale_x_discrete(limits = c("Stressor", "PaperID"),
          #                  expand = c(0.15, 0.05)) +
          theme_void()
        g <- g + labs(caption = "Studies occuring more than two times in the entire body of reviewed literature are considered overrepresented relative to the rest.") + theme(plot.caption = element_text(size = 14, hjust = 0))
        return(g)
        
      } 
    }
  )
  get.leaf<-reactive(
    {
      dm <- get.leafdata()
      # g <- leaflet(dm) %>%
      #   addTiles() %>%
      #   setView(lng = 6, lat = 55, zoom = 5) %>%
      #   addAwesomeMarkers(~lng, ~lat, icon = icons, label = ~as.character(Endpoint), popup = ~paste0("Article: ", PaperID, "<br>",
      #                                                                                                "Species: ", Species, "<br>",
      #                                                                                                "Stressor: ", Stressor, "<br>",
      #                                                                                                "Effect: ", Effect, " (", Direction, ")","<br>",
      #                                                                                                "Study type: ", Approach.category, "<br>",
      #                                                                                                "<a href=\"", DOI_link , "\">", Title, "</a>")) %>%
      #   addLegend(colors = pal_endpoint, labels = names(pal_endpoint), position = "bottomright")
      # 
      icons_plot <- awesomeIcons(
        icon = dm$icon,
        iconColor = "lightgrey",
        library = "fa",
        markerColor = dm$fill
      )
      pal <- pal_endpoint[unique(dm$Endpoint)]
      g <- leaflet(dm) %>%
        addTiles() %>%
        setView(lng = 5, lat = 55, zoom = 5) %>%
        addAwesomeMarkers(~lng, ~lat, icon = icons_plot, label = ~as.character(Endpoint), popup = ~paste0("Article: ", PaperID, "<br>",
                                                                                                          "Stressor: ", Stressor, "<br>",
                                                                                                          "Effect: ", Effect, " (", Direction, ")","<br>",
                                                                                                          "Study type: ", Approach.category, "<br>",
                                                                                                          "<a href=\"", DOI_link , "\">", 
                                                                                                          Title, "</a>")) %>%
        addLegend(colors = pal, labels = names(pal), position = "bottomright")
      
      return(g)
    }
  )
  get.tooltip <- reactive(
    {
      de=get.data()
      
      if(input$mode=="Pathways")
      {
        if(input$endpoint!="All" & input$endpoint!="Commercial fish" & input$endpoint!="Marine mammals") {
          g <- ggplot(data = de,
                      aes(axis1 = Stressor,   # First variable on the X-axis
                          axis2 = Effect, # Second variable on the X-axis
                          # axis3 = Endpoint,   # Third variable on the X-axis
                          y = freq)) +
            geom_alluvium(aes(fill = Direction)) +
            geom_stratum() +
            geom_text(stat = "stratum",
                      aes(label = after_stat(stratum))) +
            scale_x_discrete(limits = c("Stressor", "Effect"),
                             expand = c(0.15, 0.05)) +
            theme_void() +
            labs(title = paste(input$endpoint, "Stressor Linkages")) +
            theme(plot.title = element_text(size = 22, face = "bold"))
        } else {
          g <- ggplot(data = de,
                      aes(axis1 = Stressor,   # First variable on the X-axis
                          axis2 = Effect, # Second variable on the X-axis
                          axis3 = Endpoint,   # Third variable on the X-axis
                          y = freq)) +
            geom_alluvium(aes(fill = Direction)) +
            geom_stratum() +
            geom_text(stat = "stratum",
                      aes(label = after_stat(stratum))) +
            scale_x_discrete(limits = c("Stressor", "Endpoint"),
                             expand = c(0.15, 0.05)) +
            theme_void() +
            labs(title = paste("All Stressor Linkages")) +
            theme(plot.title = element_text(size = 22, face = "bold"))
        }
        
      } else if(input$mode=="All Studies") 
      {
        # plot based on individual papers
        g <- ggplot(data = de,
                    aes(axis1 = Stressor,   # First variable on the X-axis
                        axis2 = Effect, # Second variable on the X-axis
                        axis3 = Endpoint,   # Third variable on the X-axis
                        axis4 = PaperID,
                        y = freq)) +
          geom_alluvium(aes(fill = Direction)) +
          geom_stratum() +
          geom_text(stat = "stratum",
                    aes(label = after_stat(stratum))) +
          scale_x_discrete(limits = c("Stressor", "PaperID"),
                           expand = c(0.15, 0.05)) +
          theme_void()
        
      } else if(input$mode=="Overrepresented Studies") 
      {
        # plot based on individual papers
        g <- ggplot(data = de,
                    aes(axis1 = Stressor,   # First variable on the X-axis
                        axis2 = Effect, # Second variable on the X-axis
                        axis3 = Endpoint,   # Third variable on the X-axis
                        axis4 = PaperID,
                        y = freq)) +
          geom_alluvium(aes(fill = Direction)) +
          geom_stratum() +
          geom_text(stat = "stratum",
                    aes(label = after_stat(stratum))) +
          scale_x_discrete(limits = c("Stressor", "PaperID"),
                           expand = c(0.15, 0.05)) +
          theme_void()
        
      }
      # g <- get.plot()
      pbuilt <- ggplot_build(g)
      
      return(pbuilt)
    }
  )
  # plot
  output$count <- renderText(get.count())
  output$plot <- renderPlot(get.plot())
  output$leaf <- renderLeaflet(get.leaf())
  output$tooltip <- renderText(
    if(!is.null(input$plot_hover)) {
      hover <- input$plot_hover
      x_coord <- round(hover$x)
      pbuilt <- get.tooltip()
      #### get polygon data ----
      # Add width parameter, and then convert built plot data to xsplines
      data_draw <- transform(pbuilt$data[[1]], width = alluvium_width)
      groups_to_draw <- split(data_draw, data_draw$group)
      group_xsplines <- lapply(groups_to_draw,
                               data_to_alluvium) 
      
      # Convert xspline coordinates to grid object.
      xspline_coords <- lapply(
        group_xsplines,
        function(coords) grid::xsplineGrob(x = coords$x, 
                                           y = coords$y, 
                                           shape = coords$shape, 
                                           open = FALSE)
      )
      # Use grid::xsplinePoints to draw the curve for each polygon
      xspline_points <- lapply(xspline_coords, grid::xsplinePoints)
      # Define the x and y axis limits in grid coordinates (old) and plot
      # coordinates (new)
      xrange_old <- range(unlist(lapply(
        xspline_points,
        function(pts) as.numeric(pts$x)
      )))
      yrange_old <- range(unlist(lapply(
        xspline_points,
        function(pts) as.numeric(pts$y)
      )))
      xrange_new <- c(1 - alluvium_width/2, max(pbuilt$data[[1]]$x) + alluvium_width/2) 
      yrange_new <- c(0, sum(pbuilt$data[[2]]$count[pbuilt$data[[2]]$x == 1]))
      # Define function to convert grid graphics coordinates to data coordinates
      new_range_transform <- function(x_old, range_old, range_new) {
        (x_old - range_old[1])/(range_old[2] - range_old[1]) *
          (range_new[2] - range_new[1]) + range_new[1]
      }
      
      # Using the x and y limits, convert the grid coordinates into plot coordinates.
      polygon_coords <- lapply(xspline_points, function(pts) {
        x_trans <- new_range_transform(x_old = as.numeric(pts$x), 
                                       range_old = xrange_old, 
                                       range_new = xrange_new)
        y_trans <- new_range_transform(x_old = as.numeric(pts$y), 
                                       range_old = yrange_old, 
                                       range_new = yrange_new)
        list(x = x_trans, y = y_trans)
      })
      # if true, in a stratum
      #### convert hover data to tooltip ----
      if(abs(hover$x - x_coord) < (node_width / 2)) {
        node_row <- pbuilt$data[[2]]$x == x_coord & hover$y > pbuilt$data[[2]]$ymin & hover$y < pbuilt$data[[2]]$ymax
        node_label <- pbuilt$data[[2]]$stratum[node_row]
        node_n <- pbuilt$data[[2]]$count[node_row]
        renderTags(
          tags$div(
            node_label, tags$br(),
            "n =", node_n,
            style = paste0(
              "position: absolute; ",
              "top: ", hover$coords_css$y + offset + 450, "px; ",
              "left: ", hover$coords_css$x + offset, "px; ",
              "background: gray; ",
              "padding: 1px; ",
              "color: white; "
            )
          )
        )$html
      } else { # else it is in an alluvium
        hover_within_flow <- sapply(
          polygon_coords,
          function(pol) point.in.polygon(point.x = hover$x, 
                                         point.y = hover$y, 
                                         pol.x = pol$x, 
                                         pol.y = pol$y)
        )
        if (any(hover_within_flow)) {
          coord_id <- rev(which(hover_within_flow == 1))[1]
          flow_label <- paste(groups_to_draw[[coord_id]]$stratum, collapse = ' -> ')
          flow_n <- groups_to_draw[[coord_id]]$count[1]
          renderTags(
            tags$div(
              flow_label, tags$br(),
              "n =", flow_n,
              style = paste0(
                "position: absolute; ",
                "top: ", hover$coords_css$y + offset + 450, "px; ",
                "left: ", hover$coords_css$x + offset, "px; ",
                "background: gray; ",
                "padding: 3px; ",
                "color: white; "
              )
            )
          )$html
        }
      }
    }
    # return(NULL)
  )
}

shinyApp(ui, server)