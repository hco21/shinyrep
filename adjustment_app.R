library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)


SAMS <- read_csv("C:/Users/holej/Documents/danl-310-classnotes/SAM_dataset.csv")


SAMS <- SAMS |>
  mutate(Season = case_when(
    Athlete_Team == "Basketball" ~ "Winter",
    Athlete_Team == "Equestrian" ~ "Spring",
    Athlete_Team == "Field Hockey" ~ "Fall",
    Athlete_Team == "Golf" ~ "Fall",
    Athlete_Team == "Ice Hockey" ~ "Winter",
    Athlete_Team == "Lacrosse" ~ "Spring", 
    Athlete_Team == "Soccer" ~ "Fall",
    Athlete_Team == "Softball" ~ "Spring",
    Athlete_Team == "Swimming and Diving" ~ "Winter", 
    Athlete_Team == "Tennis" ~ "Fall",
    Athlete_Team == "Track/Cross Country" ~ "All Seasons",
    Athlete_Team == "Volleyball" ~ "Fall"
  ))

SAMS <- SAMS |>
  rowwise() |>
  mutate(Overall_Adjustment = mean(c_across(c(
    Emotional_1:Emotional_4,
    Academic_1:Academic_4,
    Athletic_1:Athletic_4,
    Social_1:Social_4
  )), na.rm = TRUE)) |>
  ungroup()



emotional_long <- SAMS |>
  select(Adjusted_ID, Athlete_Team, Season, starts_with("emotional_")) |>
  pivot_longer(
    cols = starts_with("emotional_"),
    names_to = "Week",
    values_to = "Emotional_Adjustment"
  ) |>
  mutate(Week = parse_number(Week),
         Type = "Emotional") |>
  rename(Score = Emotional_Adjustment) |>
  mutate(Type = "Emotional")

athletic_long <- SAMS |>
  select(Adjusted_ID, Athlete_Team, Season, starts_with("athletic_")) |>
  pivot_longer(
    cols = starts_with("athletic_"),
    names_to = "Week",
    values_to = "Athletic_Adjustment"
  ) |>
  mutate(Week = parse_number(Week),
         Type = "Athletic")  |>
  rename(Score = Athletic_Adjustment) |>
  mutate(Type = "Athletic")

academic_long <- SAMS |>
  select(Adjusted_ID, Athlete_Team, Season, starts_with("academic_")) |>
  pivot_longer(
    cols = starts_with("academic_"),
    names_to = "Week",
    values_to = "Academic_Adjustment"
  ) |>
  mutate(Week = parse_number(Week),
         Type = "Academic") |>
  rename(Score = Academic_Adjustment) |>
  mutate(Type = "Academic")

social_long <- SAMS |>
  select(Adjusted_ID, Athlete_Team, Season, starts_with("social_")) |>
  pivot_longer(
    cols = starts_with("social_"),
    names_to = "Week",
    values_to = "Social_Adjustment"
  ) |>
  mutate(Week = parse_number(Week),
         Type = "Social") |>
  rename(Score = Social_Adjustment) |>
  mutate(Type = "Social")

adjustment_long <- bind_rows(
  emotional_long,
  academic_long, 
  athletic_long, 
  social_long
)



ui <- fluidPage(
  titlePanel("Student-Athlete Adjustment Over Time by Season"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select Season:",
                  choices = c("Fall", "Winter", "Spring", "All Seasons")),
      selectInput("type", "Select Adjustment Type:",
                  choices = c("Emotional", "Academic", "Athletic", "Social"))
    ),
    
    mainPanel(
      plotlyOutput("adjustmentPlot")
    )
  )
)

server <- function(input, output) {
  
  output$adjustmentPlot <- renderPlotly({
    plot_data <- adjustment_long |>
      filter(Season == input$season, 
             Type == input$type) |>
      group_by(Week) |>
      summarise(Average_Score = mean(Score, na.rm = TRUE), 
                .groups = "drop")
  
    p <- ggplot(plot_data, 
                aes(x = Week, 
                    y = Average_Score)) +
      geom_line(color = "steelblue", 
                size = 1.2) +
      geom_point(color = "darkblue", 
                 size = 2) +
      labs(
        title = paste(input$type, "Adjustment Over Time -", 
                      input$season, "Athletes"),
        x = "Target Week", 
        y = "Average Adjustment Score"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", 
                                  hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
