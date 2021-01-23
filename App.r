library(shiny)
library(plotly)
library(reshape2)
library(foreign)
library(tibble)
library(markdown)
library(tidyverse)
library(stringr)

# Load data

data <- read.csv("./bes.csv")

# UI

ui <- fluidPage(
  titlePanel("Visualizing regression"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "z",
        label = "Dependent variable",
        choices = names(data),
        selected = "political_trust",
        multiple = FALSE
      ),
      selectInput(
        inputId = "x",
        label = "Independent variable 1",
        choices = names(data),
        selected = "age",
        multiple = FALSE
      ),
      selectInput(
        inputId = "y",
        label = "Independent variable 2",
        choices = c("none", names(data)),
        selected = "ideology",
        multiple = FALSE
      ),
      checkboxInput(
        inputId = "interaction",
        label = "Interaction",
        value = FALSE
      )
    ),

    mainPanel(
      tableOutput(outputId = "table"),

      plotlyOutput(outputId = "p", height = "500px"),
    )
  ),

  tabsetPanel(
    tabPanel("Instructions", includeMarkdown("instruction.md")),
    tabPanel("About", includeMarkdown("about.md"))
  )
)

# Server

server <- function(input, output, ...) {

  # This shows a regression table
  output$table <- renderTable({
    if (input$y == "none") {
      lm <- lm(as.formula(paste0(input$z, " ~ ", input$x)), data)

      summary(lm)$coefficients %>%
        as.data.frame() %>%
        rename(
          "Coefficient" = `Estimate`,
          "SE" = `Std. Error`,
          "t-value" = `t value`,
          "p" = `Pr(>|t|)`
        ) %>%
        rownames_to_column() %>%
        mutate(rowname = gsub("_", " ",.$rowname)) %>%
        mutate(rowname = gsub("\\(|\\)", "",.$rowname)) %>%
        mutate(rowname = str_to_title(rowname)) %>%
        rename(" " = rowname) %>%
        mutate(across(c(Coefficient, SE, `t-value`), round, 3)) %>%
        mutate(p = format(round(p, 3), nsmall = 3))
    } else {
      if (input$interaction == FALSE) {
        lm <- lm(as.formula(paste0(input$z, " ~ ", input$x, " + ", input$y)), data)
      } else {
        lm <- lm(as.formula(paste0(input$z, " ~ ", input$x, "*", input$y)), data)
      }

      summary(lm)$coefficients %>%
        as.data.frame() %>%
        rename(
          "Coefficient" = `Estimate`,
          "SE" = `Std. Error`,
          "t-value" = `t value`,
          "p" = `Pr(>|t|)`
        ) %>%
        rownames_to_column() %>%
        mutate(rowname = gsub("_", " ",.$rowname)) %>%
        mutate(rowname = gsub("\\(|\\)", "",.$rowname)) %>%
        mutate(rowname = str_to_title(rowname)) %>%
        rename(" " = rowname) %>%
        mutate(across(c(Coefficient, SE, `t-value`), round, 3)) %>%
        mutate(p = format(round(p, 3), nsmall = 3))
    }
  })

  # This shows a plot
  output$p <- renderPlotly({

    # This helps distinguishing binary from continuous variables
    if (length(table(data[, input$x])) == 2) {
      jitter_x <- 0.7
      tickmode_x <- "array"
      tickvals_x <- c(1, 2)
      ticktext_x <- c("No", "Yes")
    } else if (length(table(data[, input$x])) == 4) {
      jitter_x <- 2.5
      tickmode_x <- "array"
      tickvals_x <- c(0, 1, 2, 3)
      ticktext_x <- c(0, 1, 2, 3)
    } else {
      jitter_x <- 2.5
      tickmode_x <- "auto"
      tickvals_x <- NULL
      ticktext_x <- NULL
    }
    if (input$y != "none") {
      if (length(table(data[, input$y])) == 2) {
        jitter_y <- 0.7
        tickmode_y <- "array"
        tickvals_y <- c(1, 2)
        ticktext_y <- c("No", "Yes")
      } else if (length(table(data[, input$y])) == 4) {
        jitter_y <- 2.5
        tickmode_y <- "array"
        tickvals_y <- c(0, 1, 2, 3)
        ticktext_y <- c(0, 1, 2, 3)
      } else {
        jitter_y <- 2.5
        tickmode_y <- "auto"
        tickvals_y <- NULL
        ticktext_y <- NULL
      }
    }
    if (length(table(data[, input$z])) == 2) {
      jitter_z <- 0.7
      tickmode_z <- "array"
      tickvals_z <- c(1, 2)
      ticktext_z <- c("No", "Yes")
    } else if (length(table(data[, input$z])) == 4) {
      jitter_z <- 2.5
      tickmode_z <- "array"
      tickvals_z <- c(0, 1, 2, 3)
      ticktext_z <- c(0, 1, 2, 3)
    } else {
      jitter_z <- 2.5
      tickmode_z <- "auto"
      tickvals_z <- NULL
      ticktext_z <- NULL
    }

    if (input$y == "none") {
      data %>%
        filter(!is.na(get(input$z))) %>%
        filter(!is.na(get(input$x))) %>%
        plot_ly() %>%
        add_markers(
          y = ~ jitter(get(input$z), factor = jitter_z),
          x = ~ jitter(get(input$x), factor = jitter_x),
          marker = list(size = 4, color = "#787878", opacity = 0.5),
          showlegend = FALSE
        ) %>%
        add_lines(
          y = ~ fitted(lm(get(input$z) ~ get(input$x))),
          x = ~ get(input$x), line = list(size = 3, color = "#444444"),
          showlegend = TRUE
        ) %>%
        layout(
          xaxis = list(
            title = str_to_title(gsub("_", " ", input$x)),
            tickmode = tickmode_x,
            # Next two only take effect if "tickmode" set to "array"
            tickvals = tickvals_x,
            ticktext = ticktext_x
          ),
          yaxis = list(
            title = str_to_title(gsub("_", " ", input$z)),
            tickmode = tickmode_z,
            # Next two only take effect if "tickmode" set to "array"
            tickvals = tickvals_z,
            ticktext = ticktext_z
          )
        )
    } else {
      if (input$interaction == FALSE) {
        lm <- lm(as.formula(paste0(input$z, " ~ ", input$x, " + ", input$y)), data)
      } else {
        lm <- lm(as.formula(paste0(input$z, " ~ ", input$x, "*", input$y)), data)
      }

      axis_x <- seq(min(data[, input$x], na.rm = T),
        max(data[, input$x], na.rm = T),
        by = 0.2
      )
      axis_y <- seq(min(data[, input$y], na.rm = T),
        max(data[, input$y], na.rm = T),
        by = 0.2
      )

      lm_surface <- expand.grid(x = axis_x, y = axis_y, KEEP.OUT.ATTRS = F)
      colnames(lm_surface) <- c(input$x, input$y)
      lm_surface[, input$z] <- predict.lm(lm, newdata = lm_surface)
      lm_surface <- acast(lm_surface, as.formula(paste0(input$y, " ~ ", input$x)),
        value.var = input$z
      )

      data %>%
        filter(!is.na(get(input$z))) %>%
        filter(!is.na(get(input$x))) %>%
        filter(!is.na(get(input$y))) %>%
        plot_ly(.,
          x = ~ jitter(get(input$x), factor = jitter_x),
          y = ~ jitter(get(input$y), factor = jitter_y),
          z = ~ jitter(get(input$z), factor = jitter_z),
          type = "scatter3d", mode = "markers",
          marker = list(size = 2, color = "#787878", opacity = 0.5)
        ) %>%
        add_trace(.,
          z = lm_surface,
          x = axis_x,
          y = axis_y,
          type = "surface",
          showscale = FALSE,
          colorscale = list(c(0,1), c("#bcd8f9","#0c5bb8"))
        ) %>%
        layout(scene = list(
          xaxis = list(
            title = str_to_title(gsub("_", " ", input$x)),
            tickmode = tickmode_x,
            tickvals = tickvals_x,
            ticktext = ticktext_x
          ),
          yaxis = list(
            title = str_to_title(gsub("_", " ", input$y)),
            tickmode = tickmode_y,
            tickvals = tickvals_y,
            ticktext = ticktext_y
          ),
          zaxis = list(
            title = str_to_title(gsub("_", " ", input$z)),
            tickmode = tickmode_z,
            tickvals = tickvals_z,
            ticktext = ticktext_z
          )
        ))
    }
  })
}

shinyApp(ui, server)
