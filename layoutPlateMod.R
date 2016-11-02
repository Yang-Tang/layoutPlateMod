library(magrittr)
library(dplyr)
library(tidyr)
library(highcharter)



loplate_formatData <- function(df, nrow, ncol) {

  # df: Well(chr), Value(num), Color_by(chr), Label(chr)

  stopifnot('Well' %in% names(df))
  stopifnot('Value' %in% names(df))
  stopifnot('Color_by' %in% names(df))
  stopifnot('Label' %in% names(df))

  df %<>%
    mutate(Well = as.character(Well),
           Value = as.numeric(Value),
           Color_by = as.character(Color_by),
           Label = as.character(Label)) %>%
    replace_na(list(Label = '?'))

  row_names <- LETTERS[1:nrow]
  col_names <- as.character(1:ncol)
  full_plate <- expand.grid(Row = row_names, Col = col_names,
                            stringsAsFactors = F) %>%
    unite(Well, Row, Col, sep = '', remove = F)

  full_plate %>%
    inner_join(df, by = 'Well') %>%
    mutate(Row = factor(Row, levels = row_names),
           Col = factor(Col, levels = col_names),
           x = Col %>% as.integer() %>% subtract(1),
           y = Row %>% as.integer() %>% subtract(1),
           z = Value,
           Color_by = factor(Color_by, levels = unique(df$Color_by))) %>%
    rename(name = Label) %>%
    select(x, y, z, name, Color_by, Well, Row, Col)
}

loplate_buildPlot <- function(data) {

  assign_colors <- function(x) {
    stopifnot(is.factor(x))

    colors <- c('#7cb5ec', '#90ed7d', '#f7a35c', '#8085e9',
                '#f15c80', '#e4d354', '#2b908f', '#f45b5b', '#91e8e1')
    x %>%
      as.integer() %>%
      {(. - 1) %% length(colors) + 1} %>%
      colors[.] %>%
      inset(is.na(.), '#434348')
  }

  tooltip_format <-"
    function(){
      return '<span style=\"color: ' +
        this.color +
        '\">\u25CF</span> ' +
        this.series.name +
        '<br/>' +
        this.Well +
        ': <b>' +
        (Math.round(this.z * 100) / 100) +
        '</b><br/>';
    }
  "
  rows <- data$Row %>% levels()
  cols <- data$Col %>% levels()

  lst <- data %>%
    group_by(Color_by) %>%
    do(data = list_parse(.[, c('x', 'y', 'z', 'name', 'Well')])) %>%
    ungroup() %>%
    rename(name = Color_by) %>%
    arrange(name) %>%
    mutate(color = assign_colors(name),
           name = name %>% as.character() %>% inset(is.na(.), 'N.A.'),
           type = 'bubble') %>%
    list_parse()

  highchart() %>%
    hc_chart(marginTop = 70, zoomType = 'xy', animation = F,
             events = list(
               selection = JS(
                 "function(event){",
                 "  event.preventDefault();",
                 "  $.each(this.series, function(i, series){",
                 "    $.each(series.points, function(j, point){",
                 "      if(point.x >= event.xAxis[0].min &&",
                 "         point.x <= event.xAxis[0].max &&",
                 "         point.y >= event.yAxis[0].min &&",
                 "         point.y <= event.yAxis[0].max){",
                 "        point.select(true, true);",
                 "      } else {",
                 "        point.select(false, true);",
                 "      };",
                 "    });",
                 "  });",
                 "  var selectedPoints = this.getSelectedPoints();",
                 "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                 "  var hc_id = this.container.parentNode.id",
                 "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                 "  return false;",
                 "}"
               )
             )) %>%
    hc_subtitle(align = 'left', verticalAlign = 'bottom',
                text = 'Select wells by mouse dragging or shift+click') %>%
    hc_xAxis(min = 0, max = length(cols) - 1, categories = cols,
             title = list(), opposite = T,
             tickLength = 0, gridLineWidth = 1) %>%
    hc_yAxis(min = 0, max = length(rows) - 1, categories = rows,
             title = list(), reversed = T,
             tickLength = 0, gridLineWidth = 1) %>%
    hc_legend(align = 'right', layout = 'vertical', verticalAlign = 'middle') %>%
    hc_plotOptions(bubble = list(
      animation = F,
      maxSize = paste0(round(90/length(rows), 0), '%'),
      # minSize = paste0(round(22/length(rows), 0), '%'),
      minSize = 10,
      dataLabels = list(enabled = T, format = '{point.name}',
                        style = list(color = 'black', fontSize = '10px')),
      tooltip = list(headerFormat = '', pointFormatter = JS(tooltip_format)),
      marker = list(states = list(select = list(fillColor = NULL, lineColor = 'yellow'))),
      events = list(
        click = JS(
          "function(event){",
          "  var chart=event.point.series.chart;",
          "  if (event.shiftKey){",
          "    var related_points = chart.getSelectedPoints();",
          "    related_points.push(event.point);",
          "    var all_x = related_points.map(function(p) {return p.x;});",
          "    var all_y = related_points.map(function(p) {return p.y;});",
          "    var x_min = Math.min.apply(null, all_x);",
          "    var x_max = Math.max.apply(null, all_x);",
          "    var y_min = Math.min.apply(null, all_y);",
          "    var y_max = Math.max.apply(null, all_y);",
          "    $.each(chart.series, function (i, s){",
          "      $.each(s.data, function(j, p){",
          "        if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
          "          p.select(true, true);",
          "        } else {",
          "          p.select(false, true);",
          "        }",
          "       });",
          "    });",
          "  } else {",
          "    event.point.select(null,false);",
          "  }",
          "  var selectedPoints = chart.getSelectedPoints();",
          "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
          "  var hc_id = chart.container.parentNode.id",
          "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
          "}"
        )
      )
    )) %>%
    hc_add_series_list(lst) %>%
    hc_exporting(enabled = T, sourceWidth = 800, sourceHeight = 500,
                 buttons = list(
                   contextButton = list(enabled = T, align = 'left',
                                        text = 'Download', symbol = ''),
                   extendButton = list(
                     align = 'left',
                     text = 'Extend_selection',
                     menuItems = list(
                       list(
                         text = 'Up',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(y_min === this.yAxis[0].min) {return;};",
                           "  y_min = y_min - 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      };",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       ),
                       list(
                         text = 'Down',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(y_max === this.yAxis[0].max) {return;};",
                           "  y_max = y_max + 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      };",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       ),
                       list(
                         text = 'Left',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(x_min === this.xAxis[0].min) {return;};",
                           "  x_min = x_min - 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      };",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       ),
                       list(
                         text = 'Right',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(x_max === this.xAxis[0].max) {return;};",
                           "  x_max = x_max + 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      };",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       )
                     )
                   ),
                   moveButton = list(
                     align = 'left',
                     text = 'Move_selection',
                     menuItems = list(
                       list(
                         text = 'Up',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(y_min === this.yAxis[0].min) {return;};",
                           "  y_min = y_min - 1;",
                           "  y_max = y_max - 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      } else {",
                           "        p.select(false, true);",
                           "      }",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       ),
                       list(
                         text = 'Down',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(y_max === this.yAxis[0].max) {return;};",
                           "  y_min = y_min + 1;",
                           "  y_max = y_max + 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      } else {",
                           "        p.select(false, true);",
                           "      }",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       ),
                       list(
                         text = 'Left',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(x_min === this.xAxis[0].min) {return;};",
                           "  x_min = x_min - 1;",
                           "  x_max = x_max - 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      } else {",
                           "        p.select(false, true);",
                           "      }",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       ),
                       list(
                         text = 'Right',
                         onclick = JS(
                           "function(){",
                           "  var selected_points = this.getSelectedPoints();",
                           "  var all_x = selected_points.map(function(p) {return p.x;});",
                           "  var all_y = selected_points.map(function(p) {return p.y;});",
                           "  var x_min = Math.min.apply(null, all_x);",
                           "  var x_max = Math.max.apply(null, all_x);",
                           "  var y_min = Math.min.apply(null, all_y);",
                           "  var y_max = Math.max.apply(null, all_y);",
                           "  if(x_max === this.xAxis[0].max) {return;};",
                           "  x_min = x_min + 1;",
                           "  x_max = x_max + 1;",
                           "  $.each(this.series, function (i, s){",
                           "    $.each(s.data, function(j, p){",
                           "      if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {",
                           "        p.select(true, true);",
                           "      } else {",
                           "        p.select(false, true);",
                           "      }",
                           "    });",
                           "  });",
                           "  var selectedPoints = this.getSelectedPoints();",
                           "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                           "  var hc_id = this.container.parentNode.id",
                           "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                           "}"
                         )
                       )
                     )
                   ),
                   unSelectAllButton = list(
                     align = 'left',
                     text = 'Unselect_all',
                     onclick = JS(
                       "function(){",
                       "  $.each(this.series, function(i, series){",
                       "    $.each(series.points, function(j, point){",
                       "        point.select(false, true);",
                       "    });",
                       "  });",
                       "  var hc_id = this.container.parentNode.id",
                       "  Shiny.onInputChange(hc_id + '_selected', null);",
                       "}"
                     )
                   ),
                   selectAllButton = list(
                     align = 'left',
                     text = 'Select_all',
                     onclick = JS(
                       "function(){",
                       "  $.each(this.series, function(i, series){",
                       "    $.each(series.points, function(j, point){",
                       "        point.select(true, true);",
                       "    });",
                       "  });",
                       "  var selectedPoints = this.getSelectedPoints();",
                       "  var selectedWells = selectedPoints.map(function(a) {return a.Well;});",
                       "  var hc_id = this.container.parentNode.id",
                       "  Shiny.onInputChange(hc_id + '_selected', selectedWells);",
                       "}"
                     )
                   )
                 )
    )
}

loplateUI <- function(id, width = "1000px", height = "600px") {
  ns <- NS(id)
  highchartOutput(ns('plate'), width, height)
}

loplate <- function(input, output, session, data, nrow, ncol) {

  output$plate <- renderHighchart({
    validate(need(data(), ''))
    data() %>%
      loplate_formatData(nrow, ncol) %>%
      loplate_buildPlot()
  })

  selection <- eventReactive(input$plate_selected, {
    data() %>%
      filter(Well %in% input$plate_selected)
  })

  return(selection)
}
