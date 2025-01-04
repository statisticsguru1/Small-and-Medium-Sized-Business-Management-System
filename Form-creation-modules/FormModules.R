# Global.R
library(shiny)
library(bslib) 
library(bsicons)# For tooltips

# Define the create field submodule UI

createFieldUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(strong("Input settings"),
                      width = 300,
                tagList(
                  selectInput(ns("data_type"),tagList(paste( "Data Type" ),
                                                      tooltip(bs_icon("info-circle"),
                                                              "The type of data you want users to input",
                                                              placement = "right")),
                              choices = c("text", "number", "date","range", "binary","multiple choice", "file", "action")),
                  uiOutput(ns("specific_input")),
                  uiOutput(ns("inputparams"))
                  
                  )
                ),
    uiOutput(ns("chosen_input"))
  )

}

# Define the create field submodule server logic

createFieldServer <- function(id, fields) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$data_type, {
      output$specific_input <- renderUI({
        switch(input$data_type,
               "text" = selectInput(ns("specific_type"),
                                    tagList(paste("Input Method"),
                                            tooltip(
                                              bs_icon("info-circle"),
                                              "Data input method",
                                              placement = "right")), choices = c("textInput", "textAreaInput")),
               "number" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                  tooltip(
                                                                    bs_icon("info-circle"),
                                                                    "Data input method",
                                                                    placement = "right")), choices = c("numericInput", "sliderInput")),
               "date" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                tooltip(
                                                                  bs_icon("info-circle"),
                                                                  "Data input method",
                                                                  placement = "right")), choices = c("dateInput","dateslider")),
               "range"=selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                               tooltip(
                                                                 bs_icon("info-circle"),
                                                                 "Data input method",
                                                                 placement = "right")), choices = c("numeric range","date range", "dateRangeInput")),
               "binary" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                   tooltip(
                                                                     bs_icon("info-circle"),
                                                                     "Data input method",
                                                                     placement = "right")), choices = c("binaryradioButtons", "checkboxInput")),
               "multiple choice" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                   tooltip(
                                                                     bs_icon("info-circle"),
                                                                     "Data input method",
                                                                     placement = "right")), choices = c("selectInput","selectizeInput","radioButtons", "checkboxGroupInput")),
               "file" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                tooltip(
                                                                  bs_icon("info-circle"),
                                                                  "Data input method",
                                                                  placement = "right")), choices = c("fileInput")),
               
               "action" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                  tooltip(
                                                                    bs_icon("info-circle"),
                                                                    "Data input method",
                                                                    placement = "right")), choices = c("actionButton", "actionLink")),
               "password" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                    tooltip(
                                                                      bs_icon("info-circle"),
                                                                      "Data input method",
                                                                      placement = "right")), choices = c("password")),
               "currency" = selectInput(ns("specific_type"),tagList(paste("Input Method"),
                                                                    tooltip(
                                                                      bs_icon("info-circle"),
                                                                      "Data input method",
                                                                      placement = "right")), choices = c("currency")),
               NULL
        )
      })
    })
    
    observeEvent(input$specific_type, {
      output$inputparams<-renderUI({
        switch(input$specific_type,
               "textInput"=tagList(
                 strong("Text input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name",
                                     placement = "right"))),
                 textInput(ns("value"),
                           tagList(paste("value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The default value",
                                     placement = "right")),value=""),
                 textInput(ns("placeholder"),
                           tagList(paste("placeholder"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                                       "A character string giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.",
                                                       placement = "right")),value="Enter value"),
                 textInput(ns("width"),
                              tagList(paste("width"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The width of the input, e.g. '400px', or '100%'",
                                        placement = "right")),value="400px")
               ),
               "textAreaInput" = tagList(
                 strong("Text Area input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right"))),
                 textInput(ns("value"),
                           tagList(paste("value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The default value",
                                     placement = "right")),value=""),
                 textInput(ns("width"),
                              tagList(paste("width"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The width of the input, e.g. '400px', or '100%';",
                                        placement = "right")),value="400px"),
                 textInput(ns("height"),
                              tagList(paste("height"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The height of the input, e.g. '400px', or '100%';",
                                        placement = "right")),value='400px'),
                 numericInput(ns("cols"),
                              tagList(paste("cols"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Value of the visible character columns of the input, e.g. 80. This argument will only take effect if there is not a CSS width rule defined for this element; such a rule could come from the width argument of this function or from a containing page layout",
                                        placement = "right")),value=NULL),
                 numericInput(ns("rows"),
                              tagList(paste("rows"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The value of the visible character rows of the input, e.g. 6. If the height argument is specified, height will take precedence in the browser's rendering.",
                                        placement = "right")),value=NULL),
                 textInput(ns("placeholder"),
                           tagList(paste("placeholder"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "A character string giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.",
                                     placement = "right")),value="Type your text here"),
                 selectInput(ns("resize"),
                             tagList(paste("resize"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       "Which directions the textarea box can be resized. Can be one of 'both', 'none', 'vertical', and 'horizontal'. The default, NULL",
                                       placement = "right")),
                             choices=c("both", "none", "vertical","horizontal")
                             
                             )
               ),
               "numericInput" = tagList(
                 strong("Number input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right")),value=""),
                 numericInput(ns("value"),
                           tagList(paste("value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The default value",
                                     placement = "right")),value=0),
                 numericInput(ns("min"),
                              tagList(paste("minimum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Minimum allowed value",
                                        placement = "right")),value=-.Machine$double.xmax),
                 numericInput(ns("max"),
                              tagList(paste("maximum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Maximum allowed value",
                                        placement = "right")),value= .Machine$double.xmax),
                 numericInput(ns("step"),
                              tagList(paste("step"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "	Interval to use when stepping between min and max",
                                        placement = "right")),value=1),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px")),
               "sliderInput" = tagList(
                 strong("slider parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right")),value=""),
                 numericInput(ns("value"),
                           tagList(paste("value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The default value",
                                     placement = "right")),value=5),
                 numericInput(ns("min"),
                              tagList(paste("minimum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Minimum allowed value",
                                        placement = "right")),value=1),
                 numericInput(ns("max"),
                              tagList(paste("maximum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Maximum allowed value",
                                        placement = "right")),value=10),
                 numericInput(ns("step"),
                              tagList(paste("step"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "	Interval to use when stepping between min and max",
                                        placement = "right")),value=1),
                 fluidRow(
                   column(width=6,
                 checkboxInput("round",tagList(paste("round"),
                                               tooltip(
                                                 bsicons::bs_icon("info-circle"),
                                                 "TRUE to round all values to the nearest integer; FALSE if no rounding is desired; or an integer to round to that number of digits (for example, 1 will round to the nearest 10, and -2 will round to the nearest .01). Any rounding will be applied after snapping to the nearest step",
                                                 placement = "right")),value=F)),
                 column(width=6,
                 checkboxInput("ticks",tagList(paste("ticks"),
                                               tooltip(
                                                 bsicons::bs_icon("info-circle"),
                                                 "FALSE to hide tick marks, TRUE to show them according to some simple heuristics.",
                                                 placement = "right")),value=T))),
                 conditionalPanel(
                   condition ="input.round==1",
                   numericInput(ns("dcpl"),
                                tagList(paste("dcpl"),
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "number of decimals to round",
                                          placement = "right")),value=2),
                   ns=ns),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px"),
                 textInput(ns("sep"),
                           tagList(paste("separator"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Separator between thousands places in numbers.",
                                     placement = "right")),value=NULL),
                 textInput(ns("pre"),
                           tagList(paste("prefix"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "A prefix string to put in front of the value.",
                                     placement = "right")),value=NULL),
                 textInput(ns("post"),
                           tagList(paste("suffix"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "A suffix string to put after the value.",
                                     placement = "right")),value=NULL)),
               "dateslider" = tagList(
                 strong("Date slider parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right")),value=""),
                 dateInput(ns("value"),
                              tagList(paste("value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The default value",
                                        placement = "right"))),
                dateInput(ns("min"),
                              tagList(paste("minimum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Minimum allowed value",
                                        placement = "right")),value='0000-01-01'),
                 dateInput(ns("max"),
                              tagList(paste("maximum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Maximum allowed value",
                                        placement = "right")),value="9999-12-31"),
                 numericInput(ns("step"),
                              tagList(paste("step"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "	Interval to use when stepping between min and max",
                                        placement = "right")),value=1),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px"),
                 textInput(ns("sep"),
                           tagList(paste("separator"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Separator between thousands places in numbers.",
                                     placement = "right")),value=NULL),
                 textInput(ns("pre"),
                           tagList(paste("prefix"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "A prefix string to put in front of the value.",
                                     placement = "right")),value=NULL),
                 textInput(ns("post"),
                           tagList(paste("suffix"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "A suffix string to put after the value.",
                                     placement = "right")),value=NULL)),
               "dateInput" = tagList(
                 strong("Date input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right"))),
                 dateInput(ns("value"),
                           tagList(paste("value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The starting date. Either a Date object, or a string in yyyy-mm-dd format. If NULL (the default), will use the current date in the client's time zone",
                                     placement = "right")),value=NULL),
                 dateInput(ns("min"),
                              tagList(paste("minimum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The minimum allowed date. Either a Date object, or a string in yyyy-mm-dd format.",
                                        placement = "right")),value="0000-01-01"),
                 dateInput(ns("max"),
                              tagList(paste("maximum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The maximum allowed date. Either a Date object, or a string in yyyy-mm-dd format.",
                                        placement = "right")),value='9999-12-31'),
                 textInput(ns("format"),
                              tagList(paste("format"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The format of the date to display in the browser. Defaults to 'yyyy-mm-dd'.",
                                        placement = "right")),value="yyyy-mm-dd"),
                 selectInput(ns('startview'),
                             tagList(paste("startview"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       'The date range shown when the input object is first clicked. Can be "month" (the default), "year", or "decade".',
                                       placement = "right")),selected="month",choices = c("month","year","decade")),
                 numericInput(ns("weekstart"),
                              tagList(paste("weekstart"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Which day is the start of the week. Should be an integer from 0 (Sunday) to 6 (Saturday)",
                                        placement = "right")),value=0,min=0,max=6,step=1),
                 selectInput(ns('language'),
                             tagList(paste("language"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       'The language used for month and day names. Default is "en". Other valid values include "ar", "az", "bg", "bs", "ca", "cs", "cy", "da", "de", "el", "en-AU", "en-GB", "eo", "es", "et", "eu", "fa", "fi", "fo", "fr-CH", "fr", "gl", "he", "hr", "hu", "hy", "id", "is", "it-CH", "it", "ja", "ka", "kh", "kk", "ko", "kr", "lt", "lv", "me", "mk", "mn", "ms", "nb", "nl-BE", "nl", "no", "pl", "pt-BR", "pt", "ro", "rs-latin", "rs", "ru", "sk", "sl", "sq", "sr-latin", "sr", "sv", "sw", "th", "tr", "uk", "vi", "zh-CN", and "zh-TW".',
                                       placement = "right")),selected="en",choices = c("ar", "az", "bg", "bs", "ca", "cs", "cy", "da", "de", "el", "en-AU","en","en-GB", "eo", "es", "et", "eu", "fa", "fi", "fo", "fr-CH", "fr", "gl", "he", "hr", "hu", "hy", "id", "is", "it-CH", "it", "ja", "ka", "kh", "kk", "ko", "kr", "lt", "lv", "me", "mk", "mn", "ms", "nb", "nl-BE", "nl", "no", "pl", "pt-BR", "pt", "ro", "rs-latin", "rs", "ru", "sk", "sl", "sq", "sr-latin", "sr", "sv", "sw", "th", "tr", "uk", "vi", "zh-CN","zh-TW")),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px"),
                 checkboxInput(ns("autoclose"),tagList(paste("autoclose"),
                                               tooltip(
                                                 bsicons::bs_icon("info-circle"),
                                                 "Whether or not to close the datepicker immediately when a date is selected.",
                                                 placement = "right")),value=F),
                 
                 textInput(ns("datesdisabled"),
                           tagList(paste("datesdisabled"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Which dates should be disabled. Enter dates in format yyyy-mm-dd seperate dates with a','.",
                                     placement = "right")),value=""),
                 textInput(ns("daysofweekdisabled"),
                              tagList(paste("daysofweekdisabled"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Days of the week that should be disabled. Should be a integer vector with values from 0 (Sunday) to 6 (Saturday).",
                                        placement = "right")),value="")),
               
               "numeric range" = tagList(
                   strong("numeric range parameters"),
                   textInput(ns("fieldlabel"),
                             tagList(paste("Field Label"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       "Your field name/field label",
                                       placement = "right")),value=""),
                   numericInput(ns("min"),
                                tagList(paste("minimum value"),
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "Minimum allowed value",
                                          placement = "right")),value=1),
                   numericInput(ns("max"),
                                tagList(paste("maximum value"),
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "Maximum allowed value",
                                          placement = "right")),value=20),
                   numericInput(ns("lowervalue"),
                                tagList(paste("Lower bound"),
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "The lower bound of the range",
                                          placement = "right")),value=5),
                   numericInput(ns("uppervalue"),
                                tagList(paste("value"),
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "The upperbound of the range",
                                          placement = "right")),value=15),
                   numericInput(ns("step"),
                                tagList(paste("step"),
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "	Interval to use when stepping between min and max",
                                          placement = "right")),value=1),
                   fluidRow(
                     column(width=6,
                            checkboxInput("round",tagList(paste("round"),
                                                          tooltip(
                                                            bsicons::bs_icon("info-circle"),
                                                            "TRUE to round all values to the nearest integer; FALSE if no rounding is desired; or an integer to round to that number of digits (for example, 1 will round to the nearest 10, and -2 will round to the nearest .01). Any rounding will be applied after snapping to the nearest step",
                                                            placement = "right")),value=F)),
                     column(width=6,
                            checkboxInput("ticks",tagList(paste("ticks"),
                                                          tooltip(
                                                            bsicons::bs_icon("info-circle"),
                                                            "FALSE to hide tick marks, TRUE to show them according to some simple heuristics.",
                                                            placement = "right")),value=T))),
                   conditionalPanel(
                     condition ="input.round==1",
                     numericInput(ns("dcpl"),
                                  tagList(paste("dcpl"),
                                          tooltip(
                                            bsicons::bs_icon("info-circle"),
                                            "number of decimals to round",
                                            placement = "right")),value=2),
                     ns=ns),
                   textInput(ns("width"),
                             tagList(paste("width"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       "The width of the input, e.g. '400px', or '100%';",
                                       placement = "right")),value="400px"),
                   textInput(ns("sep"),
                             tagList(paste("separator"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       "Separator between thousands places in numbers.",
                                       placement = "right")),value=NULL),
                   textInput(ns("pre"),
                             tagList(paste("prefix"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       "A prefix string to put in front of the value.",
                                       placement = "right")),value=NULL),
                   textInput(ns("post"),
                             tagList(paste("suffix"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       "A suffix string to put after the value.",
                                       placement = "right")),value=NULL)),
               "date range" = tagList(
                 strong("date range parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right")),value=""),
                 dateInput(ns("min"),
                              tagList(paste("minimum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Minimum allowed value",
                                        placement = "right")),value='1000-01-01'),
                dateInput(ns("max"),
                              tagList(paste("maximum value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Maximum allowed value",
                                        placement = "right")),value="3000-12-31"),
                 dateInput(ns("lowervalue"),
                              tagList(paste("Lower bound"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The lower bound of the range",
                                        placement = "right")),value=lubridate::today()-100000),
                 dateInput(ns("uppervalue"),
                              tagList(paste("value"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "The upperbound of the range",
                                        placement = "right")),value=lubridate::today()+100000),
                 numericInput(ns("step"),
                              tagList(paste("step"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "	Interval to use when stepping between min and max",
                                        placement = "right")),value=1),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px"),
                 textInput(ns("pre"),
                           tagList(paste("prefix"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "A prefix string to put in front of the value.",
                                     placement = "right")),value=NULL),
                 textInput(ns("post"),
                           tagList(paste("suffix"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "A suffix string to put after the value.",
                                     placement = "right")),value=NULL)),
               "dateRangeInput" = tagList(
                 strong("Date input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right"))),
                 dateInput(ns("start"),
                           tagList(paste("Lower bound"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The lower bound of the range",
                                     placement = "right")),value=lubridate::today()-10),
                 dateInput(ns("end"),
                           tagList(paste("value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The upperbound of the range",
                                     placement = "right")),value=lubridate::today()+10),
                 dateInput(ns("min"),
                           tagList(paste("minimum value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The minimum allowed date. Either a Date object, or a string in yyyy-mm-dd format.",
                                     placement = "right")),value="0000-01-01"),
                 dateInput(ns("max"),
                           tagList(paste("maximum value"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The maximum allowed date. Either a Date object, or a string in yyyy-mm-dd format.",
                                     placement = "right")),value='9999-12-31'),
                 textInput(ns("format"),
                           tagList(paste("format"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The format of the date to display in the browser. Defaults to 'yyyy-mm-dd'.",
                                     placement = "right")),value="yyyy-mm-dd"),
                 selectInput(ns('startview'),
                             tagList(paste("startview"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       'The date range shown when the input object is first clicked. Can be "month" (the default), "year", or "decade".',
                                       placement = "right")),selected="month",choices = c("month","year","decade")),
                 numericInput(ns("weekstart"),
                              tagList(paste("weekstart"),
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Which day is the start of the week. Should be an integer from 0 (Sunday) to 6 (Saturday)",
                                        placement = "right")),value=0,min=0,max=6,step=1),
                 selectInput(ns('language'),
                             tagList(paste("language"),
                                     tooltip(
                                       bsicons::bs_icon("info-circle"),
                                       'The language used for month and day names. Default is "en". Other valid values include "ar", "az", "bg", "bs", "ca", "cs", "cy", "da", "de", "el", "en-AU", "en-GB", "eo", "es", "et", "eu", "fa", "fi", "fo", "fr-CH", "fr", "gl", "he", "hr", "hu", "hy", "id", "is", "it-CH", "it", "ja", "ka", "kh", "kk", "ko", "kr", "lt", "lv", "me", "mk", "mn", "ms", "nb", "nl-BE", "nl", "no", "pl", "pt-BR", "pt", "ro", "rs-latin", "rs", "ru", "sk", "sl", "sq", "sr-latin", "sr", "sv", "sw", "th", "tr", "uk", "vi", "zh-CN", and "zh-TW".',
                                       placement = "right")),selected="en",choices = c("ar", "az", "bg", "bs", "ca", "cs", "cy", "da", "de", "el", "en-AU","en","en-GB", "eo", "es", "et", "eu", "fa", "fi", "fo", "fr-CH", "fr", "gl", "he", "hr", "hu", "hy", "id", "is", "it-CH", "it", "ja", "ka", "kh", "kk", "ko", "kr", "lt", "lv", "me", "mk", "mn", "ms", "nb", "nl-BE", "nl", "no", "pl", "pt-BR", "pt", "ro", "rs-latin", "rs", "ru", "sk", "sl", "sq", "sr-latin", "sr", "sv", "sw", "th", "tr", "uk", "vi", "zh-CN","zh-TW")),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px"),
                 checkboxInput(ns("autoclose"),tagList(paste("autoclose"),
                                                   tooltip(
                                                     bsicons::bs_icon("info-circle"),
                                                     "Whether or not to close the datepicker immediately when a date is selected.",
                                                     placement = "right")),value=F)),
               "binaryradioButtons" = tagList(
                 strong("radioButtons input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right"))),
                 textInput(ns("choices"),
                           tagList(paste("choices"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Choice labels, with their corresponding labels,
                                     seperate using ','or ';'. Only the first two will be used",
                                     placement = "right")),
                           value = "Yes=Yes,No=No"),
                 checkboxInput(ns("inline"),
                               tagList(paste("inline"),
                                       tooltip(
                                         bsicons::bs_icon("info-circle"),
                                         "If TRUE, render the choices inline (i.e. horizontally)",
                                         placement = "right"))
                               
                               )
               ),
               
               "checkboxInput" = tagList(
                 strong("checkbox input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right"))),
                 selectInput(ns("value"),
                               tagList(paste("value"),
                                       tooltip(
                                         bsicons::bs_icon("info-circle"),
                                         "The default value",
                                         placement = "right")),
                             choices=c(TRUE,FALSE)         
                 ),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px")
               ),
               
               "selectInput" = tagList(
                 strong("select input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right"))),
                 textInput(ns("choices"), "Choices (comma-separated)"),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px")
               ),
               
               "selectizeInput" = tagList(
                 strong("selecticizeInput input parameters"),
                 textInput(ns("fieldlabel"),
                           tagList(paste("Field Label"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Your field name/field label",
                                     placement = "right"))),
                 textInput(ns("choices"), "Choices (comma-separated)",value=""),
                 checkboxInput(ns("multiple"), "Is selection of multiple items allowed",value = T),
                 textInput(ns("width"),
                           tagList(paste("width"),
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "The width of the input, e.g. '400px', or '100%';",
                                     placement = "right")),value="400px"),
                 textInput(ns("placeholder"),"select a placeholder",value="Enter choices"),
                 checkboxInput(ns("create"), "create choices",value = T),
                 numericInput(ns("maxitems"),"maximum items",value = Inf),
               ),
               
               

               "checkboxGroupInput" = tagList(
                 textInput(ns("choices"), "Choices (comma-separated)")
               ),
               "checkboxInput" = tagList(),
               "fileInput" = tagList(),
               "actionButton" = tagList(
                 textInput(ns("button_icon"), "Icon (optional)")
               ),
               "actionLink" = tagList(
                 textInput(ns("link_icon"), "Icon (optional)")
               ),
               NULL
        )
        
      })

      output$chosen_input <- renderUI({
        switch(input$specific_type,
               "textInput" =card(
                 fill=T,
                 card_header("Text Input Preview",
                             actionButton("add_input","Save this input"),
                             class = "d-flex justify-content-between"),
                 tagList(
                 textInput(ns("id"),
                           label=input$fieldlabel,
                           value=input$value,
                           width=input$width,
                           placeholder=input$placeholder
                 )
               )
               ),
               "textAreaInput" =card(
                 fill=T,
                 card_header("Text Input Preview",
                             actionButton("add_input","Save this input"),
                             class = "d-flex justify-content-between"),
                 
                 tagList(
                 textAreaInput(ns("id"),
                               label=input$fieldlabel,
                               value=input$value,
                               width=input$width,
                               height=input$height,
                               cols=input$cols,
                               rows=input$rows,
                               placeholder=input$placeholder,
                               resize=input$resize)
               )),
               "numericInput" ={
                  req(input$min,input$max,input$value,input$step) 
                 card(
                 fill=T,
                 card_header("Numeric Input Preview",
                             actionButton("add_input","Save this input"),
                             class = "d-flex justify-content-between"),
                 tagList(
                 numericInput(ns("id"),
                              label=input$fieldlabel,
                              value=as.numeric(input$value),
                              max=as.numeric(input$max),
                              min=as.numeric(input$min),
                              step=as.numeric(input$step),
                              width=input$width)
               ))},
               "sliderInput" ={
                 req(input$min,input$max,input$value,input$step)
                 card(
                 fill=T,
                 card_header("Slider Input Preview",
                             actionButton("add_input","Save this input"),
                             class = "d-flex justify-content-between"),
                 tagList(
                   sliderInput(ns("id"),
                                label=input$fieldlabel,
                                value=input$value,
                                max=input$max,
                                min=input$min,
                                step=input$step,
                               #round=input$round,
                               #ticks=input$ticks,
                               width=input$width,
                               sep=input$sep,
                               pre=input$pre,
                               post=input$post,
                               animate=animationOptions(10)
                              )
                 ))
               },
               "dateslider" ={
                 req(input$min,input$max,input$value,input$step)
                 print(input$value)
                 card(
                   fill=T,
                   card_header("Slider Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     sliderInput(ns("id"),
                                 label=input$fieldlabel,
                                 value=as.Date(input$value),
                                 max=as.Date(input$max),
                                 min=as.Date(input$min),
                                 ticks = TRUE,
                                 step=input$step,
                                 width=input$width,
                                 pre=input$pre,
                                 post=input$post,
                                 animate=animationOptions(10)
                     )
                   ))
               },
               "dateInput"={
                 req(input$value,input$max,input$min,input$format,input$startview,input$weekstart)
                 card(
                   fill=T,
                   card_header("Date Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     dateInput(ns("id"),
                                 label=input$fieldlabel,
                                 value=input$value,
                                 max=input$max,
                                 min=input$min,
                                 format=input$format,
                                 startview=input$startview,
                                 weekstart=input$weekstart,
                                 language=input$language,
                                 width=input$width,
                                 #autoclose=input$autoclose,
                                 datesdisabled=as.Date(strsplit(input$datesdisabled,";")[[1]]),
                                 daysofweekdisabled=as.numeric(strsplit(input$daysofweekdisabled,",")[[1]])
                     )
                   ))
               },
               "numeric range" ={
                 req(input$min,input$max,input$lowervalue,input$uppervalue,input$step)
                 card(
                   fill=T,
                   card_header("Numeric Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     sliderInput(ns("id"),
                                 label=input$fieldlabel,
                                 value=c(input$lowervalue,input$uppervalue),
                                 max=input$max,
                                 min=input$min,
                                 step=input$step,
                                 #round=input$round,
                                 #ticks=input$ticks,
                                 width=input$width,
                                 sep=input$sep,
                                 pre=input$pre,
                                 post=input$post,
                                 animate=animationOptions(10)
                     )
                   ))
               },
               "date range" ={
                 req(input$min,input$max,input$lowervalue,input$uppervalue,input$step)
                 card(
                   fill=T,
                   card_header("Date Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     sliderInput(ns("id"),
                                 label=input$fieldlabel,
                                 value=c(as.Date(input$lowervalue),as.Date(input$uppervalue)),
                                 max=as.Date(input$max),
                                 min=as.Date(input$min),
                                 step=input$step,
                                 width=input$width,
                                 pre=input$pre,
                                 post=input$post,
                                 animate=animationOptions(10)
                     )
                   ))
               },
               "dateRangeInput"={
                 req(input$max,input$min,input$format,input$startview,input$weekstart)
                 card(
                   fill=T,
                   card_header("Date Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     dateRangeInput(ns("id"),
                               label=input$fieldlabel,
                               start=as.Date(input$start),
                               end=as.Date(input$end),
                               max=as.Date(input$max),
                               min=as.Date(input$min),
                               format=input$format,
                               startview=input$startview,
                               weekstart=input$weekstart,
                               language=input$language,
                               width=input$width,
                               #autoclose=input$autoclose
                               )
                   ))
               },
               "binaryradioButtons"={
                 req(input$choices)
                 split_vec<-strsplit(strsplit(input$choices, "[,;]")[[1]][1:2],"=")
                 
                 # Extract values and names
                 values <- sapply(split_vec, `[`, 2)  # Get the second part (values)
                 names(values) <- sapply(split_vec, `[`, 1)  # Set the first part as names
                 
                 card(
                   fill=T,
                   card_header("Binary Input button Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     radioButtons(ns("id"),
                                    label=input$fieldlabel,
                                    choices=values,
                                    inline=input$inline,
                                    width=input$width
                     )
                   ))
               },
               "checkboxInput"={
                 req(input$value)
                 card(
                   fill=T,
                   card_header("checkbox Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     checkboxInput(ns("id"),
                                    label=input$fieldlabel,
                                    value= base::as.logical(input$value),
                                    width=input$width
                     )
                   ))
               },
               "selectInput"={
                 #req(input$choices)
                 card(
                   fill=T,
                   min_height='600px',
                   card_header("Select Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     selectInput(ns("id"),
                                   label=input$fieldlabel,
                                   #value= base::as.logical(input$value),
                                   width=input$width,
                                   choices = strsplit(input$choices, "[,;]")[[1]],
                                   multiple=F,
                                 selectize=T
                                 )
                   ))
               },
               "selectizeInput"={
                 #req(!is.null(input$choices))
                 card(
                   fill=T,
                   min_height='600px',
                   card_header("Selectize Input Preview",
                               actionButton("add_input","Save this input"),
                               class = "d-flex justify-content-between"),
                   tagList(
                     selectizeInput(ns("id"),
                                 label=input$fieldlabel,
                                 #selected= input$value,
                                 width=input$width,
                                 choices = strsplit(input$choices, "[,;]")[[1]],
                                 multiple=input$multiple,
                                 #selectize=T,
                                 options = list(placeholder = input$placeholder,
                                                maxOptions=Inf,
                                                create=input$create,
                                                maxItems = input$maxitems)
                                 )
                   ))
               }
               
               
        )
      })
      
    })
    
    
    observeEvent(input$add_field, {
      new_field <- list(
        label = input$label,
        data_type = input$data_type,
        specific_type = input$specific_type,
        params = switch(input$specific_type,
                        "textInput" = list(placeholder = input$placeholder, min_chars = input$min_chars, max_chars = input$max_chars),
                        "textAreaInput" = list(placeholder = input$placeholder, rows = input$rows),
                        "passwordInput" = list(),
                        "numericInput" = list(min_value = input$min_value, max_value = input$max_value, step = input$step),
                        "sliderInput" = list(min_value = input$min_value, max_value = input$max_value, step = input$step, value = input$value),
                        "dateInput" = list(min_date = input$min_date, max_date = input$max_date),
                        "dateRangeInput" = list(start_date = input$start_date, end_date = input$end_date),
                        "selectInput" = list(choices = strsplit(input$choices, ",")[[1]], multiple = input$multiple),
                        "radioButtons" = list(choices = strsplit(input$choices, ",")[[1]]),
                        "checkboxGroupInput" = list(choices = strsplit(input$choices, ",")[[1]]),
                        "checkboxInput" = list(),
                        "fileInput" = list(),
                        "actionButton" = list(icon = input$button_icon),
                        "actionLink" = list(icon = input$link_icon),
                        list()
        )
      )
      fields(c(fields(), list(new_field)))
    })
  })
}


sectionUI <- function(id, section_name) {
  ns <- NS(id)
  tagList(
    h3(section_name),
    uiOutput(ns("fields_ui")),
    actionLink(ns("add_field"), "Add New Field"),
    actionLink(ns("remove_section"), "Remove Section")
  )
}

sectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    fields <- reactiveVal(list())
    
    observeEvent(input$add_field, {
      field_id <- paste0("field_", length(fields()) + 1)
      fields(c(fields(), field_id))
      output$fields_ui <- renderUI({
        lapply(fields(), function(field) {
          createInputFieldUI(field)
        })
      })
      lapply(fields(), function(field) {
        createInputFieldServer(field)
      })
    })
    
    
    observeEvent(input$remove_section, {
      # Logic to remove the section...
    })
  })
}

# Define the form module UI

formmoduleUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = tagList("Admin", bsTooltip(id = ns("tooltip"), title = "Admin Panel Tooltip", placement = "right")),
    fluidPage(
      createFieldUI(ns("create_field"))
    )
  )
}

# Define the form module server logic

formmoduleServer <- function(id, fields) {
  moduleServer(id, function(input, output, session) {
    createFieldServer("create_field", fields)
  })
}

