#' @title baRcodeR GUI 
#' 
#' @description This addin will allow you to interactive create ID codes and generate PDF files
#' of QR codes.
#' @export
#' @import qrcode
#' @examples 
#' if(interactive()){
#' library(baRcodeR)
#' make_labels()
#' }


make_labels<-function() {
  # user interface
  # base::library(qrcode)
  ui<-miniUI::miniPage(
    miniUI::gadgetTitleBar("baRcodeR"),
    miniUI::miniTabstripPanel(id = NULL, selected = NULL, between = NULL,
                              # simple label tab
      miniUI::miniTabPanel("Simple ID Code Generation", value = graphics::title, icon = shiny::icon("bars"),
                   miniUI::miniContentPanel(
                     shiny::fillRow(
                       shiny::fillCol(shiny::tagList(# user input elements
                         shiny::tags$h1("Simple ID Codes", id = "title"),
                         shiny::textInput("prefix", "ID String", value = "", width=NULL, placeholder="Type in ... ..."),
                         shiny::numericInput("start_number", "From (integer)", value = NULL, min = 1, max = Inf, width=NULL),
                         shiny::numericInput("end_number", "To (integer)", value = NULL, min = 1, max = Inf, width=NULL),
                         shiny::numericInput("digits", "digits", value = 3, min = 1, max = Inf, width=NULL),
                         # textOutput("check"),
                         shiny::actionButton("make", "Create Label.csv"))),
                       shiny::fillRow(shiny::tagList(# output code snippet for reproducibility
                         shiny::tags$h3("Reproducible code"),
                         shiny::verbatimTextOutput("label_code"),
                         # output showing label preview
                         shiny::tags$h3("Preview"),
                         DT::DTOutput("label_df")))
                     )

                                   )),
      # hierarchy label tab
      miniUI::miniTabPanel("Hierarchical ID Code Generation", value = graphics::title, icon = shiny::icon("sitemap"),
                           miniUI::miniContentPanel(
                             shiny::fillRow(
                               shiny::fillCol(shiny::tagList(
                                 # ui elements
                                 shiny::tags$h1("Hierarchical ID Codes", id = "title"),
                                 shiny::numericInput("hier_digits", "digits", value = 2, min = 1, max = Inf, width=NULL),
                                 shiny::textInput("hier_prefix", "ID String", value = "", width=NULL, placeholder="Type in ... ..."),
                                 shiny::numericInput("hier_start_number", "From (integer)", value = NULL, min = 1, max = Inf, width=NULL),
                                 shiny::numericInput("hier_end_number", "To (integer)", value = NULL, min = 1, max = Inf, width=NULL),
                                 shiny::actionButton('insertBtn', 'Add level'),
                                 shiny::actionButton('removeBtn', 'Remove level'),
                                 # shiny::actionButton("hier_label_preview", "Preview Labels"),
                                 shiny::actionButton("hier_label_make", "Create Labels.csv")
                               )),
                               shiny::fillCol(shiny::tagList(
                                 # code snippet
                                 shiny::tags$h3("Reproducible Code"),
                                 shiny::verbatimTextOutput("hier_code"),
                                 # output elements
                                 shiny::tags$h3("Hierarchy"),
                                 # output hierarchy as df
                                 shiny::verbatimTextOutput("list_check"),
                                 shiny::tags$h3("Label Preview"),
                                 # label preview
                                 DT::DTOutput("hier_label_df")
                               ))
                             )

                           )),
      # tab for pdf output
      miniUI::miniTabPanel("Barcode Creation", value= graphics::title, icon = shiny::icon("qrcode"),
                   miniUI::miniContentPanel(
                     shiny::fillRow(
                       shiny::fillCol(
                         shiny::tagList(
                           shiny::fileInput("labels", "1. Choose a text file of ID codes.", multiple=FALSE,
                               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                     shiny::checkboxInput("header", "Header in file?", value=TRUE),
                     # radioButtons("header", "Header in file?", choices = c(Yes = TRUE, No = F), selected = TRUE),
                     shiny::tags$h3("2. (Optional) Modify PDF from default values"),
                     shiny::textInput("filename", "PDF file name", value = "LabelsOut"),
                     shiny::selectInput(inputId = "err_corr", label = "Error Correction", choices = c("L (up to 7% damage)"="L", "M (up to 15% damage)"= "M", "Q (up to 25% damage)" = "Q", "H (up to 30% damage)" = "H"), multiple=FALSE),
                     shiny::selectInput("type", "Barcode Type", choices = list("Matrix (2D)" = "matrix", "Linear (1D)" = "linear"), multiple = F),
                     shiny::numericInput("font_size", "Font Size", value = 2.5, min = 2.2, max = 4.7),
                     shiny::radioButtons("across", "Print across?", choices = c(Yes = TRUE, No = FALSE), selected = TRUE),
                     shiny::numericInput("erow", "# of rows to skip", value = 0, min = 0, max = 20, width=NULL),
                     shiny::numericInput("ecol", "# of columns to skip", value = 0, min = 0, max = 20, width=NULL),
                     shiny::checkboxInput("trunc", "Truncate label text?", value=FALSE),
                     shiny::numericInput("numrow", "Number of label rows on sheet", value = 20, min = 1, max = 100, width=NULL, step = 1),
                     shiny::numericInput("numcol", "Number of label columbs on sheet", value = 4, min = 1, max = 100, width=NULL, step = 1),
                     shiny::numericInput("page_width", "Page Width (in)", value = 8.5, min = 1, max = 20, width=NULL, step = 0.5),
                     shiny::numericInput("page_height", "Page Height (in)", value = 11, min = 1, max = 20, width=NULL, step = 0.5),
                     shiny::numericInput("width_margin", "Width margin of page (in)", value = 0.25, min = 0, max = 20, width=NULL, step = 0.05),
                     shiny::numericInput("height_margin", "Height margin of page (in)", value = 0.5, min = 0, max = 20, width=NULL, step = 0.05),
                     shiny::numericInput("label_width", "Width of label (in)", value = NA, min=0, max=100),
                     shiny::numericInput("label_height", "Height of label (in)", value = NA, min=0, max=100),
                     shiny::numericInput("x_space", "Horizontal space between barcode and text", value = 0, min = 0, max = 1),
                     shiny::numericInput("y_space", "Vertical location of text on label", value = 0.5, min = 0, max = 1)
                         )
                       ),
                       shiny::fillCol(
                         shiny::tagList(shiny::tags$h3("3. Click 'Import ID Code File' to import and check format of file."),
                     shiny::actionButton("label_check", "Import ID Code File"),
                     # output elements
                     shiny::tags$h3("Preview"),
                     shiny::plotOutput("label_preview", height = "auto", width = "auto"),
                     # label preview datatable
                     DT::DTOutput("check_make_labels"),
                     # code snippet
                     shiny::tags$h3("Reproducible Code"),
                     shiny::verbatimTextOutput("PDF_code_render"),
                     shiny::tags$h3("4. Click 'Make PDF'"),
                     shiny::tags$body("Wait for 'Done' to show up before opening PDF file"),
                     shiny::actionButton("make_pdf", "Make PDF"),
                     # status of pdf making
                     shiny::textOutput("PDF_status"))
                       )
                     )

                   )
                      )
    ## content panel end
  )
)
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    # simple label serverside
    output$check <- shiny::renderText({paste0(input$prefix, input$start_number, input$end_number)})
    # making simple labels
    Labels <- shiny::reactive({
      shiny::validate(
        shiny::need(input$prefix != "", "Please enter a prefix"),
        shiny::need(input$start_number != "", "Please enter a starting value"),
        shiny::need(input$end_number != "", "Please enter an ending value"),
        shiny::need(input$digits != "", "Please enter the number of digits")
      )
      baRcodeR::uniqID_maker(user=FALSE, string = input$prefix, level = seq(input$start_number, input$end_number), digits = input$digits)
    })
    # preview of simple labels
    output$label_df<-DT::renderDataTable(Labels())
    # writing simple labels
    shiny::observeEvent(input$make, {
      fileName<-sprintf("Labels_%s.csv", Sys.Date())
      utils::write.csv(Labels(), file = file.path(getwd(), fileName), row.names=FALSE)

    })
    output$label_code<-shiny::renderPrint(noquote(paste0("uniqID_maker(user = FALSE, string = \'", input$prefix, "\', ", "level = c(", input$start_number, ",", input$end_number, "), digits = ", input$digits, ")")))
    # pdf making server side
    # check label file
    Labels_pdf<-shiny::eventReactive(input$label_check, {
      shiny::req(input$labels)
      Labels<-utils::read.csv(input$labels$datapath, header=input$header, stringsAsFactors = F)
      Labels
    })
    # preview label file
    output$check_make_labels<-DT::renderDataTable(Labels_pdf(), server = FALSE, selection = list(mode = "single", target = "column", selected = 1))
    output$label_preview <- shiny::renderImage({
      if(input$type == "matrix") {
        code_vp <- grid::viewport(x=grid::unit(0.05, "npc"), y=grid::unit(0.8, "npc"), width = grid::unit(0.3 * (input$page_width - 2 * input$width_margin)/input$numcol, "in"), height = grid::unit(0.6 * (input$page_height - 2 * input$height_margin)/input$numrow, "in"), just=c("left", "top"))
        label_vp <- grid::viewport(x=grid::unit((0.4 + 0.6 * input$x_space)* (input$page_width - 2 * input$width_margin)/input$numcol, "in"), y=grid::unit(input$y_space, "npc"), width = grid::unit(0.4, "npc"), height = grid::unit(0.8, "npc"), just=c("left", "center"))
        label_plot <- qrcode_make(Labels = Labels_pdf()[1, input$check_make_labels_columns_selected], ErrCorr = input$err_corr)
      } else {
        code_vp <- grid::viewport(x=grid::unit(0.05, "npc"), y=grid::unit(0.8, "npc"), width = grid::unit(0.9 * (input$page_width - 2 * input$width_margin)/input$numcol, "in"), height = grid::unit(0.8 * (input$page_height - 2 * input$height_margin)/input$numrow, "in"), just=c("left", "top"))
        # text_height <- ifelse(input$Fsz / 72 > (input$page_height - 2 * input$height_margin)/input$numrow * 0.3, (input$page_height - 2 * input$height_margin)/input$numrow * 0.3, input$Fsz/72)
        label_vp <- grid::viewport(x=grid::unit(0.5, "npc"), y = grid::unit(1, "npc"), width = grid::unit(1, "npc"), height = grid::unit((input$page_height - 2 * input$height_margin)/input$numrow * 0.3, "in"), just = c("centre", "top"))
        label_plot <- code_128_make(Labels = Labels_pdf()[1, input$check_make_labels_columns_selected])
      }
      outputfile <- tempfile(fileext=".png")
      grDevices::png(outputfile, width = (input$page_width - 2 * input$width_margin)/input$numcol, (input$page_height - 2 * input$height_margin)/input$numrow, units = "in", res=100)
      # grid::grid.rect()
      grid::pushViewport(code_vp)
      grid::grid.draw(label_plot)
      grid::popViewport()
      grid::pushViewport(label_vp)
      grid::grid.text(label = Labels_pdf()[1, input$check_make_labels_columns_selected], gp = grid::gpar(fontsize = input$font_size, lineheight = 0.8))
      grDevices::dev.off()
      list(src = outputfile,
           width = 80 * (input$page_width - 2 * input$width_margin)/input$numcol, 
           height = 80 * (input$page_height - 2 * input$height_margin)/input$numrow,
           alt = "Label Preview")
    }, deleteFile = TRUE
    )
    # text indicator that pdf finished making
    PDF_done<-shiny::eventReactive(input$make_pdf, {
      baRcodeR::custom_create_PDF(user=FALSE, Labels = Labels_pdf()[, input$check_make_labels_columns_selected], name = input$filename, type = input$type, ErrCorr = input$err_corr, Fsz = input$font_size, Across = input$across, ERows = input$erow, ECols = input$ecol, trunc = input$trunc, numrow = input$numrow, numcol = input$numcol, page_width = input$page_width, page_height = input$ page_height, height_margin = input$height_margin, width_margin = input$width_margin, label_width = input$label_width, label_height = input$label_height, x_space = input$x_space, y_space = input$y_space)
      status<-"Done"
      status
    })
    PDF_code_snippet<-shiny::reactive({
      noquote(paste0("custom_create_PDF(user=FALSE, Labels = label_csv[,", input$check_make_labels_columns_selected, "], name = \'", input$filename, "\' type = \'", input$type, "\', ErrCorr = \'", input$err_corr, "\', Fsz = ", input$font_size, ", Across = ", input$across, ", ERows = ", input$erow, ", ECols = ", input$ecol, ", trunc = ", input$trunc, ", numrow = ", input$numrow, ", numcol = ", input$numcol, ", page_width = ", input$page_width, ", page_height = ", input$page_height, ", width_margin = ", input$width_margin, ", height_margin = ", input$height_margin, ", label_width = ", input$label_width, ", label_height = ", input$label_height,", x_space = ", input$x_space, ", y_space = ", input$y_space, ")"))
      })
    csv_code_snippet<-shiny::reactive({noquote(paste0("label_csv <- read.csv( \'", input$labels$name, "\', header = ", input$header, ", stringsAsFactors = F)"))})
    output$PDF_code_render<-shiny::renderText({
      paste(csv_code_snippet(), PDF_code_snippet(), sep = "\n")
      })
    # label preview


    # rendering of pdf indicator
    output$PDF_status<-shiny::renderPrint({print(PDF_done())})
    # server-side for hierarchical values
    # set reactiveValues to store the level inputs
    values<-shiny::reactiveValues()
    # set up data frame within reactiveValue function
    values$df<-data.frame(Prefix = character(0), start=integer(), end=integer(), stringsAsFactors = FALSE)
    # delete row from the df if button is pressed.
    shiny::observeEvent(input$removeBtn, {
      shiny::isolate(values$df<-values$df[-(nrow(values$df)),])
    })
    # add level to df
    shiny::observeEvent(input$insertBtn, {
      # level_name<-input$insertBtn
      shiny::validate(
        shiny::need(input$hier_prefix != "", "Please enter a prefix"),
        shiny::need(input$hier_start_number != "", "Please enter a starting value"),
        shiny::need(input$hier_end_number != "", "Please enter an ending value"),
        shiny::need(input$hier_digits != "", "Please enter the number of digits")
      )
      shiny::isolate(values$df[nrow(values$df) + 1,]<-c(input$hier_prefix, input$hier_start_number, input$hier_end_number))
      shiny::updateTextInput(session = session, "hier_prefix", "Label String", value = character(0))
      shiny::updateNumericInput(session = session, "hier_start_number", "From (integer)", value = numeric(0), min = 1, max = Inf)
      shiny::updateNumericInput(session = session, "hier_end_number", "To (integer)", value = numeric(0), min = 1, max = Inf)
    })
    # make hierarchical labels
    hier_label_df<-shiny::reactive({
      # shiny::validate(
      #   shiny::need(input$hier_prefix != "", "Please enter a prefix"),
      #   shiny::need(input$hier_start_number != "", "Please enter a starting value"),
      #   shiny::need(input$hier_end_number != "", "Please enter an ending value"),
      #   shiny::need(input$hier_digits != "", "Please enter the number of digits")
      # )
      shiny::validate(
        shiny::need(nrow(values$df) != 0, "Please add a level")
      )
      hierarchy <- split(values$df, seq(nrow(values$df)))
      hier_Labels <- baRcodeR::uniqID_hier_maker(user=FALSE, hierarchy = hierarchy, end = NULL, digits = input$hier_digits)
      hier_Labels
    })
    hier_code_snippet_obj<-shiny::reactive({
      begin_string<-noquote(strsplit(paste(split(values$df, seq(nrow(values$df))), collapse=', '), ' ')[[1]])
      replace_string<-gsub(pattern = "list\\(", replacement = "c\\(", begin_string)
      replace_string<-paste(replace_string, sep="", collapse="")
      noquote(paste0("uniqID_hier_maker(user = FALSE, hierarchy = list(", replace_string, "), end = NULL, digits = ", input$hier_digits, ")"))

      })
    output$hier_code<-shiny::renderText(hier_code_snippet_obj())
    # rough df of the level df
    output$list_check<-shiny::renderPrint(values$df)
    # preview of hierarchical labels
    output$hier_label_df<-DT::renderDataTable(hier_label_df())
    # make the csv file
    shiny::observeEvent(input$hier_label_make, {
      fileName<-sprintf("Labels_%s.csv", Sys.Date())
      utils::write.csv(hier_label_df(), file = file.path(getwd(), fileName), row.names=FALSE)
    })

    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    shiny::observeEvent(input$done, {

      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      shiny::stopApp()
    })
  }
  # Run the application
  # shinyApp(ui = ui, server = server)
  viewer <- shiny::dialogViewer("baRcodeR", width = 800, height = 1000)
  shiny::runGadget(ui, server, viewer = viewer)

}


