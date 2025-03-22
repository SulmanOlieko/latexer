#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Helper function: choose an icon based on file extension
  getFileIcon <- function(filename) {
    ext <- tolower(tools::file_ext(filename))
    if (ext == "tex") return(icon("file-code"))
    else if (ext == "bib") return(icon("file-text"))
    else if (ext %in% c("txt", "rnw")) return(icon("file-alt"))
    else if (ext == "pdf") return(icon("file-pdf"))
    else if (ext %in% c("png", "jpg", "jpeg", "gif")) return(icon("file-image"))
    else if (ext %in% c("doc", "docx")) return(icon("file-word"))
    else if (ext %in% c("xls", "xlsx")) return(icon("file-excel"))
    else if (ext %in% c("ppt", "pptx")) return(icon("file-powerpoint"))
    else if (ext %in% c("zip", "tar", "gz", "7z")) return(icon("file-archive"))
    else if (ext %in% c("html", "htm", "js", "css", "xml", "json")) return(icon("file-code"))
    else if (ext %in% c("c", "cpp", "java", "py", "sh")) return(icon("file-code"))
    else if (ext %in% c("mp3", "wav")) return(icon("file-audio"))
    else if (ext %in% c("mp4", "mov", "avi", "mkv")) return(icon("file-video"))
    else return(icon("file"))
  }

  # Reactive values to track files
  rv_files <- reactiveVal(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
  currentFile <- reactiveVal("")
  compileLog <- reactiveVal("")
  rv_compiled <- reactiveVal(list.files(compiledDir))
  dockerLog <- reactiveVal("")

  # ------------------------- PERSISTENT CACHING SETUP -------------------------
  cacheDir <- "cache"
  if (!dir.exists(cacheDir)) dir.create(cacheDir)
  compileLogFile <- file.path(cacheDir, "compileLog.txt")
  dockerLogFile <- file.path(cacheDir, "dockerLog.txt")
  themeFile     <- file.path(cacheDir, "theme.txt")
  lastFileFile  <- file.path(cacheDir, "lastFile.txt")
  uiSettingsFile<- file.path(cacheDir, "uiSettings.json")

  if (file.exists(compileLogFile)) {
    cachedCompileLog <- paste(readLines(compileLogFile, warn = FALSE), collapse = "\n")
    compileLog(cachedCompileLog)
    session$onFlushed(function(){ updateAceEditor(session, "consoleEditor", value = cachedCompileLog) })
  }
  if (file.exists(dockerLogFile)) {
    cachedDockerLog <- paste(readLines(dockerLogFile, warn = FALSE), collapse = "\n")
    dockerLog(cachedDockerLog)
    session$onFlushed(function(){ updateAceEditor(session, "dockerConsole", value = cachedDockerLog) })
  }
  if (file.exists(themeFile)) {
    savedTheme <- readLines(themeFile, warn = FALSE)
    session$onFlushed(function(){ updateSelectInput(session, "editorTheme", selected = savedTheme) })
  }
  if (file.exists(lastFileFile)) {
    lastF <- readLines(lastFileFile, warn = FALSE)
    currentFile(lastF)
  }

  # Default UI settings now use 80:20 splits for both editor and preview areas.
  uiSettings <- reactiveValues(
    editorConsoleVisible = TRUE,
    dockerConsoleVisible = TRUE,
    editorAreaSizes = c(80, 20),
    pdfPreviewSizes  = c(80, 20)
  )
  if (file.exists(uiSettingsFile)) {
    savedUI <- fromJSON(readLines(uiSettingsFile, warn = FALSE))
    uiSettings$editorConsoleVisible <- savedUI$editorConsoleVisible
    uiSettings$dockerConsoleVisible  <- savedUI$dockerConsoleVisible
    uiSettings$editorAreaSizes       <- savedUI$editorAreaSizes
    uiSettings$pdfPreviewSizes       <- savedUI$pdfPreviewSizes
    session$onFlushed(function(){
      session$sendCustomMessage("applyUISettings", list(
        editorConsoleVisible = isolate(uiSettings$editorConsoleVisible),
        dockerConsoleVisible = isolate(uiSettings$dockerConsoleVisible),
        editorAreaSizes = c(0, isolate(uiSettings$editorAreaSizes)),
        pdfPreviewSizes  = c(0, isolate(uiSettings$pdfPreviewSizes))
      ))
    })
  }

  saveUISettings <- function() {
    settings <- list(
      editorConsoleVisible = uiSettings$editorConsoleVisible,
      dockerConsoleVisible = uiSettings$dockerConsoleVisible,
      editorAreaSizes = uiSettings$editorAreaSizes,
      pdfPreviewSizes  = uiSettings$pdfPreviewSizes
    )
    writeLines(toJSON(settings, auto_unbox = TRUE, pretty = TRUE), uiSettingsFile)
  }

  observeEvent(input$editorConsoleVisible, { uiSettings$editorConsoleVisible <- input$editorConsoleVisible; saveUISettings() })
  observeEvent(input$dockerConsoleVisible, { uiSettings$dockerConsoleVisible <- input$dockerConsoleVisible; saveUISettings() })
  observeEvent(input$editorAreaSizes, { uiSettings$editorAreaSizes <- input$editorAreaSizes; saveUISettings() })
  observeEvent(input$pdfPreviewSizes, { uiSettings$pdfPreviewSizes <- input$pdfPreviewSizes; saveUISettings() })

  observeEvent(currentFile(), { writeLines(currentFile(), lastFileFile) })

  updateStatus <- function(txt) { session$sendCustomMessage("updateStatus", txt) }
  appendLog <- function(msg) {
    newLog <- paste0(compileLog(), msg, "\n")
    compileLog(newLog)
    updateAceEditor(session, "consoleEditor", value = newLog)
  }
  appendDockerLog <- function(msg) {
    newLog <- paste0(dockerLog(), msg, "\n")
    dockerLog(newLog)
    updateAceEditor(session, "dockerConsole", value = newLog)
    session$sendCustomMessage("scrollDockerConsole", "")
  }
  refreshCompiled <- function() { rv_compiled(list.files(compiledDir)) }

  ### FILE LISTS ###
  # Recursive file tree with actions for files and folders.
  buildFileTree <- function(dirPath = "") {
    fullDirPath <- file.path(uploadDir, dirPath)
    items <- list.files(fullDirPath, full.names = FALSE, include.dirs = TRUE, recursive = FALSE)
    if (length(items) == 0) return(NULL)

    li_elements <- lapply(items, function(item) {
      itemPath <- if (dirPath == "") item else file.path(dirPath, item)
      fullItemPath <- file.path(uploadDir, itemPath)
      isDir <- file.info(fullItemPath)$isdir
      safeId <- gsub("[^A-Za-z0-9]", "_", itemPath)

      if (isDir) {
        # Folder node: folder-name with toggle icon on left; folder actions on right.
        folderNode <- tags$div(
          class = "folder-item",
          `data-path` = itemPath,
          draggable = "true",
          tags$div(class = "folder-name",
                   tags$i(class = "fas fa-caret-right toggle-icon"),
                   icon("folder"), " ", item
          ),
          tags$div(class = "folder-actions",
                   actionLink(paste0("download_", safeId), icon("download"), title = "Download Folder"),
                   actionLink(paste0("delete_", safeId), icon("trash"), title = "Delete Folder"),
                   actionLink(paste0("move_", safeId), icon("arrows-alt"), title = "Move Folder"),
                   actionLink(paste0("rename_", safeId), icon("pencil"), title = "Rename Folder")
          )
        )
        childTree <- buildFileTree(itemPath)
        tags$li(
          folderNode,
          if (!is.null(childTree)) tags$ul(class = "child-tree", style = "display:none;", childTree)
        )
      } else {
        # File node: file name (with icon) on left; file actions on right.
        fileNode <- tags$div(
          class = "file-item",
          `data-path` = itemPath,
          draggable = "true",
          tags$div(class = "file-name", getFileIcon(item), " ", item),
          tags$div(class = "file-actions",
                   if (tolower(tools::file_ext(item)) %in% text_extensions)
                     actionLink(paste0("edit_", safeId), icon("edit"), title = "Edit"),
                   actionLink(paste0("preview_", safeId), icon("eye"), title = "Preview"),
                   actionLink(paste0("download_", safeId), icon("download"), title = "Download"),
                   actionLink(paste0("delete_", safeId), icon("trash"), title = "Delete"),
                   actionLink(paste0("move_", safeId), icon("arrows-alt"), title = "Move"),
                   actionLink(paste0("rename_", safeId), icon("pencil"), title = "Rename")
          )
        )
        tags$li(fileNode)
      }
    })
    return(li_elements)
  }

  output$fileListSidebar <- renderUI({
    dummy <- rv_files()

    fileTree <- buildFileTree()
    if (is.null(fileTree)) {
      p("No files in project.")
    } else {
      tags$div(
        tags$ul(class = "file-tree", fileTree)
      )
    }
  })

  output$compiledFileList <- renderUI({
    files <- rv_compiled()
    allowed_compiled <- c("output.pdf", "output.aux", "output.bbl", "output.log", "output.out", "output.blg", "output.synctex.gz")
    files_to_show <- intersect(files, allowed_compiled)
    if (length(files_to_show) == 0) {
      p("No compiled files.")
    } else {
      tagList(
        lapply(files_to_show, function(fileName) {
          fileId <- gsub("[^A-Za-z0-9]", "_", fileName)
          div(class = "file-list-item",
              div(class = "file-name", fileName),
              div(class = "file-actions",
                  actionLink(paste0("preview_compiled_", fileId), icon("eye"), title = "Preview"),
                  actionLink(paste0("download_compiled_", fileId), icon("download"), title = "Download"),
                  actionLink(paste0("delete_compiled_", fileId), icon("trash"), title = "Delete")
              )
          )
        })
      )
    }
  })

  # Observers for compiled file actions
  observe({
    files <- rv_compiled()
    lapply(files, function(fileName) {
      local({
        fileId <- gsub("[^A-Za-z0-9]", "_", fileName)
        observeEvent(input[[paste0("preview_compiled_", fileId)]], {
          showModal(modalDialog(
            title = paste("Preview", fileName),
            tags$iframe(style = "width:100%; height:400px;", src = file.path("compiled", fileName)),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("download_compiled_", fileId)]], {
          showModal(modalDialog(
            title = paste("Download", fileName),
            p("Click the link below to download the file:"),
            tags$a(href = file.path("compiled", fileName), fileName, download = fileName, target = "_blank"),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("delete_compiled_", fileId)]], {
          showModal(modalDialog(
            title = "Confirm Delete",
            paste("Are you sure you want to delete", fileName, "?"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(paste0("confirmDelete_compiled_", fileId), "Delete", class = "btn btn-danger")
            )
          ))
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("confirmDelete_compiled_", fileId)]], {
          fpath <- file.path(compiledDir, fileName)
          if (file.exists(fpath)) {
            file.remove(fpath)
            refreshCompiled()
          }
          removeModal()
        }, ignoreInit = TRUE)
      })
    })
  })

  output$mainFileSelect <- renderUI({
    texFiles <- list.files(uploadDir, pattern = "\\.tex$", ignore.case = TRUE, recursive = TRUE)
    if (length(texFiles) > 0) {
      selectInput("compileMainFile", "Select Main .tex File", choices = texFiles,
                  selected = if (currentFile() %in% texFiles) currentFile() else texFiles[1])
    } else {
      p("No .tex files available")
    }
  })

  observeEvent(input$compileMainFile, {
    req(input$compileMainFile)
    filePath <- file.path(uploadDir, input$compileMainFile)
    if (file.exists(filePath)) {
      content <- paste(readLines(filePath, warn = FALSE), collapse = "\n")
      updateAceEditor(session, "sourceEditor", value = I(content))
      currentFile(input$compileMainFile)
      updateStatus(paste0(input$compileMainFile, " loaded."))
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$uploadFiles, {
    req(input$uploadFiles)
    for (i in seq_len(nrow(input$uploadFiles))) {
      destPath <- file.path(uploadDir, input$uploadFiles$name[i])
      file.copy(input$uploadFiles$datapath[i], destPath, overwrite = TRUE)
    }
    rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
  })

  observeEvent(input$newFile, {
    showModal(modalDialog(
      title = "Create New File",
      textInput("newFileName", "File Name (without extension)"),
      selectInput("newFileType", "File Type",
                  choices = c("LaTeX (.tex)" = ".tex", "BibTeX (.bib)" = ".bib", "Text (.txt)" = ".txt")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createNewFile", "Create", class = "btn btn-primary")
      )
    ))
  })

  observeEvent(input$createNewFile, {
    req(input$newFileName, input$newFileType)
    new_name <- paste0(input$newFileName, input$newFileType)
    new_path <- file.path(uploadDir, new_name)
    if (file.exists(new_path)) {
      showModal(modalDialog(
        title = "Error",
        paste("File", new_name, "already exists."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      writeLines("", new_path)
      rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
      updateAceEditor(session, "sourceEditor", value = "")
      currentFile(new_name)
      updateStatus(paste0(new_name, " (saved)"))
      removeModal()
    }
  })

  observeEvent(input$newFolder, {
    showModal(modalDialog(
      title = "Create New Folder",
      textInput("newFolderName", "Folder Name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createNewFolder", "Create", class = "btn btn-primary")
      )
    ))
  })

  observeEvent(input$createNewFolder, {
    req(input$newFolderName)
    new_folder_path <- file.path(uploadDir, input$newFolderName)
    if (dir.exists(new_folder_path) || file.exists(new_folder_path)) {
      showModal(modalDialog(
        title = "Error",
        paste("Folder", input$newFolderName, "already exists."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      dir.create(new_folder_path)
      rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
      removeModal()
    }
  })

  # Observers for file and folder actions
  observe({
    items <- rv_files()
    lapply(items, function(item) {
      local({
        safeId <- gsub("[^A-Za-z0-9]", "_", item)
        fullPath <- file.path(uploadDir, item)
        isDir <- file.info(fullPath)$isdir

        # For files: edit, preview, download
        if (!isDir && tolower(tools::file_ext(item)) %in% text_extensions) {
          observeEvent(input[[paste0("edit_", safeId)]], {
            content <- paste(readLines(fullPath, warn = FALSE), collapse = "\n")
            updateAceEditor(session, "sourceEditor", value = I(content))
            currentFile(item)
            updateStatus(paste0(item, " (loaded)"))
          }, ignoreInit = TRUE)
        }
        if (!isDir) {
          observeEvent(input[[paste0("preview_", safeId)]], {
            if (tolower(tools::file_ext(item)) %in% text_extensions) {
              content <- paste(readLines(fullPath, warn = FALSE), collapse = "\n")
              showModal(modalDialog(
                title = paste("Preview", item),
                aceEditor("previewEditor", value = content, mode = "plain_text",
                          readOnly = TRUE, height = "400px", wordWrap = TRUE),
                easyClose = TRUE,
                footer = modalButton("Close")
              ))
            } else if (tolower(tools::file_ext(item)) %in% c("png", "jpg", "jpeg", "gif")) {
              showModal(modalDialog(
                title = paste("Preview", item),
                tags$img(src = file.path("uploads", item), style = "max-width:100%; height:auto;"),
                easyClose = TRUE,
                footer = modalButton("Close")
              ))
            } else if (tolower(tools::file_ext(item)) == "pdf") {
              showModal(modalDialog(
                title = paste("Preview", item),
                tags$object(data = file.path("uploads", item), type = "application/pdf", style = "width:100%; height:400px;"),
                easyClose = TRUE,
                footer = modalButton("Close")
              ))
            } else {
              showModal(modalDialog(
                title = paste("Preview", item),
                tags$a(href = file.path("uploads", item), "Download File", target = "_blank"),
                easyClose = TRUE,
                footer = modalButton("Close")
              ))
            }
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("download_", safeId)]], {
            showModal(modalDialog(
              title = paste("Download", item),
              p("Click the link below to download the file:"),
              tags$a(href = file.path("uploads", item), item, download = item, target = "_blank"),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }, ignoreInit = TRUE)
        }

        # For folders: download (zip), delete, move, rename actions
        if (isDir) {
          observeEvent(input[[paste0("download_", safeId)]], {
            folderPath <- file.path(uploadDir, item)
            zipFile <- file.path(uploadDir, paste0("folder_", safeId, ".zip"))
            if (file.exists(zipFile)) file.remove(zipFile)
            zip(zipfile = zipFile, files = list.files(folderPath, full.names = TRUE))
            showModal(modalDialog(
              title = paste("Download Folder", item),
              p("Click the link below to download the folder as a zip file:"),
              tags$a(href = file.path("uploads", basename(zipFile)), "Download Folder", target = "_blank"),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }, ignoreInit = TRUE)
        }

        # Delete action for both files and folders
        observeEvent(input[[paste0("delete_", safeId)]], {
          if (isDir) {
            folderContents <- list.files(fullPath)
            contentTag <- if (length(folderContents) > 0) {
              tags$div(
                tags$p("This folder contains:"),
                tags$ul(lapply(folderContents, function(x) { tags$li(x) }))
              )
            } else {
              tags$p("This folder is empty.")
            }
            showModal(modalDialog(
              title = paste("Confirm Delete", item),
              tagList(p(paste("Are you sure you want to delete the folder", item, "and all its contents?")), contentTag),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0("confirmDelete_", safeId), "Delete", class = "btn btn-danger")
              )
            ))
          } else {
            showModal(modalDialog(
              title = "Confirm Delete",
              paste("Are you sure you want to delete", item, "?"),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0("confirmDelete_", safeId), "Delete", class = "btn btn-danger")
              )
            ))
          }
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("confirmDelete_", safeId)]], {
          if (file.exists(fullPath)) {
            if (isDir) {
              unlink(fullPath, recursive = TRUE)
            } else {
              file.remove(fullPath)
            }
            rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
          }
          removeModal()
        }, ignoreInit = TRUE)

        # Move action for both files and folders
        observeEvent(input[[paste0("move_", safeId)]], {
          showModal(modalDialog(
            title = paste("Move", item),
            selectInput("destinationFolder", "Select Destination Folder",
                        choices = {
                          folders <- list.dirs(uploadDir, recursive = FALSE, full.names = FALSE)
                          c("Root" = "ROOT", folders)
                        },
                        selected = "ROOT"
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(paste0("confirmMove_", safeId), "Move", class = "btn btn-primary")
            )
          ))
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("confirmMove_", safeId)]], {
          dest <- input$destinationFolder
          oldPath <- fullPath
          newPath <- if (dest == "ROOT") file.path(uploadDir, basename(item))
          else file.path(uploadDir, dest, basename(item))
          if (dest != "ROOT" && !dir.exists(file.path(uploadDir, dest))) {
            showModal(modalDialog(
              title = "Error",
              paste("Destination folder", dest, "does not exist."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          } else {
            file.rename(oldPath, newPath)
            rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
            removeModal()
          }
        }, ignoreInit = TRUE)

        # Rename action for both files and folders
        observeEvent(input[[paste0("rename_", safeId)]], {
          showModal(modalDialog(
            title = paste("Rename", item),
            textInput("newName", "New Name (include extension if file)", value = basename(item)),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(paste0("confirmRename_", safeId), "Rename", class = "btn btn-primary")
            )
          ))
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("confirmRename_", safeId)]], {
          newName <- input$newName
          if (newName == "") {
            showModal(modalDialog(
              title = "Error",
              "New name cannot be empty.",
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          } else {
            dirPart <- dirname(item)
            newPath <- if (dirPart == ".") file.path(uploadDir, newName) else file.path(uploadDir, dirPart, newName)
            if (file.exists(newPath)) {
              showModal(modalDialog(
                title = "Error",
                paste("A file/folder with name", newName, "already exists."),
                easyClose = TRUE,
                footer = modalButton("Close")
              ))
            } else {
              file.rename(fullPath, newPath)
              rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
              removeModal()
            }
          }
        }, ignoreInit = TRUE)
      })
    })
  })

  output$bulkDownloadUploaded <- downloadHandler(
    filename = function() { paste("uploads-", Sys.Date(), ".zip", sep = "") },
    content = function(file) {
      files <- list.files(uploadDir, full.names = TRUE)
      if (length(files) > 0) { zip(file, files = files, flags = "-j") }
    },
    contentType = "application/zip"
  )

  output$bulkDownloadCompiled <- downloadHandler(
    filename = function() { paste("compiled-", Sys.Date(), ".zip", sep = "") },
    content = function(file) {
      files <- list.files(compiledDir, full.names = TRUE)
      if (length(files) > 0) { zip(file, files = files, flags = "-j") }
    },
    contentType = "application/zip"
  )

  observeEvent(input$bulkDeleteUploaded, {
    showModal(modalDialog(
      title = "Confirm Bulk Delete",
      "Are you sure you want to delete all uploaded files?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmBulkDeleteUploaded", "Delete All", class = "btn btn-danger")
      )
    ))
  })
  observeEvent(input$confirmBulkDeleteUploaded, {
    files <- list.files(uploadDir, full.names = TRUE)
    if (length(files) > 0) unlink(files, recursive = TRUE)
    rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
    removeModal()
  })

  observeEvent(input$bulkDeleteCompiled, {
    showModal(modalDialog(
      title = "Confirm Bulk Delete",
      "Are you sure you want to delete all compiled files?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmBulkDeleteCompiled", "Delete All", class = "btn btn-danger")
      )
    ))
  })
  observeEvent(input$confirmBulkDeleteCompiled, {
    files <- list.files(compiledDir, full.names = TRUE)
    if (length(files) > 0) file.remove(files)
    rv_compiled(list.files(compiledDir))
    removeModal()
  })

  compileFunction <- function() {
    if (is.null(input$compileMainFile) || input$compileMainFile == "") {
      appendLog("No main .tex file selected.")
      return(NULL)
    }
    compileLog("")
    dockerLog("")
    updateAceEditor(session, "consoleEditor", value = "")
    updateAceEditor(session, "dockerConsole", value = "")

    # === NEW: Clear the compiled folder before starting a new compile ===
    compiled_files <- list.files(compiledDir, full.names = TRUE)
    if (length(compiled_files) > 0) {
      unlink(compiled_files, recursive = TRUE)
    }

    # Show the compile spinner next to the button label
    session$sendCustomMessage("toggleCompileSpinner", TRUE)

    main_tex <- input$compileMainFile
    appendLog("Compilation started.")

    syncUploadsToCompiled <- function() {
      appendLog("Syncing project files to compiled folder...")
      files_to_copy <- list.files(uploadDir, full.names = TRUE)
      if (length(files_to_copy) > 0) {
        file.copy(files_to_copy, compiledDir, overwrite = TRUE, recursive = FALSE)
      }
      appendLog("Sync complete.")
    }
    syncUploadsToCompiled()

    docker_run_pdflatex <- function(texfile) {
      appendLog(paste("Running pdflatex on", texfile, "..."))
      proc <- processx::process$new("docker",
                                    args = c(
                                      "run", "--rm",
                                      "-v", paste0(normalizePath(compiledDir, winslash = "/"), ":/compiled"),
                                      "-w", "/compiled",
                                      "texlive/texlive",
                                      "pdflatex", "-interaction=nonstopmode", "-jobname=output", texfile
                                    ),
                                    stdout = "|", stderr = "|"
      )
      while(proc$is_alive()){
        new_output <- proc$read_output_lines()
        if (length(new_output) > 0) { for(line in new_output) { appendDockerLog(line) } }
        new_error <- proc$read_error_lines()
        if (length(new_error) > 0) { for(line in new_error) { appendDockerLog(line) } }
        Sys.sleep(0.1)
      }
      final_output <- proc$read_all_output_lines()
      final_error <- proc$read_all_error_lines()
      for(line in final_output) { appendDockerLog(line) }
      for(line in final_error) { appendDockerLog(line) }
      appendLog("pdflatex run complete.")
    }

    docker_run_bibtex <- function() {
      appendLog("Running bibtex on output...")
      proc <- processx::process$new("docker",
                                    args = c(
                                      "run", "--rm",
                                      "-v", paste0(normalizePath(compiledDir, winslash = "/"), ":/compiled"),
                                      "-w", "/compiled",
                                      "texlive/texlive",
                                      "bibtex", "output"
                                    ),
                                    stdout = "|", stderr = "|"
      )
      while(proc$is_alive()){
        new_output <- proc$read_output_lines()
        if (length(new_output) > 0) { for(line in new_output) { appendDockerLog(line) } }
        new_error <- proc$read_error_lines()
        if (length(new_error) > 0) { for(line in new_error) { appendDockerLog(line) } }
        Sys.sleep(0.1)
      }
      final_output <- proc$read_all_output_lines()
      final_error <- proc$read_all_error_lines()
      for(line in final_output) { appendDockerLog(line) }
      for(line in final_error) { appendDockerLog(line) }
      appendLog("bibtex run complete.")
    }

    docker_run_pdflatex(main_tex)
    docker_run_bibtex()
    docker_run_pdflatex(main_tex)
    docker_run_pdflatex(main_tex)

    pdfPath <- file.path(compiledDir, "output.pdf")
    if (!file.exists(pdfPath)) {
      appendLog("Compilation error: output.pdf not found.")
      session$sendCustomMessage("toggleCompileSpinner", FALSE)
      return(NULL)
    }

    appendLog("Compilation finished successfully.")
    refreshCompiled()

    updatePdfViewer <- function() {
      ts <- as.numeric(Sys.time())
      pdf_url <- paste0("compiled/output.pdf?t=", ts)
      output$pdfViewUI <- renderUI({
        tags$object(data = pdf_url, type = "application/pdf", style = "width:100%; height:75vh;")
      })
    }

    updatePdfViewer()
    later(updatePdfViewer, 5.0)

    # Hide the compile spinner after finishing
    session$sendCustomMessage("toggleCompileSpinner", FALSE)
  }

  observeEvent(input$compile, { compileFunction() })

  observe({
    pdfPath <- file.path(compiledDir, "output.pdf")
    if (file.exists(pdfPath)) {
      output$pdfViewUI <- renderUI({
        tags$object(data = "compiled/output.pdf", type = "application/pdf", style = "width:100%; height:75vh;")
      })
    }
  })

  observeEvent(input$editorTheme, {
    updateAceEditor(session, "sourceEditor", theme = input$editorTheme)
    updateAceEditor(session, "consoleEditor", theme = input$editorTheme)
    updateAceEditor(session, "dockerConsole", theme = input$editorTheme)
  })

  autoSaveSource <- debounce(reactive(input$sourceEditor), 1000)

  observeEvent(autoSaveSource(), {
    req(currentFile())
    filePath <- file.path(uploadDir, currentFile())
    writeLines(autoSaveSource(), filePath)
    updateStatus(paste0(currentFile(), " auto-saved at ", format(Sys.time(), "%H:%M:%S")))
    showNotification("Changes saved", duration = 1, type = "message")
  }, ignoreInit = TRUE)

  observeEvent(compileLog(), { writeLines(compileLog(), compileLogFile) })
  observeEvent(dockerLog(), { writeLines(dockerLog(), dockerLogFile) })
  observeEvent(input$editorTheme, { writeLines(input$editorTheme, themeFile) })

  # Observer for drag and drop move events
  observeEvent(input$dragDropMove, {
    req(input$dragDropMove$source, input$dragDropMove$target)
    sourcePath <- file.path(uploadDir, input$dragDropMove$source)
    targetFolder <- file.path(uploadDir, input$dragDropMove$target)
    if (!dir.exists(targetFolder)) {
      showNotification("Target folder does not exist", type = "error")
      return()
    }
    newPath <- file.path(targetFolder, basename(sourcePath))
    if (file.exists(newPath) || dir.exists(newPath)) {
      showNotification("A file/folder with the same name exists in the target folder.", type = "error")
      return()
    }
    file.rename(sourcePath, newPath)
    rv_files(list.files(uploadDir, recursive = TRUE, include.dirs = TRUE))
    showNotification("Item moved successfully.", type = "message")
  })
}
