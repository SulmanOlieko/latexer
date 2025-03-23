#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
  # Connectivity banner (initially hidden) with spinner and countdown
    div(id = "connectivityBanner", style = "display:none;",
        tags$span(id = "connectivitySpinner", class = "fa fa-sync fa-spin", style = "color:black; margin-right:5px;"),
        tags$span("Your internet connectivity is slow or down. Retrying sync in "),
        tags$span(id = "retryCountdown", "10"),
        tags$span(" seconds...")
    ),
    # Online banner (initially hidden) that appears when connection is restored
    div(id = "onlineBanner", style = "display:none; position: fixed; bottom: 0; left: 0; right: 0; background-color: rgba(0,128,0,0.7); color: #fff; text-align: center; padding: 5px; font-size: 12px; z-index: 1051;",
        "Yay, we're back online!"
    ),
    # Save notification banner for CMD+S/CTRL+S (initially hidden)
    div(id = "saveNotification", style = "display:none; position: fixed; top: 0; left: 0; right: 0; background-color: #337ab7; color: #fff; text-align: center; padding: 5px; font-size: 12px; z-index: 1052;",
        "No need to save manually – changes are automatically saved as you go!"
    ),

    tags$head(
      # Link to manifest for offline support
      tags$link(rel = "manifest", href = "manifest.json"),
      # Include Split.js for resizable panels
      tags$script(src = "https://unpkg.com/split.js/dist/split.min.js"),
      # Register the service worker for caching assets
      tags$script(HTML(" if ('serviceWorker' in navigator) {
        navigator.serviceWorker.register('sw.js')
          .then(function(registration) {
            console.log('Service Worker registration successful with scope: ', registration.scope);
          })
          .catch(function(err) {
            console.log('Service Worker registration failed: ', err);
          });
      }
      ")),
      tags$style(HTML("                        /* Global Solarized Dark Theme */
          body {background-color:#002b36; color:#93a1a1; margin:0; padding:0; font-family:sans-serif;}
          #header {background:#002b36; color:#fff; padding:10px 20px; height:60px; display:flex; justify-content:space-between; align-items:center; border-bottom:2px solid #586e75;}
          #header h1 {margin:0; font-size:24px;}
          #mainArea {height:calc(100vh - 60px); display:flex; overflow:hidden;}

          /* Panels */
          #fileSidebar, #editorArea, #pdfPreview {height:100%; overflow-y:auto; padding:10px;}
          #fileSidebar {background:#073642; border-right:1px solid #586e75;}
          #editorArea {background:#073642; border-right:1px solid #586e75; position:relative;}
          #pdfPreview {background:#073642; display:flex; flex-direction:column; padding:0; position:relative;}
          #pdfPreview .pdf-header {padding:10px; background:#002b36; border-bottom:1px solid #586e75; color:#fff;}
          #pdfPreview .pdf-header h4 {margin:0;}
          #pdfContainer, #dockerConsoleContainer {overflow-y:auto;}

          /* Common Container Styles */
          .well, .shiny-input-container {background:#073642 !important; border:1px solid #586e75 !important; color:#93a1a1; padding:10px; box-shadow:none;}

          /* File Management (Original file-tree styling) */
          .file-tree, .file-tree ul {list-style-type:none; margin:0; padding-left:15px;}
          .file-tree li {margin:3px 0;}
          .file-item, .folder-item {display:flex; justify-content:space-between; align-items:center; padding:5px 10px; cursor:pointer; background:#073642; border:1px solid #586e75; color:#93a1a1;}
          .file-item:hover, .folder-item:hover {background:#586e75;}
          .toggle-icon {margin-right:5px;}

          /* Additional styling for file management containers */
          #fileSidebar .shiny-input-container {
            background-color: #073642;
            border: 1px solid #586e75;
            padding: 10px;
            color: #93a1a1;
          }
          .file-list {
            margin: 0;
            padding: 0;
          }
          .file-list-item {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 5px 0;
            border-bottom: 1px solid #586e75;
          }
          .file-name {
            flex-grow: 1;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }
          .file-actions {
            display: flex;
            gap: 5px;
          }

          /* Status Bar */
          #statusBar {padding:5px 10px; background:#073642; border-top:1px solid #586e75; font-size:12px; color:#fff;}

          /* Ace Editor Borders */
          .ace_editor {border:1px solid #586e75 !important;}

          /* Modal Dialog Styling */
          .modal-content {
            background-color: #073642;
            color: #93a1a1;
            border: 1px solid #586e75;
          }
          .modal-header {
            background-color: #002b36;
            color: #fff;
            border-bottom: 1px solid #586e75;
          }
          .modal-footer {
            background-color: #002b36;
            color: #fff;
            border-top: 1px solid #586e75;
          }
          .modal-body {
            background-color: #073642;
            color: #93a1a1;
          }
          .modal-title {
            color: #fff;
          }

          /* Override default Shiny notifications to display at the top center */
          .shiny-notification {
            top: 10px !important;
            bottom: auto !important;
            left: 50% !important;
            transform: translateX(-50%);
          }

          /* Connectivity banner styling - now at bottom, smaller and more transparent */
          #connectivityBanner {
            position: fixed;
            bottom: 0;
            left: 0;
            right: 0;
            background-color: rgba(255,204,0,0.7);
            color: #000;
            text-align: center;
            padding: 5px;
            font-size: 12px;
            z-index: 1050;
          }

          /* Custom Scrollbar Styling for general elements */
          ::-webkit-scrollbar {
            background-color: #002b36 !important;
          }
          ::-webkit-scrollbar-thumb {
            background-color: #002b36 !important;
          }
          ::-webkit-scrollbar-track {
            background-color: #002b36 !important;
          }
          * {
            scrollbar-color: #002b36 #002b36 !important;
          }
          /* Ace Editor specific scrollbar styling */
          .ace_scrollbar, .ace_scrollbar-v, .ace_scrollbar-h {
            background: #002b36 !important;
          }
        ")),
      tags$script(HTML("                        Shiny.addCustomMessageHandler('updateStatus', function(message) {
            document.getElementById('statusBar').innerText = message;
          });
          Shiny.addCustomMessageHandler('scrollDockerConsole', function(message) {
            var editor = ace.edit('dockerConsole');
            if (editor) {
              var session = editor.getSession();
              editor.scrollToLine(session.getLength(), true, true, function() {});
            }
          });
          Shiny.addCustomMessageHandler('applyUISettings', function(data) {
            if (data.editorConsoleVisible === false) {
              document.getElementById('consoleEditor').style.display = 'none';
              document.getElementById('toggleEditorConsoleItem').innerHTML = 'Show Editor Console';
            } else {
              document.getElementById('consoleEditor').style.display = 'block';
              document.getElementById('toggleEditorConsoleItem').innerHTML = 'Hide Editor Console';
            }
            if (data.dockerConsoleVisible === false) {
              document.getElementById('dockerConsoleContainer').style.display = 'none';
              document.getElementById('toggleDockerConsoleItem').innerHTML = 'Show Docker Console';
            } else {
              document.getElementById('dockerConsoleContainer').style.display = 'block';
              document.getElementById('toggleDockerConsoleItem').innerHTML = 'Hide Docker Console';
            }
          });
          Shiny.addCustomMessageHandler('toggleCompileSpinner', function(show) {
            if (show) {
              $('#compileSpinner').show();
            } else {
              $('#compileSpinner').hide();
            }
          });
          $(document).ready(function() {
            $(document).on('click', '.folder-name', function(e) {
              var childTree = $(this).closest('li').find('ul.child-tree').first();
              childTree.slideToggle(100);
              $(this).find('.toggle-icon').toggleClass('fa-caret-right fa-caret-down');
              e.stopPropagation();
            });
            Split(['#fileSidebar', '#editorArea', '#pdfPreview'], {sizes: [20,45,35], minSize: [150,200,200], gutterSize:8, cursor:'col-resize'});
            Split(['#sourceEditor', '#consoleEditor'], {
              direction:'vertical',
              sizes:[80,20],
              minSize:[100,50],
              gutterSize:8,
              cursor:'row-resize',
              onDragEnd: function(sizes){
                Shiny.setInputValue('editorAreaSizes', sizes);
                ace.edit('sourceEditor').resize();
                ace.edit('consoleEditor').resize();
              }
            });
            Split(['#pdfContainer', '#dockerConsoleContainer'], {
              direction:'vertical',
              sizes:[80,20],
              minSize:[100,50],
              gutterSize:8,
              cursor:'row-resize',
              onDragEnd: function(sizes){
                Shiny.setInputValue('pdfPreviewSizes', sizes);
              }
            });

            // Connectivity event listeners with spinner and dynamic countdown
            var connectivityTimer;
            var baseCountdown = 10;
            var currentCountdown = baseCountdown;

            function startConnectivityTimer() {
              if (connectivityTimer) return;
              document.getElementById('connectivityBanner').style.display = 'block';
              currentCountdown = baseCountdown;
              document.getElementById('retryCountdown').innerText = currentCountdown;
              connectivityTimer = setInterval(function() {
                currentCountdown--;
                if (currentCountdown <= 0) {
                  baseCountdown += 5;
                  currentCountdown = baseCountdown;
                }
                document.getElementById('retryCountdown').innerText = currentCountdown;
              }, 1000);
            }

            function stopConnectivityTimer() {
              if (connectivityTimer) {
                clearInterval(connectivityTimer);
                connectivityTimer = null;
              }
              baseCountdown = 10;
              currentCountdown = baseCountdown;
              document.getElementById('retryCountdown').innerText = '';
            }

            window.addEventListener('offline', function(e) {
              startConnectivityTimer();
            });

            window.addEventListener('online', function(e) {
              stopConnectivityTimer();
              document.getElementById('connectivityBanner').style.display = 'none';
              // Show the online banner for 5 seconds
              var onlineBanner = document.getElementById('onlineBanner');
              onlineBanner.style.display = 'block';
              setTimeout(function(){ onlineBanner.style.display = 'none'; }, 5000);
            });

            if (!navigator.onLine) {
              startConnectivityTimer();
            }

            // Listen for CMD+S / CTRL+S and show save notification instead of manual saving
            document.addEventListener('keydown', function(e) {
              if ((e.ctrlKey || e.metaKey) && e.keyCode === 83) {
                e.preventDefault();
                var saveNoti = document.getElementById('saveNotification');
                if (saveNoti) {
                  saveNoti.style.display = 'block';
                  setTimeout(function(){ saveNoti.style.display = 'none'; }, 3000);
                }
              }
            });
          });
        ")),
      # New script block for drag and drop functionality
      tags$script(HTML("
        $(document).on('dragstart', '.file-item, .folder-item', function(e) {
          e.originalEvent.dataTransfer.setData('text/plain', $(this).attr('data-path'));
        });
        $(document).on('dragover', '.folder-item', function(e) {
          e.preventDefault();
        });
        $(document).on('drop', '.folder-item', function(e) {
          e.preventDefault();
          var sourcePath = e.originalEvent.dataTransfer.getData('text/plain');
          var targetPath = $(this).attr('data-path');
          if (sourcePath === targetPath) return;
          Shiny.setInputValue('dragDropMove', { source: sourcePath, target: targetPath, nonce: Math.random() });
        });
      "))
    ),
    # Header
    div(id = "header",
        div(style = "display: flex; align-items: center;",
            tags$img(src = "https://raw.githubusercontent.com/SulmanOlieko/latexer/master/latexer-sticker.png",
                     style = "height:40px; width:auto; margin-right:10px;"),
            h1("LaTeXeR", style="margin: 0;")
        ),
        div(
          actionButton("compile", HTML("Compile <span id='compileSpinner' style='display:none;'><i class='fa fa-spinner fa-spin'></i></span>"), class = "btn btn-success"),
          div(style = "display:inline-block; width:150px; margin-left:10px;",
              selectInput("editorTheme", "",
                          choices = c("tomorrow", "monokai", "github", "solarized_dark", "solarized_light"),
                          selected = "solarized_dark", selectize = FALSE)
          )
        )
    ),
    # Main Area
    div(id = "mainArea",
        # File Sidebar (updated with new behavior)
        div(id = "fileSidebar",
            wellPanel(
              uiOutput("mainFileSelect")
            ),
            wellPanel(
              # Icons on the right: New File & New Folder inside Upload Files container
              div(style = "display: flex; gap: 10px; margin-bottom: 10px; justify-content: flex-end;",
                  actionLink("newFile", label = icon("file"), title = "New File"),
                  actionLink("newFolder", label = icon("folder-plus"), title = "New Folder")
              ),
              fileInput("uploadFiles", "Upload Files", multiple = TRUE,
                        accept = c(
                          ".tex", ".bib", ".bst", ".cls", ".cfg", ".sty", ".txt", ".rnw",
                          ".dta", ".csv", ".xls", ".xlsx", ".doc", ".docx", ".ppt", ".pptx",
                          ".rdata", ".rds",
                          ".zip", ".tar", ".gz", ".7z",
                          ".png", ".jpg", ".jpeg", ".gif", ".pdf",
                          ".html", ".htm", ".js", ".css", ".xml", ".json", ".md", ".sql",
                          ".py", ".c", ".cpp", ".java", ".sh",
                          ".mp3", ".mp4", ".mov", ".avi", ".mkv",
                          ".odt", ".ods", ".odp", ".rtf"
                        ))
            ),
            wellPanel(
              h4("Project Files"),
              div(class = "bulk-actions",
                  downloadButton("bulkDownloadUploaded", "Download All", class = "btn btn-default"),
                  actionButton("bulkDeleteUploaded", "Delete All", class = "btn btn-danger")
              ),
              # Recursive file tree with actions for files and folders.
              # (Ensure that the UI generating these items wraps file names in a container with class \"file-name\"
              # and the icons in a container with class \"file-actions\" so that the above CSS takes effect.)
              uiOutput("fileListSidebar")
            ),
            wellPanel(
              h4("Compiled Files"),
              div(class = "bulk-actions",
                  downloadButton("bulkDownloadCompiled", "Download All", class = "btn btn-default"),
                  actionButton("bulkDeleteCompiled", "Delete All", class = "btn btn-danger")
              ),
              uiOutput("compiledFileList")
            )
        ),
        # Editor Area
        div(id = "editorArea",
            div(id = "statusBar", style="height:40px; padding:5px 10px;", "No file selected"),
            div(id = "editorSplit", style="height: calc(100% - 40px);",
                aceEditor("sourceEditor", value = "", mode = "latex", height = "100%", fontSize = 14, wordWrap = TRUE, autoComplete = "live"),
                aceEditor("consoleEditor", value = "", mode = "text", readOnly = TRUE, height = "100%", fontSize = 12, wordWrap = TRUE)
            )
        ),
        # PDF Preview Area with Docker console below
        div(id = "pdfPreview",
            div(class = "pdf-header", h4("PDF Preview")),
            div(id = "pdfContainer", uiOutput("pdfViewUI")),
            div(id = "dockerConsoleContainer",
                aceEditor("dockerConsole", value = "", mode = "text", readOnly = TRUE, height = "100%", fontSize = 12, wordWrap = TRUE)
            )
        )
    )
  )
}
