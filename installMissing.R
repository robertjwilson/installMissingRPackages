
# Load required packages --------------------------------------------------
require(tools);require(dplyr);require(stringr)

rm(list = ls())

installMissing <-
  function (dir.missing = "~", ask2install = FALSE, all.folders = TRUE) {
    # dir.missing = folder you want to check
    # ask2install = check whether you want to install individual packages
    # all.folder = look into subfolders or not
    all.dirs <-
      list.dirs(dir.missing, recursive = all.folders)  # Get all of the subfolders in the directory
    print("locating packages used in folder")
    # Loop through all of the subfolders....
    for (dd in all.dirs)
    {
      # Loop through all of the R files in the subfolder
      for (ff in list.files(dd)[file_ext(list.files(dd)) == "R"]) {
        linn = readLines(paste0(dd, "/",ff))    # Grab all of the lines of code in the R file
        #  Loop through the lines
        for (i in 1:length(linn)) {
          if (length(linn[i]) > 0)
            if (!is.na(linn[i]))
              if (str_detect(linn[i], "library\\(") |
                  str_detect(linn[i], "require\\(")) {
                # split the string up based on ;
                subline <- strsplit(linn[i], ";")[[1]]
                for (jj in 1:length(subline)) {
                  # First check if require or library is in the line of code
                  if (str_detect(subline[jj], "require") |
                      str_detect(subline[jj], "library")) {
                    # Now strip out the package called by require or library
                    # Currently this assumes code is of the form require(package), not require(package = package)
                    pack.name <- subline[jj] %>%
                      str_sub(
                        subline[jj] %>% str_locate(c("require", "library")) %>%
                          as.data.frame() %>%
                          select(end) %>%
                          filter(!is.na(end)) %>% slice(1) + 2, nchar(subline[jj])
                      )
                    
                    pack.name <- pack.name %>%
                      str_sub(
                        1,pack.name %>% str_locate(c(",", "\\)")) %>%
                          as.data.frame() %>%
                          select(end) %>%
                          filter(!is.na(end)) %>%
                          summarize(end = min(end)) - 1
                      ) %>%
                      str_replace_all('\\"', '') %>%
                      str_replace_all(" ", "")
                    # add the package to the list to be checked for installation
                    if (!exists("pack2install"))
                      pack2install <-
                      data_frame(Package = pack.name)
                    else
                      pack2install <-
                      bind_rows(data_frame(Package = pack.name), pack2install)
                  }
                }
              }
        }
      }
    }
    # Restrict the packages to stuff that is not installed already
    pack2install <- unique(pack2install) %>%
      dplyr::filter(Package %in% installed.packages() == FALSE)
    # Get a list of the packages that are not available on CRAN
    packunavail <- pack2install %>%
      dplyr::filter(Package %in% available.packages() == FALSE)
    
    # Ask if you want to print unavailable packages
    if (length(packunavail) > 0) {
      printUnavailable <-
        readline(prompt = "Do you want to list missing, but unavailable packages? (y/n)")
      if (printUnavailable == "y")
        for (i in 1:nrow(packunavail))
          print(as.character(packunavail[i,1]))
    }
    # Strip the unavilable packages from the packages to install
    pack2install <- unique(pack2install) %>%
      dplyr::filter(Package %in% installed.packages() == FALSE) %>%
      dplyr::filter(Package %in% available.packages() == TRUE)
    
    # Install the packages
    for (i in 1:nrow(pack2install))
    {
      if (ask2install)
        installPackage <-
          readline(prompt = paste(
            "Do you want to install?", as.character(pack2install[i,1]) , "(y/n)"
          ))
      else
        installPackage = "y"
      # Ideally the below will check if the package actually installed and track what is failing to install
      if (installPackage == "y")
        install.packages(as.character(pack2install[i,1]), quiet = FALSE)
    }
  }

# Example of function
installMissing(dir.missing = "~", ask2install = FALSE)
