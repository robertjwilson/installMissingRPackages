require(tools);require(magrittr);require(dplyr);require(stringr)

rm(list = ls())
setwd("~/Dropbox/")

installMissing <- function (dir.missing = "~", ask2install = FALSE) {
  all.dirs <- list.dirs(dir.missing)
  print("locating packages used in folder")
  for(dd in all.dirs)
  {
    for(ff in list.files(dd)[file_ext(list.files(dd))== "R"]){
      # We are now able to read through R files   
      linn=readLines(paste0(dd, "/",ff))
      
      for (i in 1:length(linn)){
        if(length(linn[i]) > 0)
          if(!is.na(linn[i]))
            if(str_detect(linn[i], "library\\(") | str_detect(linn[i], "require\\(")){
              # split the string up based on ;
              subline <- strsplit(linn[i], ";")[[1]]
              for(jj in 1:length(subline)){
                if(str_detect(subline[jj], "require") | str_detect(subline[jj], "library")){
                pack.name <- subline[jj] %>%
                  str_sub(subline[jj] %>%str_locate(c("require", "library")) %>%
                  as.data.frame() %>%
                  select(end) %>%
                  filter(!is.na(end)) %>% slice(1) + 2, nchar(subline[jj]))
  
                pack.name <- pack.name %>% 
                  str_sub(1,pack.name %>%str_locate(c(",", "\\)")) %>%
                            as.data.frame() %>%
                            select(end) %>%
                            filter(!is.na(end)) %>%
                            summarize(end = min(end))-1) %>%
                  str_replace_all('\\"', '') %>%
                  str_replace_all(" ", "")
                
                if(!exists("pack2install"))
                  pack2install <- data_frame(Package = pack.name) else
                    pack2install <- bind_rows(data_frame(Package = pack.name), pack2install)
        # We now have a package that should be installed
            }
          }
          
  
        }
  
      }
      
    }
  
  }
  
  
  pack2install <- unique(pack2install) %>%
    dplyr::filter(Package %in% installed.packages() == FALSE)

  packunavail <- pack2install %>%
    dplyr::filter(Package %in% available.packages() == FALSE)
  
  # print("Do you want to list missing, but unavailable packages")
  if(length(packunavail) > 0){
  printUnavailable <- readline(prompt="Do you want to list missing, but unavailable packages? (y/n)") 
  if(printUnavailable == "y")
    for(i in 1:nrow(packunavail))
      print(as.character(packunavail[i,1]))
      
  }
  pack2install <- unique(pack2install) %>%
    dplyr::filter(Package %in% installed.packages() == FALSE) %>%
      dplyr::filter(Package %in% available.packages() == FALSE)
  
  
  for(i in 1:nrow(pack2install))
  {
      if(ask2install)
        installPackage <- readline(prompt=paste("Do you want to install?", as.character(pack2install[i,1]) , "(y/n)")) else
          installPackage = "y"

     if(installPackage == "y")
        install.packages(as.character(pack2install[i,1]), quiet = TRUE)
  
    
  }
}


installMissing(dir.missing = "~", ask2install = TRUE)


