# So swirl does not repeat execution of plot commands

# Returns TRUE if e$val is identical to the value that would
# have been created by the correct expression.
creates_val_identical_to <- function(correctExpr){
  e <- get("e", parent.frame())
  correctVal <- eval(parse(text=correctExpr), cleanEnv(e$snapshot))
  results <- expectThat(e$val,
                        is_identical_to(correctVal, label=correctExpr),
                        label=deparse(e$expr))
  return(results$passed)
}


# Returns TRUE if e$expr matches any of the expressions given
# (as characters) in the argument.
ANY_of_exprs <- function(...){
  e <- get("e", parent.frame())
  any(sapply(c(...), function(expr)omnitest(expr)))
}

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Get the value which a user either entered directly or was computed
# by the command he or she entered.
getVal <- function(){
  getState()$val
}

# Get the last expression which the user entered at the R console.
getExpr <- function(){
  getState()$expr
}

#######################################################################################
# Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

# take care of subbmition
submition <- function() {
  
  # getting envirament
  e <- get("e", parent.frame())
  #EE <<- e
  #print (EE)
  
  # is answar is No - than it is OK - doing nothing
  if(e$val == "No") return(TRUE)
  
  if (e$skipped){
    msg = sprintf("It seems that you have skipped some questions, namely %d.\nIt is highly recommended to submit result only then you finished it completely.", e$skips)
    message(msg)
    message("Do you want to proceed anyway?")
    procced <- select.list(c("Yes", "No"), graphics = FALSE)    
    if (procced!='Yes'){
      message("Submission is aborted. Have a nice day and try next time.")      
    }
  }  
  
  good <- FALSE
  while(!good) {
    
    # Get info
    name <- readline_clean("What is your name assosiated with the course?")
    takeTime <- readline_clean("How much time did you spend for this lesson? Give your time estimate expressed in minutes.")
    takeTime <- as.numeric(takeTime)
    while (!is.finite(takeTime)){
      message("Given time is not a number! Pleas try again.")
      takeTime <- readline_clean("How much time did you spend for this lesson? Give your time estimate expressed in minutes.")
      takeTime <- as.numeric(takeTime)
    }
    
    # Repeat back to them
    message("\nPlease, check your name. If it is misspecified you might lose your work. Does everything look good?\n")
    message("Your name: ", name)
    
    yn <- select.list(c("Yes", "No"), graphics = FALSE)
    if(yn == "Yes") good <- TRUE
  }
  
  sysInfo = Sys.info()
  sysUser = sysInfo["user"]
  sysName = sysInfo["sysname"]
  sysTime = Sys.time()
  
  encoded_log = submit_log(name=name, takeTime=takeTime, sysUser=sysUser, sysName=sysName, sysTime=sysTime)
  
  hrule()
  message("I just tried to open your bowser and prepare info for submition. If its OK, submit the form.")
  message("\nIf it failed, please save the string below and report the problem.\n")
  message(encoded_log)
  hrule()
  
  # Return TRUE to satisfy swirl and return to course menu
  TRUE
}

submit_log <- function(...){
  
  # Please edit the link below
  pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSdxcG4Ia2nfRS-t0fiX9ShuWc1AjX2YDRr-INvVIIdzqKbOug/viewform?entry.1707857444"
  
  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }
  
  # ilit log list
  log_ = list(...)
  
  # update with actual log
  temp <- tempfile()    
  log_pure <- getLog()
  log_[names(log_pure)] = log_pure
  
  # save
  saveRDS(log_, file =temp)
  encoded_log = base64encode(temp)
  browseURL(paste0(pre_fill_link, encoded_log))
  return(encoded_log)
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}