
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
url1<-url2<-""
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

load("data_for_plots.RData")
failure_loc_zip <- ungroup(failure_loc_zip)

result_text <- as.data.frame(cbind(c("P","F","PRS","ABA","ABR","R"),c("Pass","Fail","Pass with Rectification within one hour", "Abandon Test", "Abort Test", "Refusal to Test")),stringsAsFactors = FALSE)
colnames(result_text) <- c("Abbreviation","Explanation")
testtype_text <- as.data.frame(cbind(c("All","N","F","PM","PR","RF","PL"),c("All tests","Full initial test","Full retest","Partial retest (repaired offsite)", "Partial retest (repaired onsite)","Refusal to test","Partial retest after leaving")),stringsAsFactors = FALSE)
colnames(testtype_text) <- c("Abbreviation","Explanation")
vehicleclass_text <- as.data.frame(cbind(c(1,2,3,4,"4A",5,"5A",7),c("Motorcycles up to 200cm³","All other motorcycles","3 wheeled vehicles with less than 450kg unladen weight","Cars, Taxis, Ambulances, Goods vehicles. All with at most 12 seats.","Like 4 with additional seat belt checks","Vehicles with more than 12 seats","Like 5 with aditional seat belt checks","Goods vehicles over 3tons up to and including 3.5t DGW")),stringsAsFactors = FALSE)
colnames(vehicleclass_text) <- c("Abbreviation","Explanation")
mycolours <- c("highlight" = "red", "normal" = "black","F" = "black","N" = "blue","PL" = "yellow","PM" = "orange", "PR" = "darkgreen","RF"="darkblue")
mysizes <- c("highlight" = 3,"normal" = 1,"F"=1,"N"=1,"PL" = 1,"PM" = 1, "PR"=1, "RF"=1)
mycolours2 <- c("F"="red","P"="green","PRS" = "darkgreen","ABA" = "blue","ABR" = "yellow", "R"="darkred")
second_plot <- ungroup(second_plot) %>%
  filter(!is.na(age))




shinyServer(function(input, output,session) {
  values <- reactiveValues(myurl = c(), parent_tab = "")
  observe({
    # make sure this is called on pageload (to look at the query string)
    # and whenever any tab is successfully changed.
    # If you want to stop running this code after the initial load was
    # successful so that further manual tab changes don't run this,
    # maybe just have some boolean flag for that.
    
    input$navbarid
    input$tab_sub_tabs
    query <- parseQueryString(session$clientData$url_search)
    url <- query$url
    if (is.null(url)) {
      url <- ""
    }
    
    # "depth" is how many levels the url in the query string is
    depth <- function(x) length(unlist(strsplit(x,"/")))
    
    # if we reached the end, done!
    if (length(values$myurl) == depth(url)) {
      return()
    }
    # base case - need to tell it what the first main nav name is
    else if (length(values$myurl) == 0) {
      values$parent_tab <- "navbarid"
    }
    # if we're waiting for a tab switch but the UI hasn't updated yet
    else if (is.null(input[[values$parent_tab]])) {
      return()
    }
    # same - waiting for a tab switch
    else if (tail(values$myurl, 1) != input[[values$parent_tab]]) {
      return()
    }
    # the UI is on the tab that we last switched to, and there are more
    # tabs to switch inside the current tab
    # make sure the tabs follow the naming scheme
    else {
      values$parent_tab <- paste0(tail(values$myurl, 1), "_tabs")
    }
    
    # figure out the id/value of the next tab
    new_tab <- unlist(strsplit(url, "/"))[length(values$myurl)+1]
    
    # easy peasy.
    updateTabsetPanel(session, values$parent_tab, new_tab)
    values$myurl <- c(values$myurl, new_tab)
  })
  
  output$testtypetext <- renderText({
    testtype_text$Explanation[testtype_text$Abbreviation==input$testtype]
  })
  output$resulttypetext <- renderText({
    result_text$Explanation[result_text$Abbreviation==input$resulttype]
  })
  output$resulttypetext2 <- renderText({
    result_text$Explanation[result_text$Abbreviation==input$testresult]
  })
  output$TableTest <- renderTable({
    testtype_text
  })
  output$TableResult <- renderTable({
    result_text
  })
  output$TableResult2 <- renderTable({
    result_text
  })
  output$TableVehicle <- renderTable({
    vehicleclass_text
  })
  output$vehicleclasstext <- renderText({
    vehicleclass_text$Explanation[vehicleclass_text$Abbreviation==input$vehicleclass]
  })

  output$plot1 <- renderPlot({
    
    if(input$testtype!="All"){
      dat <- failure_loc_zip%>%
        filter(type==input$testtype) %>%
        mutate(highlight = "normal")
    } else{
      dat <- failure_loc_zip %>%
        mutate(highlight = type)
    }
    
    result <- result_text$Explanation[result_text$Abbreviation==input$resulttype]
    
    total_number_of_tests <- dat%>%
      select(postcode,n) %>%
      group_by(postcode) %>%
      summarise(total=sum(n))
    data <- dat %>%
      left_join(total_number_of_tests, by="postcode") %>%
      filter(result == input$resulttype) %>%
      mutate(proportion = n / total) %>%
      mutate(highlight = ifelse(postcode == input$postcode,"highlight",highlight))

    ggplot(data,aes(log(n,10),proportion)) + theme_economist() + geom_point(aes(colour = highlight,size=highlight)) + ggtitle(paste0("Log(Number Tests per Area) vs Proportion of Test Result ", result)) + xlab("Log(Number Tests per Area") + ylab(paste0("Proportion of ", result, " of all Tests")) + scale_color_manual("Status",values = mycolours) + scale_size_manual("Status",values = mysizes)
      
      })
  
  output$plot2 <-renderPlot({
    
    if("All" == input$vehicleclass){
      make_switch<-input$makes
      brand_list <- makes
    } else if("1" == input$vehicleclass){
      make_switch<-input$makes1
      brand_list <- makes_1
    } else if("2" == input$vehicleclass){
      make_switch<-input$makes2
      brand_list <- makes_2
    } else if("3" == input$vehicleclass){
      make_switch<-input$makes3
      brand_list <- makes_3
    } else if("4" == input$vehicleclass){
      make_switch<-input$makes4
      brand_list <- makes_4
    } else if("4A" == input$vehicleclass){
      make_switch<-input$makes4a
      brand_list <- makes4a
    } else if("5" == input$vehicleclass){
      make_switch<-input$makes5
      brand_list <- makes5
    } else if("5A" == input$vehicleclass){
      make_switch<-input$makes5a
      brand_list <- makes_5a
    } else if("7" == input$vehicleclass){
      make_switch<-input$makes7
      brand_list <- makes7
    } 
    
    if(input$vehicleclass =="All"){
    dat <- second_plot 
    } else{
      dat <- second_plot %>%
        filter(testclassid==input$vehicleclass)
    }
    
    dat$make <- ifelse((dat$make %in% brand_list),dat$make,"Other")
    
    if(make_switch!="All"){
      dat <- dat %>%
        filter(make == make_switch)
    }
    
    
    
    total_number_tests <- dat %>%
      group_by(age) %>%
      summarise(total = sum(n))
    
    dat <- dat %>%
      select(age,result,n) %>%
      group_by(age,result) %>%
      summarise(sum = sum(n)) %>%
      left_join(total_number_tests, by = "age") %>%
      mutate(proportion = sum/total) %>%
      arrange(age,result) %>%
      select(-total)
    
    
    
    plot1 <- ggplot(dat,aes(x = age,y = log(sum,10))) + geom_bar(stat = "identity",aes(colour=factor(dat$result),fill=factor(dat$result))) + theme_economist() + ggtitle("Result types by age") + xlab("Age") + ylab("Log(number of results)") +  scale_fill_manual(values = mycolours2) + scale_color_manual(values = mycolours2) + theme(legend.title = element_blank())
    plot2 <- ggplot(dat,aes(x = age,y = proportion)) + geom_bar(stat = "identity",colour="grey",aes(fill=factor(dat$result))) + theme_economist() + ggtitle("Result type proportion by age") + xlab("Age") + ylab("Proportion of result type") +  scale_fill_manual(values = mycolours2)  + theme(legend.title = element_blank())#+ scale_color_manual(values = mycolours2)
    multiplot(plot1,plot2,cols=1)
  }) 

})
