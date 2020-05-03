This has been deployed into shinyapps.io.

https://kensin.shinyapps.io/Covid-19/



1. tags utilization  
https://blog.csdn.net/u014801157/article/details/47111651


2. Why need to use reactive
https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/

3. Common layout, inpout and output of Shiny
https://pauke.live/post/shiny-app%E5%88%B6%E4%BD%9C%E5%9F%BA%E6%9C%AC%E6%80%9D%E8%B7%AF/

3. ?functionName or help(functionName) or ??functionName, this one search the Internet

4. 
The www directory is the directory expected by a Shiny application to locally store the elements that will be rendered in the web browser and are not the output of the scripts. 
This includes images, HTML, .js files, and so on. 
This directory must be at the same level as UI.R and server.R, that is, on the application's main folder.

5. reactive VS observe
https://pauke.live/post/shiny-app%E5%88%B6%E4%BD%9C%E5%9F%BA%E6%9C%AC%E6%80%9D%E8%B7%AF/
reactive has return value, but observe has no return value.
var = reactive({})  -> var can be regarded as global variable
observe({var}) -> var is just variable and its lifetime is only among braces

eventReactive Vs observeEvent
Same reason

reactiveValues as init value
https://shiny.programmingpedia.net/en/tutorial/10787/reactive-reactivevalue-and-eventreactive-observe-and-observeevent-in-shiny

6. Packrat is a dependency management system for R
Still has a challanging how to compatible with R server. maybe new R sever compatabile for the latest package version. 
So using docker image is better
https://rstudio.com/resources/webinars/managing-package-dependencies-in-r-with-packrat/ => video to see
http://rstudio-pubs-static.s3.amazonaws.com/221948_fb7215fecb0d49ac903f701fd8d45132.html
https://github.com/rstudio/packrat

7. 
packrat::init(infer.dependencies = FALSE)      => just a blank packrat project
install.packages("packagename")                => normally install your need package
should see the package has been installed, try to run the project
packrat::snapshot(infer.dependencies = FALSE)  => save the package intalled on the private library

Note: do not use if(!require("package")), it still points the global library

8.
LeafLet
https://blog.gtwang.org/r/r-leaflet-interactive-map-package-tutorial/ 
http://rstudio.github.io/leaflet/map_widget.html
=> leaflet(df) %>% addTitle() %>% addCircles(lng = ~Long, lat = ~Lat) ~ meaning Long or Lat from df

9. How to combine front and server with JS
https://shiny.rstudio.com/articles/js-send-message.html

