// show and then hide div.busy when shiny is "busy" loading leaflet map
// https://stackoverflow.com/questions/51774255/displaying-a-loading-spinner-on-a-map-when-updating-with-leafletproxy-in-shiny/52269825 
// https://stackoverflow.com/questions/26004302/shiny-app-busy-indicator
//
setInterval(function(){
    if ($('html').attr('class')=='shiny-busy') {
        setTimeout(function() {
            if ($('html').attr('class')=='shiny-busy') {
                $('div.busy').show();
            }
        }, 0);
    } else {
        $('div.busy').hide();
    }
}, 100);

