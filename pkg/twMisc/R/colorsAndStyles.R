twXlscol07 <- function(){ 
    ### Returns 10 Colours from Excel2007 are quite colour-blind safe
    ##details<< \describe{\item{Providing standard colors functionality}{ \itemize{
    ## \item \code{twXlscol07} (this function): Returns 10 Colours from Excel2007 are quite colour-blind safe
    ## \item \code{\link{twXlscol}}: Returns a list of colours corresponding to Excel, that are well distinguishable 
    ## \item \code{\link{twContCols16}}: Return a character vector of green to magenta colorblind safe continuos scale 
    ## \item \code{\link{twLtys}}: Returns a character vector line types, that are well distinguishable.
    ## \item \code{\link{fColConv}}: Encode colors to avoid symbol font, which gives problems in pdf-Output 
    ## }}}
    #
    ##seealso<< \code{\link{twMisc}}
    list( blue="#4572A7", red="#AA4643", green="#89A54E", violet="#71588F"
            , seablue="#4198AF", orange="#DB843D", lightblue="#93A9CF", pink="#D19392"
            , lightgreen="#B9CD96", lightviolet="#A99BBD" ) 
}
attr(twXlscol07,"ex") <- function(){
    barplot(structure(rep(1,10),names=names(twXlscol07())),col=unlist(twXlscol07()), horiz=TRUE, las=1 )
    #twBlindCol <- list(blue="#0072B2",orange="#D55E00",yellow="#F0E442",lightblue="#56B4E9",lightorange="#E69F00"
    #,pink="#CC79A7",green="#2B9F78",black="#000000")
    #barplot(structure(rep(1,8),names=names(twBlindCol)),col=unlist(twBlindCol), horiz=TRUE, las=1 )
}

fColConv <- function(
        ### color encoding to avoid symbol font, which gives problems in pdf-Output   
        cols  ##<<  list of colors
){
    ###details<< applying fColConv causes problems in emf output
    ##seealso<< \code{\link{twXlscol07}}, \code{\link{twMisc}}
    if( is.list(cols))
        lapply( cols, function(cols){ rgb(t(col2rgb((cols)))/255, alpha=0.99)} )
    else
        rgb(t(col2rgb((cols)))/255, alpha=0.99)
}

twXlscol <- function(){
    ### Returns a list of colours corresponding to Excel, that are well distinguishable
    ##seealso<< \code{\link{twXlscol07}}, \code{\link{twMisc}}
    localTwXlscol <- fColConv( list( blue="#000080", red="#800000", green="#008000", violet="#660066", brown="#666600", turquise="#006666", blueviolet="#330099", redviolet="#990033", greenbrown="#339900", redbrown="#993300", blueturquise="#003399", greenturquise="#009933" ))
    function() localTwXlscol
}


twLtys <- function(){
    ### Returns a character vector line types, that are well distinguishable.
    ##seealso<< \code{\link{twXlscol07}}, \code{\link{twMisc}}
    c("22", "44", "13", "1343", "73", "2262", "12223242", "F282", "F4448444", "224282F2", "F1")
}

twContCols16 <- function(){
    ### Return a character vector of green to magenta colorblind safe continuos scale
    ##details<<
    ## see  http://geography.uoregon.edu/datagraphics/color_scales.htm
    ##seealso<< \code{\link{twXlscol07}}, \code{\link{twMisc}}
    c(
            rgb(0,80,0, maxColorValue=255)
            , rgb(0,134,0, maxColorValue=255)
            , rgb(0,187,0, maxColorValue=255)
            , rgb(0,241,0, maxColorValue=255)
            , rgb(80,255,80, maxColorValue=255)
            , rgb(134,255,134, maxColorValue=255)
            , rgb(187,255,187, maxColorValue=255)
            , rgb(255,255,255, maxColorValue=255)
            , rgb(255,241,255, maxColorValue=255)
            , rgb(255,187,255, maxColorValue=255)
            , rgb(255,134,255, maxColorValue=255)
            , rgb(255,80,255, maxColorValue=255)
            , rgb(241,0,241, maxColorValue=255)
            , rgb(187,0,187, maxColorValue=255)
            , rgb(134,0,134, maxColorValue=255)
            , rgb(80,0,80, maxColorValue=255)
    )
}

