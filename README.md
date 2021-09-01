## Image Vizualization and Analysis of Reddit Data

The early 2010s began a period of internet-based activism that was immediately embraced by much of mainstream liberalism without any rigorous analysis or appraisal of the organizational structure and limitations of these internet-based movements (Nagle, 2017). Image based web forums such as Reddit, 4chan and 8chan were the basis of many of these movements.

## Installation

ImagePlotting is not yet on the CRAN but can be installed with:

``` r
devtools::install_github("https://github.com/dungates/ImagePlotting")
```

## File Structure

 - Data: All data retrieved using code in the import folder can be found here.
 
 - Import: All code used to import images and collect metadata.
 
 - Analysis: Single function ala Qtip to conduct analysis for all but stack features. Set good defaults, but allow passage of arguments to other underlying functions like those in color distance.
 
 - Output: There are three output models: ImagePlot, Image Montage, and ggplot2 with geom_image. Subsequent visual analyses including histograms and gganimate.

 
 - Images: Images used in analysis (way too much for data folder so condensed as a zip?)

## Citations

  
1. Schonig J. “Liking” as creating: On aesthetic category memes. New Media & Society. 2020;22(1):26-48. doi:10.1177/1461444819855727

2. Angela Nagle. 2017. Kill All Normies: Online Culture Wars From 4Chan And Tumblr To Trump And The Alt-Right. Zero Books, Alresford, GBR.

3. 


**Package Dependencies**

- [ImageFluency](https://github.com/stm/imagefluency)

- [trackdem](https://github.com/marjoleinbruijning/trackdem)

- [ColorDistance](https://cran.r-project.org/web/packages/colordistance/vignettes/colordistance-introduction.html)

- [OpenImageR](https://cran.r-project.org/web/packages/OpenImageR/vignettes/The_OpenImageR_package.html)

- [ggimage](https://guangchuangyu.github.io/2017/04/ggimage/)

- [Tidyverse](https://www.tidyverse.org/)
