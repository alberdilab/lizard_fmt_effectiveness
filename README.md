# lizard_fmt_effectiveness
Code repository for the lizard faecal microbiota transplant effectiveness study

[alberdilab.github.io/lizard_fmt_effectiveness](https://alberdilab.github.io/lizard_fmt_effectiveness)

While the webbook provides a user-friendly overview of the procedures, analyses can be directly reproduced using the Rmd documents. Note that the code chunks that require heavy computation have been tuned off using 'eval=FALSE'. To re-render the webbook, you can use the following code:

```
library(bookdown)
library(htmlwidgets)
library(webshot)

render_book(input = ".", output_format = "bookdown::gitbook", output_dir = "docs")

```
