Clinician's App
======

Shiny app for gathering clinician's predictions for patient worsening.

### Install & Run

In R, you'll need to install the following dependencies: 
```
install.packages(c("shiny", "RSQLite", "rjson", "jsonlite", "dlpyr"))
```

Then launch the app from R with either:
```
library(shiny)
runGithub("antoine-lizee/BIHack")
```

or cloning the repository (`git clone ...`) and running the downloaded code in its folder:
```
library(shiny)
runApp()
```

If you want to investigate further, turn on the `b_DEBUG` switch in the `global.R` file.

