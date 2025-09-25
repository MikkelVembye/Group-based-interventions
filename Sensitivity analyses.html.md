---
title: "Sensitivity analyses for Group-Based Review"
author: "Mikkel H. Vembye"
subtitle: ""
date: "2025-09-25"
format:
  html: 
    keep-md: true
    self-contained: true
    grid: 
      margin-width: 350px
    code-fold: true
    code-summary: "Show the code"
    toc: true
    toc-location: left
pdf-engine: pdflatex
execute: 
  echo: true
  warning: false
  message: false
knitr:
  opts_chunk: 
    fig.pos: "H"
    fig.retina: 2
    cache: FALSE
    R.options:
      knitr.graphics.auto_pdf: true
      width: 100
      knitr.kable.NA: "-"
      dplyr.summarise.inform: FALSE
      pillar.print_max: 50
reference-location: margin
citation-location: margin
bibliography: bibliography.bib  
---

[@Dalgaard2025]

# Colophon

::: {.callout-note icon=false appearance="simple" title="Session Information" collapse=false #session-info}



::: {.cell}
::: {.cell-output .cell-output-stdout}

```
─ Session info ───────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.5.1 (2025-06-13 ucrt)
 os       Windows 11 x64 (build 22631)
 system   x86_64, mingw32
 ui       RTerm
 language (EN)
 collate  Danish_Denmark.utf8
 ctype    Danish_Denmark.utf8
 tz       Europe/Copenhagen
 date     2025-09-25
 pandoc   3.6.3 @ C:/RStudio-2025.09.0-387/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
 quarto   NA @ C:\\RSTUDI~1.0-3\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe

─ Packages ───────────────────────────────────────────────────────────────────────────────────────
 package     * version date (UTC) lib source
 cli           3.6.5   2025-04-23 [1] CRAN (R 4.5.1)
 digest        0.6.37  2024-08-19 [1] CRAN (R 4.5.1)
 evaluate      1.0.5   2025-08-27 [1] CRAN (R 4.5.1)
 fastmap       1.2.0   2024-05-15 [1] CRAN (R 4.5.1)
 htmltools     0.5.8.1 2024-04-04 [1] CRAN (R 4.5.1)
 htmlwidgets   1.6.4   2023-12-06 [1] CRAN (R 4.5.1)
 jsonlite      2.0.0   2025-03-27 [1] CRAN (R 4.5.1)
 knitr         1.50    2025-03-16 [1] CRAN (R 4.5.1)
 rlang         1.1.6   2025-04-11 [1] CRAN (R 4.5.1)
 rmarkdown     2.29    2024-11-04 [1] CRAN (R 4.5.1)
 rstudioapi    0.17.1  2024-10-22 [1] CRAN (R 4.5.1)
 sessioninfo   1.2.3   2025-02-05 [1] CRAN (R 4.5.1)
 xfun          0.53    2025-08-19 [1] CRAN (R 4.5.1)
 yaml          2.3.10  2024-07-26 [1] CRAN (R 4.5.0)

 [1] C:/Users/B199526/AppData/Local/Programs/R/R-4.5.1/library

──────────────────────────────────────────────────────────────────────────────────────────────────
```


:::
:::


:::

