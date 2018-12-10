## Ozone_for_CDPHE
Kriged daily and monthly estimates of ozone exposures at census tracts in the Denver metro area for CDPHE research project

**Date created:** December 10, 2018

**Author:** Sheena Martenies

**Contact:** Sheena.Martenies@colostate.edu


#### Analytical Notes

02_Summarizing_AQS_Data.R only needs to be used if the hourly data are collected 
from the AQS API

04_Plotting_AQS_Data.R is under construction 


#### Notes for Keving Berg, CDPHE (12/10/18)

- Monitoring data were collected from the US EPA AQS data mart (01_AQS_Data_Scrape.R)

- All estiamtes for ozone are in ppb

- I've generated estimates for ozone at census tract centroids using both inverse-distance
weighting and ordinary kriging (03_Kriging_AQS_Data.R) 

    - Sometimes it's a challenge to fit a semivarigram to the data, 
and kriged estimates aren't available.
    - You'll have to take a look at the distribution of each daily estimate in order
to see if you want to use the kriged data. IDW estiamtes may be better given some of
the limitations (time resolution, spatial coverage of monitors)

- Due to storage issues on my U:/ drive, the output files are currently [here]
(https://drive.google.com/open?id=1-omBeF4YsEzJOFT12N9fvQXp2UqfnUIR)
    
    - "8hour_" is the daily 8hour max concentration (not necessarily contemporaneous)
    - "8hour_monthly" is the monthly mean of daily 8hour max concentrations (also not necessarily contemporaneous)
    - "Daily_" is the daily mean concentration

- One major limiation is that most of the monitors are close to the 25.
This will generate errors in the predictions

- An alternative approach may be to average these concentrations over weeks or 
months to get better estimates of concentrations, but that will likely depend on your
analysis. It's better to average at the monitors and re-krige. If this is something you
want, let me know!

#### Notes on the kriging analysis:
- See Li and Heap for a nice explanation of kriging diagnostics
https://pdfs.semanticscholar.org/686c/29a81eab59d7f6b7e2c4b060b1184323a122.pdf

- If the monitoring data weren't normal, the code attempts to use log-transformed
estimates. We need to apply a correction to get estimates back in the original
scale (See Oliver and Webster 2007, PAGE 185)
https://books.google.com/books?hl=en&lr=&id=WBwSyvIvNY8C&oi=fnd&pg=PR5&ots=CCLmSNqK1c&sig=lFZanxv2eVSKec6nPdESzuIFrA4#v=onepage&q&f=false

    - Note: A back-transformed variance estimate for OK cannot be calculated because the mean is not known (Oliver and Webster 2007, page 185)




