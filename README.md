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

- All estimates for ozone have been converted to ppb

- I've generated estimates for ozone at census tract centroids using both inverse-distance
weighting (IDW) and ordinary kriging (OK) (03_Kriging_AQS_Data.R) 

    - Sometimes it's a challenge to fit a semivariogram to the data, and therefore kriged estimates aren't always available.For these days, IDW predictions are still available
    - You'll have to take a look at the distribution of each daily estimate in order
to see if you want to use the kriged data. IDW estimates may be better given some of
the limitations (time resolution, spatial coverage of monitors)

- Due to storage issues on my U:/ drive, the output files are currently on my Google Drive:
(https://drive.google.com/open?id=1Exb2mP28GVNICK1k_Pk52cZSal1Tr6K-)
    
    - "8hour_" is the daily 8hour max concentration (not necessarily contemporaneous)
    - "8hour_monthly" is the monthly mean of daily 8hour max concentrations (also not necessarily contemporaneous)
    - "Daily_" is the daily mean concentration
    
    - "88101" is PM~2.5~ (ug/m3) and "44201" is ozone (ppb)
    
- There are three files for each year and pollutant:

    - (pollutant name)_(year)_Kriged.csv are the IDW and ordinary kriging predictions for each census tract
    - (pollutant name)_(year)_CV.csv are the cross-validation results
    - (pollutant name)_(year)_Diagnostics.csv are some additional model diagnostic variables (see notes on kriging analysis below)
  
- One major limitation of the analysis is that most of the monitors are close to the 25. This will result in less reliable predictions farther from the highway.

- An alternative approach may be to average these concentrations over weeks or months to get better estimates of concentrations, but that will likely depend on your analysis. It's better to average at the monitors and re-krige. If this is something you need/want, let me know and I'll be happy to generate other metrics!

#### Notes on the kriging analysis:
- See Li and Heap for a nice explanation of kriging diagnostics

    - https://pdfs.semanticscholar.org/686c/29a81eab59d7f6b7e2c4b060b1184323a122.pdf

- If the monitoring data for each day/month weren't normally distributed, the code attempts to use log-transformed
estimates. We need to apply a correction to get estimates back in the original scale (See Oliver and Webster 2007, Page 185)

    - https://books.google.com/books?hl=en&lr=&id=WBwSyvIvNY8C&oi=fnd&pg=PR5&ots=CCLmSNqK1c&sig=lFZanxv2eVSKec6nPdESzuIFrA4#v=onepage&q&f=false

    - Note: A back-transformed variance estimate for OK cannot be calculated because the mean is not known (Oliver and Webster 2007, page 185)




