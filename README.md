# Visualizing US House Election Results

(Under Construction 🚧)

## Examples 1: Two-party Vote Changes in West Virginia from 1976 to 2020

```r
library(tidyverse)
library(sf)
library(devtools)

devtools::load_all()
congressional_votes_total <- get_congressional_vote_data()
```

```r
p <- plots_two_party_spreads(congressional_votes_total, 'West Virginia')
p <- p +
  theme(text = element_text(size = 7.5))
print(p)
```

<img src="fig/WV_congressional_changes.png" alt="drawing" width="700"/>

## Examples 2: Two-party Vote Changes in New York from 1976 to 2020

```r
p <- plot_vote_shares(congressional_votes_total, 'New York')
p <- p +
  theme(text = element_text(size = 7.5))
print(p)
```

<img src="fig/NY_congressional_DR_shares.png" alt="drawing" width="900"/>
