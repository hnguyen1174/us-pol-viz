# Visualizing US House Election Results

## Examples

```r
p <- plots_two_party_spreads(congressional_votes_total, 'West Virginia')
p <- p +
  theme(text = element_text(size = 7.5))
print(p)
```

<img src="fig/WV_congressional_changes.png" alt="drawing" width="700"/>
