# Evolution of taste processing shifts dietary preference

> Enrico Bertolini, Daniel Münch, Justine Pascual, Noemi Sgammeglia, Matteo Bruzzone, Carlos Ribeiro, and Thomas O. Auer

This repository shares data and scripts behind the analyses presented in the following works:

- [Bertolini et al., 2024 bioRxiv](https://www.biorxiv.org/content/10.1101/2024.10.11.617601v1)

- [Bertolini et al., 2025 Nature]()

---

## Contents

- `data/` - processed datasets  (.tsv)
- `scripts/` - analysis and visualization code (.Rmd)
- `output/` -  generated figures

---

## Usage example

```r
library(tidyverse)

for (i in list.files("../R/", full.names = TRUE, pattern = '.R')) {
  source(i)
}
```

### Colorimetric assay data

```r
read_tsv(file = "../data/behaviour_colorimetric_assays.tsv", show_col_types = FALSE) %>%
  filter(
    assay_type == "72-well colorimetric",
    two_choice == "noni juice vs grape juice"
  ) %>%
  select(everything()) -> df
  
df %>%
  plot_PI(
    x_var = species,
    order_x = order_species(),
    title = "72-well colorimetric assay"
  )
    
df %>%
  plot_perc_feeding(
    x_var = species,
    order_x = order_species()
  )    
```

![](./output/72_well_colorimetric_Dtrio.png)

### flyPAD assay data

![](./output/flyPAD_Dtrio.png)

### cell counts

![](./output/taste_cell_counts_Dtrio.png)

---

## License

This project is licensed under [MIT](LICENSE).
