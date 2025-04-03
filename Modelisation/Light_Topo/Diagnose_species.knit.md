---
title: "Diagnose species"
date: "2025-04-01"
format: html
self-contained: true
theme: cosmo
editor: source
code-fold: true
params:
  species: Licania_membranacea
---



::: {.cell}

```{.r .cell-code}
print(params$species)
```

::: {.cell-output .cell-output-stdout}

```
[1] "Chaetocarpus_schomburgkianus"
```


:::

```{.r .cell-code}
fit <- list()
for(V in Var){
  tryCatch({
    chain_path <- file.path("Chains", "Affine", V, params$species)
    fit[[V]] <- as_cmdstan_fit(list.files(chain_path,
                                          full.names = TRUE))},
    error=function(e){cat("ERROR :",params$species, V, conditionMessage(e), "\n")}
  )
}
# names(fit)
```
:::

::: {.cell}

```{.r .cell-code}
lapply(names(fit), function(x)
  fit[[x]]$draws(c("alpha", "beta1")) %>% 
    mcmc_trace() +
    labs(title= x)
)
```

::: {.cell-output .cell-output-stdout}

```
[[1]]
```


:::

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/chains-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

[[2]]
```


:::

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/chains-2.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
lapply(names(fit), function(x)
  fit[[x]]$draws(c("alpha", "beta1")) %>% 
    mcmc_dens() +
    labs(title= x)
)
```

::: {.cell-output .cell-output-stdout}

```
[[1]]
```


:::

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/posteriors-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

[[2]]
```


:::

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/posteriors-2.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
for(i in names(fit)){
  DATA <- fit[[i]]$summary("p") # p posterior
  # prediction environment 
  if(i=="HAND") DATA$x <- seq(-0.37, 23.34, length.out = 100)
  if(i=="TWI") DATA$x <- seq(3.88, 11.02, length.out = 100)
  
  print(
    ggplot(DATA, aes(x = x)) +
      theme_minimal() +
      geom_point(aes(y = median), size=0.7) + # p posterior median
      geom_ribbon(aes(ymin = q5, ymax = q95), color = 'red', alpha = 0.2) + # P quantiles
      # geom_vline(xintercept  = fits[[f]]$summary("O")$median, color = "#00AFBB", linewidth=1) + # Opt median
      # geom_vline(xintercept  = fits[[f]]$summary("O")$q5, linetype="dashed") + # Opt Q5%
      # geom_vline(xintercept  = fits[[f]]$summary("O")$q95, linetype="dashed") + # Opt Q95%
      labs(title= i)
  )
}
```

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/predictions-1.png){width=672}
:::

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/predictions-2.png){width=672}
:::
:::

