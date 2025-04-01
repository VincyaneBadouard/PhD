---
title: "Diagnose species"
date: "2025-03-31"
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
chain_path_A <- file.path("Chains", "Affine", params$species)
fit_A <- as_cmdstan_fit(list.files(chain_path_A, full.names = TRUE))

chain_path_Q <- file.path("Chains", "Quadratic", params$species)
fit_Q <- as_cmdstan_fit(list.files(chain_path_Q, full.names = TRUE))
```

::: {.cell-output .cell-output-stderr}

```
Warning: 771 of 4000 (19.0%) transitions ended with a divergence.
See https://mc-stan.org/misc/warnings for details.
```


:::
:::

::: {.cell}

```{.r .cell-code}
fit_A$draws(c("alpha", "beta1")) %>% 
  mcmc_trace() +
  labs(title= "Affine")
```

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/chains-1.png){width=672}
:::

```{.r .cell-code}
fit_Q$draws(c("alpha", "beta1", "beta2")) %>% 
  mcmc_trace() +
  labs(title= "Quadratic")
```

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/chains-2.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
fit_A$draws(c("alpha", "beta1")) %>% 
  mcmc_dens() +
  labs(title= "Affine")
```

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/posteriors-1.png){width=672}
:::

```{.r .cell-code}
fit_Q$draws(c("alpha", "beta1", "beta2")) %>% 
  mcmc_dens() +
  labs(title= "Quadratic")
```

::: {.cell-output-display}
![](Diagnose_species_files/figure-html/posteriors-2.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
for(fit in c(fit_A, fit_Q)){
  
  model_name <- ifelse("beta2" %in% fit$summary()$variable, "Quadratic", "Affine")

  print(
    fit$summary("p") %>% # p posterior
      mutate(x = seq(5.7325, 23.61375, length.out = 100)) %>% # prediction environment
      ggplot(aes(x = x)) +
      theme_minimal() +
      geom_point(aes(y = median), size=0.7) + # p posterior median
      geom_ribbon(aes(ymin = q5, ymax = q95), color = 'red', alpha = 0.2) + # P quantiles
      # geom_vline(xintercept  = fits[[f]]$summary("O")$median, color = "#00AFBB", linewidth=1) + # Opt median
      # geom_vline(xintercept  = fits[[f]]$summary("O")$q5, linetype="dashed") + # Opt Q5%
      # geom_vline(xintercept  = fits[[f]]$summary("O")$q95, linetype="dashed") + # Opt Q95%
      labs(title= model_name)
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

