# Curva epidemica: consideraciones y limitantes



[![DOI](https://zenodo.org/badge/251691938.svg)](https://zenodo.org/badge/latestdoi/251691938)



Este repositorio posee las instrucciones para reproducir la Figura 1 del borrador: "Recomendaciones para describir de forma adecuada una curva epidémica de COVID-19"
(DOI: https://doi.org/10.17843/rpmesp.2020.372.5461)

## Instrucciones

1. Instalar los paquetes necesarios.

```r
if(!require("remotes")) install.packages("remotes")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("aweek")) install.packages("aweek")
if(!require("skimr")) install.packages("skimr")
if(!require("patchwork")) install.packages("patchwork")
if(!require("covid19viz")) remotes::install_github("avallecam/covid19viz")
```

2. Abrir y ejecutar el archivo __`covid19_fig02.R`__

## Citar

```
@software{andree_valle_campos_2020_4014221,
  author       = {Andree Valle Campos},
  title        = {avallecam/covid19\_peru\_curvas: First release},
  month        = sep,
  year         = 2020,
  publisher    = {Zenodo},
  version      = {v1.0},
  doi          = {10.5281/zenodo.4014221},
  url          = {https://doi.org/10.5281/zenodo.4014221}
}

@article{RPMESP5461,
	author = {Steev Loyola y Andree Valle y Stephanie Montero y Gabriel Carrasco-Escobar},
	title = {Recomendaciones para describir de forma adecuada una curva epidémica de COVID-19},
	journal = {Revista Peruana de Medicina Experimental y Salud Pública},
	volume = {37},
	number = {2},
	year = {2020},
	keywords = {},
	abstract = {-},
	issn = {1726-4642},	
	pages = {378--80},	
	doi = {10.17843/rpmesp.2020.372.5461},
	url = {https://rpmesp.ins.gob.pe/index.php/rpmesp/article/view/5461}
}
```

## Referencias

- about curve cumulative
    + use the incident curve
    + statistical dependence (autocorrelation) in cumulative data
    + https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(03)13335-1/fulltext

- about delays
    + https://twitter.com/AdamJKucharski/status/1229708001243795458

- abount modeling SEIR with stochastic rather than deterministic models
    + https://royalsocietypublishing.org/doi/full/10.1098/rspb.2015.0347

- about the dataset
    + https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30119-5/fulltext
