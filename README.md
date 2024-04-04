# taxref4R

This package allows to use the French [TAXREF API](https://taxref.mnhn.fr/taxref-web/api/doc) from R. TAXREF is the taxonomic reference system used in France and is the corner stone of the French biodiversity database [INPN](https://inpn.mnhn.fr/accueil/index). Besides the taxonomic information, the API also allows to get information from [the status database](https://inpn.mnhn.fr/telechargement/referentielEspece/bdc-statuts-especes) containing information about protection and conservation levels of taxa.

## Installation

The taxref4R package can be installed from GitHub.

```{r}
if (!require("pak")) install.packages("pak")

pak::pkg_install("CedricMondy/taxref4R")
```
