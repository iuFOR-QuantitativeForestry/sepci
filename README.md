# SEPCI
An r-package and resources for plant competition index computation built over the capabilities of the r-package [siplab](https://github.com/ogarciav/siplab).

---

## :book: Abstract 
Competition is driving plant growth by modulating jointly with other drivers the potential growth due to site, genetic and ontogeny factors. Usually, competition is assessed in plant growth modelling and analysis by non-spatially explicit indices. However, stand spatial structure defined by plants positions and relative sizes is crucial in terms of stand growth allocation between trees. Several software tools exist for computing spatially explicit competition indices but requires additional coding to define specific indices. In response, we present sepci, an R package, for computing both known spatially explicit competition indices and novel indices that depend on measures retrievable with profile functions.

---

## :computer: Installation
To install the package use ```devtools::install_github("iuFOR-QuantitativeForestry/sepci/sepci")```

---

## :page_facing_up: Documentation and Examples
### :scroll: Vignettes
[Complex competition indexes in forests](https://html-preview.github.io/?url=https://github.com/iuFOR-QuantitativeForestry/sepci/blob/main/sepci/doc/sepci.html)

### :hammer: Examples
Collection of example code using sepci and siplab capabilities
* [Create a new index](examples/creating_a_new_index.R)
* [Split an index](examples/splitting_an_index.R) by inter vs intra species competition or by competition by below and by above.
* [Plotting the competition field for a mean tree](examples/plotting_mean_tree_competition_field.R)

---

## :information_source: License

The content of this repository is under the [GPL v3 license](./LICENSE).

---

## :link: About the authors:

#### Andrés Bravo Núñez:

[![](https://github.com/andbrav.png?size=50)](https://github.com/andbrav) \\
[ORCID](https://orcid.org/0009-0003-6650-3487) \\
[Researchgate](https://www.researchgate.net/profile/Andres-Bravo-Nunez-2) \\
[LinkedIn](https://es.linkedin.com/in/andbrav) \\
[UVa](https://portaldelaciencia.uva.es/investigadores/874028/detalle)

#### Felipe Bravo Oviedo:

[![](https://github.com/Felipe-Bravo.png?size=50)](https://github.com/Felipe-Bravo) \\
[ORCID](https://orcid.org/0000-0001-7348-6695) \\
[Researchgate](https://www.researchgate.net/profile/Felipe-Bravo-11) \\
[LinkedIn](https://www.linkedin.com/in/felipebravooviedo) \\
[Twitter](https://twitter.com/fbravo_SFM) \\
[UVa](https://portaldelaciencia.uva.es/investigadores/181874/detalle)

