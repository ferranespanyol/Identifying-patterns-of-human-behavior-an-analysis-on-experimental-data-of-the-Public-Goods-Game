# Identifying patterns of human behavior: an analysis on experimental data of the Public Goods Game

This repository is a collection of [Jupyter](https://jupyter.org/) notebooks organized according the different chapters and sections of this study. The fundamental objective of them is to help in the identification of behavior pattern of participants in a collective risk dilemma by means of data science methodologies. To fulfill this objective we propose the use of Machine Learning (ML) techniques to improve the identification of these patterns based on the characterization of groups of players that acted in much the same way. 

The whole code, until 30.08.2018, is the result of a final Master's Thesis of the [Master's Degree in Fundamental Principles of Data Science](http://www.ub.edu/estudis/en/mastersuniversitaris/cienciadades/introduction) supervised by Josep Perelló, PhD. The [Master's Thesis report](https://github.com/axelbrando/Mixture-Density-Networks-for-distribution-and-uncertainty-estimation/blob/master/ABrando-MDN-MasterThesis.pdf) is published in this repository in a PDF format. To summary all the contents I explained in the report, will be possible to consult the [slides of the presentation](). Any contribution or idea to continue the lines of the proposed work will be very welcome.



## Implemented tricks and techniques

> - Classic statistical techniques. 
> - Unsupervised clustering algorithms: K-Means, Agglomerative Clustering, DBSCAN and MAP-DP.
> - Selection of the optimal number of clusters based on NbClust ([Malika Charrad et al. 2006](https://www.researchgate.net/publication/275463140_Determining_the_number_of_clusters_using_NbClust_package))
> - Supervised classification algorithms: Logistic Regression, Linear Discriminant Analysis, Naive Bayes, Decision Tree, K-Nearest Neighbors, Support Vector Machine.

## Some Python libraries used

> - sklearn
> - scipy
> - pandas
> - collections

## Some R libraries used

> - fitdistrplus
> - NbClust
> - cluster
> - ineq

## Implemented visualisation functionalities

> - Generic implementation to visualise mean and SE (as errorbar) of the most important variables of the Collective-Risk Dilemma game..
> - Generic implementation to visualise the clustering assignment on 2D using PCA.


## Notebooks
(Currently tested Python 3)

#### [Exploratory Analysis](https://github.com/ferranespanyol/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game/blob/master/TFM%20-%20Exploratory%20Analysis.ipynb)

#### [General Results (classic statistical results)](https://github.com/ferranespanyol/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game/blob/master/TFM%20-%20General%20Results.ipynb)

#### [Clustering Analysis (unsupervised learning)](https://github.com/ferranespanyol/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game/blob/master/TFM%20-%20Cluster%20Analysis.ipynb)

#### [Classification Analysis (supervised learning)](https://github.com/ferranespanyol/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game/blob/master/TFM%20-%20Classification%20Analysis.ipynb) 

#### [MAP-DP](https://github.com/ferranespanyol/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game/blob/master/MAP-DP.ipynb) 



## Contributions

Contributions are welcome!  For bug reports or requests please [submit an issue](https://github.com/axelbrando/Mixture-Density-Networks-for-distribution-and-uncertainty-estimation/issues).

## Contact  

Feel free to contact me to discuss any issues, questions or comments.

* GitHub: [ferranespanyol](https://github.com/ferranespanyol)


### BibTex reference format for citation for the Code
```
@misc{PGGFEspañolMasterThesis,
title={Identifying patterns of human behavior: an analysis on experimental data of the Public Goods Game (PGG)},
url={https://github.com/ferranespanyol/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game},
note={GitHub repository with a collection of Jupyter notebooks intended to identify patterns in a PGG.},
author={Ferran Español},
  year={2018}
}
```
### BibTex reference format for citation for the report of the Master's Thesis

```
@misc{PGGFEspañolMasterThesis,
title={Identifying patterns of human behavior: an analysis on experimental data of the Public Goods Game},
url={https://github.com/axelbrando/Mixture-Density-Networks-for-distribution-and-uncertainty-estimation/blob/master/ABrando-MDN-MasterThesis.pdf},
note={Report of the Master's Thesis: Identifying patterns of human behavior: an analysis on experimental data of the Public Goods Game.},
author={Ferran Español},
  year={2018}
}
```

