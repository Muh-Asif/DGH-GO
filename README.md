# DGH-GO
Dissecting the Genetic Heterogeneity of complex diseases using Gene Ontology (GO), an interactive and user friendly web application.

Neurodevelopmental disorders (NDDs) such as Autism Spectrum Disorder (ASD), Schizophrenia and Intellectual Disability are phenotypically heterogeneous and difficult to diagnose at early-age. The genetic heterogeneity of NDDs matches to their clinical-variability. The different NDDs share biological mechanisms that further complex the patient's stratification, thus, limiting the applications of personalized medicine for NDDs.



DGH-GO aims to address the genetic heterogeneity of complex diseases by startifying the genes into clusters and consist of following modules:

- **Input** 

Allows the users to upload the genes 
- Semantic similarty matrix


Calculates the functional similarties between genes using GO structure

- **Dimension reudction** 

Provides the 2D viuslzation of simantic similarty matrix that allows biologist to look deep in their input. Currently, DGGH-GO support two dimension reduction methods, T-SNE and Principal Coordinate Analysis. 
- **Clustering** 

Performs clustering by emplyoing four different clustering methods, (K-means, Hierarchical, Fuzzy and PAM clustering). The user may change the clustering parameters and see their effect on stratification results immediately.  

DGH-GO performs functional similarities, dimension reduction and clustering, coupled with interactive visualization and control over analysis, which allows biologists to explore and analyze their datasets, without the need to know how to execute complex methods.


**Installation of DGH-GO** 

-	Install R from https://cran.r-project.org/bin/windows/base/ for your operating system
-	Download required R packages. CRAN: R packages can be downloaded from CRAN, an official repository maintained by R community. R function to install R package from CRAN: install.packages("package_name"). Bioconductor: Download R packages intended for bioinformatics data analysis. Details for installing Bioconductor and Bioinformatics related R packages are provided at https://www.bioconductor.org/install/
-	Download Shiny from https://www.r-project.org/nosvn/pandoc/shiny.html
-	After installing all required packages, run DGH-GO_App.R shiny file

**Tutorial for using DGH-GO** 

-	**Importing targeted genes file**
<pre>
Upload the gene and disease containing file:
</pre>

<img width="468" alt="image" src="https://user-images.githubusercontent.com/39635420/191662925-74c5463d-f8f6-4c93-9164-78c2800cbe10.png">

-	**Calculate genes functionality similarities by choosing semantic similarity measure**
<pre>
Choose the semantic similarty matrix:
</pre>


![Picture1 (resized)](https://user-images.githubusercontent.com/39635420/227709447-27168d3a-f10a-4f62-8ba7-59341d958632.png)
<pre>
Genes functional similarty matrix:
</pre>


![Picture2 (resized)](https://user-images.githubusercontent.com/39635420/227709475-a2753627-fd63-429e-9b61-aa16199cc479.png)


-	**Apply and compare dimension reduction methods (PCoA, T-SNE, UMAP and PCA)**
<pre>
Output of different dimension reduction methods:
</pre>


![Picture3 (resized)](https://user-images.githubusercontent.com/39635420/227709554-9a03587b-7b7c-455c-82a8-95a63f36087d.png)


-	**Apply and compare clustering methods (K- means, PAM, Fuzzy and Agglomerative Hierarchical Clustering)** 
<pre>
Chossing the clusteerig methods and applying it to similarty matrix:
</pre>
![Picture4 (resized)](https://user-images.githubusercontent.com/39635420/227709665-cb123fcb-c221-4394-af4b-d379ecf2d8bf.png)
<pre>
Determining the optimal number of clusters:
</pre>


![Picture7 (resized)](https://user-images.githubusercontent.com/39635420/227709682-69a6e008-0704-4585-bf6f-5079ffe907db.png)

<pre>
Quantifying the clustering validation: 
</pre>

![Picture5 (resized)](https://user-images.githubusercontent.com/39635420/227709723-72cf71fa-7359-4ff2-9063-fcad5dc8c415.png)

<pre>
Genes with assigned clusters: 
</pre>

![Picture6 (resized)](https://user-images.githubusercontent.com/39635420/227709712-58734f91-1e5f-4ba0-b33b-e7ff90fc0093.png)





## Applications of DGH-GO

- Exploring the shared etilogy of complex diseases (**please see the SharedEtiologyComplexDiseases_DGH_GO.Rmd**)
- Defining the gene signatures for a given clincial outcome (**please see the DiseaseGeneSignature_DGH_GO.Rmd**)  

## Frequently asked question

-	What is DGH-GO?<br/>
DGH-GO is a shiny based web application that can be used to infer the functional related diseases genes, that may or may not lead to distinct disease phenotype.  
-	How to install it?<br/>
Please visit installation section. 
-	What are applications of DGH-GO?<br/>
DGH-GO can be used to infer group of genes converging on similar biological process. Thus, ultimately helping in grouping patients based on genetic similarity, a key step in precision medicine or systems biomedicine.   
-	Can I define a disease specific signature of genes?<br/>
Yes, after installing the DGH-GO (please see the installation guide) upload a CSV file containing two columns, where first column names as “genes” and second column names as “class”. Genes column must contain the targeted genes and class column must have your interested disease name.
-	How can I find if two disease have shared etiology?<br/>
prepare a CSV file containing targeted genes and their respective known diseases. Place genes into “genes” column and disease into “class” column of CSV file. Upload file on DGH-GO and calculate semantic similarities between genes. Apply different dimension reduction methods from dimension reduction module to dig deeper into your dataset. Grasp a bigger picture of gene grouping from 2D plots of DGH-GO. Play with different methods and with their parameters to analyze data. After extensive visualization combined with biological knowledge proceed to Clustering module for the identification of number of clusters. Assess the frequency of each disease gene in all clusters and assign a cluster title based on biological inference.         
-	I am a biologist and from where I should start the DGH-GO?<br/>
Familiarize yourself with R installation and follow DGH-GO installation guide.   
-	I am a bioinformatican and wanted to modify the DGH-GO?<br/>
The DGH-GO source code can be obtained from its github and open to any modification. 
-	Does DGH-GO work for large number of genes, for example for 5000 genes?<br/>

DGH-GO may work with any number of input genes. However, to calculate functional similarities, DGH-GO browse gene ontology data from its consortium. In summary, the execution time of semantic similarity calculation module depend on internet connection.  





