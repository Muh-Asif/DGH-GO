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

-	Importing targeted genes file

<img width="468" alt="image" src="https://user-images.githubusercontent.com/39635420/191662925-74c5463d-f8f6-4c93-9164-78c2800cbe10.png">

-	Calculate genes functionality similarities by choosing semantic similarity measure
<img width="465" alt="image" src="https://user-images.githubusercontent.com/39635420/191663463-65b30700-e009-4695-9ce7-5bfd2a219b4e.png">

-	Apply and compare dimension reduction methods (PCoA, T-SNE, UMAP and PCA)

<img width="465" alt="image" src="https://user-images.githubusercontent.com/39635420/191663584-3aded9bc-1a0c-4a32-a0c4-b76a8a51a325.png">

<img width="465" alt="image" src="https://user-images.githubusercontent.com/39635420/191663618-1d518d0c-632a-44e8-962b-b1beea416f01.png">

<img width="454" alt="image" src="https://user-images.githubusercontent.com/39635420/191663635-c5bcffeb-f72b-4ab7-824e-f0588840d365.png">

<img width="449" alt="image" src="https://user-images.githubusercontent.com/39635420/191663655-c8b0231f-d81e-40e5-ad0b-a47fea29993e.png">

-	Apply and compare clustering methods (K- means, PAM, Fuzzy and Agglomerative Hierarchical Clustering) 

<img width="459" alt="image" src="https://user-images.githubusercontent.com/39635420/191663731-bb91a1fc-ebb0-48df-9afd-cdce83011431.png">
<img width="463" alt="image" src="https://user-images.githubusercontent.com/39635420/191663757-83c2dcf8-e45a-46c1-915c-dc8c6af1a068.png">
<img width="459" alt="image" src="https://user-images.githubusercontent.com/39635420/191663782-5295750b-847c-40ab-8735-9ffc9f13fce9.png">
<img width="451" alt="image" src="https://user-images.githubusercontent.com/39635420/191663823-bb50b9b9-e9b6-4669-9e94-cc54b609bd14.png">
<img width="457" alt="image" src="https://user-images.githubusercontent.com/39635420/191663839-952f2fcd-fc66-4d78-a38e-5d09217efc35.png">
<img width="455" alt="image" src="https://user-images.githubusercontent.com/39635420/191663946-3261546a-9c81-4ee6-9c7b-fed726aad66e.png">




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





