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
- **CLustering** 

Performs clustering by emplyoing four different clustering methods, (K-means, Hierarchical, Fuzzy and PAM clustering). The user may change the clustering parameters and see their effect on stratification results immediately.  

DGH-GO performs functional similarities, dimension reduction and clustering, coupled with interactive visualization and control over analysis, which allows biologists to explore and analyze their datasets, without the need to know how to execute complex methods.

## Applications of DGH-GO

- Exploring the shared etilogy of complex diseases (**please see the SharedEtiology.Rmd**)
- Defining the gene signatures for a given clincial outcome (**please see the GeneSignature.Rmd**)  

