DGH-GO: Clustering genes disrupted by rare CNVs in ASD patients
================
Asif M.

# Summary

This document shows the utility of DGH-GO in studying the genetic
heterogeneity of Autism Spectrum Disorder (ASD).

-   Data set (genes)
    -   source:
    -   Genes disrupted by rare CNVs in ASD patients were selected

# Input genes

    ## preparing gene to GO mapping data...

    ## preparing IC data...

    ## [1] 3698    3

    ##      ALIAS ENTREZID                                  GENENAME
    ## 1 SERPINA3       12                  serpin family A member 3
    ## 2    AADAC       13                 arylacetamide deacetylase
    ## 3     AARS       16                    alanyl-tRNA synthetase
    ## 4     ABAT       18          4-aminobutyrate aminotransferase
    ## 5    ABCA1       19 ATP binding cassette subfamily A member 1
    ## 6    ABCA2       20 ATP binding cassette subfamily A member 2

    ## [1] 3698

    ## 
    ##  ASD 
    ## 3698

# Dimension Reduction

## Pricipal Coordinate Analysis

    ## [1] 2457 2458

![](GeneSignature_files/figure-gfm/cmdScale-1.png)<!-- --> ## T-SNE
![](GeneSignature_files/figure-gfm/tsne-1.png)<!-- -->

# Clustering

## Agglomerative hierarchical clustering

    ## Warning: `gather_()` was deprecated in tidyr 1.2.0.
    ## Please use `gather()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

![](GeneSignature_files/figure-gfm/ahc_clus-1.png)<!-- -->![](GeneSignature_files/figure-gfm/ahc_clus-2.png)<!-- -->

    ##   cluster size ave.sil.width
    ## 1       1 1512          0.29
    ## 2       2  945          0.08

![](GeneSignature_files/figure-gfm/ahc_clus-3.png)<!-- -->![](GeneSignature_files/figure-gfm/ahc_clus-4.png)<!-- -->

    ## [1] 1512    3

    ##      genes class cluster
    ## 1 SERPINA3   ASD       1
    ## 2    AADAC   ASD       1
    ## 3     ABAT   ASD       1
    ## 4    ABCA1   ASD       1
    ## 5    ABCA2   ASD       1
    ## 6    ABCA3   ASD       1

    ## 
    ##  ASD 
    ## 1512

# Functional enrichment analysis

![](GeneSignature_files/figure-gfm/fea-1.png)<!-- -->

    ## [1] "\n"

## Enriched Human Phenotype Ontology terms

![](GeneSignature_files/figure-gfm/feaHPO-1.png)<!-- -->

## Enriched GO terms

![](GeneSignature_files/figure-gfm/feaGO-1.png)<!-- -->

## Enriched KEGG Pathways

![](GeneSignature_files/figure-gfm/feaKegg-1.png)<!-- -->
