# NTL
R code for using blackmarble to assess economic changes as a result of changes in night time lights

#### Introduction

Nighttime Light (NTL) data analysis, a tool of immense value, allows us to delve into the intricate world of economic activities across geographic regions. This comprehensive statistical approach, powered by R, manipulates, analyzes, and visualizes these data through a series of interlinked phases. Each analysis phase builds upon the previous, culminating in a user-driven exploration of NTL changes over time. This essay elucidates the methodologies and statistical processes employed in each phase, leveraging R’s capabilities to prepare GDP data, integrate geographic data, perform regression analyses, and provide interactive user inputs for real-time data exploration.

#### Phase 1: Preparing GDP Data

The first phase of the analysis involves preparing GDP data and converting it from its original format into a long format. This restructuring is crucial as it aligns the data format with that produced by the `bm_extract` function used later in the analysis. The GDP data, typically in a comprehensive format with columns representing different years, is transformed using the `pivot_longer` function from the `tidyr` package. This function melts the multiple columns of GDP into two columns, one for the year and one for the GDP value, thus simplifying future data merging processes. This preparatory step is essential because `bm_extract,` which extracts NTL data, does not allow additional columns to reformat into a long format during its operation. Therefore, aligning the GDP data format facilitates subsequent data joining processes.

#### Phase 2: Loading and Processing GADM Data

In the second phase, Geographic Data Files (GADM) are loaded at level one to provide spatial context to the NTL data. The `game` function, possibly supplemented by packages such as `raster` or `sf,` is used to fetch these data for the specified region, in this case, Ukraine. Following the data retrieval, `bm_extract` is employed to process the NTL data, incorporating it within the spatial framework of the GADM data. This step is critical as it enhances the NTL data with geographic specificity, allowing for detailed regional analyses in the subsequent phases.

#### Phase 3: Data Integration and Regression Analysis

The third phase integrates the previously prepared GDP data with the geographically enhanced NTL data. This integration achieves a data-joining process based on shared identifiers such as geographic names and dates. Once integrated, the comprehensive dataset undergoes statistical modeling. Multiple regression models are explored to ascertain the relationship between GDP and changes in NTL, with additional factors such as regional identifiers considered. The models vary in complexity, incorporating interactions and categorical distinctions between regions to refine the accuracy of the analyses. The best-performing model is then selected based on statistical criteria, and its coefficients are extracted. These coefficients estimate the dollar value change in GDP attributable to variations in NTL, providing a quantitative basis for economic assessment.

#### Phase 4: User-Driven Exploration of NTL Changes

The final phase, designed with you in mind, empowers users to explore NTL changes interactively based on specific dates. Through a user-friendly interface, individuals can input a date range, triggering the retrieval and processing of NTL data for that period. The system uses the regression coefficients derived in Phase 3 to estimate and visualize the economic impacts of NTL variations across Ukraine. This phase offers insights into temporal changes in economic activity and allows users to engage directly with the data, enhancing the applicability and understanding of NTL analyses.

#### Conclusion

The structured approach employed in this analysis harnesses the full potential of R, a tool known for its flexibility, for statistical data processing, from preparation to interactive exploration. By methodically transforming, integrating, and modeling the data across four distinct phases, the analysis provides a robust framework for understanding the economic implications of NTL data. This methodology not only exemplifies the power of statistical analysis in economic geography but also highlights the flexibility of R as a tool for complex data analysis and visualization in real time, empowering you in your data analysis journey.
