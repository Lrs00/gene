Normalization: use SVD, Combat and subtract mean to remove bias

Mixed_effect_WT: debiased the data by subtracting the random effect and then applied a Random Forest model. The model was trained and tested exclusively on the WT (wild-type) data

Gene_WT: Applied a Random Forest model on the debiased data, which had been processed using Singular Value Decomposition (SVD) and Combat to correct for batch effects. 
The model was trained and tested exclusively on WT (wild-type) data

SVD: Applied SVD method for data correction, one for WT and the other for all mutants. 
