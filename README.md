# Photosynthesis

## Batch Effect Correction and Normalization Pipelines

This repository documents the normalization and batch-effect correction approaches used in **Phase 1** and **Phase 2** of the analysis.

---

## Phase 1

We tested **two normalization techniques**:

- **Quantile normalization**  
  [`phase1_qn_slope(5.18).ipynb`](phase1_qn_slope(5.18).ipynb)

- **Demedian normalization**  
  [`phase1_demedian(5.18).ipynb`](phase1_demedian(5.18).ipynb)

---

## Phase 2

We tested **four techniques**:

1. **Quantile normalization within plates**  
   [`phase2_q.n._30(5.18).ipynb`](phase2_q.n._30(5.18).ipynb)  
   [`phase2_q.n._31(5.18).ipynb`](phase2_q.n._31(5.18).ipynb)  
   [`phase2_q.n._32(5.18).ipynb`](phase2_q.n._32(5.18).ipynb)  
   [`phase2_q.n._33(5.18).ipynb`](phase2_q.n._33(5.18).ipynb)  
   [`phase2_q.n_within_slope_add99no33(5.18).ipynb`](phase2_q.n_within_slope_add99no33(5.18).ipynb)

2. **Quantile normalization across plates**  
   [`phase2_q.n._across_slope_add99no33(5.18).ipynb`](phase2_q.n._across_slope_add99no33(5.18).ipynb)

3. **Bayesian model within plates**  
   [`phase2_std_within_bay(5.18).ipynb`](phase2_std_within_bay(5.18).ipynb)

4. **Bayesian model across plates**  
   [`phase2_bay_across.R`](phase2_bay_across.R)  
   [`phase2_std_across_bay_no33add99(5.18).ipynb`](phase2_std_across_bay_no33add99(5.18).ipynb)

---

## Bayesian Model Description

Both Bayesian models are based on the same hierarchical model defined in  
[`mutant_plate_model2.stan`](mutant_plate_model2.stan):

- One model is fit **separately on replicate plates**.  
- The other is fit **jointly across all plates**.

---

## Evaluation Metrics

We used the following metrics to assess and compare normalization methods:

1. **Reproducibility** between Phase 1 and Phase 2 after batch-effect correction  
   [`phase 2&1 slope_compare1(5.18).ipynb`](phase%202%261%20slope_compare1(5.18).ipynb)

2. **Residual batch effects** after correction  
   [`test_plate_effect1.ipynb`](test_plate_effect1.ipynb)
