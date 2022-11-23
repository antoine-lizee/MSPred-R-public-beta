Add / Compute Yrs to .... when not known + OnsetToYr5RelapseCount As "Trajectory markers"
Interaction with genetics

# Post Lab meeting:

## Sensitivity analysis 
- with/out the "corner case" that is complex to explain to reviewers.
- EDSS groups?

## Data
New MRIs
Test some data pre-processing (DD, EDSS gradients?)
Include gradients
Include genetics

## Method: [Done]
Making sure that CV is well described: 
- no cross-longitudinal
- shared fold-making randomization

## Results
VS Clinician?
Look into individual features? [No]
Replication dataset? [Done]

# DISCUSSION / IDEAS:
Make clear that MRI is old metrics
ROC is not optimizing toward anything "Cost related" | Compare to SVM with AUCPR5

## Future
Move to trajectory models
Combination of models might be better across the board. (SVM > NB, but sometimes NB > SVM) [Small]

## Limited performance
Breadth of data? No ecological data for instance.
Amount of data? Typically smaller than in classic ML problems. 

## CCL
This predictive score might be a relevant way to combine all data into one gauge. "hot" or "not". Best performance might not be over 0.8 because of some additional randomness. Size of data set, depth of the metrics, but maybe breadth might be more useful.



