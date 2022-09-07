Semantic Diversity Computational Model from:

Hoffman, P. and A. M. Woollams (2015). "Opposing effects of semantic diversity in lexical and semantic relatedness decisions." Journal of Experimental Psychology: Human Perception and Performance 41: 385-402.
http://dx.doi.org/10.1037/a0038995

This code should allow you to create the model in Lens. To download Lens, go to: http://web.stanford.edu/group/mbc/LENSManual/


finalreps.xlsx – was used to generate the training exemplars

finalreps.in – training exemplars to be read by Lens

make_model.in – script to set up the model and load exemplars. After running this script in Lens, you should be able to inspect the model architecture and to train the model by clicking Train

outputwrite.in – additional functions to write model outputs to file.

wt directory – contains the weights for the 10 trained models used to generate results in the paper

out directory – raw output from each of the 10 models for the 96 words used in training. Each row provides the activation values for each semantic unit at a single timepoint (or “tick”) during processing, upon presentation of a given word. There are 28 ticks so the first 28 rows belong to the first word, the next 28 to the second word, and so on up to the 96th word. To generate these data from a trained model, type the following into the Lens console: timewrite sem filename.out 96

