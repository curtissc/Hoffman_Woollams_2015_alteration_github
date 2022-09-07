# Hoffman_Woollams_2015_alteration_github
This model is an alteration of neural network model of [Hoffman &amp; Woollams 2015](https://doi.org/10.1037/a0038995) for modeling a lexical decision task. 

## Introduction

Hoffman & Woollams (2015) investigated a measure of lexical semantic ambiguity called semantic diversity (SemD), which is a corpus-based measure modeling the degree to which any word may appear in a larger or smaller number of contexts. High values of SemD are more ambiguous, low values less ambiguous. Prior to their modeling experiment, the authors found that higher SemD sped decisions on whether a string of letters was a word (i.e., lexical decision) but slowed judging whether two words were semantically related (relative to lower SemD). In this paper, the authors sought to model their findings using neural network models. They modeled SemD on the theory that words with higher SemD were less well-specified, i.e. noisier than words with lower SemD. They argue that high SemD words’ semantic representations are less well-specified than those of low SemD words because high SemD words are associated with a larger variety of contexts.

To provide proof of concept for the impact of more or less poorly specified representations on processing, H&W (2015) simulated relatedness decisions and lexical decisions in a three-layer, feedforward connectionist model. Half of the semantic representations learned by the model were more well-specified (low SemD), and half were more poorly specified (high SemD). This was achieved by having the model learn multiple, slightly altered iterations of the same semantic representation, where “high SemD” representations had more variation and “low SemD” representations had less variation. Because of these variations, the representations learned by the model were more or less “noisy”. Additionally, semantic representations were designed so that each word had one related representation that overlapped by a number of semantic feature nodes and one unrelated representation with very little overlap so that the model could perform relatedness decisions. After training, when the model was presented with a high SemD word, more activation was observed on average in the semantic layer than when the model was presented with a low SemD word. This was considered evidence of the benefit of high SemD in lexical decision, as the authors assumed that more activation of semantic units in a person would result in faster lexical decisions. 

In a separate set of experiments [Chapman and Martin 2021](http://dx.doi.org/10.1037/xge0001123) found an interaction in lexical decision between SemD and word frequency (i.e., how often a word occurs across a corpus of documents), such that high frequency increases the beneficial effects of high SemD and vice versa. The current experiment sought to model this interaction by incorporating frequency into the lexical decision model. Finding such an interaction with a standard implementation of word frequency would lend credence to H&W (2015)'s hypothesis on how SemD impacts semantic representations. 

## Methods:
This simulation used the model architecture of H&W (2015)—a three-layer, feed forward connectionist model with 25 orthographic units, 25 hidden units, and 50 semantic units, with each layer fully connected to the next and the hidden layer fully connected to itself. Unit activation ranged between 0 and 1 and was computed by a logistic function. Hidden and semantic units received a fixed bias input of -5, which caused their activation to remain close to its minimum level unless they were receiving input activation. The model was implemented using [LENS software] (www.stanford.edu/group/mbc/LENSManual/index.html).

Orthographic and semantic representations were those used by H&W (2015), which were generated for 48 low and 48 high SemD words. Orthographic patterns included one of ten units activated to represent the first consonant, one of five units activated to represent the vowel, and one of ten units activated to represent the second consonant. These representations were not created to simulate known words but simply to approximate single syllable words. Semantic prototype patterns were created so that 25/50 units were activated. These were randomly assigned to the 96 words. High and low SemD words differed in the degree to which their prototype representations were altered, as described below relative to model training.

In order to probe the word frequency x SemD interaction in this model, low and high SemD words were each assigned frequency values such that half had high frequency and half low frequency. The values assigned to high and low frequency groups differed between low and high SemD groups to account for the fact that frequency and SemD correlate, and thus high SemD words tend to fall into a higher frequency band than low SemD words. To choose frequency values for words, I utilized the large range of frequency from the roughly 40,000 words of the [English Lexicon Project] (https://elexicon.wustl.edu/) (Balota et al., 2007). These words were divided into six frequency and six SemD quantiles, of which I sampled low frequency and SemD values from the second quantiles and high frequency and SemD values from the fifth quantiles (e.g., low frequency, high SemD values were selected from the second frequency quantile within the fifth SemD quantile). I chose to use six quantiles to avoid the influence of extreme values but retain a reasonable distance between high and low values. For each frequency x SemD quantile, 24 frequency values were randomly sampled.  

During training, the network was presented on each trial with an orthographic pattern and was trained to activate the correct pattern of units in the semantic layer. Processing occurred over seven time intervals subdivided into four time steps. The orthographic activation pattern was hard-clamped throughout this period as activation cycled through the network. On the final two time intervals, the correct pattern of activation was applied to semantic units and error was computed by comparing actual activation to target activation. Semantic unit activation was then adjusted using the back-propagation over time algorithm (Rumelhart, Hinton, & Williams, 1986), which makes small, incremental changes to connection weights so that over many trials the network creates a reliable semantic activation pattern in response to an orthographic input. Word frequency was implemented in the model’s learning mechanism. Error by which back-propagation adjustments were made was scaled according to a word’s frequency, such that more frequent words enacted larger weight changes than less frequent words. Because the current model only differed from that of H&W (2015) in that it additionally implemented word frequency, it will be referred to as “the model with frequency”.

Crucially, semantic target patterns changed slightly each on each trial for a given word. This variation was the implementation of the authors’ theory of semantic diversity—i.e., the theory that a word’s meaning varies as a function of the context in which it is observed (H&W, 2015). Each word was associated with 50 different target semantic patterns, each of which was a distortion of its prototype pattern. In the original and the current model, for high SemD words, each unit in the prototype had a probability of 0.2 of changing from its original value (i.e., 0 to 1 or 1 to 0), and for low SemD words the probability was 0.1. This created a larger amount of variation in the representation of high SemD words than low SemD words, while still creating some variation in low SemD words. Across training, all 50 target patterns for each of the 96 words were presented.

The network’s other training parameters included a learning rate of 0.2, momentum of 0.9 applied when the gradient of the error slope was <1, and a weight decay of 10<sup>-6</sup> applied to all connections. Weight updates occurred after presentation of the full stimulus set (50 versions of all 96 word presentations), and training proceeded for 1,000 updates. To encourage the network to activate all semantic information associated with a word, a constant of 0.5 was added to all active targets. 

For each model evaluation, ten separate networks were trained in this manner, each with different random starting weights so that effects could be compared across items and networks. Representations across SemD and frequency groups were compared by several measures. Primarily, mean activation across units of the semantic layer was used, as in H&W (2015). At times, it was relevant to investigate the polarity of units or the cross-entropy error of units. Polarity, also known as stress, is the degree to which units tend to take on more extreme values (see Plaut, 1997). Because activation in the current model ranges from 0 to 1, a polarity of 0 for a given unit would indicate unit activation of 0.5. Polarity values approaching 1 would indicate activation closer to 0 or 1 (i.e., further from 0.5). Cross-entropy error, conceptually, represents the distance between the active representation and the target representation. It was calculated using the equation of [Mirman & Chen 2012](https://doi.org/10.1037/a0027175).

## Results

Consistent with the original model of H&W (2015), the model with frequency was able to associate the 96 orthographic patterns with variable semantic patterns. The semantic pattern learned by the model was a composite of the 50 different versions of the prototype semantic pattern for each word. Representations with high SemD, i.e., more poorly specified representations, were noisier than those with low SemD, i.e., better specified representations. This was observed in the average polarity of representations. Across models, the average polarity of units across the semantic layer for high SemD words was 0.54, whereas the average polarity for low SemD words was 0.59, t(18) = 8.03, p < .001. This result confirms that low SemD words had more well-specified representations than high SemD words.

Also consistent with the original H&W model, the model with frequency showed more mean activation across the semantic layer for high SemD words (M = 28.6) than low SemD words (M = 27.3). This was true across items (F(1,9) = 204.6, p < .001) and across models (F(1,23) = 344.4, p < .001). Higher average activation across semantic units for high SemD words than low SemD words was interpreted by H&W as an explanation for the benefit of high SemD in lexical decision. 

The model with frequency also showed more activation across the semantic layer for low frequency (M = 28.4) than high frequency words (M = 27.4). This was true across items (F(1,9) = 446.6, p < .001) and across models (F(1,23) = 163.6, p < .001). If higher mean activation across units were interpreted as beneficial, as was done for SemD by H&W (2015), the results for high and low frequency in the current model would suggest that low frequency provides a benefit to RTs or accuracy in lexical decision. However, this interpretation would clearly be incorrect, as seen in the results from section one above and results from previous literature (e.g., Balota & Chumbley, 1984; Forster & Chambers, 1973; Chapman & Martin, 2021). Thus, we must not interpret higher mean activation across semantic units as reflecting superior performance in lexical decision.

Most relevant to our concerns, the H&W model with frequency showed an interaction between frequency and SemD such that low SemD words showed a stronger effect of word frequency than did high SemD words across items (F(1,9) =89.4, p < .001) and across models (F(1,23) = 22.2, p < .001). Word frequency effects for both high and low SemD words showed higher mean activation for low than high frequency items. This pattern does not reflect the interaction found in several lexical decision databases across hundreds of participants and thousands of items, which showed that high SemD words showed stronger effects of word frequency. Furthermore, this pattern of model performance, though it shows an interaction between frequency and SemD, cannot reflect real performance, as seen by the contradictory findings for frequency and SemD. If more activation across the semantic layer is taken to reflect better lexical decision performance, this model predicts incorrectly that low frequency words will show a benefit over high frequency words, though it correctly predicts a benefit for high SemD over low SemD words. If less activation were taken to reflect better lexical decision performance, this model predicts incorrectly that low SemD words will show a benefit over high SemD words, though it correctly predicts a benefit of high frequency over low frequency words. For this model to be meaningful, mean semantic activation would have to correlate with behavior in some way, and this is clearly not so. Thus, this simulation shows that the theory of H&W (2015) on the effect of SemD on representations is flawed and that mean activation across the semantic layer cannot be interpreted as the ultimate decision generator in lexical decision, contrary to the claims of H&W (2015). 

