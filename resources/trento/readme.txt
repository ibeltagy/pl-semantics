This dataset consists of a ground truth dataset mixed with a
non-paraphrasing alternative dataset:

=============================================================

GROUND TRUTH SETS: Obtained from the MSR Video Description corpus
(Chen and Dolan, 2011) by including only those sentences that had a
simple structure entirely accounted for by 4 categories (noun, verb,
adjective and determiner). The verb was always converted to present
tense, and in some cases in which the original set contained sentences
that are not acceptable paraphrases of each other, small changes were
applied (e.g., A woman plays -> A man plays or A man plays -> Two men
play).

To the sentences so obtained, a few sentences were added for each
ground truth set. The new sentences were obtained by adding
attributive adjectives or determiners to the nouns. Determiners and
adjectives were chosen so that they would not distort the meaning of
the sentence.

The ground truth sets are denoted by Arabic numerals and they are
grouped into larger sets marked with Roman numbers ((I), (II) etc.,
viz. the sentences in the ground truth sets within e.g. (I) are closer
to each other than to the sentences in the ground truth sets marked by
e.g. (II).

The sentences of the ground truth set are marked by P
(paraphrases). E.g.

1
P 5 20 A man plays a guitar
P 5 20 A man plays a acoustic guitar
P 5 20 A man plays a electric guitar
P 5 20 A seated man plays guitar
P 5 20 A old man plays guitar
P 5 20 The man plays the guitar
P 5 20 The man plays music
P 5 20 A man plays a instrument

is set nr. 1 of the ground truth dataset

2 
P 168 178 A girl plays violin
P 168 178 A lady plays violin
P 168 178 A woman plays violin
P 168 178 A woman plays the violin

is set nr. 2 of the the ground truth dataset

=============================================================

NON-PARAPHRASING ALTERNATE SETS: The "non-paraphrasing sentences" were
generated starting from the P described above.

This sets consist of scrambled sentences obtained by changing the word
order (marked by S), sentences with modified determiner, e.g. A is
replaced by No, (marked by D) and sentences obtained from D by
scrambling the word order (marked by SD).

Note that the change in determiner, unlike in the case of the
ground-truth sets above, is disruptive of meaning compatibility (A
girl plays violin vs NO girl plays violin).

These sentences together (S: scrambled, D: modified DET, SD: scambled
and modified DET) are considered non-paraphrasing sentences (NP).

=============================================================

The sentences of both sets are ordered according to their borad set
number (I, II, ...), then ground truth set number (1,2,3...)  and
specific type (P, S, D and SD).

For example, the following is set nr. 1 of the dataset, which consists
of 4 subset of sentences P, S, D, SD:

(I)

1
P 5 20 A man plays a guitar
P 5 20 A man plays a acoustic guitar
P 5 20 A man plays a electric guitar
P 5 20 A old man plays guitar
P 5 20 The man plays the guitar
P 5 20 The man plays music
P 5 20 A man plays a instrument

S 5 20 A guitar plays a man
S 5 20 A guitar plays no man
S 5 20 A acoustic guitar plays a man
S 5 20 A instrument plays a man

D  5 20 No man plays no guitar
SD 5 20 No guitar plays no man
SD 5 20 No guitar plays a man
D 5 20 Some man plays no guitar
SD 5 20 Some guitar plays no man
SD 20 No guitar plays some man
D 5 20 The man plays no guitar
SD 5 20 The guitar plays no man
SD 5 20 No guitar plays the man

D 5 20 No man plays a acoustic guitar
D 5 20 A man plays no acoustic guitar
SD 5 20 A guitar plays no acoustic man
SD 5 20 A acoustic guitar plays no man
SD 5 20 No acoustic guitar plays a man
D 5 20 No man plays a old guitar
SD 5 20 No guitar plays a old man

=============================================================

Evaluation methods for compositional models based on this dataset:

1) Clustering:

Cluster the representation of the **P sentences**. Report the entropy
and purity for the model.

2) P vs NP distance:

For each paraphrasing sentence (P), compute the mean of the cosine of
the sentence with all the sentences in the same ground truth set (with
all the P sentences) (cos.para) and the mean of the cosine with all
the non-paraphrasing sentences (with all the NP sentences, S, D, SD)
of the same set (cos.nonpara); then compute the difference between
cos.para and cos.nonpara (diff.para.nonpara = cos.para - cos.nonpara).
Compute the mean of the diff.para.nonpara for all the sentences in the
data set. The higher the resulting value, the better the model.

=============================================================

For further information, please refer to:

N. Pham, R. Bernardi, Y-Z. Zhang and M. Baroni. To appear. Sentence
paraphrase detection: When determiners and word order make the
difference. Proceedings of the Towards a Formal Distributional
Semantics Workshop at IWCS 2013

and cite this paper if using the data set for published work.
