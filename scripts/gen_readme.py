#!/usr/bin/env python

import argparse

APPROACHES = {
    'run1primary': 'Using probabilistic logic to combine logical and distributional semantics, as well as a vector representation for the whole sentence, and counts of negated words (no, none, not, etc...)',
    'run2':        'Using probabilistic logic to combine logical and distributional semantics, using cosine as a lexical entailment predictor',
    'run3':        'Using probabilistic logic to combine logical and distributional semantics, using a supervised lexical entailment predictor',
    'run4':        'Using a distributional vector space, and counts of negated words (no, none, not, etc)'
}

TOOLS = {
    'run1primary': 'Markov Logic Networks, Probabilistic Similarity Logic, BOXER',
    'run2':        'Markov Logic Networks, Probabilistic Similarity Logic, BOXER',
    'run3':        'Markov Logic Networks, Probabilistic Similarity Logic, BOXER',
    'run4':        'None'
}

FEATURES = {
    'run1primary': 'Logical representation of the sentences, distributional representation of words and phrases',
    'run2':        'Logical representation of the sentences, distributional representation of words and phrases',
    'run3':        'Logical representation of the sentences, distributional representation of words and phrases',
    'run4':        'Distributional representation of words and phrases.'
}


TEMPLATE = """1. UTexas
2. University of Texas at Austin
3. Stephen Roller <roller@cs.utexas.edu>
4. UTexas_%s.zip
5. System specs
- 5.1 Core approach: %s
- 5.2 Supervised or unsupervised: Supervised
- 5.3 Critical features used: %s
- 5.4 Critical tools used:  %s
- 5.5 Significant data pre/post-processing: n/a
- 5.6:
    - The BLESS list of lexical relations
    - Distributional vector space computed from ukWaC, Wackypedia, BNC and Gigaword
6. References (if applicable)"""

def main():
    parser = argparse.ArgumentParser('Generates the readme file for a run.')
    parser.add_argument('--run', help='Run name. Either "run1primary", "run2", "run3", or "run4".')
    args = parser.parse_args()

    assert args.run in ('run1primary', 'run2', 'run3', 'run4')

    print TEMPLATE % (args.run, APPROACHES[args.run], FEATURES[args.run], TOOLS[args.run])


if __name__ == '__main__':
    main()


