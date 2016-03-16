#!/bin/bash

candc/bin/pos --model candc/models/pos | candc/bin/ner --model candc/models/ner --ifmt "%w|%p \n" --ofmt "%w|%p|%n \n" |  candc/bin/chunk --model candc/models/chunk --ifmt "%w|%p|%n \n" --ofmt "%w|%p|%n|%c \n"  | candc/bin/parser --model candc/models/parser/ --super candc/models/super --ifmt "%w|%p|%n|%c \n" --pos candc/models/pos  -printer boxer
