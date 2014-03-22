#!/usr/bin/env python

import sys

def main():
    for line in sys.stdin:
        line = line.rstrip()
        fields = line.split("\t")
        wordname = fields.pop(0)
        while fields:
            fieldname = fields.pop(0)
            fieldval = fields.pop(0)
            if not fieldval:
                continue
            print "\t".join([wordname, fieldname, fieldval])

if __name__ == '__main__':
    main()
