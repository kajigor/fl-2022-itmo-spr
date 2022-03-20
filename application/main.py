#!/usr/bin/env python

import lib.parser as parser
import sys

def main(args):
    if(len(args) < 1):
        print("No file specified!")
        return
    try:
        a = parser.parseFromFile(args[1])
    except parser.ParseError as e:
        print(e.message)
        return
    #FIXME: test
    print(a.go("aaaaabbbc"))
        

if __name__ == "__main__":
    main(sys.argv)
