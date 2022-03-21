from os import stat
from automat import Automatos

class ParseError(Exception):
    def __init__(self, message):
        self.message = message
        super().__init__(self.message)

def isVertFirst(x):
    return x >= 'a' and x <= 'z' or x >= 'A' and x <= 'Z'

def isVertInner(x):
    return isVertFirst(x) or x >= '0' and x <= '9' or x == '_'

def toLineEnd(string, start):
    while string[start] != '\n':
        start += 1
    return start

def parseStringWithSpaces(string, start):
    res = []
    i = start
    state = 0
    buffer = ""
    while string[i] != '\n':
        ch = string[i]
        if state == 0:
            if ch == "'":
                state = 3
                i += 1
            elif isVertFirst(ch):
                state = 1
            else:
                i += 1
        elif state == 1:
            buffer += ch
            state = 2
            i += 1
        elif state == 2:
            if not isVertInner(ch):
                res.append(buffer)
                buffer = ""
                state = 0
            else:
                buffer += ch
                i += 1
        else:
            if ch == "'":
                res.append(buffer)
                buffer = ""
                state = 0
                i += 1
            else:
                buffer += ch
                i += 1

    if buffer != "":
        res.append(buffer)
    return (i + 1, res)
    

def parse(string):
    alphabet = []

    # Parse alphabet
    i = 0
    while string[i] != '\n':
        alphabet.append(string[i])
        i += 1

    # Parse verts
    sidx, start = parseStringWithSpaces(string, i + 1)
    tidx, terminals_parsed = parseStringWithSpaces(string, sidx)
 
    if string[tidx] != '=':
        raise ParseError("Syntax error, expected =")

    # fill automaton
    a = Automatos(alphabet)   

    # parse actions
    tidx = toLineEnd(string, tidx) + 1
    while string[tidx] != '=':
        tidx, actions = parseStringWithSpaces(string, tidx)
        if len(actions) < 3:
            raise ParseError("Syntax error, expected more args")
        if not a.has(actions[0]):
            a.addState(actions[0])
        if not a.has(actions[1]):
            a.addState(actions[1])
        if actions[2] not in alphabet:
            raise ParseError("Error, lit not in alphabet")
        a.addAction(actions[0], actions[1], actions[2])

    if len(start) != 1:
        raise ParseError("Error, start point mast be single")
    if not a.has(start[0]):
        raise ParseError("Error, start point not found")
    a.makeRoot(start[0])

    if len(terminals_parsed) == 0:
        raise ParseError("Error, not terminal points")

    for t in terminals_parsed:
        if not a.has(t):
            raise ParseError("Error, terminal point not found")
        a.makeTerminal(t)

    return a

def parseFromFile(filename):
    with open(filename) as file:
        return parse(file.read())
