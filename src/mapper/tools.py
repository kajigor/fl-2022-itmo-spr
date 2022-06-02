
# from src.mapper.TreeMapper import *

def is_same(dct, name1, name2):
    
    if name1 not in dct:
        raise RuntimeError(f"Wrong key {name1}")
    
    if name2 not in dct:
        raise RuntimeError(f"Wrong key {name2}")

    def is_same_inner(node1, node2):
        
        if node1.type() != node2.type():
            return False

        if len(node1) != len(node2):
            return False

        if node1.type() == 'OR' or node1.type() == 'AND':
            for i in range(len(node1)):
                if not is_same_inner(node1[i], node2[i]):
                    return False
            return True
        
        if node1.type() == 'RULE':
            if node1.isTerminal() != node2.isTerminal():
                return False
            if node1.val() != node2.val():
                if not (node1.val() == name1 and node2.val() == name2):
                    return False
            return True

        return True

    return is_same_inner(dct[name1], dct[name2])

def rename(dct, fr, to):
    
    def rename_inner(node):
        if node.type() == 'RULE':
            if node.val() == fr and not node.isTerminal():
                node.rename(to)
            return

        if node.type() == 'EPSILON':
            return

        for item in node.getItems():
            rename_inner(item)
        
    for key in dct:
        rename_inner(dct[key])

    return dct

def remove_duplicates(dct):

    while True:
        found = False

        name1 = None 
        name2 = None

        for k1 in dct:
            for k2 in dct:
                if k1 == k2:
                    continue
                if is_same(dct, k1, k2):
                    name1 = k1
                    name2 = k2
                    found = True
                    break
            if found:
                break
        
        if not found:
            break
        else:
            dct.pop(name2)
            rename(dct, name2, name1)

def pick_a_name(prefix, *dct):
    s = prefix
    while True:
        ch = ord('A')
        while ch <= ord('Z'):
            contains = False
            for d in dct:
                if ((s + chr(ch) in d)):
                    contains = True
            
            if not contains:
                return s + chr(ch)

            ch += 1
        s += prefix


def pick_a_name_from_set(names):
    s = 'X'
    while True:
        ch = ord('A')
        while ch <= ord('Z'):
            if not ((s + chr(ch) in names)):
                return s + chr(ch)
            ch += 1
        s += 'X'