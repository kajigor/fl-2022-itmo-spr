
from pprint import pprint


def cyk(dct, word, report):

    cursor = report.section('cyk')

    def find(a,b = None):

        def inner_find(rule):
            if (rule.type() == 'RULE') and (b is None):
                return (rule.val() == a) and (rule.isTerminal() == True) 

            if (rule.type() == 'AND') and (b is not None):
                return (rule[0].val() == a) and (rule[1].val() == b)

            if rule.type() == 'OR':
                for r in rule.getItems():
                    if inner_find(r):
                        return True

            return False

        res = []
        for key in dct:
            if inner_find(dct[key]):
                res.append(key)

        return res

    steps = []
    matrix = [[]]
    for i, l in enumerate(word):
        steps.append([0, i, []])
        matrix[0].append(find(l))
    
    i = 1
    while i < len(word):
        line = []
        find_steps = []
        for pos in range(len(word) - i):
            cell = []
            for j in range(i):
                c1 = matrix[j][pos]
                c2 = matrix[i - j - 1][pos + 1 + j]
                find_steps.append([[j, pos], [i - j - 1, [pos + 1 + j]]])
                for n1 in c1:
                    for n2 in c2:
                        cell = cell + find(n1,n2)
            steps.append([i, pos, find_steps])
            line.append(cell)

        i += 1
        matrix.append(line)
    
    cursor.part('steps').set_val(steps)
    cursor.part('result').set_val(matrix)
    cursor.part('verdict').set_val('_START' in matrix[len(word) - 1][0])

    return matrix

    
