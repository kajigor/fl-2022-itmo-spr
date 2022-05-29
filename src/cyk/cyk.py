
def cyk(dct, word, report):

    cursor = report.section('cyk')

    def find(a,b = None):

        def inner_find(rule):
            if (rule.type() == 'RULE') and (b is None):
                return (rule.val() == a['val']) and (rule.isTerminal() == True) 

            if (rule.type() == 'AND') and (b is not None):
                return (rule[0].val() == a['val']) and (rule[1].val() == b['val'])

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

    steps = 0
    matrix = [[]]
    for i, l in enumerate(word):
        steps += 1
        found = find({ 'val' : l })
        res = []
        for r in found:
            res.append({ 'val' : r, 'step' : -1 })
        matrix[0].append(res)
    
    i = 1
    while i < len(word):
        line = []
        find_steps = []
        for pos in range(len(word) - i):
            cell = []
            for j in range(i):
                steps += 1
                c1 = matrix[j][pos]
                c2 = matrix[i - j - 1][pos + 1 + j]
                find_steps.append([[j, pos], [i - j - 1, [pos + 1 + j]]])
                for n1 in c1:
                    for n2 in c2:
                        found = find(n1,n2)
                        for f in found:
                            cell.append({ 'val' : f, 'step' : j })
            line.append(cell)

        for _ in range(i):
            line.append([])

        i += 1
        matrix.append(line)
    
    meta = {}
    meta['word'] = word
    meta['steps'] = steps
    meta['result'] = matrix

    result = False
    for item in matrix[len(word) - 1][0]:
        if item['val'] == '_START':
            result = True
            break
    
    meta['verdict'] = result

    cursor.part('checks').append(meta)

    return result

    
