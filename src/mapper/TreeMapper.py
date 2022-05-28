from matplotlib.style import context
from src.grammar.gen.MyGrammarParser import MyGrammarParser

class Epsilon:
    def __init__(self):
        pass

    def isTerminal(self):
        return True

    def getItems(self):
        return []

    def add(self, item):
        item = item.make_final()
        if item.type() == 'OR':
            item.add(self)
            return item
        else:
            raise RuntimeError("Can not add epsilon to not OR")

    def val(self):
        return None

    def setMod(self, m):
        pass

    def getMods(self):
        pass

    def containsEps(self):
        return True

    def containsTerminal(self, _):
        return False

    def containsNotTerminal(self, _):
        return False

    def make_final(self):
        return self

    def type(self):
        return "EPSILON"
    
    def __str__(self):
        return "#e"

    def __repr__(self):
        return "#e"

    def __len__(self):
        return 1

class Rule:
    def __init__(self, value, terminal) :
        self.__value = value
        self.__terminal = terminal
        self.__mods = None

    def isTerminal(self):
        return self.__terminal
    
    def getItems(self):
        return []

    def add(self, item):
        item = item.make_final()
        if item.type() == 'OR':
            item.add(self)
            return item
        else:
            raise RuntimeError("Can not add rule to not OR")

    def val(self):
        return self.__value

    def setMod(self, m):
        self.__mods = m

    def containsEps(self):
        return False
    
    def containsTerminal(self, val):
        return self.__terminal and self.__value == val

    def containsNotTerminal(self, val):
        return (not self.__terminal) and self.__value == val

    def getMods(self):
        return self.__mods

    def make_final(self):
        return self

    def __len__(self):
        return 1

    def type(self):
        return "RULE"

    def __str__(self):
        if self.__terminal:
            return f'"{self.__value}"'
        else:
            return self.__value

    def __repr__(self):
        if self.__terminal:
            return f'"{self.__value}"'
        else:
            return self.__value

class ContextAnd:
    def __init__(self):
        self.__items = []
        self.__mods = None

    def setMod(self, m):
        self.__mods = m

    def getMods(self):
        return self.__mods

    def getItems(self):
        return self.__items

    def add(self, item):
        item = item.make_final()
        if len(item) == 0:
            return self
        if item.type() == 'AND':
            for i in item.getItems():
                self.__items.append(i)
            return self
        self.__items.append(item)
        return self

    def __getitem__(self, idx):
        return self.__items[idx]

    def containsEps(self):
        for i in self.__items:
            if i.containsEps():
                return True
        return False
    
    def containsTerminal(self, val):
        for i in self.__items:
            if i.containsTerminal(val):
                return True
        return False

    def containsNotTerminal(self, val):
        for i in self.__items:
            if i.containsNotTerminal(val):
                return True
        return False

    def first(self):
        return self.__items[0]

    def removeIdx(self, idx):
        self.__items.remove(self.__items[idx])

    def removeVal(self, val):
        self.__items.remove(val)

    def make_final(self):
        if len(self.__items) == 1:
            return self.__items[0].make_final()
        return self

    def __str__(self):
        res = []
        for i in self.__items:
            res.append(str(i))
        return " ".join(res)

    def __repr__(self):
        res = []
        for i in self.__items:
            res.append(repr(i))
        return " ".join(res)

    def __len__(self):
        return len(self.__items)

    def type(self):
        return "AND"

class ContextOr:
    def __init__(self):
        self.__items = []
        self.__mods = None

    def setMod(self, m):
        self.__mods = m

    def getMods(self):
        return self.__mods

    def getItems(self):
        return self.__items

    def add(self, item):

        def contains(i):
            if i.type() != 'RULE':
                return False
            for j in self.__items:
                if i.type() == j.type() and i.isTerminal() == j.isTerminal() and i.val() == j.val():
                    return True
            return False

        item = item.make_final()
        if len(item) == 0:
            return self
        if item.type() == 'OR':
            for i in item.getItems():
                if not contains(i):
                    self.__items.append(i)
            return self
        if contains(item):
            return self
        self.__items.append(item)
        return self

    def containsEps(self):
        for i in self.__items:
            if i.containsEps():
                return True
        return False
    
    def containsTerminal(self, val):
        for i in self.__items:
            if i.containsTerminal(val):
                return True
        return False

    def containsNotTerminal(self, val):
        for i in self.__items:
            if i.containsNotTerminal(val):
                return True
        return False

    def __getitem__(self, idx):
        return self.__items[idx]

    def first(self):
        return self.__items[0]

    def removeIdx(self, idx):
        self.__items.remove(self.__items[idx])

    def removeVal(self, val):
        self.__items.remove(val)

    def __str__(self):
        res = []
        for i in self.__items:
            res.append(str(i))
        return " | ".join(res)

    def __repr__(self):
        res = []
        for i in self.__items:
            res.append(repr(i))
        return " | ".join(res)

    def __len__(self):
        return len(self.__items)

    def make_final(self):
        if len(self.__items) == 1:
            return self.__items[0].make_final()
        return self

    def type(self):
        return "OR"

def where(c, r, count = None):
    if not hasattr(c, 'getRuleIndex'):
        return False
    return c.getRuleIndex() == r and (count is None or c.getChildCount() == count)

def make_dict(node):

    side_dict = {}
    name_var = 0

    start_item = None

    def take_name():
        nonlocal name_var
        name_var += 1
        return f"${name_var}" 

    def make_dict_inner(node):
        nonlocal start_item
        if where(node, MyGrammarParser.RULE_startRule):
            return make_dict_inner(node.children[0])
        if where(node, MyGrammarParser.RULE_block):
            d = {}
            for n in node.children:
                if where(n, MyGrammarParser.RULE_item):
                    key, val = make_dict_inner(n)
                    if key in d:
                        if d[key].type() == 'OR':
                            d[key].add(val)
                        else:
                            d[key] = ContextOr().add(val).add(d[key])
                    else:
                        d[key] = val.make_final()
            return d

        if where(node, MyGrammarParser.RULE_item):
            return make_dict_inner(node.children[0])

        if where(node, MyGrammarParser.RULE_token) or where(node, MyGrammarParser.RULE_ruleStatement):
            key = node.children[0].getText()
            if start_item is None:
                start_item = key
            return (key, make_dict_inner(node.children[2]))
            
        if where(node, MyGrammarParser.RULE_contents):
            storage = ContextOr()
            for c in node.children:
                if where(c, MyGrammarParser.RULE_content):
                    storage.add(make_dict_inner(c))
            return storage.first() if len(storage) == 1 else storage

        if where(node, MyGrammarParser.RULE_content):
            if node.getChildCount() == 0:
                return Epsilon()
            storage = ContextAnd()
            for c in node.children:
                res = make_dict_inner(c)
                if res.type() == 'OR' and node.getChildCount() > 1:
                    name = take_name()
                    side_dict[name] = res.make_final()
                    storage.add(Rule(name, False))
                else:
                    storage.add(res)
            return storage.first() if len(storage) == 1 else storage

        if where(node, MyGrammarParser.RULE_expression, 1):
            return make_dict_inner(node.children[0])

        if where(node, MyGrammarParser.RULE_expression, 2):
            res = make_dict_inner(node.children[0])
            op = node.children[1].getText()

            name = None
            terminal = None
            if res.type() != 'RULE':
                name = take_name()
                side_dict[name] = res.make_final()
                terminal = False
            else:
                name = res.val()
                terminal = res.isTerminal()

            if op == '*':
                tmp = take_name()
                box = ContextOr() \
                    .add(Epsilon()) \
                    .add(ContextAnd().add(Rule(tmp, False)).add(Rule(name, terminal)))
                side_dict[tmp] = box
                return Rule(tmp, False)

            if op == '?':
                tmp = take_name()
                box = ContextOr() \
                    .add(Epsilon()) \
                    .add(Rule(name, terminal))

                side_dict[tmp] = box
                return Rule(tmp, False)

            if op == '+':
                tmp = take_name()
                box = ContextOr() \
                    .add(Epsilon()) \
                    .add(ContextAnd().add(Rule(tmp, False)).add(Rule(name, terminal)))
                side_dict[tmp] = box
                return ContextAnd().add(Rule(name, terminal)).add(Rule(tmp, False))

            return res

        if where(node, MyGrammarParser.RULE_expression, 4):
            res = make_dict_inner(node.children[0])
            
            name = None
            terminal = None
            if res.type() != 'RULE':
                name = take_name()
                side_dict[name] = res.make_final()
                terminal = False
            else:
                name = res.val()
                terminal = res.isTerminal()

            bucket = ContextAnd()
            for _ in range(int(node.children[2].getText())):
                bucket.add(Rule(name, terminal))

            return bucket.make_final()

        if where(node, MyGrammarParser.RULE_expression, 6):
            res = make_dict_inner(node.children[0])
            
            name = None
            terminal = None
            if res.type() != 'RULE':
                name = take_name()
                side_dict[name] = res.make_final()
                terminal = False
            else:
                name = res.val()
                terminal = res.isTerminal()


            bucket = ContextOr()
            s = int(node.children[2].getText())
            t = int(node.children[4].getText())
            
            for i in range(t - s + 1):
                innerBucket = ContextAnd()
                for _ in range(s + i):
                    innerBucket.add(Rule(name, terminal))
                
                bucket.add(innerBucket)

            return bucket.make_final()

        if where(node, MyGrammarParser.RULE_term, 3):
            return make_dict_inner(node.children[1])

        if where(node, MyGrammarParser.RULE_term, 1):
            return make_dict_inner(node.children[0])

        if where(node, MyGrammarParser.RULE_value, 3):
            storage = ContextOr()

            start = ord(node.children[0].getText()[1:-1])
            finish = ord(node.children[2].getText()[1:-1])
            for i in range(start, finish + 1):
                storage.add(Rule(chr(i), True))

            return storage.first() if len(storage) == 1 else storage

        if where(node, MyGrammarParser.RULE_value, 1):
            if not where(node.getChild(0), MyGrammarParser.RULE_name):
                text = node.getChild(0).getText()[1:-1]
                storage = ContextAnd()
                for letter in text:
                    storage.add(Rule(letter, True))
                return storage
            else:
                return make_dict_inner(node.getChild(0))

        if where(node, MyGrammarParser.RULE_name):
            return Rule(node.children[0].getText(), False)

    res = make_dict_inner(node)
    for key in side_dict:
        res[key] = side_dict[key]
    
    return start_item, res
