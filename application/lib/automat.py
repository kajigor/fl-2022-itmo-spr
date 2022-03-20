
class State:
    def __init__(self, name, is_terminal = False):
        self.name = name
        self.is_terminal = is_terminal
        self.children = {}
    def add(self, action, state):
        if action not in self.children:
            self.children[action] = []
        self.children[action].append(state)
    def go(self, lit, branch = 0):
        if lit not in self.children:
            return None
        return self.children[lit][branch]

class Automatos:
    def __init__(self, alfabet):
        self.states = {}
        self.root = None
        self.alfabet = alfabet
    def addState(self, name):
        if name in self.states:
            return
        self.states[name] = State(name)
    def makeTerminal(self, name):
        self.states[name].is_terminal = True
    def addAction(self, name_start, name_finish, lit):
        self.states[name_start].add(lit, self.states[name_finish])
    def makeRoot(self, name):
        self.root = self.states[name]
    def has(self, name):
        return name in self.states
    def go(self, str):
        current = self.root
        for c in str:
            if current is None:
                break
            current = current.go(c)
        return current != None and current.is_terminal
        
    

