import json

class Selector:
    def __init__(self, report):
        self.report = report
        self.__section = None
        self.__attribute = None
        self.__paths = []

    def section(self, name):
        self.__section = name
        self.__paths = []
        return self

    def then(self, name):
        self.__check_path__()
        self.__paths.append(name)
        return self
    
    def part(self, name):
        self.__attribute = name
        self.__paths = []
        return self

    def __check_path__(self):
        if self.__section is None or self.__attribute is None:
            raise RuntimeError("Report section and path are not specified")

    def __set_path__(self, val):
        self.__check_path__()
        if not self.__section in self.report.body:
            self.report.body[self.__section] = {}

        paths = [self.__attribute] + self.__paths
        last = paths.pop()
        obj = self.report.body[self.__section]
    
        for n in paths:
            if n not in obj:
                obj[n] = {}
            obj = obj[n]

        obj[last] = val
        
    def set_val(self, val):
        self.__set_path__(val)

    def set_dict(self, dct):
        d = {}
        for key in dct:
            d[key] = str(dct[key])
        
        self.set_val(d)

    def get(self):
        self.__check_path__()
        obj = self.report.body[self.__section][self.__attribute]
        for n in self.__paths:
            obj = obj[n]
        return obj

    def append(self, val):
        self.__check_path__()

        if not self.__section in self.report.body:
            self.report.body[self.__section] = {}
        
        paths = [self.__attribute] + self.__paths
        last = paths.pop()
        obj = self.report.body[self.__section]
    
        for n in paths:
            if n not in obj:
                obj[n] = {}
            obj = obj[n]

        if last not in obj:
            obj[last] = [val]
        elif not isinstance(obj[last], list):
            raise "Not a list"
        else:
            obj[last].append(val)

class Report:

    def __init__(self):
        self.body = {}

    def section(self, name):
        return Selector(self).section(name)

    def save(self, name = 'data'):
        with open(f"{name}.json", 'w') as file:
            json.dump(self.body, file)

    def save_final(self, name = 'cnf'):
        with open(f"{name}.txt", 'w') as file:
            res = self.body['cnf']['dict']
            text = f"{self.body['initial']['start']} : {res[self.body['initial']['start']]}\n"
            for key in res:
                if key == self.body['initial']['start']:
                    continue
                text += f"{key} : {res[key]}\n"
            file.write(text)