from src.test.grammar import main as tmain
import sys
from antlr4 import FileStream, CommonTokenStream
from src.grammar.gen.MyGrammarLexer import MyGrammarLexer
from src.grammar.gen.MyGrammarParser import MyGrammarParser
from src.mapper.TreeMapper import make_dict
from pprint import pprint
from src.normal_form.transforms import transform

def console(args):
    if len(args)==1:
        print("No file specified!")
        return
    if(args[1]=='test'):
        tmain([args[0]]+args[2:])
        return
    try:
        input = FileStream(args[1])
        lexer = MyGrammarLexer(input)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        res_dict = make_dict(tree)
        pprint(res_dict)

    except parser.ParseError as e:
        print(e.message)
        return

    cnf_dict, name_of_file = transform(res_dict)
    while True:
        arguments = sys.stdin.readline().split(maxsplit=1)
        if len(arguments) == 1:
            command = arguments[0]
            if command == "exit":
                return
            elif command == "cnf": 
                # нормальная форма Хомского, напечатать
                pprint(cnf_dict)
            elif command == "cnfSteps": 
                # напечатать шаги преобразований в Хомского
                with open(name_of_file) as f:
                    while True:
                        line = f.readline()
                        if not line:
                            break
                        # выводим строку
                        print(line.strip())
            else:
                print(f"Command {command} not found")

        elif len(arguments) > 1:
            command, arguments = arguments
            if command == "check":
                """if automat_for_check.go(arguments.strip()):
                    print("String is accepted")
                else:
                    print("String is not accepted")"""
            else:
                print(f"Command {command} not found")

        else:
            print("Not enough arguments")

if __name__ == '__main__':
    console(sys.argv)

