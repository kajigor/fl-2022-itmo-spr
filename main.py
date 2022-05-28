import subprocess
from antlr4.error.Errors import ParseCancellationException
from src.cyk.cyk import cyk
from src.report.tools import Report

from src.test.grammar import main as tmain
import sys
from antlr4 import FileStream, CommonTokenStream
from src.grammar.gen.MyGrammarLexer import MyGrammarLexer
from src.grammar.gen.MyGrammarParser import MyGrammarParser
from src.mapper.TreeMapper import make_dict
from pprint import pprint
from src.normal_form.transforms import transform

import pathlib

def console(args):

    report = Report()

    if len(args) == 1:
        print("No file specified!")
        return
    if args[1] == 'test':
        tmain([args[0]] + args[2:])
        return
    try:
        input = FileStream(args[1])
        lexer = MyGrammarLexer(input)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        start_item, res_dict = make_dict(tree)
        
        report.section('initial').part('start').set_val(start_item)
        report.section('initial').part('dict').set_dict(res_dict)
        # pprint(res_dict)

    except ParseCancellationException:
        return

    cnf_dict, name_of_file = transform(res_dict, start_item, report)

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
            elif command == 'save':
                report.save()
                file = pathlib.Path(__file__).parent.resolve().joinpath('view','generator.js')
                subprocess.run(['node', file, 'data.json', 'report.html'])
                print('Done!')
            else:
                print(f"Command {command} not found")

        elif len(arguments) > 1:
            command, arg = arguments
            if command == "check":
                cyk(cnf_dict, arg[:-1], report)
            else:
                print(f"Command {command} not found")
        else:
            print("Not enough arguments")


if __name__ == '__main__':
    console(sys.argv)
