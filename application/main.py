#!/usr/bin/env python

import lib.parser as parser
import sys


def main(args):
    if len(args) < 1:
        print("No file specified!")
        return

    try:
        automat = parser.parseFromFile(args[1])
    except parser.ParseError as e:
        print(e.message)
        return

    while True:
        arguments = sys.stdin.readline().split(maxsplit=1)
        if len(arguments) == 1:
            command = arguments[0]
            if command == "exit":
                return
            elif command in ["check"]:
                print("Not enough arguments")
            else:
                print(f"Command {command} not found")

        elif len(arguments) > 1:
            command, arguments = arguments
            if command == "check":
                if automat.go(arguments.strip()):
                    print("String is accepted")
                else:
                    print("String is not accepted")
            else:
                print(f"Command {command} not found")

        else:
            print("Not enough arguments")


if __name__ == "__main__":
    main(sys.argv)
