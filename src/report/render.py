import pypugjs
import pathlib
import os 

dir_path = os.path.dirname(os.path.realpath(__file__))

def make_report():
    with open(pathlib.Path(dir_path, 'template.pug').absolute(), 'w+') as pugfile:
        print(pypugjs.process(pugfile.read()))
