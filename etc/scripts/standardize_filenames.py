#!/usr/local/bin/python3
'''This script is a recursive corrector of SICP filenames!
Written in python, on account of python being pretty hip,
it evaluates the language of inconsistent file names and
returns consistent file names!

It is gratuitous overkill, but I think it fits the theme...
Maaaaaybe. If the theme is evaluators, vs good programming
design!

On reflection, this approach doesn't even make sense, since 
my naming convention isn't remotely recursive...

Oh well! When you have a hammer, as the saying goes...
A lesson has been learned.
'''

import collections
import os
import os.path as path
from pathlib import *
import re

ExpressionType = collections.namedtuple('ExpressionType',
                                        ['predicate', 'evaluator'])
                                        
##############################
# Dispatch table

class DispatchTable(object):
    ''' "Data directed programming" -
    takes an expression, and, based on the expression types
    registered with this class, either evaluates the expressions
    or raises a LookupError!
    '''
    def __init__(self):
        self.table = []

    def register(self, expression_type):
        self.table.append(expression_type)

    def dispatch(self, expr):
        for etype in self.table:
            if etype.predicate(expr):
                return etype.evaluator(expr)
        error_string = "Invalid expression {}".format(expr)
        raise LookupError(error_string)

g_dispatch_table = DispatchTable()

##############################
# Expression Types

def is_filename(expr):
    return expr.endswith(".lisp")
def eval_filename(expr):
    base, ext = os.path.splitext(expr)
    return evaluate(base) + ext

split_regex = re.compile('-|_')
def is_range(expr):
    return split_regex.search(expr) is not None
def eval_range(expr):
    part1, part2 = split_regex.split(expr)
    return evaluate(part1) + '-' + evaluate(part2)

def is_number(expr):
    return re.match('[0-9]+(\.[0-9]+)+', expr) is not None
def eval_number(expr):
    parts = expr.split('.')
    return '.'.join(['{:02d}'.format(int(x)) for x in parts])

def is_string(expr):
    return True
def eval_string(expr):
    return expr

##############################
# Registration

g_dispatch_table.register(ExpressionType(is_filename, eval_filename))
g_dispatch_table.register(ExpressionType(is_range,  eval_range))
g_dispatch_table.register(ExpressionType(is_number, eval_number))
g_dispatch_table.register(ExpressionType(is_string, eval_string))

##############################
# Our humble evaluate function

def evaluate(expr):
    return g_dispatch_table.dispatch(expr)

##############################
# Our command line tool (boo!)

if __name__ == "__main__":
    import sys
    input_path = sys.argv[1] if len(sys.argv) == 2 else '.'
    directory = Path(input_path)
    lisp_files = directory.glob('*.lisp')
    for file in lisp_files:
        try:
            file.rename(Path(directory, evaluate(file.name)))
        except LookupError as err:
            print("failed to convert {}".format(file))
            print(err)
