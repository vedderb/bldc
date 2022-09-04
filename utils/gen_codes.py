
    # Copyright 2019,2021 Joel Svensson	svenssonjoel@yahoo.se

    # This program is free software: you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation, either version 3 of the License, or
    # (at your option) any later version.

    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.

    # You should have received a copy of the GNU General Public License
    # along with this program.  If not, see <http://www.gnu.org/licenses/>.


import huffman
import collections

symchars  = 'abcdefghijklmnopqrstuvwxyz!?'
numchars  = '0123456789'
funchars  = ['+','-','*','/','=','<','>','.',':','#','\\\"','\\\\', '\'', ' ', '`', ',',',@', '_', '.']
short_lispnames = ['(', ')', '))', '((', '[', ']' ]
long_lispnames  = ['lambda', 'if', 'list', 'quote', 'progn', 'define', 'let',
                   'cons', 'car', 'cdr', '((',  '))', ')))', '))))', 'nil']
long_lispnames2  = ['lambda', 'if', 'list', 'quote', 'progn', 'define', 'let',
                    'cons', 'car', 'cdr', 'nil']
lispnames = short_lispnames + long_lispnames2

def total_bits(c) :
    sum = 0;
    for code in c:
        sum += len(code[1])
    return sum

#TODO: implement a scoring function to maximize against in the search
#      - small maximum number of bits
#      - cheap common strings
def score_fun() :
    return 0


def minimum_total_bits_codes() :
    min_total_num_bits = 1000000
    min_codes = []
    r = [100, 200, 300, 400, 500, 600, 700]
    space = [ (x,y,z,v) for x in r for y in r
                          for z in r for v in r]
                          #for w in r]
    for point in space :
        
        numchars_w  = [(x, (point[0] * 10) / len(numchars)) for x in numchars]
        symchars_w  = [(x, (point[1] * 5) / len(symchars)) for x in symchars]
        funchars_w  = [(x, (point[2] * 20) / len(funchars)) for x in funchars]
        lispnames_w = [(x, (point[3] * 15) / len(lispnames)) for x in lispnames]
        all_w = symchars_w + lispnames_w + funchars_w + numchars_w
        codes = huffman.codebook(all_w).items()
        size = total_bits(codes)
        # print("smallest: % d current: % d\n" % (min_total_num_bits, size)  )
        if size < min_total_num_bits :
            min_total_num_bits = size
            min_codes = codes

    return (min_total_num_bits, min_codes)

def make_c() :
    codes = minimum_total_bits_codes()

    print("Total number of bits %d\n" % codes[0])
    
    max_key  = max([len(x[0]) for x in codes[1]]);
    max_code = max([len(x[1]) for x in codes[1]]);
    
    code_map = ''

    first = True;
    
    for code in codes[1] :
        if (first) :
            code_map = '    { \"' + code[0] + '\", \"' + code[1] + '\" }' + code_map
            first = False
        else :
            code_map = '    { \"' + code[0] + '\", \"' + code[1] + '\" },\n' + code_map
                  
    c_str = '#define NUM_CODES ' + str(len(codes[1])) + '\n' \
            '#define MAX_KEY_LENGTH ' + str(max_key) + '\n' + \
            '#define MAX_CODE_LENGTH ' + str(max_code) + '\n' + \
            'char *codes[NUM_CODES][2] = {\n' + code_map + '\n    };\n'
    
    
    return c_str

def generate_codes() :
    numchars_w = [(x, 10) for x in numchars]
    symchars_w = [(x, 20) for x in symchars]
    short_lispnames_w = [(x, 100) for x in short_lispnames]
    long_lispnames_w =  [(x, 1)  for x in long_lispnames2]
    funchars_w = [(x, 9) for x in funchars]

    all_w = funchars_w + numchars_w + symchars_w + short_lispnames_w + long_lispnames_w
    codes = huffman.codebook(all_w).items()
    
    return codes
    
def make_c_codes() :
    codes = generate_codes()

    
    max_key  = max([len(x[0]) for x in codes]);
    max_code = max([len(x[1]) for x in codes]);
    
    code_map = ''

    first = True;
    
    for code in codes :
        if (first) :
            code_map = '    { \"' + code[0] + '\", \"' + code[1] + '\" }' + code_map
            first = False
        else :
            code_map = '    { \"' + code[0] + '\", \"' + code[1] + '\" },\n' + code_map
                  
    c_str = '#define NUM_CODES ' + str(len(codes)) + '\n' \
            '#define MAX_KEY_LENGTH ' + str(max_key) + '\n' + \
            '#define MAX_CODE_LENGTH ' + str(max_code) + '\n' + \
            'char *codes[NUM_CODES][2] = {\n' + code_map + '\n    };\n'
    
    
    return c_str
    


    
    
    
