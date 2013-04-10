from __future__ import division
import numpy as np
import random
import csv
import re
import string
import unicodedata
import operator
import pickle
#import matplotlib#.cbook
import matplotlib#.pyplot as pl
from nltk.corpus import stopwords
from nltk.stem.wordnet import WordNetLemmatizer
from collections import Counter
import sets




#-------------------------
# DATA CLEAN UP
#-------------------------


def clean_up_data( matrix ):
    # clean up the matrix
    clean = [ clean_up( row[:] ) for row in matrix ]
    return clean

def clean_up_data_no_prune( matrix ):
    matrix = matrix[1:]
    # clean up the matrix
    clean = [ clean_up_no_prune( row[:] ) for row in matrix ]
    return clean

# helper
# wrapper for all the preprocessing functions
def clean_up(matrix_row):
    fresh = remove_words(     # Remove words like an, is, the, etc.
            plurals(          # Convert plural words into singular
            remove_dot(       # Remove periods at the end of words
            str.split(        # Split the string into words, space as a separator
            conv_uni(         # Convert from unicode to string
            remove_punc(      # Remove punctuations like ",!?.  
            strip_html(       # Convert html special characters into equivalent form, instead of acii (might be redundant with remove_func on top)
            merge_rows(       # Merge rows just in case they're separated by the csv reader( for now, one row is one feedback of one student)  
              matrix_row )))) # The actual row
            .lower()))))       # Convert everything into lower case  
    return fresh

# helper
# wrapper for all the preprocessing functions
def clean_up_no_prune(matrix_row):
    fresh = remove_dot(
            plurals(          # Convert plural words into singular
            str.split(        # Split the string into words, space as a separator
            conv_uni(         # Convert from unicode to string
            remove_punc(      # Remove punctuations like ",!?.  
            strip_html(       # Convert html special characters into equivalent form, instead of acii (might be redundant with remove_func on top)
            merge_rows(       # Merge rows just in case they're separated by the csv reader( for now, one row is one feedback of one student)  
              matrix_row )))) # The actual row
            .lower())))      # Convert everything into lower case  
    return fresh

# helper
# convert unicode to string
# if string already, return that
def conv_uni(unicode_row):
    if isinstance(unicode_row, str):
        return unicode_row
    clean = unicodedata.normalize('NFKD', unicode_row).encode('ascii','ignore')
    return clean

# helper
# merge rows in the matrix
# e.g. [ 'hello', 'hi' ] will be [ 'hello hi' ]
def merge_rows(matrix_row):
    t = ''
    for i in matrix_row:
        t += i
    return t

def plurals( words ):
    lmtzr = WordNetLemmatizer()
    new_words = [ lmtzr.lemmatize(elem) for elem in words ]
    return new_words

def remove_words( words ):
    extra = ['explain', 'equation']
    dolch220 = ['a', 'all', 'after', 'always', 'about', 'and', 'am', 'again',
                'around', 'better', 'away', 'are', 'an', 'because', 'bring',
                'bat', 'any', 'been', 'carry', 'blue', 'ate', 'as', 'before',
                'can', 'be', 'ask', 'best', 'cut', 'come', 'by', 'both', 'done',
                'down', 'could', 'buy', 'draw', 'find', 'but', 'every', 'call',
                'drink', 'for', 'came', 'funny', 'did', 'from', 'does', 'fall',
                'go', 'do', 'give', "don't", 'far', 'dont', 'help', 'eat', 'going',
                'full', 'here', 'had', 'got', 'I', 'get', 'has', 'grow', 'in',
                'good', 'her', 'found', 'hold', 'is', 'have', 'him', 'gave',
                'it', 'he', 'his', 'goes', 'hurt', 'jump', 'into', 'how', 'if',
                'like', 'just', 'its', 'keep', 'look', 'must', 'know', 'made',
                'kind', 'make', 'new', 'let', 'many', 'laugh', 'me', 'no',
                'live', 'off', 'my', 'now', 'may', 'or', 'long', 'not', 'on',
                'of', 'much', 'our', 'old', 'read', 'myself', 'play', 'out',
                'once', 'right', 'never', 'please', 'open', 'sing', 'only',
                'run', 'pretty', 'over', 'sit', 'own', 'said', 'ran', 'put',
                'sleep', 'pick', 'see', 'ride', 'round', 'tell', 'the', 'saw',
                'some', 'their', 'shall', 'say', 'stop', 'these', 'show',
                'to', 'she', 'take', 'those', 'so', 'thank', 'upon', 'up',
                'soon', 'them', 'us', 'start', 'we', 'that', 'then', 'use',
                'where', 'there', 'think', 'very', 'today', 'they', 'walk',
                'wash', 'together', 'you', 'this', 'were', 'which', 'try',
                'too', 'when', 'why', 'under', 'wish', 'want', 'work', 'was',
                'would', 'well', 'write', 'went', 'your', 'what', 'white',
                'hill', 'who', 'will', 'with', 'yes']    
    words_to_remove = stopwords.words('english') + dolch220 + extra
    return filter(lambda x: x not in words_to_remove, words)

def remove_dot( words ):
    new_words=[]
    for word in words:
        if word.endswith("."):
            new_words += [word[:-1]]
        else:
            new_words += [word]
    return new_words

# helper
# remove punctuations for easy analysis
def remove_punc(sentence):
    # pad punctuations with space
    # just in case so 2 words wont stick together
    # e.g. avoid this: "by this!Also" --> "by thisAlso"
    
    sentence = re.sub('([,!?()])', r' \1 ', sentence)
    # get rid of punctuations
    exclude = set(string.punctuation)
    # don't remove these two symbols
    exclude.remove('-')
    exclude.remove('_')
    exclude.remove('.')
    #print sentence
    # remove the punctuations
    sentence = ''.join(ch for ch in sentence if ch not in exclude)
    return sentence

    #return filter(lambda x: dictionary[x] > minimum, words)

# helper
# This function is by Fredrik Lundh
# convert html special characters
def strip_html(text):
    def fixup(m):
        text = m.group(0)
        if text[:1] == "<":
            return "" # ignore tags
        if text[:2] == "&#":
            try:
                if text[:3] == "&#x":
                    return unichr(int(text[3:-1], 16))
                else:
                    return unichr(int(text[2:-1]))
            except ValueError:
                pass
        elif text[:1] == "&":
            import htmlentitydefs
            entity = htmlentitydefs.entitydefs.get(text[1:-1])
            if entity:
                if entity[:2] == "&#":
                    try:
                        return unichr(int(entity[2:-1]))
                    except ValueError:
                        pass
                else:
                    return unicode(entity, "iso-8859-1")
        return text # leave as is
    return re.sub("(?s)<[^>]*>|&#?\w+;", fixup, text)



#-------------------------
# COUNTING
#-------------------------#

def count_all_words(matrix):
    c1 = Counter()
    for row in matrix:
        c1 += Counter(row)
    return c1

def count_student_words( words, blankdictionary ):
    x = dict(blankdictionary)
    for word in words:
        if word in x:
            x[word] = x[word] + 1
    return x

def dict_to_array( dictionary ):
    x = sort_by_key( dictionary )
    x = np.array( x )#, dtype = [('y', '|S11'), ('Value', float)] )
    return x
    
def matrix_of_Counts(matrix, blank):
    c1 = Counter()
    for row in matrix:  
        c1 += Counter(row)
    return c1

def recreate_wordle_matrix( dictionary ):
    new_list = []
    for word in dictionary.keys():
        for i in range(int(round(dictionary.get(word)))):
            new_list += [word]
    return new_list

def recreate_wordle_matrix_from_array( array ):
    new_list = []
    for word in c3.keys():
        for i in range(c3.get(word)):
            new_list += [word]
    return new_list

def set_dict_values_to_zero( dictionary ):
    x = dict(dictionary)
    for key in x.keys():
        x[key] = 0
    return x

def set_minimum(dictionary, minimum):
    x = dict(dictionary)
    for key in x.keys():
        if x[key] <= minimum:
            del x[key]
    return x

def set_threshold( matrix, dictionary, minimum ):
    return [ set_minimum(row, dictionary, minimum) for row in matrix ]

def sort_by_key(unsorted):
    sorted_tuple = sorted(unsorted.items(), key=operator.itemgetter(0), reverse=False)
    return sorted_tuple

def sort_by_value(unsorted):
    sorted_tuple = sorted(unsorted.items(), key=operator.itemgetter(1), reverse=True)
    return sorted_tuple

# constuct the word matrix
# only accepts a 2D student comment matrix
def word_matrix( student_comment_matrix, dictionary ):
    z = []
    for student_comment in student_comment_matrix:
        a = count_student_words( student_comment, dictionary )
        a = dict_to_array(a)
        a = map(int, a[:,1])
        z = z+[a]
    x = np.array(z)
    return x


#-------------------------
# FILE IO
#-------------------------

def print_to_file( name, matrix ):    
    f1=open(name, 'w+')
    for row in matrix:
        print >>f1, row
    f1.close()
    
def save_matrix_to_file( name, matrix ):
    save_file( name, matrix )

def load_matrix_from_file( name ):
    return load_file( name )

def save_dict_file( filename, dictionary ):
    save_file( filename, dictionary )

def open_dict_file( filename ):
    return load_file( filename )

def save_words_file( filename, words ):
    save_file( filename, words )

def open_words_file( filename ):
    return load_file( filename )

def save_file( filename, data ):
    f = open( filename, 'wb' ) #b means binary
    pickle.dump( data, f)
    f.close()

def load_file( filename ):
    f = open( filename, 'rb' )
    data = pickle.load( f )
    f.close()
    return data

def read_answer_log( filename, quizname, numbers ):

    answer_log = csv.reader(open(filename, 'rb'), delimiter='\t', quoting=csv.QUOTE_NONE)
    # convert to a list
    y = list(answer_log)
    # then convert to a numpy matrix
    matrix2 = np.array(y)

    # only retrieve the test we want
    harvest = [ row for row in matrix2 if get_test_name_from_line( row ) == quizname]
    # only get the numbers we specified 
    harvest = [ row for row in harvest if get_number_from_line( row ) in numbers ]
    harvest = np.array(harvest)
    # only latest submit of a student is considered
    harvest = remove_duplicate_submits( harvest, quizname, numbers)
    harvest = [ get_essay_from_line(row) for row in harvest ]
    harvest = np.array(harvest)
    return harvest

def remove_duplicate_submits( matrix, quizname, numbers ):
    names = set([])

    # add names to the set
    # the set doesn't add if the name
    # is in it already
    for row in matrix:
        name = get_name_from_line(row)
        names.add(name)
    latest_stamp = 0

    # for each name in the set
    # get the latest time stamp
    # then filter out the earlier ones
    while True:
        curname = names.pop()
        submatrix = np.array([ row for row in matrix if get_name_from_line(row) == curname])
        for row in submatrix:
            stamp = get_time_stamp_from_line(row)
            if latest_stamp < stamp:
                latest_stamp = stamp
        
        matrix = [ row for row in matrix if (curname != get_name_from_line(row)) or (get_time_stamp_from_line(row) == latest_stamp) ]       
        latest_stamp = 0
        if len(names) == 0:
            break
    
    matrix = np.array(matrix)
    return matrix

def get_essay_from_line( row ):
    return str.split(row[2], ']')

def get_name_from_line( row ):
    return str.split(str.split(row[0], ']')[1], '|' )[1]

def get_test_name_from_line( row ):
    return str.split(str.split(row[0], ']')[1], '|' )[2]

def get_time_stamp_from_line( row ):
    return str.split(row[1], ']')

def get_number_from_line( row ):
    return int(str.split(str.split(row[0], ']')[1], '|' )[3])

#-------------------------
# WEIGHTS
#-------------------------

# matrix must be a numpy 2D array
def local_log_weighting( matrix ):
    A = matrix
    (rows,_) = A.shape
    for i in range(rows):
        A[i] = np.log2(A[i]+1)
    return A

# matrix must be a numpy 2D array
def local_aug_weighting( matrix ):
    A = matrix
    (rows,_) = A.shape
    y = 0
    # this is array wise math
    for i in range(rows):
        m = A[i].max(axis=0)
        A[i] = ((A[i]/m)+1) / 2
    return A

# matrix must be a numpy 2D array
def local_binary_weighting( matrix ):
    A = matrix
    (rows,columns) = A.shape
    y = 0
    for i in range(rows):
        for j in range(columns):
            if A[i][j] != 0:
                A[i][j] = 1                
    return A

def global_normal_weighting( matrix ):
    # for each word, square the values then sum them
    # return the inverse of that 
    (x,_) = matrix.shape
    print matrix.shape
    g = np.zeros(x)

    for i in range(x):
        g[i] = 1/np.sqrt(np.sum((matrix[i])**2))

    return g

def global_gfldf_weighting( matrix ):
    # for each word, square the values then sum them
    # return the inverse of that 
    (x,_) = matrix.shape
    g = np.zeros(x)

    for i in range(x):
        gf = np.sum(matrix[i])
        df = np.count_nonzero(matrix[i])
        g[i] = gf/df
    return g

def global_ldf_weighting( matrix ):
    # for each word, square the values then sum them
    # return the inverse of that 
    (x,_) = matrix.shape
    g = np.zeros(x)

    for i in range(x):
        df = np.count_nonzero(matrix[i])
        g[i] = np.log2(( x /( 1 + df )))                       
    return g

def global_entropy_weighting( matrix ):
    # for each word, square the values then sum themi]
    # return the inverse of that 
    (x,_) = matrix.shape
    g = np.zeros(x)
    
    for i in range(x):
        temp = matrix[i]               
        gf = np.sum(matrix[i])
        temp = temp/gf
        a = temp
        for j in range(len(temp)):
            if temp[j] == 0:
                a[j] = 0
            else:
                a[j] = np.log2( temp[j] )
        temp = temp * a        
        temp = temp / np.log2( x )
        g[i] = 1 + np.sum( temp )    
    return g

def best_weighting( matrix ):
    g = global_entropy_weighting( matrix )
    A = matrix
    (rows,_) = A.shape
    for i in range(rows):
        A[i] = g[i] * np.log2(A[i]+1)
    return A

def weight_matrix(matrix, local_weight, global_weight):
    A = matrix
    B = matrix
    
    if local_weight == 'log' or local_weight == 1:
        A = local_log_weighting( A )
    elif local_weight =='aug' or local_weight == 2:
        A = local_aug_weighting( A )
    elif local_weight =='binary' or local_weight == 3:
        A = local_binary_weighting( A ) 
    else:
        A = A

    if global_weight == 'norm' or global_weight == 1:
        g = global_normal_weighting( B ) 
    elif global_weight == 'gfldf' or global_weight == 2:
        g = global_gfldf_weighting( B ) 
    elif global_weight == 'ldf' or global_weight == 3:
        g = global_ldf_weighting( B )
    elif global_weight == 'entropy' or global_weight == 4:
        g = global_entropy_weighting( B )
    else:
        g = np.array(1)
  
    return np.multiply(A.T, g).T

#-------------------------
# START HERE
#-------------------------

def JITT( freq, g, rank, filename, quizname ):
    # read the answer log given by the file name
    # choose the proper quiz name specified 
    # essays are always question 1, so manually specified
    answermatrix = read_answer_log( filename, quizname, [1])
    #print_to_file('rawfile.txt', answermatrix)

    #---------------------------------------------------
    # BIG DOC block
    #---------------------------------------------------

    # handle the pre-processing
    fixed_matrix = clean_up_data(answermatrix)
    # print to file
    print_to_file('BigDocWordle.txt', fixed_matrix)
    # construct the initial dictionary
    dictionary = count_all_words(fixed_matrix)    

    #---------------------------------------------------
    # DOC Matrix block
    #---------------------------------------------------

    # get the columns needed for the doc matrix    
    columns = set_dict_values_to_zero(dictionary)
    # construct doc matrix
    A_doc_matrix = word_matrix(fixed_matrix, columns)

    #---------------------------------------------------
    # Matrix Preprocessing block
    #---------------------------------------------------

    # preprocess the doc matrix
    # A_prime has words as columns, students as rows
    A_prime = weight_matrix( A_doc_matrix, freq, g )
    words = dict_to_array( dictionary )
    # recreate the original matrix from a frequency matrix
    word_array = words[0:,0]
    word_freq_A_prime = [ round(np.sum(row)) for row in A_prime.T ]
    zip_freq_to_words = dict(zip(word_array, word_freq_A_prime))
    # create something that wordle understands
    A_prime_wordle = recreate_wordle_matrix(zip_freq_to_words)
    print_to_file('ProcessedMatrix.txt', A_prime_wordle)
    
    #---------------------------------------------------
    # Matrix SVD block
    #---------------------------------------------------

    # get SVD of the A_prime transpose matrix
    # transpose because, we want the words to be the
    # rows and students as columns
    # it's also weighted
    U,S,VT = np.linalg.svd(A_prime.T, full_matrices=False)

    # only need rank k
    U_k = np.array(U[:,:rank])
    
    S_k = np.array(S[0:rank])
    V_k = np.array(VT[0:rank])
    
    left = U_k * S_k
    
    # rank_k matrix is representation of the answer log
    # words as rows columns as students
    Rank_k = np.outer( left, V_k.T )

    # recreate rank_k wordle
    # print out rank_k matrix 
    word_freq_Rank_k = [ round(np.sum(row)) for row in Rank_k ]
    Rank_k_dict = dict(zip(word_array, word_freq_Rank_k))
    Rank_k_wordle = recreate_wordle_matrix(Rank_k_dict)
    
    print_to_file('Rank_k_wordle.txt', Rank_k_wordle)

    
if __name__=="__main__":

    JITT( 0, 0, 1, 'answer_log', 'essayType')
   