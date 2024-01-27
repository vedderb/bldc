from glob import glob
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

bench_files = glob('stored_results/*')
headers = ('File','Eval time (s)')

benches = ['q2.lisp', 'fibonacci_tail.lisp', 'dec_cnt3.lisp',
           'dec_cnt1.lisp', 'fibonacci.lisp', 'tak.lisp',
           'dec_cnt2.lisp', 'insertionsort.lisp', 'tail_call_200k.lisp',
           'loop_200k.lisp', 'sort500.lisp', 'env_lookup.lisp' ]

data = []

plt.figure(figsize=(10.0, 5.0)) # in inches!
cmap = plt.get_cmap('jet')
colors = cmap(np.linspace(0, 1.0, len(benches)))
for bench, color in zip(benches,colors):
    dict = {}
    for file in bench_files:
        file_info = file.split('benchresult')[1]
        file_details = file_info.split('_')
        df = pd.read_csv(file,index_col='File')
        
        date = file_details[0] + '-' + file_details[1] + '-' + file_details[2] + '-' + file_details[3] + '-' + file_details[4]

        if (bench in df.index):
            row = df.loc[bench]
            dict.update({date : row[1]});
        # else:
        #     print("missing data point ", bench, file )
                        
    lists = sorted(dict.items()) # sorted by key, return a list of tuples
    x, y = zip(*lists) # unpack a list of pairs into two tuples
    plt.plot(x, y, label=bench, color=color)


lgd = plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
ax = plt.gca()
for tick in ax.get_xticklabels():
    tick.set_rotation(90)
ax.tick_params(axis='both', which='major', labelsize=6)
ax.tick_params(axis='both', which='minor', labelsize=4)
ax.set_facecolor("lightgray");
plt.ylabel("Sec")
plt.grid()
plt.savefig('benchresults.png', dpi=600, bbox_extra_artists=(lgd,), bbox_inches='tight')     
plt.yscale('log')
plt.savefig('benchresults_log.png', dpi=600, bbox_extra_artists=(lgd,), bbox_inches='tight')     
