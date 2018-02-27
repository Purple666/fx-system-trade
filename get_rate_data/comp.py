import sys
import pandas as pd
from datetime import datetime

df = pd.read_csv(sys.stdin, dtype={'date': str, 'time': str})

pt = 0
pbid = 0
for r in df.itertuples():
    t = int(datetime.strptime(r[2] + r[3], "%Y%m%d%H%M%S").timestamp() / 60)
    if t - pt != 1 and pt != 0:
        for ht in range(pt + 1, t):
            print(ht, ",", pbid)
    print(t, ",", r[7])
    pt = t
    pbid = r[7]


