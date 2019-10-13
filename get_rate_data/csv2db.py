import sys
import pandas as pd
import json
import redis
import argparse
from datetime import datetime

redis = redis.Redis(host='openshift.flg.jp', port=30379, db=0)
redis.delete('fx')
pipe = redis.pipeline()
df = pd.read_csv(sys.stdin, dtype={'date': str, 'time': str})

chart = []
no = 0
for r in df.itertuples():
    t = int(datetime.strptime(r[2] + r[3], "%Y%m%d%H%M%S").timestamp() / 60)
    pipe.rpush('fx', json.dumps({'no': no, 'time': t, 'close': r[7] }))
    no = no + 1

pipe.execute()

