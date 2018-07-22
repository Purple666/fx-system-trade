import sys
import pandas as pd
import json
import pymongo
import argparse
from datetime import datetime

client = pymongo.MongoClient('openshift.flg.jp', 30017)
db = client.fx
db.rate.drop()
co = db.rate

df = pd.read_csv(sys.stdin, dtype={'date': str, 'time': str})

chart = []
no = 0
for r in df.itertuples():
    t = int(datetime.strptime(r[2] + r[3], "%Y%m%d%H%M%S").timestamp() / 60)
    chart.append({'no': no, 'time': t, 'open': r[4], 'high': r[5], 'low': r[6], 'close': r[7] })
    no = no + 1

co.create_index([('no', pymongo.ASCENDING)], unique=True)
co.insert_many(chart)

