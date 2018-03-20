import sys
import pandas as pd
import json
import pymongo
import argparse
from datetime import datetime

client = pymongo.MongoClient('fx.flg.jp', 27017)
db = client.fx
co = db.rate

df = pd.read_csv(sys.stdin, dtype={'date': str, 'time': str})

chart = []
for r in df.itertuples():
    t = int(datetime.strptime(r[2] + r[3], "%Y%m%d%H%M%S").timestamp() / 60)
    chart.append({'time': t, 'open': r[4], 'high': r[5], 'low': r[6], 'close': r[7] })

db.drop_collection(co)
co.create_index([('time', pymongo.ASCENDING)], unique=True)
co.insert_many(chart)

