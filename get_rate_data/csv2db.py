import sys
import pandas as pd
import json
import pymongo
import argparse
from datetime import datetime

client = pymongo.MongoClient('fx.flg.jp', 27017)
db = client.fx
co = db.rate

df = pd.read_csv(sys.stdin)

chart = []
for r in df.itertuples():
    chart.append({'time': int(r[1]), 'bid': r[2] })

db.drop_collection(co)
co.create_index([('time', pymongo.ASCENDING)], unique=True)
co.insert_many(chart)

