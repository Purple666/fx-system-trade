import oandapy
import pymongo
import time
from datetime import datetime, timezone, timedelta

if __name__ == "__main__":

    environment = 'practice'
    access_token = 'b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117'
    account = '6716490'
    headers = {}
    headers['X-Accept-Datetime-Format'] = 'UNIX'

    JST = timezone(timedelta(hours=+9), 'JST')

    client = pymongo.MongoClient('mongo', 27017)
    db = client.fx
    co = db.rate
    
    oanda = oandapy.API(environment=environment, access_token=access_token, headers=headers)
    db_price = {}
    
    while True:
        response = oanda.get_history(instrument="USD_JPY", count=1, granularity="M1")
        price = response.get("candles")[0]
        loc = datetime.fromtimestamp(int(price['time']) / 1000000, JST)            
        db_price['time'] = int(int(price['time']) / (1000000 * 60))
        db_price['open'] = price['openBid']
        db_price['high'] = price['highBid']
        db_price['low'] = price['lowBid']
        db_price['close'] = price['closeBid']

        document = co.find_one(sort=[("no", -1)])

        if document['time'] == db_price['time']:
            db_price['no'] = document['no']
            co.update_one({"no": db_price['no']}, {"$set": db_price}, upsert = True)
        else:
            db_price['no'] = document['no'] + 1
            co.update_one({"no": db_price['no']}, {"$set": db_price}, upsert = True)
            print("rate : %s %d %d %6.2f %6.2f %6.2f %6.2f" % (loc, db_price['no'], db_price['time'], db_price['open'], db_price['high'], db_price['low'], db_price['close']))

        time.sleep(15)

            
