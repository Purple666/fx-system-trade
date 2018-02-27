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
    
    client = pymongo.MongoClient('fx-mongo', 27017)
    db = client.fx
    co = db.rate
    
    oanda = oandapy.API(environment=environment, access_token=access_token, headers=headers)

    while True:
        response = oanda.get_prices(instruments="USD_JPY")
        price = response.get("prices")[0]
        loc = datetime.fromtimestamp(int(price['time']) / 1000000, JST)            
        price['time'] = int(int(price['time']) / (1000000 * 60))
        del price['instrument']
        del price['ask']
        print("rate : %s %d %6.2f" % (loc, price['time'], price['bid']))
        co.update_one({"time": price['time']}, {"$set": price}, upsert = True)

        time.sleep(60)

            
