import requests
import pymongo
import time
from datetime import datetime, timedelta, timezone
import dateutil.parser

if __name__ == "__main__":

    JST = timezone(timedelta(hours=+9), 'JST')
    
    #client = pymongo.MongoClient('mongo', 27017)
    client_mongo = pymongo.MongoClient('openshift.flg.jp', 30017)
    db = client_mongo.fx
    co = db.rate
    
    db_price = {}
    same = 0
    
    while True:
        try:
            response = requests.get('https://api-fxpractice.oanda.com/v3/accounts/101-009-11751301-001/pricing',
                                    params={'instruments': 'USD_JPY'},
                                    headers={'content-type': 'application/json',
                                             'Authorization': 'Bearer 041fff2f1e9950579315d9a8d629ef9f-5b7c44123e8fc34c65951f4d3332b96b'})
            json = response.json()

            price = json['prices'][0]
            loc = dateutil.parser.parse(price['time'])
        
            db_price['time'] = int(loc.replace(tzinfo=JST).timestamp() / 60)
            db_price['close'] = float(price['bids'][0]['price'])
                                             

            document = co.find_one(sort=[("no", -1)])

            if document['time'] == db_price['time'] and same < 4 * 60:
                db_price['no'] = document['no']
                co.update_one({"no": db_price['no']}, {"$set": db_price}, upsert = True)
                same += 1
            elif document['time'] != db_price['time']:
                db_price['no'] = document['no'] + 1
                co.update_one({"no": db_price['no']}, {"$set": db_price}, upsert = True)
                print("rate : %s %d %d %6.3f" % (loc.astimezone(tzinfo=JST), db_price['no'], db_price['time'], db_price['close']))
                same = 0
        except Exception as e:
            print(e)
                
        time.sleep(15)

            
