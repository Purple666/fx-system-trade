import requests
import redis
import json
import time
from datetime import datetime, timedelta, timezone
import dateutil.parser

if __name__ == "__main__":

    JST = timezone(timedelta(hours=+9), 'JST')
    
    redis = redis.Redis(host='openshift.flg.jp', port=30379, db=0)

    now_price = {}
    same = 0
    
    while True:
        try:
            response = requests.get('https://api-fxpractice.oanda.com/v3/accounts/101-009-11751301-001/pricing',
                                    params={'instruments': 'USD_JPY'},
                                    headers={'content-type': 'application/json',
                                             'Authorization': 'Bearer 041fff2f1e9950579315d9a8d629ef9f-5b7c44123e8fc34c65951f4d3332b96b'})

            responce_json = response.json()
            price = responce_json['prices'][0]
            loc = dateutil.parser.parse(price['time'])
        
            now_price['time'] = int(loc.replace(tzinfo=JST).timestamp() / 60)
            now_price['close'] = float(price['bids'][0]['price'])

            last_no = redis.llen("fx") - 1
            db_price = json.loads(redis.lindex("fx", last_no))

            if now_price['time'] == db_price['time'] and same < 4 * 60:
                now_price['no'] = db_price['no']
                redis.lset("fx", last_no, json.dumps(now_price))
                same += 1
            elif now_price['time'] != db_price['time']:
                now_price['no'] = db_price['no'] + 1
                redis.rpush("fx", json.dumps(now_price))
                print("rate : %s %d %d %6.3f" % (loc.astimezone(), now_price['no'], now_price['time'], now_price['close']))
                same = 0
        except Exception as e:
            print(e)
                
        time.sleep(15)

            
