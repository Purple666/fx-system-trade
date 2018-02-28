import oandapy
import pymongo
import time

class Stream(oandapy.Streamer):
    def __init__(self, *args, **kwargs):
        super(Stream, self).__init__(*args, **kwargs)
        client = pymongo.MongoClient('fx-mongo', 27017)
        db = client.fx
        self.co = db.rate
        self.ptime = 0
        self.pdata = None
        
    def on_success(self, data):
        #co.insert_one({"test": 3})
        print(data)
        data = data['tick']
        data['time'] = int(int(data['time']) / (1000000 * 60))
        del data['instrument']
        del data['ask']
        if self.ptime != data['time']:
            print("rate : %d %6.2f" % (data['time'], data['bid']))
            if self.pdata != None:
                self.co.update_one({"time": self.pdata['time']}, {"$set": self.pdata}, upsert = True)
            self.co.update_one({"time": data['time']}, {"$set": data}, upsert = True)
            self.ptime = data['time']
        else:
            self.pdata = data

    def on_error(self, data):
        self.disconnect()

if __name__ == "__main__":

    environment = "practice"
    access_token = "b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
    account = "6716490"

    while True:
        r = Stream(access_token=access_token, environment=environment)
        try:
            r.rates(account, ignore_heartbeat=True, instruments="USD_JPY")
        except:
            print("r.rates error")
            time.sleep(60)
        time.sleep(1)

            
