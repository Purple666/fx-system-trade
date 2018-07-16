import pymongo
import time

client = pymongo.MongoClient('openshift.flg.jp', 30017)
db = client.fx

co = db.fsd
db.drop_collection(co)

co = db.trade_practice
db.drop_collection(co)

co = db.trade_practice_weekly
db.drop_collection(co)


    
