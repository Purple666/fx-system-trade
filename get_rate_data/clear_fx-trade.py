import pymongo
import time

client = pymongo.MongoClient('fx.flg.jp', 27017)
db = client.fx
co = db.fsd
db.drop_collection(co)

co = db.trade_practice
db.drop_collection(co)


    
