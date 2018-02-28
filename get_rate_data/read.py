import pymongo
import time

client = pymongo.MongoClient('fx-mongo', 27017)
db = client.fx
co = db.rate

for data in co.find(limit = 10).sort('time', pymongo.DESCENDING):
    print(data)

    
