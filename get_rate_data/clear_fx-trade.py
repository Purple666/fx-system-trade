import pymongo
import time

client = pymongo.MongoClient('openshift.flg.jp', 30017)
db = client.fx

db.fsd_trade.drop()
db.fsd_backtest.drop()
db.trade_practice.drop()
db.trade_practice_weekly.drop()


    
