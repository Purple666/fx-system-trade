import pymongo
import time

client = pymongo.MongoClient('openshift.flg.jp', 30017)
db = client.fx

db.fxsetting_log.drop()
db.result_backtest.drop()
db.result_trade_sim.drop()
db.trade_practice.drop()
db.trade_practice_weekly.drop()

    
