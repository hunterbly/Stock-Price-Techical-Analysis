import sys
from datetime import datetime, timedelta
from bs4 import BeautifulSoup
import urllib2
import requests
import os
import re
import psycopg2
import pandas as pd

# Get Max date for every stock

try:
    conn=psycopg2.connect("dbname='stock' user='postgres' host='127.0.0.1'")
except:
    print "I am unable to connect to the database."
	
cur = conn.cursor()
#cur.execute("""SELECT max(date), code from test group by code""")
cur.execute("""SELECT max(date), code from stock_price group by code""")


max_date_array = cur.fetchall()
conn.close()

# Convert array to indexed dataframe, indexed by code
df_max_date = pd.DataFrame(max_date_array, columns = ['date', 'code'])
df_max_date = df_max_date.set_index('code')



# stockList = list(range(1,11))
stockList = list(range(1,4000)) + list(range(4601,4609)) + list(range(6030,6031)) + list(range(6099,6900))


insertList = []


for num in stockList:
    try:
        

        ticker = str(num).zfill(4)
        
        try:
            
            max_date = df_max_date['date'].loc[ticker]
        except:
            max_date = datetime.now().date() - timedelta(days=14)
            print("No such records for Code {0}".format(ticker))
        
        url = 'http://eoddata.com/stockquote/HKEX/' + ticker + '.htm'

        print('url:' + url)
        print(' ')

        c = requests.get(url).content
        soup = BeautifulSoup(c, 'html.parser')
        quote_table = soup.find_all("table", class_="quotes")

        rows = quote_table[0].find_all('tr')

        for row in rows[1:]:
            cols = row.find_all('td')
            
            a = datetime.strptime(cols[0].text.strip(),'%m/%d/%y').date()
            b = max_date
            
            if (a > b):
                print("Code: {0}, Recode Date: {1}, DB Date: {2}".format(ticker, a, b))
                cols = [ele.text.strip() for ele in cols]
                cols.append(ticker)
                insertList.append([ele.replace(",","") for ele in cols if ele])
            


        print(" ")
        print("======================================")
		
    except Exception as e:
        print ' Exception: ', e

# Insert into db        
# print(insertList)

try:
    conn=psycopg2.connect("dbname='stock' user='postgres' host='127.0.0.1'")
except:
    print "I am unable to connect to the database."
	

cur = conn.cursor()

for record in insertList:
    #cur.execute("INSERT into test(id, date, open, high, low, close, volumn, adj, code) VALUES (DEFAULT, %s, %s, %s, %s, %s, %s, %s, %s)", record)
    cur.execute("INSERT into stock_price(id, date, open, high, low, close, volumn, adj, code) VALUES (DEFAULT, %s, %s, %s, %s, %s, %s, %s, %s)", record)

conn.commit()
# conn.close()