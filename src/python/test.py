import projectm36 as db

conn = db.connect('ws', 'localhost', 8000, None, 'spam')
cursor = conn.cursor()
res = cursor.execute(db.RelationVariable('true'))
print res
#for row in res
#    print row

