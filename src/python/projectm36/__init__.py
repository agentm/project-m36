import websocket as ws
#this websocket-client module supports both python 2.7 and 3.x- the "websockets" module supports python 3.4+ but relies on asyncio. It might be possible to mix the modules in this driver or it make more sense to create a separate python 3.4+ database driver.
import exceptions
import json

apilevel = '2.0'
threadsafety = 2
paramstyle = None #not applicable

class Error(exceptions.StandardError):
    pass

class Warning(exceptions.StandardError):
    pass

class InterfaceError(Error):
    pass

class DatabaseError(Error):
    pass

class InternalError(DatabaseError):
    pass

class OperationalError(DatabaseError):
    pass

class ProgrammingError(DatabaseError):
    pass

class IntegrityError(DatabaseError):
    pass

class DataError(DatabaseError):
    pass

class NotSupportedError(DatabaseError):
    pass

class ProjectM36Connection(object):
    def __init__(self, proto, host, port, path, dbname):
        assert proto in ('ws', 'wss'), "proto must be one of 'ws' or 'wss'"
        self.proto = proto
        self.host = host
        self.port = port
        self.path = path
        self.dbname = dbname
        self.conn = None
        
    def connect(self):
        port = ''
        if self.port:
            port = ':{}'.format(self.port)

        path = ''
        if self.path:
            path = '/{}'.format(self.path)

        
        self.conn = ws.create_connection('{}://{}{}{}'.format(self.proto,
                                                              self.host,
                                                              port,
                                                              path))
        self.conn.send('connectdb:{}'.format(self.dbname))
        result = self.conn.recv()
        print result

        result = self.conn.recv()
        print result

        return True

    def close(self):
        if self.conn is None: #already closed
            raise ProgrammingError('attempt to close connection which was not open')
        self.conn.close()
        self.conn = None

    def commit(self):
        self.executeTutorialD(':commit')

    def rollback(self):
        self.executeTutorialD(':rollback')

    def cursor(self):
        return Cursor(self)

    def _send(self, t):
        print 'SEND:', t
        return self.conn.send(t)

    def _recv(self):
        return self.conn.recv()

class ProjectM36JSONEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, RelationalExpr):
            return obj.as_json()
        else:
            super(ProjectM36JSONEncoder, self).default(obj)

class ProjectM36JSONDecoder(json.JSONDecoder):
    def decode(self, s):
        if 
        super(ProjectM36JSONDecoder, self).decode(s)

class Cursor(object):
    def __init__(self, conn):
        self._arraysize = 1
        self._conn = conn

    @property
    def description(self):
        return []

    @property
    def rowcount(self):
        return 0

    #def callproc(self): #not applicable

    def close(self):
        return

    def executeTutorialD(self, tutoriald):
        self.conn.send()

    def execute(self, relationalExpression):
        relExprJson = json.dumps(relationalExpression, cls=ProjectM36JSONEncoder)
        self._conn._send('evalrelexpr:{}'.format(relExprJson))
        result = self._conn._recv()
        rel = json.loads(result, cls=ProjectM36JSONDecoder)
        return rel

    def executemany(self, op, params):
        raise NotImplementedError()

    def fetchone(self):
        return 0

    def fetchmany(self):
        return 0

    def fetchall(self):
        return 0

    #def nextset(self): #not applicable

    @property
    def arraysize(self):
        return self._arraysize

    @arraysize.setter
    def set_arraysize(self, size):
        self._arraysize = size

    def setinputsizes(self, sizes):
        return

    def setoutputsize(self, size, column):
        return
    
def connect(proto, host, port, path, dbname):
    conn = ProjectM36Connection(proto, host, port, path, dbname)
    conn.connect()
    return conn

class RelationalExpr(object):
    def as_json(self):
        return {'tag':self.__class__.__name__,
                'contents':self.json_contents()}

class RelationVariable(RelationalExpr):
    def __init__(self, rvname):
        self.name = rvname

    def json_contents(self):
        return [self.name, []]

class Equals(RelationalExpr):
    def __init__(self, exprA, exprB):
        self.a = exprA
        self.b = exprB
'''
(Maybe [AttributeExprBase a]) [TupleExprBase a] |
  --- | Create and reference a relation from attributes and a tuple set.
  MakeStaticRelation Attributes RelationTupleSet |
  --- | Reference an existing relation in Haskell-space.
  ExistingRelation Relation |
  --MakeFunctionalRelation (creates a relation from a tuple-generating function, potentially infinite)
  --in Tutorial D, relational variables pick up the type of the first relation assigned to them
  --relational variables should also be able to be explicitly-typed like in Haskell
  --- | Reference a relation variable by its name.
  RelationVariable RelVarName a |
  --- | Create a projection over attribute names. (Note that the 'AttributeNames' structure allows for the names to be inverted.)
  Project AttributeNames (RelationalExprBase a) |
  --- | Create a union of two relational expressions. The expressions should have identical attributes.
  Union (RelationalExprBase a) (RelationalExprBase a) |
  --- | Create a join of two relational expressions. The join occurs on attributes which are identical. If the expressions have no overlapping attributes, the join becomes a cross-product of both tuple sets.
  Join (RelationalExprBase a) (RelationalExprBase a)  |
  --- | Rename an attribute (first argument) to another (second argument).
  Rename AttributeName AttributeName (RelationalExprBase a) |
  --- | Return a relation containing all tuples of the first argument which do not appear in the second argument (minus).
  Difference (RelationalExprBase a) (RelationalExprBase a) |
  --- | Create a sub-relation composed of the first argument's attributes which will become an attribute of the result expression. The unreferenced attributes are not altered in the result but duplicate tuples in the projection of the expression minus the attribute names are compressed into one. For more information, <https://github.com/agentm/project-m36/blob/master/docs/introduction_to_the_relational_algebra.markdown#group read the relational algebra tutorial.>
  Group AttributeNames AttributeName (RelationalExprBase a) |
  --- | Create an expression to unwrap a sub-relation contained within at an attribute's name. Note that this is not always an inverse of a group operation.
  Ungroup AttributeName (RelationalExprBase a) |
  --- | Filter the tuples of the relational expression to only retain the tuples which evaluate against the restriction predicate to true.
  Restrict (RestrictionPredicateExprBase a) (RelationalExprBase a) |
  --- | Returns the true relation iff 
  Equals (RelationalExprBase a) (RelationalExprBase a) |
  NotEquals (RelationalExprBase a) (RelationalExprBase a) |
  Extend (ExtendTupleExprBase a) (RelationalExprBase a)
'''
