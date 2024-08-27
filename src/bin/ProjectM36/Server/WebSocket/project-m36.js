/**
* Returns a connection to a Project:M36 database. The connection created relies on websockets and should therefore function from any modern web browser or node server. This API is asynchronous and callback-based.
* @param {string} protocol - Choose from "ws://" and "wss://". "wss" is the SSL-wrapped websockets protocol and should therefore be preferred.
* @param {string} host - The IP address or name of the host of the database.
* @param {string} path - The websocket path where the websocket server is bound. Unless the project-m36-websocket-server is proxied, this is ignored by the server.
* @param {string} dbname - The name of the remote database.
* @param {openCallback} openCallback - A function to call when the connection is successfully open and ready for queries.
* @param {errorCallback} errorCallback - A function called if the connection process encounters an error. This is not called for errors in the queries.
* @param {statusCallback} statusCallback - A function called with synchronous and asynchronous status updates such as query errors and query results.
* @param {promptCallback} promptCallback - A function called whenever prompt information such as the current schema name and current branch name (if any).
* @param {closeCallback} closeCallback - A function called once the connection is closed.
*/
var ProjectM36Connection = function (protocol, host, port, path, dbname, readyCallback, errorCallback, statusCallback, promptCallback, notificationCallback, closeCallback) {
    this.protocol = protocol;
    this.host = host;
    this.port = port;
    this.path = path;
    this.dbname = dbname;
    var connectURL = protocol + '://' + host;
    if(port)
    {
	connectURL = connectURL + ":" + port;
    }
    if(path)
    {
	connectURL = connectURL + "/" + path;
    }
    var socket = new WebSocket(connectURL);
    var self = this;
    socket.onopen = function(event) {
	const req = {"tag":"ConnectionSetupRequest",
		     "databaseName":self.dbname}
	self.socket.send(JSON.stringify(req));
    };
    socket.onerror = function(event) { 
	errorCallback(event); 
    };
    var self = this;
    socket.onmessage = function(event) {
	var msg = JSON.parse(event.data);
	self.handleResponse(msg);
    };
    socket.onclose = function(event) {
	closeCallback(event);
    };
    this.readycallback = readyCallback;
    this.statuscallback = statusCallback;
    this.promptcallback = promptCallback;
    this.notificationcallback = notificationCallback;
    this.socket = socket;
}

ProjectM36Connection.prototype.createSessionAtHead = function(branch)
{
    const req = { "tag" : "CreateSessionAtHeadRequest",
		  "requestId" : this.makeUUID(),
		  "headName" : branch };
    this.socket.send(JSON.stringify(req));
}

/**
* The argument returned as part of the 'statusCallback'. Typically, only one of the three status types is populated. These status updates are groups together so that the callback can feed into a user interface update function.
* @callback statusCallback
* @param {Object} relationResult - The status update containing a relation or null.
* @param {Object} acknowledgementresult - The status update containing an acknowledgement that the query was executed or null.
* @param {Object} errorResult - The status update containing the error information or null.
*/
var ProjectM36Status = function (requestId, relationResult, dataFrameResult, acknowledgementResult, errorResult)
{
    this.requestId = requestId;
    this.relation = relationResult;
    this.dataframe = dataFrameResult;
    this.acknowledgement = acknowledgementResult;
    this.error = errorResult;
}

var ProjectM36SessionCreated = function (requestId, sessionId)
{
    this.requestId = requestId;
    this.sessionId = sessionId;
}

var ProjectM36PromptInfo = function(headName, schemaName, sessionId)
{
    this.headName = headName;
    this.schemaName = schemaName;
    this.sessionId = sessionId;
}

ProjectM36Connection.prototype.close = function()
{
    this.socket.close();
}

ProjectM36Connection.prototype.readyState = function()
{
    return this.socket.readyState;
}

ProjectM36Connection.prototype.handleResponse = function(message)
{
    if(message.tag == "ConnectionSetupResponse")
    {
	this.readycallback();
    }
    else if(message.tag == "RelationResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 message.jsonRelation,
						 null,
						 null,
						 null));
    }
    else if(message.tag == "DataFrameResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 null,
						 message.jsonDataFrame,
						 null,
						 null));
    }
    else if(message.tag == "CreateSessionAtHeadResponse")
    {
	this.statuscallback(new ProjectM36SessionCreated(message.requestId,
							 message.sessionId
							));
    }
    else if(message.tag == "SuccessResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 null,
						 null,
						 true,
						 null));
    }
    else if(message.tag == "TimeoutResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 null,
						 null,
						 'timeout',
						 null));
    }
    else if(message.tag == "PromptInfoResponse")
    {
	this.statuscallback(new ProjectM36PromptInfo(message.headName,
						     message.schemaName,
						     message.sessionId));
    }
    else if(message.tag == "RelationalErrorResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 null,
						 null,
						 null,
						 message.error.tag + ': ' + message.error.contents));
    }
    else if(message.tag == "TextErrorResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 null,
						 null,
						 null,
						 message.error));
    }
    else if(message.tag == "ConnectionClosedResponse")
    {
	//??
    }
    else if(message.tag == "DisplayTextResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 null,
						 null,
						 message.text,
						 null));
    }
    else if(message.tag == "HintWithResponse")
    {
	this.statuscallback(new ProjectM36Status(message.requestId,
						 null,
						 null,
						 message.hintText,
						 null));
	this.handleResponse(message.response);
    }
    else if(message.tag == "NotificationResponse")
    {
	this.notificationcallback(message.notificationName, message.evaluatedNotification);
    }
    else if(message.tag == "MessageNotExpected")
    {
	console.log("message not expected: " + message.expected);
    }
    else
    {
	console.log("received unknown message: " + JSON.stringify(message));
    }
}

// override this is this API is not available in your context
ProjectM36Connection.prototype.makeUUID = function()
{
    return self.crypto.randomUUID()
}

/** 
* Creates a new session which is necessary for executing TutorialD.
* @param {string} branch - The branch of the transaction graph, typically "master".
*/
ProjectM36Connection.prototype.createSessionAtHead = function(branch)
{
    const requestId = this.makeUUID();
    const req = { "tag": "CreateSessionAtHeadRequest",
		  "requestId": requestId,
		  "headName": branch
		};
    this.socket.send(JSON.stringify(req));
    return requestId;
}

/**
* Executes a TutorialD string.
* @param {string} tutd - The TutorialD string.
*/
ProjectM36Connection.prototype.executeTutorialD = function(sessionId, tutd)
{
    const requestId = this.makeUUID();
    const req = { "tag" : "ExecuteTutorialDRequest",
		  "requestId": requestId,
		  "sessionId": sessionId,
		  "presentation": { "jsonPresentation" : true,
				    "textPresentation" : false,
				    "htmlPresentation" : false },
		  "tutoriald" : tutd
		};
		  
    this.socket.send(JSON.stringify(req));
    return requestId;
}

/**
* A utitily function which creates and returns a table element representing the relation.
* @param {object} - The relation returned from executing a relational expression.
*/
ProjectM36Connection.prototype.generateRelation = function(relation)
{
    var table = document.createElement("table");
    
    var headers = this.generateRelationHeader(relation[0]);
    var body = this.generateRelationBody(relation[1]);
    
    table.appendChild(headers);
    table.appendChild(body);
    
    return table;
}

ProjectM36Connection.prototype.generateRelationHeader = function(header)
{
    var thead = document.createElement("thead");
    var headerrow = document.createElement("tr");
    for(var hindex=0; hindex < header.attributes.length; hindex++)
    {
	var th = document.createElement("th");
	var attrtype = this.generateAtomType(header.attributes[hindex]);
	th.appendChild(attrtype);
	headerrow.appendChild(th);
    }
    //special case- if there are no attributes (the "true" and "false" relations, then leave a class marker so that the table can be styled to appear regardless
    if(header.attributes.length == 0)
    {
	headerrow.setAttribute("class", "emptyrow");
    }
    thead.appendChild(headerrow);
    return thead;
}

ProjectM36Connection.prototype.generateAtomType = function(attr)
{
    var atomType = attr.type.tag;
    var attrName = attr.name;
    var accessory = attr[2]; //??
    var element = document.createElement("span");
    element.textContent = attrName + "::";
    if (atomType == "RelationAtomType")
    {
	element.textContent += "relation";
	var table = document.createElement("table");
	element.appendChild(table);			       
	var thead = document.createElement("thead");
	table.appendChild(thead);
	var tr = document.createElement("tr");
	thead.appendChild(tr);
	var relattrs = attr.type.contents;
	for(var attrindex = 0; attrindex < relattrs.attributes.length; attrindex++)
	{
	    var th = document.createElement("th");
	    var relattrNode = this.generateAtomType(relattrs.attributes[attrindex]);
	    th.appendChild(relattrNode);
	    tr.appendChild(th);
	}
    }
    else
    {
	var readableType = atomType.slice(0,-8);
	element.textContent += readableType;
    }
    if(accessory)
    {
	element.textContent += accessory;
    }
    return element;
}

ProjectM36Connection.prototype.generateRelationBody = function (body)
{
    var bodyrows = body["asList"]
    var tbody = document.createElement("tbody");
    for(var rindex=0; rindex < bodyrows.length; rindex++)
    {
	var tablerow = document.createElement("tr");
	var bodyrow = bodyrows[rindex];
	for(var atomindex=0; atomindex < bodyrow[1].length; atomindex++)
	{
            var td = document.createElement("td");
            var atomNode = this.generateAtom(bodyrow[1][atomindex]);
            td.appendChild(atomNode);
            tablerow.appendChild(td);
	}
	//special case- mark row with class attribute if it contains no columns
	if(bodyrow[1].length == 0)
	{
	    tablerow.setAttribute("class", "emptyrow");	    
	}
	tbody.appendChild(tablerow);
    }
    return tbody;
}

ProjectM36Connection.prototype.generateAtom = function(atom)
{
    var element = document.createElement("span");
    var atomType = atom["type"]["tag"];
    
    if(atomType == "RelationAtomType")
    {
	var rel = this.generateRelation(atom["val"]);
        element.appendChild(rel);
    }
    else if(atomType == "ScientificAtomType")
    {
	element.textContent = atom["val"][0] + "*10^" + atom["val"][1];
    }
    else
    {
	element.textContent = atom["val"];
    }
    return element;
}

ProjectM36Connection.prototype.generateDataFrame = function(dataframe)
{
    for(var attrIndex=0; attrIndex < dataframe.attributes.length; attrIndex++)
    {
	var attr = dataframe.attributes[attrIndex];
	var ordering = "↕";
	for(var orderIndex=0; orderIndex < dataframe.orders.length; orderIndex++)
	{
	    var order = dataframe.orders[orderIndex];
	    if(order[0] == attr[0])
	    {
		if(order[1] == "AscendingOrder")
		{
		    ordering = "⬆";
		}
		else if(order[1] == "DescendingOrder")
		{
		    ordering = "⬇";
		}
	    }
	}
	dataframe.attributes[attrIndex].push(ordering);   
    }
    return this.generateRelation([dataframe.attributes,
				  {asList:dataframe.tuples}]);
}

//special case to make sure that a table appears for "true" and "false"
function mungeEmptyRows()
{
    var empties = document.getElementsByClassName("emptyrow");
    for(var index=0; index < empties.length; index++)
    {
	var empty = empties[index];
	if(empty.childNodes.length == 0)
	{
	    var elementType = "td";
	    if(empty.parentNode.nodeName == "THEAD")
	    {
		elementType = "th";
	    }
	    var dudHeader = document.createElement(elementType);
	    dudHeader.innerHTML = "&nbsp;";
	    empty.appendChild(dudHeader);
	}
    }
}
