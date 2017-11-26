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
var ProjectM36Connection = function (protocol, host, port, path, dbname, openCallback, errorCallback, statusCallback, promptCallback, notificationCallback, closeCallback) {
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
	self.socket.send("connectdb:" + self.dbname);
	openCallback(event);
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
    this.statuscallback = statusCallback;
    this.promptcallback = promptCallback;
    this.notificationcallback = notificationCallback;
    this.socket = socket;
}

/**
* The argument returned as part of the 'statusCallback'. Typically, only one of the three status types is populated. These status updates are groups together so that the callback can feed into a user interface update function.
* @callback statusCallback
* @param {Object} relationResult - The status update containing a relation or null.
* @param {Object} acknowledgementresult - The status update containing an acknowledgement that the query was executed or null.
* @param {Object} errorResult - The status update containing the error information or null.
*/
var ProjectM36Status = function (relationResult, acknowledgementResult, errorResult)
{
    this.relation = relationResult;
    this.acknowledgement = acknowledgementResult;
    this.error = errorResult;
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
    var relation = message["displayrelation"];
    var acknowledged = message["acknowledged"];
    var error = message["displayerror"];
    var prompt = message["promptInfo"];
    var notification = message["notificationname"]

    if(relation)
    {
        this.statuscallback(new ProjectM36Status(relation['json'], null, null));
    }
    
    if(acknowledged)
    {
	this.statuscallback(new ProjectM36Status(null, true, null));
    }
    
    if(error)
    {
	if(error.tag)
	{
	    error=error.tag; // for error objects
	}
	this.statuscallback(new ProjectM36Status(null, null, error['json']));
    }

    if(prompt)
    {
	this.promptcallback(prompt["headname"], prompt["schemaname"]);
    }

    if(notification)
    {
	var evaldnotif = message.evaldnotification;
	this.notificationcallback(message.notificationname, evaldnotif);
    }
}

/**
* Executes a TutorialD string.
* @param {string} tutd - The TutorialD string.
*/
ProjectM36Connection.prototype.executeTutorialD = function(tutd)
{
    this.socket.send("executetutd/json:" + tutd);
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
    for(var hindex=0; hindex < header.length; hindex++)
    {
	var th = document.createElement("th");
	var attrtype = this.generateAtomType(header[hindex]);
	th.appendChild(attrtype);
	headerrow.appendChild(th);
    }
    //special case- if there are no attributes (the "true" and "false" relations, then leave a class marker so that the table can be styled to appear regardless
    if(header.length == 0)
    {
	headerrow.setAttribute("class", "emptyrow");
    }
    thead.appendChild(headerrow);
    return thead;
}

ProjectM36Connection.prototype.generateAtomType = function(attr)
{
    var atomType = attr[1]["tag"];
    var attrName = attr[0];
    var element = document.createElement("span");
    element.textContent = attrName + "::";
    if (atomType == "RelationAtomType")
    {
	var table = document.createElement("table");
	element.appendChild(table);			       
	var thead = document.createElement("thead");
	table.appendChild(thead);
	var tr = document.createElement("tr");
	thead.appendChild(tr);
	var relattrs = attr[1]["contents"];
	for(var attrindex = 0; attrindex < relattrs.length; attrindex++)
	{
	    var th = document.createElement("th");
	    var relattrNode = this.generateAtomType(relattrs[attrindex]);
	    th.appendChild(relattrNode);
	    tr.appendChild(th);
	}
    }
    else
    {
	var readableType = atomType.slice(0,-8);
	element.textContent += readableType;
    }
    return element
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
    else
    {
	element.textContent = atom["val"];
    }
    return element;
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