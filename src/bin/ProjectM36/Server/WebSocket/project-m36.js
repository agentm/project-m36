var ProjectM36Connection = function (host, port, dbname, openCallback, errorCallback, statusCallback, promptCallback, closeCallback) {
    this.host = host
    this.port = port
    this.dbname = dbname
    var connectURL = "ws://" + host + ":" + port + "/";
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
    this.socket = socket;
}

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

    if(relation)
    {
        this.statuscallback(new ProjectM36Status(relation, null, null));
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
	this.statuscallback(new ProjectM36Status(null, null, error));
    }

    if(prompt)
    {
	this.promptcallback(prompt["headname"], prompt["schemaname"]);
    }
}

ProjectM36Connection.prototype.executeTutorialD = function(tutd)
{
    this.socket.send("executetutd:" + tutd);
}

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