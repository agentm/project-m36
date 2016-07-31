var ProjectM36Connection = function (host, port, openCallback, errorCallback, statusCallback, closeCallback) {
    this.host = host
    this.port = port
    var socket = new WebSocket("ws://" + host + ":" + port);
    socket.onopen = function(event) { openCallback(event); };
    socket.onerror = function(event) { errorCallback(event); };
    var self = this;
    socket.onmessage = function(event) {
	var msg = JSON.parse(event.data);
	self.handleResponse(msg);
    };
    socket.onclose = function(event) {
	closeCallback(event);
    };
    this.statuscallback = statusCallback;
    this.socket = socket;
}

var ProjectM36Status = function (relationResult, acknowledgementResult, errorResult)
{
    this.relation = relationResult;
    this.acknowledgement = acknowledgementResult;
    this.error = errorResult;
}

ProjectM36Connection.prototype.handleResponse = function(message)
{
    var relation = message["displayrelation"];
    var acknowledged = message["acknowledged"];
    var error = message["displayerror"];
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
	this.statuscallback(new ProjectM36Status(null, null, error));
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
    thead.appendChild(headerrow);
    return thead;
}

ProjectM36Connection.prototype.generateAtomType = function(attr)
{
    var atomType = attr[1]["tag"];
    var attrName = attr[0];
    var element = document.createElement("span");
    element.textContent = attrName + "::";
    if(atomType == "AtomType")
    {
	element.textContent += attr[1]["contents"]["serialrep"][0][2];
    }
    else if (atomType == "RelationAtomType")
    {
	var table = document.createElement("table");
	element.appendChild(table);			       
	var thead = document.createElement("thead");
	element.appendChild(thead);
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
	element.textContent = "unknown";
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

