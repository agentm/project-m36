//refactor this into a client library
var socket = new WebSocket("ws://localhost:8888");
socket.onopen = function(event) {
    console.log("connected");
    setStatus("connected");
}
socket.onerror = function(event) {
    console.log("error: " + event);
}
socket.onmessage = function (event) {
    console.log("message received: " + event.data);
    var msg = JSON.parse(event.data);
    handleResponse(msg);
}
socket.onclose = function(event) {
    console.log("conn closed: " + event);
    setStatus("disconnected");
}

function setStatus(text)
{
    var newtext = document.createTextNode(text);
    var statusnode = document.getElementById("status");
    while( statusnode.firstChild ) {
	statusnode.removeChild( statusnode.firstChild );
    }
    statusnode.appendChild(newtext);
}

function execTutorialD()
{
    var tutd = document.getElementById("tutd").value;
    socket.send("executetutd:" + tutd);
    return false;
}

function handleResponse(message)
{
    var relation = message["displayrelation"];
    var acknowledged = message["acknowledged"];
    var error = message["displayerror"];
    if(relation)
    {
	var reltable = generateRelation(relation);
	var existingtable = document.getElementById("relationresult");
	while(existingtable.firstChild)
	{
	    existingtable.removeChild(existingtable.firstChild);
	}
	reltable.id = "relationresult";
	existingtable.parentNode.replaceChild(reltable, existingtable);
    }
    else
    {
    }
    
    var message = "";
    if(acknowledged)
    {
	message="acknowledged";
    }
    if(error)
    {
	message=error;
    }
    document.getElementById("resultstatus").innerHTML = message;
}

function generateRelation(relation)
{
    var table = document.createElement("table");
    
    var headers = generateRelationHeader(relation[0]);
    var body = generateRelationBody(relation[1]);
    
    table.appendChild(headers);
    table.appendChild(body);
    
    return table;
}

function generateRelationHeader(header)
{
    var thead = document.createElement("thead");
    var headerrow = document.createElement("tr");
    for(var hindex=0; hindex < header.length; hindex++)
    {
	var th = document.createElement("th");
	var attrtype = generateAtomType(header[hindex]);
	th.appendChild(attrtype);
	headerrow.appendChild(th);
    }
    thead.appendChild(headerrow);
    return thead;
}

function generateAtomType(attr)
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
	    var relattrNode = generateAtomType(relattrs[attrindex]);
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

function generateRelationBody(body)
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
            var atomNode = generateAtom(bodyrow[1][atomindex]);
            td.appendChild(atomNode);
            tablerow.appendChild(td);
	}
	tbody.appendChild(tablerow);
    }
    return tbody;
}

function generateAtom(atom)
{
    var element = document.createElement("span");
    var atomType = atom["type"]["tag"];
    
    if(atomType == "RelationAtomType")
    {
	var rel = generateRelation(atom["val"]);
        element.appendChild(rel);
    } 
    else
    {
	element.textContent = atom["val"];
    }
    return element;
}

