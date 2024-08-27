//when we send a request, we generate a pending request block to fill in later when we receive the result
function createPendingResult(requestId, title)
{
    var sheet = document.getElementById("sheet");
    var template = document.getElementById("sectiontemplate").cloneNode(true);
    template.removeAttribute("id");
    var titleSpan = document.createElement("span");
    titleSpan.textContent = title;
    template.id = "request-" + requestId
    template.getElementsByClassName("title")[0].appendChild(titleSpan);
    template.getElementsByClassName("result")[0].textContent = 'Request Pending';
    var interactor = document.getElementById("interactor");
    sheet.insertBefore(template, interactor);
    window.scrollTo(0,document.body.scrollHeight);
    
}

function appendResult(requestId, title, result)
{
    if(requestId)
    {
	const requestResult = document.getElementById("request-" + requestId);
	if(!requestResult)
	{
	    //throw new Error('Failed to find pending request block');
	    console.log('Failed to find pending request block');
	}
	const resultEl = requestResult.getElementsByClassName("result")[0];
	resultEl.textContent='';
	resultEl.appendChild(result);
	if(result.nodeName == "TABLE") // show some relation statistics
	{
	    var tupleCount = result.querySelectorAll(".result > table > tbody > tr").length
	    var attrCount = result.querySelectorAll(".result > table > thead > tr > th").length
	    var attrText = attrCount + " attribute" + (attrCount == 1 ? "" : "s")
	    var tupleText = tupleCount + " tuple" + (tupleCount == 1 ? "" : "s")
	    requestResult.getElementsByClassName("relinfo")[0].textContent = attrText + ", " + tupleText;
	    window.scrollTo(0,document.body.scrollHeight);
	}
    }
    else //message without a pending result such as connection error
    {
	//make up a request id
	const reqId = conn.makeUUID();
	createPendingResult(reqId, 'Error');
	appendResult(reqId, 'Error', result);
    }
}

function updateStatus(status)
{
    var tutd = document.getElementById("tutd").value;
    if(status instanceof ProjectM36SessionCreated)
    {
	sessionId = status.sessionId;
    }
    else if(status instanceof ProjectM36PromptInfo)
    {
	promptUpdate(status.headName, status.schemaName);
    }
    else if(status.relation)
    {
	var relastable = conn.generateRelation(status.relation);
	appendResult(status.requestId, tutd, relastable);
	mungeEmptyRows();
    }
    else if(status.dataframe)
    {
	var dataframeastable = conn.generateDataFrame(status.dataframe);
	appendResult(status.requestId, tutd, dataframeastable);
	mungeEmptyRows();
    }
    else if(status.acknowledgement)
    {
	var ok = document.createElement("span");
	ok.textContent="ok";
	appendResult(status.requestId, tutd, ok);
    }
    else if(status.error)
    {
	var error = document.createElement("span");
	error.textContent=status.error;
	appendResult(status.requestId, tutd, error);
    }
    else
    {
	throw new Error('unknown status')
    }
}

function promptUpdate(headName, schemaName)
{
    document.getElementById("promptinfo").textContent = "Current Branch: (" + headName + ") Schema: (" + schemaName + ")";
}

function relationErrorAsString(errObj)
{
    return errObj.tag + " " + errObj.contents;
}

function notificationUpdate(notificationName, evaldNotif)
{
    //generate two tables for display
    var notifDiv = document.createElement('div');
    var oldInfo = document.createElement('div');
    oldInfo.textContent = "Pre-change info: ";
    var newInfo = document.createElement('div');
    newInfo.textContent = "Post-change info: ";
    var status = "Notification received: " + notificationName;

    if(evaldNotif.reportOldRelation.Left)
    {
	var errOld = evaldNotif.reportOldRelation.Left;
	oldInfo.textContent += relationErrorAsString(errOld);
    }
    else if(evaldNotif.reportOldRelation.Right)
    {
	oldInfo.appendChild(conn.generateRelation(evaldNotif.reportOldRelation.Right));
    }

    if(evaldNotif.reportNewRelation.Left)
    {
	var errNew = evaldNotif.reportNewRelation.Left;
	newInfo.textContent += relationErrorAsString(errNew);
    }
    else if(evaldNotif.reportNewRelation.Right)
    {
	newInfo.appendChild(conn.generateRelation(evaldNotif.reportNewRelation.Right));
    }

    notifDiv.appendChild(oldInfo);
    notifDiv.appendChild(document.createElement('br'));
    notifDiv.appendChild(newInfo);
    appendResult(status,notifDiv);
    mungeEmptyRows();
}

var conn;

function connectOrDisconnect(form)
{
    var formin = form.elements;
    var protocol = formin["protocol"].value;
    var host = formin["host"].value;
    var port = formin["port"].value;
    var path = formin["path"].value;
    var dbname = formin["dbname"].value;

    var conninfo = document.getElementById("conninfo");
    var promptInfo = document.getElementById("promptinfo");
    
    if(window.conn && window.conn.readyState() == 1)
    {
	//disconnect
	window.conn.close();
	conninfo.textContent = "Connect to:";
	
	toggleConnectionFields(form, true, "Connect");
	promptInfo.textContent = "";
    }
    else
    {
	//connect
	conninfo.textContent = "Connected to:";
	window.conn = new ProjectM36Connection(protocol, host, port, path,
					       dbname,
					       connectionReady,
					       connectionError,
					       updateStatus,
					       promptUpdate,
					       notificationUpdate,
					       connectionClosed);
	toggleConnectionFields(form, false, "Connecting...");
    }
    return false;
}

function connectionError(event)
{
    var err = document.createElement("span");
    err.textContent = "Failed to connect to websocket server. Please check the connection parameters and try again.";
    appendResult(null, "Connect", err);
    connectionClosed(event)
}

function connectionClosed(event)
{
    toggleConnectionFields(document.getElementById("connection"), true, "Connect");
}

function toggleConnectionFields(form, enabled, status)
{
    form.elements["connect"].value = status;
    var readonlyElements = [form.elements["host"], 
			    form.elements["port"], 
			    form.elements["dbname"],
			    form.elements["path"]];
    const console = document.getElementById('tutd');
    var disableElements = [form.elements["protocol"],
			   console]

    var enableElements = [document.getElementById("eval"),console];
    
    for(var ein=0; ein < readonlyElements.length; ein++)
    {
	var e = readonlyElements[ein];
	if(enabled)
	{
	    e.removeAttribute("readonly");
	}
	else
	{
	    e.setAttribute("readonly", "readonly");
	}
    }
    //also update the eval/submit button
    for(var ein=0; ein < disableElements.length; ein++)
    {
	var e = disableElements[ein];
	if(enabled)
	{
	    e.removeAttribute("disabled");	    
	}
	else
	{
	    e.setAttribute("disabled", "disabled");

	}
    }

    for(var ein=0; ein < enableElements.length; ein++)
    {
	var e = enableElements[ein];
	if(!enabled)
	{
	    e.removeAttribute("disabled");	    
	}
	else
	{
	    e.setAttribute("disabled", "disabled");

	}
    }



}

function connectionReady(event)
{
    toggleConnectionFields(document.getElementById("connection"), false, "Disconnect");
    conn.createSessionAtHead("master");
}

var sessionId = null;

function execTutorialD()
{
    var tutd = document.getElementById("tutd").value;
    if(!window.conn || window.conn.readyState() != 1)
    {
	var err = document.createElement("span");
	err.textContent = "Cannot execute command until a database connection is established.";
	appendResult(null,tutd, err);
    }
    else
    {
	const requestId = conn.executeTutorialD(sessionId, tutd);
	createPendingResult(requestId, tutd);
	
    }
    return false;
}

function hideResult(element)
{
    var deleteNode = element.parentNode.parentNode;
    element.parentNode.parentNode.parentNode.removeChild(deleteNode)
}

function installSampleHandlers()
{
    var samples = document.querySelectorAll("#samples li");
    for(var idx = 0; idx < samples.length; idx++)
    {
	var tutd = document.getElementById("tutd");
	var el = samples[idx];
	el.onclick = function(el) { 
	    tutd.value = el.target.textContent;
	    flashtutd();
	}
    }
}

function setupDefaultConfig()
{
    var form = document.getElementById("connection");
    for(var key in defaultConfig)
    {
	form.elements[key].value = defaultConfig[key];
    }
}

function pageload()
{
    installSampleHandlers();
    setupDefaultConfig();
}

function copyTutorialD(el)
{
    const expr = el.parentNode.getElementsByClassName('title')[0].textContent;
    const console = document.getElementById("tutd");
    console.value = expr;
    flashtutd()
}

function flashtutd()
{
    const console = document.getElementById("tutd");
    console.classList.remove('flashupdate');
    void console.offsetWidth;
    console.classList.add('flashupdate');
}

function toggleHelp()
{
    const collapser = document.getElementById("helpcollapsible");
    if(collapser.style.visibility == 'visible')
    {
	collapser.style.visibility = 'hidden';
    }
    else
    {
	collapser.style.visibility = 'visible';
    }
}
    
