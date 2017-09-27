function appendResult(title, result)
{
    //prepend the page with an additional relation result
    var sheet = document.getElementById("sheet");
    var template = document.getElementById("sectiontemplate").cloneNode(true);
    template.removeAttribute("id");
    var titleSpan = document.createElement("span");
    titleSpan.textContent = title;
    template.getElementsByClassName("title")[0].appendChild(titleSpan);
    template.getElementsByClassName("result")[0].appendChild(result);
    if(result.nodeName == "TABLE") // show some relation statistics
    {
	var tupleCount = result.querySelectorAll(".result > table > tbody > tr").length
	var attrCount = result.querySelectorAll(".result > table > thead > tr > th").length
	var attrText = attrCount + " attribute" + (attrCount == 1 ? "" : "s")
	var tupleText = tupleCount + " tuple" + (tupleCount == 1 ? "" : "s")
	template.getElementsByClassName("relinfo")[0].textContent = attrText + ", " + tupleText;
    }
    var interactor = document.getElementById("interactor");
    sheet.insertBefore(template, interactor);
    window.scrollTo(0,document.body.scrollHeight);
}

function updateStatus(status)
{
    var tutd = document.getElementById("tutd").value;
    if(status.relation)
    {
	var relastable = conn.generateRelation(status.relation);
	appendResult(tutd, relastable);
	mungeEmptyRows();
    }
    if(status.acknowledgement)
    {
	var ok = document.createElement("span");
	ok.textContent="OK";
	appendResult(tutd, ok);
    }
    if(status.error)
    {
	var error = document.createElement("span");
	error.textContent=status.error;
	appendResult(tutd, error);
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
					       connectionOpened,
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
    appendResult("Connect", err);
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
    var disableElements = [form.elements["protocol"],
			  ]
    var enableElements = [document.getElementById("eval")];

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

function connectionOpened(event)
{
    toggleConnectionFields(document.getElementById("connection"), false, "Disconnect");
}

function execTutorialD()
{
    var tutd = document.getElementById("tutd").value;
    if(!window.conn || window.conn.readyState() != 1)
    {
	var err = document.createElement("span");
	err.textContent = "Cannot execute command until a database connection is established.";
	appendResult(tutd, err);
    }
    else
    {
	conn.executeTutorialD(tutd);
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