
// --------------------------------------------------------------------- : Running

var input_id = "#input";

// Display something in the answer box
function answer(cls, text) {
	$("#answer").attr("class",cls).html(text);
}

// Run the given input
function run(input) {
	answer("working","Working...");
	$.ajax({
		url: 'moltap.cgi',
		type: 'post',
		data: {term: input},
		dataType: "json",
		success: function(d) {
			// successfull run, display result
			var res = '';
			if (d.modelImage) {
				res += "<div class='model'>";
				res += "<img alt='counter model' src='" + d.modelImage + "'>";
				for (i in d.modelPos) {
					var p = d.modelPos[i];
					p.r += i==0 ? 4 : 1;
					var l = p.x - p.r;
					var t = p.y - p.r;
					var w = 2 * p.r;
					res += "<img id='t"+p.node+"' class='node' style='left:"+l+"px;top:"+t+"px;width:"+w+"px;height:"+w+"px;' src='style/circle-green.png'>";
					res += "<img id='f"+p.node+"' class='node' style='left:"+l+"px;top:"+t+"px;width:"+w+"px;height:"+w+"px;' src='style/circle-red.png'>";
				}
				res += "</div>";
				nodes = d.modelPos;
			}
			if (d.result == 'error') {
				res += d.text;
			} else {
				res += "<div class='truth'>" + d.result + "</div>";
				res += "<div class='term'>" + d.text + "</div>";
			}
			answer(d.result, res);
			if ($.browser.msie) return; // ie sucks
			$("#answer .nest").each(function() {
				this.addEventListener('mouseover',nestEnter,true);
				this.addEventListener('mouseout', nestExit,false);
			});
			overStack = [];
		},
		error: function(xhttp, text, err) {
			answer('error', 'Connection error<br>' + text + (err ? err : ''));
		}
	});
}

var nodes     = [];
var overStack = [];
function updateModel() {
	var v = overStack[overStack.length-1] || "";
	for (i in nodes) {
		var p = v.indexOf(nodes[i].node);
		document.getElementById("t"+nodes[i].node).style.display = p > 0 && v[p-1] == "t" ? 'block' : 'none';
		document.getElementById("f"+nodes[i].node).style.display = p > 0 && v[p-1] == "f" ? 'block' : 'none';
	}
}
function nestEnter() {
	$(this.parentNode).addClass('over');
	overStack.push(this.getAttribute('truth'));
	updateModel();
}
function nestExit() {
	$(this.parentNode).removeClass('over');
	overStack.pop();
	updateModel();
}

// Run the contents of the input box
function runInput() {
	var v = $(input_id).val();
	if (v == "") {
		answer("error", "Please enter a term");
	} else {
		run(v);
	}
}

// Run an example
function example(input) {
	if (input.indexOf("\n")!=-1) showInput(true,true);
	$(input_id).val(input);
	run(input);
}

// --------------------------------------------------------------------- : UI

// Show or hide the large box
function moreInput() {
	var more = input_id=="#input";
	if (more) {
		$('#input2').val($('#input').val());
		$('#input').val('');
	} else {
		$('#input').val($('#input2').val());
		$('#input2').val('');
	}
	showInput(more,true);
}

$('#ui').ready(function(){
	if ($('#input2').val() && !$('#input').val()) {
		showInput(true,false);
	}
});

function showInput(more, slide) {
	if (more == (input_id=="#input2")) return;
	input_id = more ? "#input2" : "#input";
	$("#more-input-btn").html(more ? "&uarr;" : "&darr;");
	$("#more-input-btn").attr('title', (more ? "Collapse" : "Expand") + " input box");
	if (slide) {
		$(more ? '#more-input' : '#less-input').slideDown();
		$(more ? '#less-input' : '#more-input').slideUp();
	} else {
		$(more ? '#more-input' : '#less-input').show();
		$(more ? '#less-input' : '#more-input').hide();
	}
}

// --------------------------------------------------------------------- : Fancy styling

$("h1").ready(function(){
	var h1 = $("h1");
	h1.html("<span class='shadow'>" + h1.html() + "</span><span>" + h1.html() + "</span>");
});

function highlight_cur() {
	var match = window.location.hash.match(/\#(.*)/);
	if (match) {
		$(".highlight").removeClass("highlight");
		$("a[name="+match[1]+"]").parent().addClass("highlight");
	}
}
$(document).ready(highlight_cur);
$(document).ready(function(){
	$("a[href]").click(function(){setTimeout(highlight_cur,1)});
});

// --------------------------------------------------------------------- : Examples

var threeHatsPre = "# There are only two blue hats\n"
                  + "let two_hats = (b1&b2 -> ~b3) & (b3&b1 -> ~b2) & (b2&b3 -> ~b1);\n\n"
                  + "# Each agent can see the colors of the other agents' hats\n"
                  + "let see_other = \n"
                  + "   (K2 b1 | K2 ~b1) & (K3 b1 | K3 ~b1) &\n"
                  + "   (K3 b2 | K3 ~b2) & (K1 b2 | K1 ~b2) &\n"
                  + "   (K1 b3 | K1 ~b3) & (K2 b3 | K2 ~b3);\n\n"
                  + "# All agents know the rules\n"
                  + "K* (two_hats & see_other)\n\n";
var threeHatsPost = "# Does any agent know the color of his own hat?\n"
                  + "-> K1 b1 | K1 ~b1 | K2 b2 | K2 ~b2 | K3 b3 | K3 ~b3\n";

var threeHats1 = threeHatsPre + threeHatsPost;

var threeHats2 = threeHatsPre
               + "# What if there are two blue hats?\n"
               + "& b1 & b2\n\n"
               + threeHatsPost;

var threeHats3 = threeHatsPre
               + "# There is at most one blue hat\n"
               + "& K* ( ~(b1 & b2) & ~(b2 & b3) & ~(b3 & b1) )\n\n"
               + threeHatsPost;

var threeHats4 = threeHatsPre
               + "# There are no blue hats\n"
               + "& K* ( ~b1 & ~b2 & ~b3 )\n\n"
               + threeHatsPost;
