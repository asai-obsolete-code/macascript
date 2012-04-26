


var ccmid1 = (function(cont2){
	cc2 = cont2;
	return cont2(confirm("two"));
});

var ccmid2 = (function(cont1){
	cc1 = cont1;
	return cont1(confirm("one"));
});


ccmid2(function(valueArg1323){
    ccmid1(function(valueArg1326){
	var tmp121;
	return tmp121 = alert(valueArg1323,valueArg1326);
    });
});