


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


var cc1,cc2,cc3,tmp121;
(function(cont3){ /* indentation level: 0 */
    cc3 = cont3;
    return cont3(confirm("three"));
})(function(valueArg3343){ /* indentation level: 0 */
    return (function(cont2){ /* indentation level: 0 */
	cc2 = cont2;
	return cont2(confirm("two"));
    })(function(valueArg3350){ /* indentation level: 1 */
	return (function(cont1){ /* indentation level: 0 */
	    cc1 = cont1;
	    return cont1(confirm("one"));
	})(function(valueArg3357){ /* indentation level: 2 */
	    return alert(valueArg3343,valueArg3350,5,valueArg3357,4);
	});
    });
});
console.log(cc1.toString());
cc1("one-again");
/* this will call cont2 and cont3 again */;
console.log(cc2.toString());
cc2("two-again");
/* this will call cont3 again */;
console.log(cc3.toString());
cc3("three-again");
/* this will call alert */;