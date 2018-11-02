/*
** This is a test function to demonstrate syntax highlighting!
*/
function myFunction() {
    var res = "";
    res = res + isNaN(123) + ": 123";
    res = res + isNaN(-1.23) + ": -1.23";
    res = res + isNaN(5-2) + ": 5-2";
    res = res + isNaN(0) + ": 0";
    res = res + isNaN('123') + ": 123
	"; 
	//oops!
    res = res + isNaN(true) + ": true";
    res = res + isNaN(undefined) + ": undefined";
    res = res + isNaN('NaN') + ": NaN";
    res = res + isNaN(NaN) + ": NaN";
    res = res + isNaN(0 / 0) + ": 0 / 0";

    document.getElementById("demo").innerHTML = res;
}
//Cool, it works!