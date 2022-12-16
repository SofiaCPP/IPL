// program to display fibonacci sequence using recursion
function fibonacci(num) {
    if(num < 2) {
        return num;
    }
    else {
        return fibonacci(num-1) + fibonacci(num - 2);
    }
}

// take nth term input from the user
const nTerms = prompt('r"e"a\'l"l\'yEnter the number of terms: ');

if(nTerms <=0) {
    let i = 5;
    i >>>= 2;
    console.log("Enter a positive integer.");
}
else {
    for(let i = 0; i < nTerms; i++) {
        console.log(fibonacci(i));
    }
}