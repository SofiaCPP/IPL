function sum(a, b, c, d) {
    let sum1  = 0;
    let sum2  = 0;
    let sum3  = 0;
    let sum4  = 0;
    for(var i = 0; i < 100; i++){
        sum1 += a;
        sum2 *= b;
        sum3 **= c;
        sum4 = sum1 << sum2 >> sum3;
    }
    return 42;
}