/*
    Author: Filip Stoyanov
    Date: 27/10/2022
*/

const arr = ["Java", "JavaScript", "C", "C++", "Haskell"];
function findElement(array, number) {
  return array.indexOf(number); // this function find element in array
}

const javaIndex = findElement(arr, "Java");
console.log(javaIndex);

for (let i = 0; i < arr.length; ++i) {
  console.log(`${arr[i]}` + arr[i]);
}
