class Nonsense {
   constructor() {
      this.arbitraryArray = ['string with no meaning', 42];
   }

   doesThisMakeSense() {
      return false;
   }

   answer(answer) {
      console.log(answer);
   }

   whatIsTheMeaning() {
      return this.arbitraryArray[this.getRandomIndex()]
   }

   getRandomIndex() {
      return Math.floor(Math.random() * 2);
   }
}

(function () {
   console.log((-~function(){}-~function(){})); // 2, now that's weird
   console.log(Array(16).join("lol" - 2) + " Batman!"); // ;)


   const nonsense = new Nonsense();

   while(nonsense.doesThisMakeSense()) { // No, it doesn't
      nonsense.answer(nonsense.whatIsTheMeaning());
   }
})();


