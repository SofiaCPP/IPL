var sayHello = Fn.new { System.print("Hello, world!") }

sayHello.call() //> Hello, world!
/* And a comment! */

var raw = """ RAW STRING """
var raw2 = """
 Electric Boogaloo 
"""

class Wren {
  flyTo(city) {
    System.print("Flying to %(city)")
  }
}

var numbers = [0, 1234, -5678, 3.14159, 1.0, -12.34, 0.0314159e02, 0.0314159e+02, 314.159e-02, 0xcaffe2]

var adjectives = Fiber.new {
  ["small", "clean", "fast"].each {|word| Fiber.yield(word) }
}

while (!adjectives.isDone) System.print(adjectives.call())

var trees = ["cedar", "birch", "oak", "willow"]
System.print(trees[0]) //> cedar
System.print(trees[-1]) //> willow

var birds = {
  "Arizona": "Cactus wren",
  "Ohio": "Northern Cardinal"
}

for (bird in birds) {
  System.print("The state bird of %(bird.key) is %(bird.value)")
}
