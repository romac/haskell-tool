
object HelloWorld {

  def main(): Unit = {
    println(true);
    println("hello");
  }

}

class Value {

  var hello: Int;
  var world: String;

  def init(h: Int, w: String): String = {
    hello = h;
    world = w;

    return w;
  }

  def doStuff(): Int = {
    var stuff: Int;

    while (hello) {
      stuff = this.doOtherStuff(hello);
    }

    return stuff;
  }

}

