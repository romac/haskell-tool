
object Matrix {
  def main(): Unit = {
    println(new Mat().run());
  }
}

class Mat {
  var size: Int;
  var values: Int[];

  def init(s: Int, initial: Int): Mat = {
    var i: Int;
    size = s;
    values = new Int[size * size];

    i = 0;
    while(i < size * size) {
      values[i] = initial;
      i = i + 1;
    }

    return this;
  }

  def add(other: Mat): Mat = {
    var result: Mat;
    var i: Int;

    result = new Mat().init(size, 0);

    i = 0;
    while(i < size * size) {
      result = result.set(i, values[i] + other.get(i));
      i = i + 1;
    }

    return result;
  }

  def set(index: Int, value: Int): Mat = {
    values[index] = value;
    return this;
  }

  def get(index: Int): Int = {
    return values[index];
  }

  def dump(): String = {
    var i: Int;
    var j: Int;
    var row: String;
    row = "";
    i = 0;
    j = 0;
    while(i < size) {
      while(j < size) {
        row = row + " " + values[i * size + j];
        j = j + 1;
      }
      println(row);
      row = "";
      i = i + 1;
      j = 0;
    }

    return "Ok";
  }

  def run(): String = {
    var a: Mat;
    var b: Mat;
    var c: Mat;

    a = new Mat().init(4, 1);
    b = new Mat().init(4, 2);
    c = a.add(b);

    return c.dump();
  }


}
