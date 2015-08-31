
object Eval {
  def main(): Unit = {
    println(new Evaluator().evalExample());
  }
}

class Expr {
  def eval(): Int = {
    println("Cannot evaluate unknown expr.");
    return 0;
  }
}

class Lit extends Expr {
  var value: Int;
  def init(v: Int): Lit = {
    value = v;
    return this;
  }
  def eval(): Int = {
    return value;
  }
}

class Add extends Expr {
  var left: Expr;
  var right: Expr;
  def init(l: Expr, r: Expr): Add = {
    left = l;
    right = r;
    return this;
  }
  def eval(): Int = {
    return left.eval() + right.eval();
  }
}

class Prod extends Expr {
  var left: Expr;
  var right: Expr;
  def init(l: Expr, r: Expr): Prod = {
    left = l;
    right = r;
    return this;
  }
  def eval(): Int = {
    return left.eval() * right.eval();
  }
}

class Evaluator {

  def evalExample(): Int = {
    var c: Expr;
    c = new Prod().init(
      new Lit().init(12),
      new Prod().init(
        new Lit().init(2),
        new Lit().init(5)
      )
    );
    return c.eval();
  }

}
