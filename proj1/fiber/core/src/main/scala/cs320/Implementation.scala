package cs320

import Value._

object Implementation extends Template {

  def interp(expr: Expr): Value = internalInterp(expr, Map())

  def asBoundedIdentifier(name: String, env: Env): Value = env.get(name) match {
    case Some(value) => value
    case None => error(s"Runtime Error: Unbound identifier: $name")
  }

  def asIntV(value: Value): IntV = value match {
    case v: IntV => v
    case _ => error(s"Runtime Error: Expected IntV, found: $value")
  }

  def asBooleanV(value: Value): BooleanV = value match {
    case v: BooleanV => v
    case _ => error(s"Runtime Error: Expected BooleanV, found: $value")
  }

  def asTupleV(value: Value): TupleV = value match {
    case v: TupleV => v
    case _ => error(s"Runtime Error: Expected TupleV, found: $value")
  }

  def asListV(value: Value): Value = value match {
    case v @ NilV => v
    case v @ ConsV(_, _) => v
    case _ => error(s"Runtime Error: Expected ListV, found: $value")
  }

  def asCloV(value: Value): CloV = value match {
    case v: CloV => v
    case _ => error(s"Runtime Error: Expected CloV, found: $value")
  }

  def internalInterp(expr: Expr, env: Env): Value = expr match {
    case IntE(value) => IntV(value)
    case BooleanE(value) => BooleanV(value)
    case Id(name) => asBoundedIdentifier(name, env)

    case Add(le, re) =>
      val IntV(lv) = asIntV(internalInterp(le, env))
      val IntV(rv) = asIntV(internalInterp(re, env))
      IntV(lv + rv)
    case Mul(le, re) =>
      val IntV(lv) = asIntV(internalInterp(le, env))
      val IntV(rv) = asIntV(internalInterp(re, env))
      IntV(lv * rv)
    case Div(le, re) =>
      val IntV(lv) = asIntV(internalInterp(le, env))
      val IntV(rv) = asIntV(internalInterp(re, env))
      if (rv == 0) error("division by zero")
      else IntV(lv / rv)
    case Mod(le, re) =>
      val IntV(lv) = asIntV(internalInterp(le, env))
      val IntV(rv) = asIntV(internalInterp(re, env))
      if (rv == 0) error("division by zero")
      else IntV(lv % rv)

    case Eq(le, re) =>
      val IntV(lv) = asIntV(internalInterp(le, env))
      val IntV(rv) = asIntV(internalInterp(re, env))
      BooleanV(lv == rv)
    case Lt(le, re) =>
      val IntV(lv) = asIntV(internalInterp(le, env))
      val IntV(rv) = asIntV(internalInterp(re, env))
      BooleanV(lv < rv)

    case If(cond, tBranch, fBranch) =>
      val BooleanV(cValue) = asBooleanV(internalInterp(cond, env))
      if (cValue) internalInterp(tBranch, env) else internalInterp(fBranch, env)

    case TupleE(exprs) =>
      val values = for (expr <- exprs) yield internalInterp(expr, env)
      TupleV(values)
    case Proj(expr, index) =>
      val TupleV(values) = asTupleV(internalInterp(expr, env))
      if (index < 1 || index > values.length)
        error(s"Runtime Error: Tuple index $index out of range")
      else values(index - 1)

    case NilE => NilV
    case ConsE(head, tail) =>
      val hValue = internalInterp(head, env)
      val tValue = asListV(internalInterp(tail, env))
      ConsV(hValue, tValue)
    case Empty(expr) =>
      asListV(internalInterp(expr, env)) match {
        case NilV => BooleanV(true)
        case _ => BooleanV(false)
      }
    case Head(expr) =>
      asListV(internalInterp(expr, env)) match {
        case ConsV(h, _) => h
        case NilV => error("Runtime Error: Cannot take head of empty list")
      }
    case Tail(expr) =>
      asListV(internalInterp(expr, env)) match {
        case ConsV(_, t) => t
        case NilV => error("Runtime Error: Cannot take tail of empty list")
      }

    case Val(name, expr, body) =>
      val value = internalInterp(expr, env)
      internalInterp(body, env + (name -> value))
    case Fun(parm, body) =>
      CloV(parm, body, env)
    case RecFuns(funs, body) =>
      var dummyEnv: Env = Map()
      val funEnv = funs.map(f => f.name -> internalInterp(Fun(f.parameters, f.body), dummyEnv))
      dummyEnv = env ++ funEnv
      internalInterp(body, dummyEnv)
    case App(fun, arg) =>
      val CloV(parm, body, fenv) = asCloV(internalInterp(fun, env))
      if (parm.length != arg.length)
        error(s"Runtime Error: Function expected ${parm.length} arguments, found ${arg.length}")
      val argEnv = for ((p, a) <- parm.zip(arg)) yield (p -> internalInterp(a, env))
      internalInterp(body, fenv ++ argEnv)
    case Test(expr, typ) => interp(expr) match {
      case IntV(_) => BooleanV(typ == IntT)
      case BooleanV(_) => BooleanV(typ == BooleanT)
      case TupleV(_) => BooleanV(typ == TupleT)
      case NilV => BooleanV(typ == ListT)
      case ConsV(_, _) => BooleanV(typ == ListT)
      case CloV(_, _, _) => BooleanV(typ == FunctionT)
    }
  }
}