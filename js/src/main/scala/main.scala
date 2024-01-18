import org.scalajs.dom

import scala.scalajs.js

import be.adamv.cz2.*


object DataParser extends Parser:
  override val empty: Int = 1
  override val singleton: Int = 2

  val symbols = RangeStorage[String](100, 1000)
  val data = RangeStorage[js.Any](2 << 24, 2 << 27)

  override def tokenizer(s: String): Expr =
    if s.head == '"' then
      if s.tail.head == '`' then data.addV(js.eval(s.tail.tail.init).asInstanceOf[js.Any])
      else symbols.addV(s)
    else
      if s.head == '`' then data.addV(js.eval(s.tail).asInstanceOf[js.Any])
      else symbols.addV(s)

  def sexprs(s: String)(using em: ExprMap[Unit]): Unit =
    val it = s.iterator.buffered
    var last = sexprUnsafe(it)
    while last != null do
      em.update(last, ())
      last = sexprUnsafe(it)


@main def m =
//  val root = dom.document.querySelector("#board").asInstanceOf[dom.html.Div]
//  val tn = org.scalajs.dom.document.createTextNode("Test")
//  root.appendChild(tn)


  val ev = ValueEvaluationAlgorithms.ignore[Unit]
  given em: ExprMap[Unit]()

  val transform_id = DataParser.symbols.add("transform")
  val ground_id = DataParser.symbols.add("ground")
  val eval_id = DataParser.symbols.add("eval")
  val call_id = DataParser.symbols.add("call")

  val pfs = collection.mutable.Map.empty[Int, ExprMap[Unit] => ExprMap[Unit]]
  var pc = 10000
  given PartialFunction[Int, ExprMap[Unit] => ExprMap[Unit]] = {
    case `ground_id` => em =>
      ExprMap.from(em.keys.collect { case Var(i) if DataParser.symbols.get(i).nonEmpty =>
        pc += 1
        val name = DataParser.symbols.get(i).get.tail.init
        pfs(pc) = em2 => ExprMap.from(em2.keys.collect { case Var(j) =>
          val ev = DataParser.data.get(j).get
          assert(js.typeOf(ev) == "function")
          val func = ev.asInstanceOf[js.Function]
          val base = pfs.getOrElse(i, _ => ExprMap())
          val recurrence = func.length
          pfs(i) = em => base(em).union(ExprMap.from(em.keys.collect{
            case Var(k) =>
              val kv = DataParser.data.get(k).get
              if recurrence == 1 then
                DataParser.data.addV(func.call(js.Object(), kv)) -> ()
              else
                pc += 1
                pfs(pc) = em => ExprMap.from(em.keys.collect{
                  case Var(kk) =>
                    val kkv = DataParser.data.get(kk).get
                    if recurrence == 2 then
                      DataParser.data.addV(func.call(js.Object(), kv, kkv)) -> ()
                    else
                      pc += 1
                      pfs(pc) = em => ExprMap.from(em.keys.collect {
                        case Var(kkk) =>
                          val kkkv = DataParser.data.get(kkk).get
                          if recurrence == 3 then
                            DataParser.data.addV(func.call(js.Object(), kv, kkv, kkkv)) -> ()
                          else
                            throw RuntimeException("Not implemented arity 4")
                      })
                      Var(pc) -> ()
                })
                Var(pc) -> ()
          }))
          Var(j) -> ()
        })
        Var(pc) -> ()
      })
    // (call "$x * $y")
    // (= (* $x $y) (call "_1*_2" $x $y))
//    case `call_id` => js.Function("")()
    //    case `eval_id` => em =>
    //      ExprMap.from(em.items.collect{
    //        case (Var(i), ()) if DataParser.strings.get(i).nonEmpty =>
    //          val s = DataParser.strings.get(i).get.tail.init
    //          val r = js.eval(s)
    //          println(js.typeOf(r))
    //          js.typeOf(r) match
    //            case "number" => DataParser.floats.addV(r.asInstanceOf[Double].toFloat) -> ()
    //            case "string" => DataParser.strings.addV(r.asInstanceOf[String]) -> ()
    //            case _ => DataParser.symbols.addV(r.toString) -> ()
    //      })

    case `transform_id` => em =>
      ExprMap.from(em.items.map((e1, s1) =>
        pc += 1
        pfs(pc) = em2 => ExprMap.from(em2.items.flatMap((e2, s2) =>
          em.transform(e1, e2).items
        ))
        Var(pc) -> ()
      ))
    case pfs(handler) => handler
  }

//  DataParser.sexprs(
//    """
//      |(FONT_SIZE 12)
//      |(= (positions $n) (0, (* (transform (FONT_SIZE $x) (* 1.2 $x)) (range 0 $n 1)))))
//      |""".stripMargin)


//  println(ev.evalGrounded(DataParser.sexpr("(eval \"12\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"3.14\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"\\\"test\\\"\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"x => x * x\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"x => x * x\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"2*4\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(ground ² \"`x => Math.pow(x, 2)\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(² `0.1)".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(ground * \"`(x, y) => x*y\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(* `0.1 `0.05)".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(ground + \"`(x, y) => x+y\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(+ `\"Hello\" `\"World\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded({
    given arg: ExprMap[Unit]()
    DataParser.sexprs(
      """
        |(+ `"Hello" `"World")
        |(+ `"Hi" `"World")
        |(+ `"Hello" `"Universe")
        |(+ `"Hi" `"Universe")
        |""".stripMargin)
    println(arg.prettyListing())
    println(arg.prettyStructuredSet())
    arg
  }).prettyListing(false))
  println(pfs)
  println(DataParser.symbols.indexToValue)
  println(DataParser.data.indexToValue)
