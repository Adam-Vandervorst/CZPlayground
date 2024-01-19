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
      if s.tail.head == '`' then
        try
          data.addV(js.eval(s.tail.tail.init).asInstanceOf[js.Any])
        catch
          case e: js.JavaScriptException => throw RuntimeException(f"${e.exception} received while parsing $s")
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

//abstract class ValueEvaluationAlgorithms[V]:
//  import ExprExamples.*
//
//  def handleLookup(emv: V, ev: V): V
//  def handleMerge(fv: V, av: V): V
//
//  def lookupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    val r = s.transform(Expr(`=`, e, $), Var(-e.nvarsN - 1))
//    r.map(w => handleLookup(w, v))
//
//  def lookupBackupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    val nv = lookupMulti(e, v)
//    if nv.isEmpty then ExprMap(e -> v)
//    else nv
//
//  def bottomUpMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    e.foldMap(i => lookupBackupMulti(Var(i), v), (fem, aem) => ExprMap.from(fem.items.flatMap((f, fv) => aem.items.flatMap((a, av) => lookupBackupMulti(App(f, a), handleMerge(fv, av)).items))))
//
//  def evalMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(bottomUpMulti(_, _).items)), _.keys.toSet)(ExprMap[V](e -> v))
//
//  def lookup(e: Expr, v: V)(using s: ExprMap[V]): Option[(Expr, V)] =
//    val es = lookupMulti(e, v)
//    if es.size > 1 then throw RuntimeException(s"Nonlinear ${e.show}, results: ${es.keys.map(_.show).mkString(",")}")
//    else es.items.headOption
//
//  def lookupBackup(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
//    lookup(e, v).getOrElse(e -> v)
//
//  def bottomUp(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
//    e.foldMap(i => lookupBackup(Var(i), v), {case ((f, fv), (a, av)) => lookupBackup(App(f, a), handleMerge(fv, av))})
//
//  def eval(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
//    fixproject[(Expr, V), Expr](bottomUp, _._1)(e -> v)
//
//  def bottomUpMultiGrounded(e: Expr, v: V)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//    var newv = 0
//    var doeval = true
//    e.foldMap(i =>
//      if i > 0 then lookupBackupMulti(Var(i), v)
//      else
//        val r = ExprMap(Var(i) -> v)
//        if i == 0 then newv += 1
//        if i < -newv then doeval = false
//        r
//      ,
//      (fem, aem) =>
//        ExprMap.from(
//          fem.items.flatMap((f, fv) => f match
//            case Var(g(grounded)) =>
//              grounded(aem).items
//            case _ =>
//              aem.items.flatMap((a, av) =>
//                if doeval then lookupBackupMulti(App(f, a), handleMerge(fv, av)).items
//                else List(App(f, a) -> handleMerge(fv, av)))
//          )
//        )
//    )
//
//  def evalGrounded(em: ExprMap[V])(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//
//    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(bottomUpMultiGrounded(_, _).items)), _.keys.toSet)(em)
//
//  def evalGrounded(e: Expr, v: V)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//    evalGrounded(ExprMap[V](e -> v))
//
//  def evalGroundedN(e: Expr, v: V, N: Int)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//    var s = ExprMap[V](e -> v)
//    for _ <- 1 to N do
//      s = ExprMap.from(s.items.flatMap(bottomUpMultiGrounded(_, _).items))
//    s
//
//  def apply(e: Expr)(using s: ExprMap[V]): ExprMap[V]
//
//  def unapply(e: Expr)(using s: ExprMap[V]): Option[(Expr, V)]


extension (inline em: ExprMap[Unit])
  inline def varMap(inline f: Int => Int): ExprMap[Unit] =
    val ks = em.em.vars.keys.toArray.map(x => f(x.toInt).toLong)
    val vs = Array.fill[Unit](ks.length)(())
    ExprMap(EM(ExprMap(), collection.mutable.LongMap.fromZip[Unit](ks, vs)))

  inline def varFlatMap(inline f: Int => IterableOnce[Int]): ExprMap[Unit] =
    val ks = em.em.vars.keys.toArray.flatMap(x => f(x.toInt).iterator.map(_.toLong))
    val vs = Array.fill[Unit](ks.length)(())
    ExprMap(EM(ExprMap(), collection.mutable.LongMap.fromZip[Unit](ks, vs)))

  inline def varFilter(inline f: Int => Boolean): ExprMap[Unit] =
    val ks = em.em.vars.keys.toArray.filter(x => f(x.toInt))
    val vs = Array.fill[Unit](ks.length)(())
    ExprMap(EM(em.em.apps, collection.mutable.LongMap.fromZip[Unit](ks, vs)))


@main def m =
//  val root = dom.document.querySelector("#board").asInstanceOf[dom.html.Div]
//  val tn = org.scalajs.dom.document.createTextNode("Test")
//  root.appendChild(tn)

  val ev = ValueEvaluationAlgorithms.ignore[Unit]
  given em: ExprMap[Unit]()

  val eq_id = DataParser.symbols.add("=")
  val transform_id = DataParser.symbols.add("transform")
  val ground_id = DataParser.symbols.add("ground")

  val pfs = collection.mutable.Map.empty[Int, ExprMap[Unit] => ExprMap[Unit]]
  var pc = 10000
  given PartialFunction[Int, ExprMap[Unit] => ExprMap[Unit]] = {
    case `ground_id` => _.varMap { i => // name
        pc += 1
//        val name = DataParser.symbols.get(i).get.tail.init
        pfs(pc) = _.varMap { j => // function
          val ev = DataParser.data.get(j).get
          assert(js.typeOf(ev) == "function")
          val func = ev.asInstanceOf[js.Function]
          val base = pfs.getOrElse(i, _ => ExprMap())
          val recurrence = func.length
          pfs(i) = arg1 => arg1.varFilter(!DataParser.data.couldContain(_)).execute(Iterator.single(Instr.Apply(i))) union arg1.varFlatMap { k =>
            if !DataParser.data.couldContain(k) then Iterator.single(k)
            else
              val kv = DataParser.data.get(k).get
              if recurrence == 1 then
                func.call(js.Object(), kv).asInstanceOf[js.Array[js.Any]].map(DataParser.data.add)
              else
                pc += 1
                val arg2_id = pc
                pfs(pc) = arg2 => arg2.varFilter(!DataParser.data.couldContain(_)).execute(Iterator.single(Instr.Apply(arg2_id))) union arg2.varFlatMap { kk =>
                  if !DataParser.data.couldContain(kk) then Iterator.empty
                  else
                    val kkv = DataParser.data.get(kk).get
                    if recurrence == 2 then
                      func.call(js.Object(), kv, kkv).asInstanceOf[js.Array[js.Any]].map(DataParser.data.add)
                    else
                      pc += 1
                      val arg3_id = pc
                      pfs(pc) = arg3 => arg3.varFilter(!DataParser.data.couldContain(_)).execute(Iterator.single(Instr.Apply(arg3_id))) union arg3.varFlatMap { kkk =>
                        if !DataParser.data.couldContain(kkk) then Iterator.single(kkk)
                        else
                          val kkkv = DataParser.data.get(kkk).get
                          if recurrence == 3 then
                            func.call(js.Object(), kv, kkv, kkkv).asInstanceOf[js.Array[js.Any]].map(DataParser.data.add)
                          else
                            throw RuntimeException("Not implemented arity 4")
                      }
                      Iterator.single(pc)
                }
                Iterator.single(pc)
          }
          j
        }
        pc
      }
    case `transform_id` => pattern =>
      ExprMap.from(pattern.items.map((e1, s1) =>
        pc += 1
        pfs(pc) = template => ExprMap.from(template.items.flatMap((e2, s2) =>
          em.transform(e1, e2).items
        ))
        Var(pc) -> ()
      ))
    case pfs(handler) => handler
  }

  DataParser.sexprs(
    """
      |(= A `"a1")
      |(= A `"a2")
      |(= B `"b1")
      |(= B `"b2")
      |(= (f $n) (S $n))
      |(FONT_SIZE `12)
      |(CORNER (Point2D `50 `10))
      |(= (upto $n) (range `0 $n `1))
      |(= (positions $n) (Point2D `0 (* (transform (FONT_SIZE $x) (* `1.2 $x)) (upto $n))))
      |(= (+ (Point2D $x1 $y1) (Point2D $x2 $y2)) (Point2D (+ $x1 $x2) (+ $y1 $y2)))
      |""".stripMargin)

  println(ev.evalGrounded(DataParser.sexpr("(ground * \"`(x, y) => [x*y]\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(ground + \"`(x, y) => [x+y]\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(* `.1 `.05)".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(ground range \"`(start, stop, step) => Array.from({ length: (stop - start) / step + 1}, (_, i) => start + (i * step))\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(range `0 `10 `1)".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(+ A B)".iterator).getOrElse(throw RuntimeException("no results")), ()).prettyListing(false))
//  println(em.prettyListing())
//  println(ev.evalGrounded(DataParser.sexpr("(f `3)".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(* `1.2 `12)".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(transform (FONT_SIZE $x) (* `1.2 $x))".iterator).getOrElse(throw RuntimeException("no results")), ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(transform (FONT_SIZE $x) (* `1.2 $x))".iterator).getOrElse(throw RuntimeException("no results")), ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(* `3.14 (range `0 $n `1))".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(upto `3)".iterator).get, ()).prettyStructuredSet())
  println(ev.evalGrounded(DataParser.sexpr("(positions `3)".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(ground logged \"`x => {console.log('g', x); return x; }\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(logged `\"test\")".iterator).get, ()).prettyListing(false))
  println(ev.evalGrounded(DataParser.sexpr("(logged (+ (transform (CORNER $x) $x) (positions `3)))".iterator).get, ()).prettyListing(false))


//  println(ev.evalGrounded(DataParser.sexpr("(eval \"12\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"3.14\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"\\\"test\\\"\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"x => x * x\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"x => x * x\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"2*4\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(ground ² \"`x => Math.pow(x, 2)\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(² `0.1)".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(ground * \"`(x, y) => x*y\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(* `0.1 `0.05)".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(ground + \"`(x, y) => x+y\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(+ `\"Hello\" `\"World\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded({
//    given arg: ExprMap[Unit]()
//    DataParser.sexprs(
//      """
//        |(+ `"Hello" `"World")
//        |(+ `"Hi" `"World")
//        |(+ `"Hello" `"Universe")
//        |(+ `"Hi" `"Universe")
//        |""".stripMargin)
//    println(arg.prettyListing())
//    println(arg.prettyStructuredSet())
//    arg
//  }).prettyListing(false))
  println(pfs)
  println(DataParser.symbols.indexToValue)
  println(DataParser.data.indexToValue)


